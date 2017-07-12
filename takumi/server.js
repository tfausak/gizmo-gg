'use strict';

const compression = require('compression');
const cors = require('cors');
const express = require('express');
const knex = require('knex');
const moment = require('moment');
const morgan = require('morgan');
const pg = require('pg');

// Application

const app = express();
app.disable('x-powered-by');
app.use(compression());
app.use(cors());
app.use(morgan('tiny'));

// Database

pg.types.setTypeParser(20, (val) => parseInt(val, 10));
const db = knex({
  client: 'pg',
  connection: process.env.TAKUMI_DATABASE || 'postgres://'
});
db.on('query', (query) => console.log(`${query.sql} -- [${query.bindings}]`));

// Constants

const arenaNamesToIgnore = [
  'HoopsStadium_P',
  'labs_circlepillars_p',
  'labs_cosmic_p',
  'labs_cosmic_v4_p',
  'labs_doublegoal_p',
  'labs_Octagon_02_P',
  'labs_Octagon_P',
  'labs_underpass_p',
  'labs_utopia_p',
  'Neotokyo_p',
  'stadium_winter_p'
];
const bodyNamesToIgnore = [
  'Armadillo',
  'Hogsticker',
  'Sweet Tooth'
];

// const startOfSeason2 = moment('2016-02-10', 'YYYY-MM-DD');
// const startOfSeason3 = moment('2016-06-20', 'YYYY-MM-DD');
// const startOfSeason4 = moment('2017-03-22', 'YYYY-MM-DD');
const startOfSeason5 = moment('2017-07-05', 'YYYY-MM-DD');

const playStationPlatformName = 'PlayStation';
const steamPlatformName = 'Steam';
const xboxPlatformName = 'Xbox';
const allPlatformNames = [
  playStationPlatformName,
  steamPlatformName,
  xboxPlatformName
];

const soloDuelPlaylistId = 10;
const doublesPlaylistId = 11;
const soloStandardPlaylistId = 12;
const standardPlaylistId = 13;
const competitivePlaylistIds = [
  soloDuelPlaylistId,
  doublesPlaylistId,
  soloStandardPlaylistId,
  standardPlaylistId
];

const playlistNames = {
  [doublesPlaylistId]: 'ranked2v2',
  [soloDuelPlaylistId]: 'ranked1v1',
  [soloStandardPlaylistId]: 'ranked3v3solo',
  [standardPlaylistId]: 'ranked3v3'
};

const arcTemplateName = 'Starbase ARC';
const standardTemplateName = 'Standard';
const wastelandTemplateName = 'Wasteland';
const competitiveTemplateNames = [
  arcTemplateName,
  standardTemplateName,
  wastelandTemplateName
];

// Helpers

const getCutoffTime = (req) => {
  switch (req.query.time) {
  case 'day': return moment().subtract(1, 'day');
  case 'week': return moment().subtract(1, 'week');
  case 'month': return moment().subtract(1, 'month');
  default: return startOfSeason5;
  }
};

const getPlatformNames = (req) => {
  switch (req.query.platform) {
  case 'playstation': return [playStationPlatformName];
  case 'steam': return [steamPlatformName];
  case 'xbox': return [xboxPlatformName];
  default: return allPlatformNames;
  }
};

const getPlaylistIds = (req) => {
  switch (req.query.playlist) {
  case playlistNames[soloDuelPlaylistId]: return [soloDuelPlaylistId];
  case playlistNames[doublesPlaylistId]: return [doublesPlaylistId];
  case playlistNames[soloStandardPlaylistId]: return [soloStandardPlaylistId];
  case playlistNames[standardPlaylistId]: return [standardPlaylistId];
  default: return competitivePlaylistIds;
  }
};

const getTemplateNames = (req) => {
  switch (req.query.map) {
  case 'arc': return [arcTemplateName];
  case 'standard': return [standardTemplateName];
  case 'wasteland': return [wastelandTemplateName];
  default: return competitiveTemplateNames;
  }
};

const getUploadState = (upload) => {
  if (upload.replay_id) {
    return 'success';
  }
  if (upload.finished_parsing_at) {
    return 'failure';
  }
  return 'pending';
};

// Handlers

const getArenasHandler = (_req, res, next) => {
  db
    .select(
      'arenas.id',
      'arenas.name',
      'arenas.template_id as templateId',
      'arena_templates.name as templateName',
      'arenas.model_id as modelId',
      'arena_models.name as modelName',
      'arenas.skin_id as skinId',
      'arena_skins.name as skinName')
    .from('arenas')
    .leftOuterJoin(
      'arena_templates', 'arena_templates.id', 'arenas.template_id')
    .leftOuterJoin('arena_models', 'arena_models.id', 'arenas.model_id')
    .leftOuterJoin('arena_skins', 'arena_skins.id', 'arenas.skin_id')
    .orderBy('arenas.name', 'asc')
    .then((arenas) => res.json(arenas))
    .catch((err) => next(err));
};

const getArenaStatsHandler = (req, res, next) => {
  const cutoff = getCutoffTime(req);
  const playlists = getPlaylistIds(req);
  const templates = getTemplateNames(req);

  db
    .with('totals', (totals) => {
      totals
        .select('games.arena_id')
        .count('games.id as games')
        .sum('games_players.score as score')
        .sum('games_players.goals as goals')
        .sum('games_players.assists as assists')
        .sum('games_players.saves as saves')
        .sum('games_players.shots as shots')
        .from('games_players')
        .innerJoin('games', 'games.id', 'games_players.game_id')
        .whereIn('games.playlist_id', playlists)
        .where('games.played_at', '>=', cutoff.format())
        .groupBy('games.arena_id');
    })
    .select(
      'arenas.id as arenaId',
      'arenas.name as arenaName',
      db.raw('coalesce(totals.games, 0) as "numGames"'),
      db.raw('coalesce(totals.score, 0) as "totalScore"'),
      db.raw('coalesce(totals.goals, 0) as "totalGoals"'),
      db.raw('coalesce(totals.assists, 0) as "totalAssists"'),
      db.raw('coalesce(totals.saves, 0) as "totalSaves"'),
      db.raw('coalesce(totals.shots, 0) as "totalShots"'))
    .from('arenas')
    .innerJoin('arena_templates', 'arena_templates.id', 'arenas.template_id')
    .leftOuterJoin('totals', 'totals.arena_id', 'arenas.id')
    .whereNotIn('arenas.name', arenaNamesToIgnore)
    .whereIn('arena_templates.name', templates)
    .orderBy('arenas.name', 'asc')
    .then((arenas) => res.json(arenas))
    .catch((err) => next(err));
};

const getBodyStatsHandler = (req, res, next) => {
  const cutoff = getCutoffTime(req);
  const playlists = getPlaylistIds(req);
  const templates = getTemplateNames(req);

  db
    .with('totals', (totals) => {
      totals
        .select('games_players.body_id')
        .count('games_players.game_id as games')
        .select(db.raw(
          'count(case when games_players.did_win then 1 end) as wins'))
        .select(db.raw(
          'count(case when not games_players.did_win then 1 end) as losses'))
        .sum('games_players.score as score')
        .sum('games_players.goals as goals')
        .sum('games_players.assists as assists')
        .sum('games_players.saves as saves')
        .sum('games_players.shots as shots')
        .from('games_players')
        .innerJoin('games', 'games.id', 'games_players.game_id')
        .innerJoin('arenas', 'arenas.id', 'games.arena_id')
        .innerJoin(
          'arena_templates', 'arena_templates.id', 'arenas.template_id')
        .whereIn('games.playlist_id', playlists)
        .where('games.played_at', '>=', cutoff.format())
        .whereNotIn('arenas.name', arenaNamesToIgnore)
        .whereIn('arena_templates.name', templates)
        .groupBy('games_players.body_id');
    })
    .select(
      'bodies.id as bodyId',
      'bodies.name as bodyName',
      db.raw('coalesce(totals.games, 0) as "numGames"'),
      db.raw('coalesce(totals.wins, 0) as "numWins"'),
      db.raw('coalesce(totals.losses, 0) as "numLosses"'),
      db.raw('coalesce(totals.score, 0) as "totalScore"'),
      db.raw('coalesce(totals.goals, 0) as "totalGoals"'),
      db.raw('coalesce(totals.assists, 0) as "totalAssists"'),
      db.raw('coalesce(totals.saves, 0) as "totalSaves"'),
      db.raw('coalesce(totals.shots, 0) as "totalShots"'))
    .from('bodies')
    .leftOuterJoin('totals', 'totals.body_id', 'bodies.id')
    .whereNotIn('bodies.name', bodyNamesToIgnore)
    .orderBy('bodies.name', 'asc')
    .then((bodies) => res.json(bodies))
    .catch((err) => next(err));
};

const getSummaryStatsHandler = (req, res, next) => {
  const cutoff = getCutoffTime(req);
  const playlists = getPlaylistIds(req);

  const getArenaStats = db
    .with('totals', (totals) => {
      totals
        .select('games.arena_id')
        .count('games.id as games')
        .from('games')
        .whereIn('games.playlist_id', playlists)
        .where('games.played_at', '>=', cutoff.format())
        .groupBy('games.arena_id');
    })
    .select('arenas.name')
    .select('totals.games as games')
    .from('arenas')
    .leftOuterJoin('totals', 'totals.arena_id', 'arenas.id')
    .whereNotIn('arenas.name', arenaNamesToIgnore)
    .orderBy('arenas.name');
  const getBodyStats = db
    .with('totals', (totals) => {
      totals
        .select('games_players.body_id')
        .count('games_players.id as players')
        .from('games_players')
        .innerJoin('games', 'games.id', 'games_players.game_id')
        .whereIn('games.playlist_id', playlists)
        .where('games.played_at', '>=', cutoff.format())
        .groupBy('games_players.body_id');
    })
    .select('bodies.name')
    .select('totals.players as players')
    .from('bodies')
    .leftOuterJoin('totals', 'totals.body_id', 'bodies.id')
    .whereNotIn('bodies.name', bodyNamesToIgnore)
    .orderBy('bodies.name');
  const getGameStats = db
    .count('games.id as total')
    .select(db.raw(
      'count(case when games.blue_win then 1 end) as blue_wins'))
    .select(db.raw(
      'count(case when not games.blue_win then 1 end) as orange_wins'))
    .from('games')
    .where('games.played_at', '>=', cutoff.format())
    .whereIn('games.playlist_id', playlists);

  Promise
    .all([getArenaStats, getBodyStats, getGameStats])
    .then(([arenas, bodies, [games]]) => {
      const numGames = games.total;
      const numPlayers = bodies.reduce((sum, body) => sum + body.players, 0);

      res.json({
        bodyFreqPct: bodies.reduce((object, body) => {
          object[body.name] = body.players / (numPlayers || 1);
          return object;
        }, {}),
        mapFreqPct: arenas.reduce((object, arena) => {
          object[arena.name] = arena.games / (numGames || 1);
          return object;
        }, {}),
        numGames,
        winPct: {
          blue: games.blue_wins / (numGames || 1),
          orange: (games.orange_wins || 0) / (numGames || 1)
        }
      });
    })
    .catch((err) => next(err));
};

const getSearchHandler = (req, res, next) => {
  const name = req.query.name || '';
  const pattern = `%${name.replace(/[_%\\]/g, '\\$&')}%`;
  const platforms = getPlatformNames(req);

  db
    .select(
      db.raw('distinct on (players.id) players.id'),
      'players.platform_id as platformId',
      'platforms.name as platformName',
      'players.remote_id as remoteId',
      'players.local_id as localId',
      'games_players.name',
      'games_players.xp',
      'games.played_at as lastSeen')
    .from('games_players')
    .innerJoin('games', 'games.id', 'games_players.game_id')
    .innerJoin('players', 'players.id', 'games_players.player_id')
    .innerJoin('platforms', 'platforms.id', 'players.platform_id')
    .where('games_players.name', 'ilike', pattern)
    .whereIn('platforms.name', platforms)
    .limit(20)
    .then((results) => res.json(results))
    .catch((err) => next(err));
};

const getUploadHandler = (req, res, next) => {
  db
    .select('finished_parsing_at', 'replay_id')
    .from('uploads')
    .where('id', req.params.id)
    .then((uploads) => uploads.length === 1 ? uploads[0] : next())
    .then((upload) => {
      if (!upload.replay_id) {
        return { replay: null, upload };
      }
      return db
        .select('game_id')
        .from('replays')
        .where('id', upload.replay_id)
        .then((replays) => replays.length === 1 ? replays[0] : next())
        .then((replay) => ({ replay, upload }));
    })
    .then(({ replay, upload }) => res.json({
      gameId: replay ? replay.game_id : null,
      state: getUploadState(upload)
    }))
    .catch((err) => next(err));
};

const getPlayerPollHandler = (req, res, next) => {
  db
    .max('games.played_at as lastPlayedAt')
    .from('games')
    .innerJoin('games_players', 'games_players.game_id', 'games.id')
    .where('games_players.player_id', req.params.id)
    .then((results) => results.length === 1 ? results[0] : next())
    .then((result) => {
      // This formatting is only necessary to match what Paladin returns. Once
      // all endpoints are served by Takumi this can just return
      // `res.json(result.lastPlayedAt)`.
      const lastPlayedAt = moment(result.lastPlayedAt);
      return res.json(lastPlayedAt.utc().format('YYYY-MM-DDTHH:mm:ss'));
    })
    .catch((err) => next(err));
};

const getPlayerRankHandler = (req, res, next) => {
  const cutoff = getCutoffTime(req);
  const playlists = getPlaylistIds(req);

  db
    .select('created_at', 'playlist_id', 'mmr', 'tier', 'division')
    .from('player_skills')
    .where('player_id', req.params.id)
    .whereIn('playlist_id', playlists)
    .where('created_at', '>=', cutoff.format())
    .orderBy('created_at', 'desc')
    .then((results) => results ? results : next())
    .then((results) => results.reduce(
      (object, skill) => {
        const key = playlistNames[skill.playlist_id];
        const value = {
          at: skill.created_at,
          division: skill.division,
          mmr: skill.mmr,
          playlist: skill.playlist_id,
          tier: skill.tier
        };

        if (object[key]) {
          if (value.mmr !== object[key][object[key].length - 1].mmr) {
            object[key].push(value);
          }
        } else {
          object[key] = [value];
        }

        return object;
      },
      {}))
    .then((results) => res.json(results))
    .catch((err) => next(err));
};

const getPlayerHistoryHandler = (req, res, next) => {
  const cutoff = getCutoffTime(req);
  const playlists = getPlaylistIds(req);
  const templates = getTemplateNames(req);

  db
    .select('games.played_at as at')
    .select(db.raw(`
      case when games_players.is_blue
      then games.blue_goals
      else games.orange_goals
      end as my_goals`))
    .select(db.raw(`
      case when games_players.is_blue
      then games.orange_goals
      else games.blue_goals
      end as their_goals`))
    .from('games_players')
    .innerJoin('games', 'games.id', 'games_players.game_id')
    .innerJoin('arenas', 'arenas.id', 'games.arena_id')
    .innerJoin('arena_templates', 'arena_templates.id', 'arenas.template_id')
    .where('games_players.player_id', req.params.id)
    .where('games_players.is_present_at_end', true)
    .where('games_players.played_at', '>=', cutoff)
    .whereIn('games_players.playlist_id', playlists)
    .whereIn('arena_templates.name', templates)
    .orderBy('games.played_at', 'desc')
    .then((results) => res.json(results))
    .catch((err) => next(err));
};

// Default handlers

const notFound = (_req, res) => res.status(404).json(null);
const internalServerError = (err, _req, res, _next) => {
  console.error(err);
  res.status(500).json(null);
};
const notImplemented = (_req, res) => res.status(501).json(null);

// Routes

app.get('/arenas', getArenasHandler);
app.get('/games/:id', notImplemented);
app.get('/search', getSearchHandler);
app.get('/stats/arenas', getArenaStatsHandler);
app.get('/stats/bodies', getBodyStatsHandler);
app.get('/stats/players/:id', notImplemented);
app.get('/stats/players/:id/arenas', notImplemented);
app.get('/stats/players/:id/bodies', notImplemented);
app.get('/stats/players/:id/history', getPlayerHistoryHandler);
app.get('/stats/players/:id/poll', getPlayerPollHandler);
app.get('/stats/players/:id/rank', getPlayerRankHandler);
app.get('/stats/summary', getSummaryStatsHandler);
app.post('/uploads', notImplemented);
app.get('/uploads/:id', getUploadHandler);

// Default routes

app.use(notFound);
app.use(internalServerError);

// Server

app.listen(8080, () => console.log('Listening on port 8080 ...'));

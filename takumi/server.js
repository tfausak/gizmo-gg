'use strict';

const compression = require('compression');
const cors = require('cors');
const express = require('express');
const knex = require('knex');
const moment = require('moment');
const morgan = require('morgan');
const pg = require('pg');

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
    .orderBy('arenas.name', 'asc');
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
    .orderBy('bodies.name', 'asc');
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
    .then((uploads) => uploads.length === 1 ? uploads[0] : Promise.reject())
    .then((upload) => {
      if (!upload.replay_id) {
        return { replay: null, upload };
      }
      return db
        .select('game_id')
        .from('replays')
        .where('id', upload.replay_id)
        .then((replays) => replays.length === 1 ? replays[0] : Promise.reject())
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
    .then((results) => results.length === 1 ? results[0] : Promise.reject())
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
    .then((results) => results ? results : Promise.reject())
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
    .select('games.played_at as playedAt')
    .select(db.raw(`
      case when games_players.is_blue
      then games.blue_goals
      else games.orange_goals
      end as myGoals`))
    .select(db.raw(`
      case when games_players.is_blue
      then games.orange_goals
      else games.blue_goals
      end as theirGoals`))
    .from('games_players')
    .innerJoin('games', 'games.id', 'games_players.game_id')
    .innerJoin('arenas', 'arenas.id', 'games.arena_id')
    .innerJoin('arena_templates', 'arena_templates.id', 'arenas.template_id')
    .where('games_players.player_id', req.params.id)
    .where('games_players.is_present_at_end', true)
    .where('games.played_at', '>=', cutoff.format())
    .whereIn('games.playlist_id', playlists)
    .whereIn('arena_templates.name', templates)
    .orderBy('games.played_at', 'desc')
    .then((results) => res.json(results))
    .catch((err) => next(err));
};

const getPlayerArenasHandler = (req, res, next) => {
  const cutoff = getCutoffTime(req);
  const playlists = getPlaylistIds(req);
  const templates = getTemplateNames(req);

  db
    .with('totals', (totals) => {
      totals
        .select('games.arena_id')
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
        .sum('games.duration as duration')
        .from('games_players')
        .innerJoin('games', 'games.id', 'games_players.game_id')
        .where('games_players.player_id', req.params.id)
        .where('games_players.is_present_at_end', 'true')
        .where('games.played_at', '>=', cutoff.format())
        .whereIn('games.playlist_id', playlists)
        .groupBy('games.arena_id');
    })
    .select(
      'arenas.id as arenaId',
      'arenas.name as arenaName',
      db.raw('coalesce(totals.games, 0) as "numGames"'),
      db.raw('coalesce(totals.wins, 0) as "numWins"'),
      db.raw('coalesce(totals.losses, 0) as "numLosses"'),
      db.raw('coalesce(totals.score, 0) as "totalScore"'),
      db.raw('coalesce(totals.goals, 0) as "totalGoals"'),
      db.raw('coalesce(totals.assists, 0) as "totalAssists"'),
      db.raw('coalesce(totals.saves, 0) as "totalSaves"'),
      db.raw('coalesce(totals.shots, 0) as "totalShots"'),
      db.raw('coalesce(totals.duration, 0) as "totalDuration"'))
    .from('arenas')
    .leftOuterJoin('totals', 'totals.arena_id', 'arenas.id')
    .innerJoin('arena_templates', 'arena_templates.id', 'arenas.template_id')
    .whereNotIn('arenas.name', arenaNamesToIgnore)
    .whereIn('arena_templates.name', templates)
    .orderBy('arenas.name', 'asc')
    .then((arenas) => res.json(arenas))
    .catch((err) => next(err));
};

const getPlayerBodiesHandler = (req, res, next) => {
  const cutoff = getCutoffTime(req);
  const playlists = getPlaylistIds(req);

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
        .sum('games.duration as duration')
        .from('games_players')
        .innerJoin('games', 'games.id', 'games_players.game_id')
        .where('games_players.player_id', req.params.id)
        .where('games_players.is_present_at_end', 'true')
        .where('games.played_at', '>=', cutoff.format())
        .whereIn('games.playlist_id', playlists)
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
      db.raw('coalesce(totals.shots, 0) as "totalShots"'),
      db.raw('coalesce(totals.duration, 0) as "totalDuration"'))
    .from('bodies')
    .leftOuterJoin('totals', 'totals.body_id', 'bodies.id')
    .whereNotIn('bodies.name', bodyNamesToIgnore)
    .orderBy('bodies.name', 'asc')
    .then((bodies) => res.json(bodies))
    .catch((err) => next(err));
};

const getGameHandler = (req, res, next) => {
  db
    .select(
      'arena_models.id as arenaModelId',
      'arena_models.name as arenaModelName',
      'arena_skins.id as arenaSkinId',
      'arena_skins.name as arenaSkinName',
      'arena_templates.id as arenaTemplateId',
      'arena_templates.name as arenaTemplateName',
      'arenas.id as arenaId',
      'arenas.name as arenaName',
      'games.blue_goals as blueGoals',
      'games.duration',
      'games.id',
      'games.orange_goals as orangeGoals',
      'games.played_at as playedAt',
      'games.playlist_id as playlistId',
      'playlists.id as playlistId',
      'playlists.name as playlistName')
    .from('games')
    .innerJoin('playlists', 'playlists.id', 'games.playlist_id')
    .innerJoin('arenas', 'arenas.id', 'games.arena_id')
    .innerJoin('arena_templates', 'arena_templates.id', 'arenas.template_id')
    .innerJoin('arena_models', 'arena_models.id', 'arenas.model_id')
    .leftOuterJoin('arena_skins', 'arena_skins.id', 'arenas.skin_id')
    .where('games.id', req.params.id)
    .then((games) => games.length === 1 ? games[0] : Promise.reject())
    .then((game) => db
      .select(
        'antennas.name as loadoutAntennaName',
        'bodies.name as loadoutBodyName',
        'decals.name as loadoutDecalName',
        'games_players.accent_color_id as loadoutAccentColorId',
        'games_players.accent_finish_id as loadoutAccentFinishId',
        'games_players.angle as cameraAngle',
        'games_players.antenna_id as loadoutAntennaId',
        'games_players.assists',
        'games_players.body_id as loadoutBodyId',
        'games_players.decal_id as loadoutDecalId',
        'games_players.distance as cameraDistance',
        'games_players.fov as cameraFov',
        'games_players.goals',
        'games_players.height as cameraHeight',
        'games_players.is_blue as isOnBlueTeam',
        'games_players.is_present_at_end as isPresentAtEnd',
        'games_players.name',
        'games_players.player_id as playerId',
        'games_players.primary_color_id as loadoutPrimaryColorId',
        'games_players.primary_finish_id as loadoutPrimaryFinishId',
        'games_players.rocket_trail_id as loadoutRocketTrailId',
        'games_players.saves',
        'games_players.score',
        'games_players.shots',
        'games_players.stiffness as cameraStiffness',
        'games_players.swivel_speed as cameraSwivelSpeed',
        'games_players.topper_id as loadoutTopperId',
        'games_players.topper_paint_id as loadoutTopperPaintId',
        'games_players.wheel_id as loadoutWheelId',
        'games_players.wheel_paint_id as loadoutWheelPaintId',
        'games_players.xp',
        'platforms.name as platformName',
        'players.local_id as localId',
        'players.platform_id as platformId',
        'players.remote_id as remoteId',
        'rocket_trails.name as loadoutRocketTrailName',
        'toppers.name as loadoutTopperName',
        'wheels.name as loadoutWheelName')
      .from('games_players')
      .innerJoin('players', 'players.id', 'games_players.player_id')
      .innerJoin('platforms', 'platforms.id', 'players.platform_id')
      .leftOuterJoin('antennas', 'antennas.id', 'games_players.antenna_id')
      .leftOuterJoin('bodies', 'bodies.id', 'games_players.body_id')
      .leftOuterJoin('decals', 'decals.id', 'games_players.decal_id')
      .leftOuterJoin(
        'rocket_trails', 'rocket_trails.id', 'games_players.rocket_trail_id')
      .leftOuterJoin('toppers', 'toppers.id', 'games_players.topper_id')
      .leftOuterJoin('wheels', 'wheels.id', 'games_players.wheel_id')
      .where('games_players.game_id', game.id)
      .orderBy('games_players.player_id', 'asc')
      .then((players) => ({ game, players })))
    .then(({ game, players }) => db
      .select(
        db.raw('distinct on (player_id) player_id as "playerId"'),
        'division',
        'matches_played as matchesPlayed',
        'mmr',
        'tier')
      .from('player_skills')
      .whereIn('player_id', players.map((player) => player.playerId))
      .where('playlist_id', game.playlistId)
      .where('created_at', '>=', moment(game.playedAt).subtract(1, 'week'))
      .where('created_at', '<=', moment(game.playedAt).add(1, 'week'))
      .orderBy('player_id', 'asc')
      .orderBy(
        db.raw('abs(extract(epoch from created_at - ?))', game.playedAt),
        'asc')
      .then((skills) => skills.reduce((object, skill) => {
        object[skill.playerId] = {
          division: skill.division,
          matchesPlayed: skill.matchesPlayed,
          mmr: skill.mmr,
          tier: skill.tier
        };
        return object;
      }, {}))
      .then((skills) => ({ game, players, skills })))
    .then(({ game, players, skills }) => res.json({
      arena: {
        id: game.arenaId,
        modelId: game.arenaModelId,
        modelName: game.arenaModelName,
        name: game.arenaName,
        skinId: game.arenaSkinId,
        skinName: game.arenaSkinName,
        templateId: game.arenaTemplateId,
        templateName: game.arenaTemplateName
      },
      blueGoals: game.blueGoals,
      duration: game.duration,
      id: game.id,
      orangeGoals: game.orangeGoals,
      playedAt: moment(game.playedAt).utc().format('YYYY-MM-DDTHH:mm:ss'),
      players: players.map((player) => ({
        assists: player.assists,
        camera: {
          angle: player.cameraAngle,
          distance: player.cameraDistance,
          fov: player.cameraFov,
          height: player.cameraHeight,
          stiffness: player.cameraStiffness,
          swivelSpeed: player.cameraSwivelSpeed
        },
        goals: player.goals,
        isOnBlueTeam: player.isOnBlueTeam,
        isPresentAtEnd: player.isPresentAtEnd,
        loadout: {
          accentColorId: player.loadoutAccentColorId,
          accentFinishId: player.loadoutAccentFinishId,
          antennaId: player.loadoutAntennaId,
          antennaName: player.loadoutAntennaName,
          bodyId: player.loadoutBodyId,
          bodyName: player.loadoutBodyName,
          decalId: player.loadoutDecalId,
          decalName: player.loadoutDecalName,
          primaryColorId: player.loadoutPrimaryColorId,
          primaryFinishId: player.loadoutPrimaryFinishId,
          rocketTrailId: player.loadoutRocketTrailId,
          rocketTrailName: player.loadoutRocketTrailName,
          topperId: player.loadoutTopperId,
          topperName: player.loadoutTopperName,
          topperPaintId: player.loadoutTopperPaintId,
          wheelId: player.loadoutWheelId,
          wheelName: player.loadoutWheelName,
          wheelPaintId: player.loadoutWheelPaintId
        },
        localId: player.localId,
        name: player.name,
        platformId: player.platformId,
        platformName: player.platformName,
        playerId: player.playerId,
        remoteId: player.remoteId,
        saves: player.saves,
        score: player.score,
        shots: player.shots,
        skill: skills[player.playerId],
        xp: player.xp
      })),
      playlistId: game.playlistId,
      playlistName: game.playlistName
    }))
    .catch((err) => next(err));
};

const getPlayerHandler = (req, res, next) => {
  const TODO = null;
  const playerId = req.params.id;

  db
    .with('sightings', (sightings) => sightings
      .select(
        db.raw('distinct on (games_players.name) games_players.name'),
        'games.played_at as playedAt')
      .from('games_players')
      .innerJoin('games', 'games.id', 'games_players.game_id')
      .where('games_players.player_id', playerId)
      .orderBy('games_players.name', 'asc')
      .orderBy('games.played_at', 'desc'))
    .select()
    .from('sightings')
    .orderBy('sightings.playedAt', 'desc')
    .then(([first, ...rest]) => first
      ? {
        aliases: rest.map((row) => row.name),
        lastPlayedAt: first.playedAt,
        name: first.name
      }
      : Promise.reject())
    .then(({ aliases, lastPlayedAt, name }) => db
      .select('platforms.*')
      .from('platforms')
      .innerJoin('players', 'players.platform_id', 'platforms.id')
      .where('players.id', playerId)
      .then((platforms) => platforms.length === 1
        ? platforms[0]
        : Promise.reject())
      .then((platform) => ({ aliases, lastPlayedAt, name, platform })))
    .then(({ aliases, lastPlayedAt, name, platform }) => db
      .select(
        db.raw('distinct on (playlists.id) playlists.id'),
        'playlists.name as playlistName',
        'player_skills.matches_played as matchesPlayed',
        'player_skills.division',
        'player_skills.tier',
        'player_skills.mmr')
      .from('player_skills')
      .innerJoin('playlists', 'playlists.id', 'player_skills.playlist_id')
      .where('player_skills.player_id', playerId)
      .whereIn('playlists.id', competitivePlaylistIds)
      .orderBy('playlists.id', 'asc')
      .orderBy('player_skills.created_at', 'desc')
      .then((skills) => skills.reduce((object, skill) => {
        object[skill.playlistName] = {
          division: skill.division,
          matchesPlayed: skill.matchesPlayed,
          mmr: skill.mmr,
          tier: skill.tier
        };
        return object;
      }, {}))
      .then((skills) => ({ aliases, lastPlayedAt, name, platform, skills })))
    .then(({ aliases, lastPlayedAt, name, platform, skills }) => res.json({
      aliases,
      games: TODO,
      lastPlayedAt,
      name,
      platform,
      skills
    }))
    .catch((err) => next(err));
};

const getHealthCheckHandler = (_req, res, next) => {
  db
    .select(db.raw('1'))
    .then(() => res.json(null))
    .catch((err) => next(err));
};

// Default handlers

const notFound = (_req, res) => res.status(404).json(null);
const internalServerError = (err, _req, res, _next) => {
  console.error(err);
  res.status(500).json(null);
};
const notImplemented = (_req, res) => res.status(501).json(null);

// Application

express()
  .disable('x-powered-by')
  .use(compression())
  .use(cors())
  .use(morgan('tiny'))
  .get('/arenas', getArenasHandler)
  .get('/games/:id', getGameHandler)
  .get('/health-check', getHealthCheckHandler)
  .get('/search', getSearchHandler)
  .get('/stats/arenas', getArenaStatsHandler)
  .get('/stats/bodies', getBodyStatsHandler)
  .get('/stats/players/:id', getPlayerHandler)
  .get('/stats/players/:id/arenas', getPlayerArenasHandler)
  .get('/stats/players/:id/bodies', getPlayerBodiesHandler)
  .get('/stats/players/:id/history', getPlayerHistoryHandler)
  .get('/stats/players/:id/poll', getPlayerPollHandler)
  .get('/stats/players/:id/rank', getPlayerRankHandler)
  .get('/stats/summary', getSummaryStatsHandler)
  .post('/uploads', notImplemented)
  .get('/uploads/:id', getUploadHandler)
  .use(notFound)
  .use(internalServerError)
  .listen(8080, () => console.log('Listening on port 8080 ...'));

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

pg.types.setTypeParser(20, (x) => parseInt(x, 10));
const db = knex({
  client: 'pg',
  connection: process.env.TAKUMI_DATABASE || 'postgres://'
});
db.on('query', (query) => console.log(`${query.sql} -- [${query.bindings}]`));

// Constants

const arenaNamesToIgnore = ['stadium_winter_p'];

const startOfSeason4 = moment('2017-03-22', 'YYYY-MM-DD');

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
  default: return startOfSeason4;
  }
};

const getPlaylistIds = (req) => {
  switch (req.query.playlist) {
  case 'ranked1v1': return [soloDuelPlaylistId];
  case 'ranked2v2': return [doublesPlaylistId];
  case 'ranked3v3solo': return [soloStandardPlaylistId];
  case 'ranked3v3': return [standardPlaylistId];
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

// Handlers

const getArenas = (_req, res, next) => {
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

const getArenaStats = (req, res, next) => {
  const cutoff = getCutoffTime(req);
  const playlists = getPlaylistIds(req);
  const templates = getTemplateNames(req);

  db
    .with('totals', (totals) => {
      totals
        .select('games.arena_id')
        .countDistinct('games.id as games')
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
    .then((stats) => res.json(stats))
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

app.get('/arenas', getArenas);
app.get('/games/:id', notImplemented);
app.get('/search', notImplemented);
app.get('/stats/arenas', getArenaStats);
app.get('/stats/bodies', notImplemented);
app.get('/stats/players', notImplemented);
app.get('/stats/players/:id/arenas', notImplemented);
app.get('/stats/players/:id/bodies', notImplemented);
app.get('/stats/players/:id/history', notImplemented);
app.get('/stats/players/:id/poll', notImplemented);
app.get('/stats/players/:id/rank', notImplemented);
app.get('/stats/summary', notImplemented);
app.post('/uploads', notImplemented);
app.get('/uploads', notImplemented);

// Default routes

app.use(notFound);
app.use(internalServerError);

// Server

app.listen(8080, () => console.log('Listening on port 8080 ...'));

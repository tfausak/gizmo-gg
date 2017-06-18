'use strict';

const compression = require('compression');
const cors = require('cors');
const express = require('express');
const knex = require('knex');
const morgan = require('morgan');

const app = express();
const db = knex({
  client: 'pg',
  connection: process.env.TAKUMI_DATABASE || 'postgres://'
});
const todo = (res) => res.status(501).json(null);

db.on('query', (query) => console.log(`${query.sql} -- [${query.bindings}]`));
app.disable('x-powered-by');
app.use(compression());
app.use(cors());
app.use(morgan('tiny'));

app.get('/arenas', (_req, res, next) => {
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
    .leftOuterJoin('arena_templates',
      'arenas.template_id', 'arena_templates.id')
    .leftOuterJoin('arena_models',
      'arenas.model_id', 'arena_models.id')
    .leftOuterJoin('arena_skins',
      'arenas.skin_id', 'arena_skins.id')
    .orderBy('arenas.name', 'asc')
    .then((arenas) => res.json(arenas))
    .catch((err) => next(err));
});

app.get('/games/:id', (_req, res) => {
  todo(res);
});

app.get('/search', (_req, res) => {
  todo(res);
});

app.get('/stats/arenas', (_req, res) => {
  todo(res);
});

app.get('/stats/bodies', (_req, res) => {
  todo(res);
});

app.get('/stats/players', (_req, res) => {
  todo(res);
});

app.get('/stats/players/:id/arenas', (_req, res) => {
  todo(res);
});

app.get('/stats/players/:id/bodies', (_req, res) => {
  todo(res);
});

app.get('/stats/players/:id/history', (_req, res) => {
  todo(res);
});

app.get('/stats/players/:id/poll', (_req, res) => {
  todo(res);
});

app.get('/stats/players/:id/rank', (_req, res) => {
  todo(res);
});

app.get('/stats/summary', (_req, res) => {
  todo(res);
});

app.post('/uploads', (_req, res) => {
  todo(res);
});

app.get('/uploads', (_req, res) => {
  todo(res);
});

app.use((_req, res, _next) => {
  res.status(404).json(null);
});

app.use((err, _req, res, _next) => {
  console.error(err);
  res.status(500).json(null);
});

app.listen(8080, () => {
  console.log('Listening on port 8080 ...');
});

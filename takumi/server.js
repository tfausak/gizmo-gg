'use strict';

const express = require('express');
const knex = require('knex');
const morgan = require('morgan');

const app = express();
const db = knex({
  client: 'pg',
  connection: process.env.TAKUMI_DATABASE
});
const todo = (res) => res.status(501).json(null);

db.on('query', (query) => console.log(`${query.sql} -- [${query.bindings}]`));
app.disable('x-powered-by');
app.use(morgan('tiny'));

app.get('/arenas', (_, res) => {
  db
    .select(
      'arenas.id',
      'arenas.name',
      'arenas.template_id as template_id',
      'arena_templates.name as template_name',
      'arenas.model_id as model_id',
      'arena_models.name as model_name',
      'arenas.skin_id as skin_id',
      'arena_skins.name as skin_name')
    .from('arenas')
    .leftOuterJoin('arena_templates',
      'arenas.template_id', 'arena_templates.id')
    .leftOuterJoin('arena_models',
      'arenas.model_id', 'arena_models.id')
    .leftOuterJoin('arena_skins',
      'arenas.skin_id', 'arena_skins.id')
    .orderBy('arenas.name', 'asc')
    .then((arenas) => res.json(arenas));
});

app.get('/games', (_, res) => {
  todo(res);
});

app.get('/search', (_, res) => {
  todo(res);
});

app.get('/stats/arenas', (_, res) => {
  todo(res);
});

app.get('/stats/bodies', (_, res) => {
  todo(res);
});

app.get('/stats/players', (_, res) => {
  todo(res);
});

app.get('/stats/players/:id/arenas', (_, res) => {
  todo(res);
});

app.get('/stats/players/:id/bodies', (_, res) => {
  todo(res);
});

app.get('/stats/players/:id/history', (_, res) => {
  todo(res);
});

app.get('/stats/players/:id/poll', (_, res) => {
  todo(res);
});

app.get('/stats/players/:id/rank', (_, res) => {
  todo(res);
});

app.get('/stats/summary', (_, res) => {
  todo(res);
});

app.post('/uploads', (_, res) => {
  todo(res);
});

app.get('/uploads', (_, res) => {
  todo(res);
});

app.listen(8080, () => {
  console.log('Listening on port 8080 ...');
});

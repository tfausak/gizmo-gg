'use strict';

const compression = require('compression');
const cors = require('cors');
const express = require('express');
const knex = require('knex');
const morgan = require('morgan');

// Application

const app = express();
app.disable('x-powered-by');
app.use(compression());
app.use(cors());
app.use(morgan('tiny'));

// Database

const db = knex({
  client: 'pg',
  connection: process.env.TAKUMI_DATABASE || 'postgres://'
});
db.on('query', (query) => console.log(`${query.sql} -- [${query.bindings}]`));

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
    .leftOuterJoin('arena_templates',
      'arenas.template_id', 'arena_templates.id')
    .leftOuterJoin('arena_models',
      'arenas.model_id', 'arena_models.id')
    .leftOuterJoin('arena_skins',
      'arenas.skin_id', 'arena_skins.id')
    .orderBy('arenas.name', 'asc')
    .then((arenas) => res.json(arenas))
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
app.get('/stats/arenas', notImplemented);
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

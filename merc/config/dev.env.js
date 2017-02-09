var merge = require('webpack-merge')
var prodEnv = require('./prod.env')

module.exports = merge(prodEnv, {
  NODE_ENV: '"development"',
  API_URL: process.env.API_URL || '"http://127.0.0.1:8080/"',
  API_DELAY_MS: 1000
})

import Vue from 'vue'
import Vuex from 'vuex'
import { getResource } from './api'
var _ = require('lodash')

Vue.use(Vuex)

function getQueryString (params) {
  params = params || {}
  if (!_.size(params)) {
    return ''
  }
  let qs = '?'
  let i = 0
  for (let key in params) {
    if (i > 0) {
      qs += '&'
    }
    qs += key + '=' + encodeURIComponent(params[key])
    i++
  }
  return qs
}

const store = new Vuex.Store({
  state: {
    cache: {}
  },
  actions: {
    GET_STATS_SUMMARY: function ({ dispatch }, params) {
      let endpoint = 'stats/summary' + getQueryString(params)
      return dispatch('FETCH', { endpoint })
    },

    GET_PLATFORMS: function ({ dispatch }) {
      let endpoint = 'platforms'
      return dispatch('FETCH', { endpoint })
    },

    GET_PLAYERS: function ({ dispatch }) {
      let endpoint = 'players'
      return dispatch('FETCH', { endpoint })
    },

    GET_PLAYER: function ({ dispatch }, params) {
      let endpoint = 'stats/players/' + params.id
      return dispatch('FETCH', { endpoint })
    },

    FETCH: function ({ commit, state }, { endpoint }) {
      if (endpoint in state.cache) {
        return Promise.resolve(state.cache[endpoint])
      }
      return getResource(endpoint)
        .then(function (data) {
          commit('SAVE_CACHE', { k: endpoint, v: data })
          return data
        })
    }
  },
  mutations: {
    SAVE_CACHE: function (state, { k, v }) {
      state.cache[k] = v
    }
  },
  getters: {}
})

export default store

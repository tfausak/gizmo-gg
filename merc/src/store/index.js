import Vue from 'vue'
import Vuex from 'vuex'
import { getEndpointUrl, getRaw, getResource } from './api'
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
    GET_ARENAS: function ({ dispatch }, params) {
      let endpoint = 'arenas'
      let url = getEndpointUrl(endpoint).replace('/api/', '/takumi/')
      return dispatch('FETCH_URL', url)
    },

    GET_UPLOAD: function ({ dispatch }, params) {
      let endpoint = 'uploads/' + params.id
      let url = getEndpointUrl(endpoint).replace('/api/', '/takumi/')
      return dispatch('FETCH_URL', url)
    },

    GET_GAME: function ({ dispatch }, params) {
      let endpoint = 'games/' + params.id
      delete params.id
      endpoint += getQueryString(params)
      return dispatch('FETCH', endpoint)
    },

    GET_PLAYER: function ({ dispatch }, params) {
      let endpoint = 'stats/players/' + params.id
      delete params.id
      endpoint += getQueryString(params)
      return dispatch('FETCH', endpoint)
    },

    GET_PLAYER_ARENAS: function ({ dispatch }, params) {
      let endpoint = 'stats/players/' + params.id + '/arenas'
      delete params.id
      endpoint += getQueryString(params)
      return dispatch('FETCH', endpoint)
    },

    GET_PLAYER_BODIES: function ({ dispatch }, params) {
      let endpoint = 'stats/players/' + params.id + '/bodies'
      delete params.id
      endpoint += getQueryString(params)
      return dispatch('FETCH', endpoint)
    },

    GET_PLAYER_POLL: function ({ dispatch }, params) {
      let endpoint = 'stats/players/' + params.id + '/poll'
      let url = getEndpointUrl(endpoint).replace('/api/', '/takumi/')
      return dispatch('GRAB_URL', url)
    },

    GET_PLAYER_RANK: function ({ dispatch }, params) {
      let endpoint = 'stats/players/' + params.id + '/rank'
      delete params.id
      endpoint += getQueryString(params)
      let url = getEndpointUrl(endpoint).replace('/api/', '/takumi/')
      return dispatch('FETCH_URL', url)
    },

    GET_SEARCH: function ({ dispatch }, params) {
      let endpoint = 'search' + getQueryString(params)
      let url = getEndpointUrl(endpoint).replace('/api/', '/takumi/')
      return dispatch('FETCH_URL', url)
    },

    GET_STATS_SUMMARY: function ({ dispatch }, params) {
      let endpoint = 'stats/summary' + getQueryString(params)
      let url = getEndpointUrl(endpoint).replace('/api/', '/takumi/')
      return dispatch('FETCH_URL', url)
    },

    GET_STATS_BODIES: function ({ dispatch }, params) {
      let endpoint = 'stats/bodies' + getQueryString(params)
      let url = getEndpointUrl(endpoint).replace('/api/', '/takumi/')
      return dispatch('FETCH_URL', url)
    },

    GET_STATS_ARENAS: function ({ dispatch }, params) {
      let endpoint = 'stats/arenas' + getQueryString(params)
      let url = getEndpointUrl(endpoint).replace('/api/', '/takumi/')
      return dispatch('FETCH_URL', url)
    },

    CLEAR_ENDPOINT: function ({ commit, state }, endpoint) {
      commit('CLEAR_CACHE', endpoint)
    },

    // No cache
    GRAB: function ({ commit, state }, endpoint) {
      let promise = getResource(endpoint)
        .then(function (data) {
          return data
        })
      return promise
    },

    GRAB_URL: function ({ commit, state }, url) {
      let promise = getRaw(url)
        .then(function (data) {
          return data
        })
      return promise
    },

    // Cached
    FETCH: function ({ commit, state }, endpoint) {
      if (endpoint in state.cache) {
        return state.cache[endpoint]
      }
      let promise = getResource(endpoint)
        .then(function (data) {
          return data
        })
      commit('SAVE_CACHE', { k: endpoint, v: promise })
      return promise
    },

    // Cached by full URL
    FETCH_URL: function ({ commit, state }, url) {
      if (url in state.cache) {
        return state.cache[url]
      }
      let promise = getRaw(url)
        .then(function (data) {
          return data
        })
      commit('SAVE_CACHE', { k: url, v: promise })
      return promise
    }
  },
  mutations: {
    SAVE_CACHE: function (state, { k, v }) {
      state.cache[k] = v
    },
    CLEAR_CACHE: function (state, endpoint) {
      _.each(state.cache, function (value, key) {
        if (key.includes(endpoint)) {
          delete state.cache[key]
        }
      })
    }
  },
  getters: {}
})

export default store

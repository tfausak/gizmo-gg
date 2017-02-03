import Vue from 'vue'
import Vuex from 'vuex'
import { getResource } from './api'

Vue.use(Vuex)

const store = new Vuex.Store({
  state: {
    cache: {}
  },
  actions: {
    GET_STATS_SUMMARY: function ({ dispatch }) {
      return dispatch('FETCH', { endpoint: 'stats/summary' })
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

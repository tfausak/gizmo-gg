import Vue from 'vue'
import Vuex from 'vuex'
import {getResource} from './api'

Vue.use(Vuex)

const store = new Vuex.Store({
  state: {},
  actions: {
    GET_STATS_SUMMARY: function ({ commit }) {
      return getResource('stats/summary')
    }
  },
  mutations: {},
  getters: {}
})

export default store

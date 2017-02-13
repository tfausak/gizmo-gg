<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-one-quarter">
          <filter-panel-component v-model="tier" title="Tier" :options="tierOptions"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="time" title="Time" :options="timeOptions"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="playlist" title="Playlist" :options="playlistOptions"></filter-panel-component>
        </div>
      </div>

      <loading-component :loading="loading"></loading-component>

      <h2 class="title">Map</h2>
      <map-table-component v-if="!loading" :source="source['byTemplate']"></map-table-component>

      <h2 class="title">Arena</h2>
      <map-table-component v-if="!loading" :source="source['byModel']"></map-table-component>

      <h2 class="title">Variant</h2>
      <map-table-component v-if="!loading" :source="source['bySkin']"></map-table-component>
    </div>
  </section>
</template>

<script>
import FilterPanelComponent from '../components/FilterPanel'
import LoadingComponent from '../components/Loading'
import MapTableComponent from './components/MapTable'

import options from '../../store/options.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    let vm = this
    vm.fetchData()
    vm.$store.dispatch('GET_ARENAS').then(function (data) {
      vm.GET_ARENAS = data
      vm.compileData()
    })
  },
  components: {
    FilterPanelComponent,
    LoadingComponent,
    MapTableComponent
  },
  computed: {
    loading: function () {
      return this.GET_STATS_ARENAS === null || this.GET_ARENAS === null
    }
  },
  data: function () {
    let playlistOptions = options.playlists()
    let tierOptions = options.tiers()
    let timeOptions = options.times()
    return {
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      tierOptions: tierOptions,
      tier: _.head(_.keys(tierOptions)),
      timeOptions: timeOptions,
      time: _.head(_.keys(timeOptions)),
      GET_STATS_ARENAS: null,
      GET_ARENAS: null,
      types: [ 'byTemplate', 'byModel', 'bySkin' ],
      type: 'byModel',
      source: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_STATS_ARENAS = null
      vm.$store.dispatch('GET_STATS_ARENAS', {
        playlist: vm.playlist,
        tier: vm.tier,
        time: vm.time
      }).then(function (data) {
        vm.GET_STATS_ARENAS = data
        vm.compileData()
      })
    },
    compileData: function () {
      let vm = this
      if (vm.loading) {
        return
      }

      vm.source = {}
      _.each(vm.types, function (type) {
        vm.source[type] = { maxFreqPct: 0, maxScore: 0, data: {} }
      })

      let totalGames = 0
      _.each(vm.GET_STATS_ARENAS, function (value, key) {
        totalGames += value.numGames
      })

      let sumCols = [
        'numGames',
        'totalShots',
        'totalGoals',
        'totalScore',
        'totalSaves',
        'totalAssists'
      ]
      _.each(vm.GET_STATS_ARENAS, function (value, key) {
        _.each(vm.GET_ARENAS, function (arena) {
          if (arena.name === value.arenaName) {
            // Template
            if (!_.has(vm.source['byTemplate']['data'], arena.templateName)) {
              vm.source['byTemplate']['data'][arena.templateName] = {
                displayName: arena.templateName
              }
              _.each(sumCols, function (col) {
                vm.source['byTemplate']['data'][arena.templateName][col] = 0
              })
            }
            _.each(sumCols, function (col) {
              vm.source['byTemplate']['data'][arena.templateName][col] += value[col]
            })

            // Model
            if (!_.has(vm.source['byModel']['data'], arena.modelName)) {
              vm.source['byModel']['data'][arena.modelName] = {
                displayName: arena.modelName
              }
              _.each(sumCols, function (col) {
                vm.source['byModel']['data'][arena.modelName][col] = 0
              })
            }
            _.each(sumCols, function (col) {
              vm.source['byModel']['data'][arena.modelName][col] += value[col]
            })

            // Skin
            let fullSkinName = arena.modelName
            if (arena.skinName) {
              fullSkinName += ' (' + arena.skinName + ')'
            }
            if (!_.has(vm.source['bySkin']['data'], fullSkinName)) {
              vm.source['bySkin']['data'][fullSkinName] = {
                displayName: fullSkinName
              }
              _.each(sumCols, function (col) {
                vm.source['bySkin']['data'][fullSkinName][col] = 0
              })
            }
            _.each(sumCols, function (col) {
              vm.source['bySkin']['data'][fullSkinName][col] += value[col]
            })
          }
        })
      })

      _.each(vm.types, function (type) {
        vm.source[type]['maxFreqPct'] = 0
        vm.source[type]['maxScore'] = 0
        vm.source[type]['data'] = _.map(vm.source[type]['data'], function (value, key) {
          value.accuracy = 0
          if (value.totalGoals > 0) {
            value.accuracy = 100
          }
          if (value.totalShots > 0) {
            value.accuracy = _.round(value.totalGoals / value.totalShots * 100, 2)
          }
          value.avgScore = _.round(value.totalScore / value.numGames, 2)
          value.freqPct = _.round(value.numGames / totalGames * 100, 2)
          vm.source[type]['maxFreqPct'] = _.max([value.freqPct, vm.source[type]['maxFreqPct']])
          vm.source[type]['maxScore'] = _.max([value.avgScore, vm.source[type]['maxScore']])
          return value
        })
      })
    }
  },
  watch: {
    playlist: function (val) { this.fetchData() },
    tier: function (val) { this.fetchData() },
    time: function (val) { this.fetchData() }
  }
}
</script>

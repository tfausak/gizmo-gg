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

      <table class="table is-striped table-outerborder table-stats" v-if="!loading">
        <thead>
          <tr>
            <sortable-th-component v-for="col in cols" @orderByCol="orderByCol" :sort="sort" :col="col.key">{{ col.name }}</sortable-th-component>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in sortedRows">
            <td>{{ row.arenaName }}</td>
            <td>
              <div class="level level-chained">
                <div class="level-item">
                  <progress class="progress is-small" :value="row.freqPct" :max="maxFreqPct"></progress>
                </div>
                <div class="level-item">
                  {{ row.freqPct }}%
                </div>
              </div>
            </td>
            <td class="has-text-right">{{ row.numGames }}</td>
            <td>
              <div class="level level-chained">
                <div class="level-item">
                  <progress class="progress is-small is-success" :value="row.avgScore" :max="maxScore"></progress>
                </div>
                <div class="level-item">
                  {{ row.avgScore }}
                </div>
              </div>
            </td>
            <td>{{ row.accuracy }}%</td>
          </tr>
        </tbody>
      </table>
    </div>
  </section>
</template>

<script>
import LoadingComponent from '../components/Loading'
import SortableThComponent from '../components/SortableTh'
import FilterPanelComponent from '../components/FilterPanel'

import options from '../../store/options.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  components: {
    SortableThComponent,
    FilterPanelComponent,
    LoadingComponent
  },
  computed: {
    sortedRows: function () {
      if (!this.GET_STATS_ARENAS) {
        return []
      }
      var sorted = _.sortBy(this.GET_STATS_ARENAS, [this.sort])
      if (!this.dir) {
        _.reverse(sorted)
      }
      return sorted
    },
    loading: function () {
      return this.GET_STATS_ARENAS === null
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
      sort: null,
      dir: 1,
      cols: [
        { key: 'arenaName', name: 'Map' },
        { key: 'freqPct', name: 'Freq. Pct' },
        { key: 'numGames', name: 'Games Played' },
        { key: 'avgScore', name: 'Avg Score' },
        { key: 'accuracy', name: 'Accuracy' }
      ],
      GET_STATS_ARENAS: null,
      maxFreqPct: 0,
      maxScore: 0
    }
  },
  methods: {
    orderByCol: function (col, dir) {
      this.sort = col
      this.dir = dir
    },
    fetchData: function () {
      this.GET_STATS_ARENAS = null
      let vm = this
      this.$store.dispatch('GET_STATS_ARENAS', {
        time: this.time,
        playlist: this.playlist,
        tier: this.tier
      }).then(function (data) {
        vm.maxFreqPct = 0
        vm.maxScore = 0
        let totalGames = 0
        _.each(data, function (value, key) {
          totalGames += value.numGames
        })
        vm.GET_STATS_ARENAS = _.map(data, function (value, key) {
          value.accuracy = 0
          if (value.totalGoals > 0) {
            value.accuracy = 100
          }
          if (value.totalShots > 0) {
            value.accuracy = _.round(value.totalGoals / value.totalShots * 100, 2)
          }
          value.avgScore = _.round(value.totalScore / value.numGames, 2)
          value.freqPct = _.round(value.numGames / totalGames * 100, 2)
          vm.maxFreqPct = _.max([value.freqPct, vm.maxFreqPct])
          vm.maxScore = _.max([value.avgScore, vm.maxScore])
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

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
          <filter-panel-component v-model="map" title="Map" :options="mapOptions"></filter-panel-component>
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
            <td>
              <div class="level level-chained">
                <div class="level-item">
                  <figure class="image is-32x32 is-circle-32x32">
                    <img :src="'/static/img/bodies/' + row.bodySlug + '.png'">
                  </figure>
                </div>
                <div class="level-item">
                  <strong>{{ row.bodyName }}</strong>
                </div>
              </div>
            </td>
            <td>
              <div class="level level-chained">
                <div class="level-item">
                  <progress class="progress is-small" :class="{ 'is-primary': row.winPct >= 50 }" :value="row.winPct" :max="maxWinPct"></progress>
                </div>
                <div class="level-item">
                  {{ row.winPct }}%
                </div>
              </div>
            </td>
            <td class="has-text-right">{{ row.numGames }}</td>
            <td>
              <div class="level">
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

import playlistOptions from '../../store/options/playlist.js'
import timeOptions from '../../store/options/time.js'
import mapOptions from '../../store/options/map.js'
import tierOptions from '../../store/options/tier.js'
import slugger from '../../store/slugger.js'

var _ = require('lodash')

export default {
  components: {
    SortableThComponent,
    FilterPanelComponent,
    LoadingComponent
  },
  data: function () {
    return {
      tierOptions: tierOptions,
      tier: _.head(_.keys(tierOptions)),
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      timeOptions: timeOptions,
      time: _.head(_.keys(timeOptions)),
      mapOptions: mapOptions,
      map: _.head(_.keys(mapOptions)),
      sort: null,
      dir: 1,
      cols: [
        { key: 'bodyName', name: 'Battle-Car' },
        { key: 'winPct', name: 'Win Pct' },
        { key: 'numGames', name: 'Games Played' },
        { key: 'avgScore', name: 'Avg Score' },
        { key: 'accuracy', name: 'Accuracy' }
      ],
      GET_STATS_BODIES: null,
      maxWinPct: 0,
      maxScore: 0
    }
  },
  watch: {
    tier: function (val) { this.fetchData() },
    map: function (val) { this.fetchData() },
    time: function (val) { this.fetchData() },
    playlist: function (val) { this.fetchData() }
  },
  computed: {
    sortedRows: function () {
      if (!this.GET_STATS_BODIES) {
        return []
      }
      var sorted = _.sortBy(this.GET_STATS_BODIES, [this.sort])
      if (!this.dir) {
        _.reverse(sorted)
      }
      return sorted
    },
    loading: function () {
      return this.GET_STATS_BODIES === null
    }
  },
  methods: {
    orderByCol: function (col, dir) {
      this.sort = col
      this.dir = dir
    },
    fetchData: function () {
      this.GET_STATS_BODIES = null
      let vm = this
      this.$store.dispatch('GET_STATS_BODIES', {
        time: this.time,
        playlist: this.playlist,
        tier: this.tier,
        map: this.map
      }).then(function (data) {
        vm.maxWinPct = 0
        vm.maxScore = 0
        vm.GET_STATS_BODIES = _.map(data, function (value, key) {
          value.winPct = _.round(value.numWins / value.numGames * 100, 2)
          value.accuracy = 0
          if (value.totalGoals > 0) {
            value.accuracy = 100
          }
          if (value.totalShots > 0) {
            value.accuracy = _.round(value.totalGoals / value.totalShots * 100, 2)
          }
          value.avgScore = _.round(value.totalScore / value.numGames, 2)
          value.bodySlug = slugger.slugBody(value.bodyName)
          vm.maxWinPct = _.max([value.winPct, vm.maxWinPct])
          vm.maxScore = _.max([value.avgScore, vm.maxScore])
          return value
        })
      })
    }
  },
  beforeMount: function () {
    this.fetchData()
  }
}
</script>

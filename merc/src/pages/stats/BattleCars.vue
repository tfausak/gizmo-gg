<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <!--
        <div class="column is-one-quarter">
          <filter-panel-component v-model="tier" title="Tier" :options="tierOptions" :sync="tier"></filter-panel-component>
        </div>
        -->
        <div class="column is-one-quarter">
          <filter-panel-component v-model="time" title="Time" :options="timeOptions" :sync="time"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="map" title="Map" :options="mapOptions" :sync="map"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="playlist" title="Playlist" :options="playlistOptions" :sync="playlist"></filter-panel-component>
        </div>
      </div>

      <loading-component :loading="loading"></loading-component>

      <table class="table is-striped is-narrow table-outerborder table-stats" v-if="!loading">
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
                  <figure class="image is-32x32 is-circle-dark">
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
import FilterPanelComponent from '../components/FilterPanel'
import LoadingComponent from '../components/Loading'
import SortableThComponent from '../components/SortableTh'
import FilterTierMixin from '../mixins/FilterTierMixin'
import FilterPlaylistMixin from '../mixins/FilterPlaylistMixin'
import FilterTimeMixin from '../mixins/FilterTimeMixin'
import FilterMapMixin from '../mixins/FilterMapMixin'

import slugger from '../../store/slugger.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  components: {
    FilterPanelComponent,
    LoadingComponent,
    SortableThComponent
  },
  computed: {
    loading: function () {
      return this.GET_STATS_BODIES === null
    },
    sortedRows: function () {
      if (!this.GET_STATS_BODIES) {
        return []
      }
      var sorted = _.sortBy(this.GET_STATS_BODIES, [this.sort])
      if (!this.dir) {
        _.reverse(sorted)
      }
      return sorted
    }
  },
  data: function () {
    return {
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
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_STATS_BODIES = null
      vm.$store.dispatch('GET_STATS_BODIES', {
        playlist: vm.playlist,
        map: vm.map,
        tier: vm.tier,
        time: vm.time
      }).then(function (data) {
        vm.maxWinPct = 0
        vm.maxScore = 0
        vm.GET_STATS_BODIES = _.map(data, function (value, key) {
          value.winPct = _.round(value.numWins / (value.numGames || 1) * 100, 2)
          value.accuracy = _.round(value.totalGoals / (value.totalShots || 1) * 100, 2)
          value.avgScore = _.round(value.totalScore / (value.numGames || 1), 2)
          value.bodySlug = slugger.slugBody(value.bodyName)
          vm.maxWinPct = _.max([value.winPct, vm.maxWinPct])
          vm.maxScore = _.max([value.avgScore, vm.maxScore])
          return value
        })
      })
    },
    orderByCol: function (col, dir) {
      this.sort = col
      this.dir = dir
    }
  },
  mixins: [ FilterTierMixin, FilterPlaylistMixin, FilterTimeMixin, FilterMapMixin ]
}
</script>

<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-one-quarter">
          <filter-panel-component v-model="time" title="Time" :options="timeOptions"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="playlist" title="Playlist" :options="playlistOptions"></filter-panel-component>
        </div>
      </div>

      <div class="columns">
        <div class="column">
          <article class="message">
            <div class="message-body">
              <p class="heading is-1">Win Pct by Team</p>
              <loading-component :loading="loading"></loading-component>
              <chart-team-wins-component :source="GET_STATS_SUMMARY.winPct" v-if="!loading"></chart-team-wins-component>
            </div>
          </article>
        </div>
        <div class="column">
          <article class="message">
            <div class="message-body">
              <p class="heading is-1">Win Pct by Team</p>
              <loading-component :loading="loading"></loading-component>
              <chart-battle-cars-component :source="GET_STATS_SUMMARY.bodyFreqPct" v-if="!loading"></chart-battle-cars-component>
            </div>
          </article>
        </div>
      </div>

      <article class="message">
        <div class="message-body">
          <p class="heading has-text-centered">
            Map Frequency
            - <router-link to="/stats/maps">more</router-link>
          </p>
          <div style="margin-top: 20px;"></div>
          <loading-component :loading="loading"></loading-component>
          <chart-maps-component :source="GET_STATS_SUMMARY.mapFreqPct" :arenas="GET_ARENAS" v-if="!loading"></chart-maps-component>
        </div>
      </article>
    </div>
  </section>
</template>

<script>
import ChartBattleCarsComponent from './components/ChartBattleCars'
import ChartMapsComponent from './components/ChartMaps'
import ChartTeamWinsComponent from './components/ChartTeamWins'
import FilterPanelComponent from '../components/FilterPanel'
import LoadingComponent from '../components/Loading'

import options from '../../store/options.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
    let vm = this
    this.$store.dispatch('GET_ARENAS').then(function (data) {
      vm.GET_ARENAS = data
    })
  },
  components: {
    ChartBattleCarsComponent,
    ChartMapsComponent,
    ChartTeamWinsComponent,
    FilterPanelComponent,
    LoadingComponent
  },
  computed: {
    loading: function () {
      return this.GET_STATS_SUMMARY === null || this.GET_ARENAS === null
    }
  },
  data: function () {
    let playlistOptions = options.playlists()
    let timeOptions = options.times()
    return {
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      timeOptions: timeOptions,
      time: _.head(_.keys(timeOptions)),
      GET_STATS_SUMMARY: null,
      GET_ARENAS: null
    }
  },
  methods: {
    fetchData: function () {
      this.GET_STATS_SUMMARY = null
      let vm = this
      this.$store.dispatch('GET_STATS_SUMMARY', {
        playlist: this.playlist,
        time: this.time
      }).then(function (data) {
        vm.GET_STATS_SUMMARY = data
      })
    }
  },
  watch: {
    playlist: function (val) { this.fetchData() },
    time: function (val) { this.fetchData() }
  }
}
</script>

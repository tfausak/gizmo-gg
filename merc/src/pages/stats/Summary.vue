<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-one-quarter">
          <filter-panel-component v-model="time" title="Time" :options="timeOptions" :sync="time"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="playlist" title="Playlist" :options="playlistOptions" :sync="playlist"></filter-panel-component>
        </div>
      </div>

      <div class="columns">
        <div class="column">
          <article class="message">
            <div class="message-body">
              <p class="heading is-1">Win Pct by Team</p>
              <chart-team-wins-component :GET_STATS_SUMMARY="GET_STATS_SUMMARY"></chart-team-wins-component>
            </div>
          </article>
        </div>
        <div class="column">
          <article class="message">
            <div class="message-body">
              <p class="heading is-1">
                Battle-Car Use
                - <router-link to="/stats/battle-cars">more</router-link>
              </p>
              <chart-battle-cars-component :GET_STATS_SUMMARY="GET_STATS_SUMMARY"></chart-battle-cars-component>
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
          <chart-maps-component :GET_STATS_SUMMARY="GET_STATS_SUMMARY"></chart-maps-component>
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
import options from '../../store/options.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  components: {
    ChartBattleCarsComponent,
    ChartMapsComponent,
    ChartTeamWinsComponent,
    FilterPanelComponent
  },
  computed: {
    loading: function () {
      return this.GET_STATS_SUMMARY === null
    },
    qPlaylist: function () {
      return this.$route.query.playlist
    },
    qTime: function () {
      return this.$route.query.time
    }
  },
  data: function () {
    let playlistOptions = options.playlists()
    let playlistDefault = _.head(_.keys(playlistOptions))
    let timeOptions = options.times()
    let timeDefault = _.head(_.keys(timeOptions))
    return {
      playlistOptions: playlistOptions,
      playlistDefault: playlistDefault,
      playlist: this.qPlaylist || playlistDefault,
      timeOptions: timeOptions,
      timeDefault: timeDefault,
      time: this.qTime || timeDefault,
      GET_STATS_SUMMARY: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_STATS_SUMMARY = null
      vm.$store.dispatch('GET_STATS_SUMMARY', {
        playlist: vm.playlist,
        time: vm.time
      }).then(function (data) {
        vm.GET_STATS_SUMMARY = data
      })
    }
  },
  watch: {
    playlist: function (val) {
      let data = Object.assign({}, this.$route.query)
      data['playlist'] = val
      this.$router.replace({
        name: 'stats',
        query: data
      })
    },
    qPlaylist: function (val) {
      this.playlist = this.$route.query.playlist || this.playlistDefault
      this.fetchData()
    },
    time: function (val) {
      let data = Object.assign({}, this.$route.query)
      data['time'] = val
      this.$router.replace({
        name: 'stats',
        query: data
      })
    },
    qTime: function (val) {
      this.time = this.$route.query.time || this.timeDefault
      this.fetchData()
    }
  }
}
</script>

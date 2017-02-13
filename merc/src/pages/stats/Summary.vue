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
          <chart-team-wins-component :source="source" :loading="loading"></chart-team-wins-component>
        </div>
        <div class="column">
          <chart-battle-cars-component :source="source" :loading="loading"></chart-battle-cars-component>
        </div>
      </div>
      <chart-maps-component :source="source" :loading="loading"></chart-maps-component>
    </div>
  </section>
</template>

<script>
import ChartTeamWinsComponent from './components/ChartTeamWins'
import ChartBattleCarsComponent from './components/ChartBattleCars'
import ChartMapsComponent from './components/ChartMaps'
import FilterPanelComponent from '../components/FilterPanel'

import playlistOptions from '../../store/options/playlist.js'
import timeOptions from '../../store/options/time.js'

var _ = require('lodash')

export default {
  components: {
    ChartTeamWinsComponent,
    ChartBattleCarsComponent,
    ChartMapsComponent,
    FilterPanelComponent
  },
  data: function () {
    return {
      loading: true,
      timeOptions: timeOptions,
      time: _.head(_.keys(timeOptions)),
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions))
    }
  },
  computed: {
    source: function () {
      let vm = this
      this.loading = true
      return this.$store.dispatch('GET_STATS_SUMMARY', {
        time: this.time,
        playlist: this.playlist
      }).then(function (data) {
        vm.loading = false
        return data
      })
    }
  }
}
</script>

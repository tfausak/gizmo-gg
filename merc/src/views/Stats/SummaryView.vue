<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-one-quarter">
          <filter-panel v-model="time" title="Time" :options="timeOptions"></filter-panel>
        </div>
        <div class="column is-one-quarter">
          <filter-panel v-model="playlist" title="Playlist" :options="playlistOptions"></filter-panel>
        </div>
      </div>

      <div class="columns">
        <div class="column">
          <wins-component :source="source"></wins-component>
        </div>
        <div class="column">
          <body-component :source="source"></body-component>
        </div>
      </div>
      <map-freq-component :source="source"></map-freq-component>
    </div>
  </section>
</template>

<script>
var _ = require('lodash')

import WinsComponent from '../../components/charts/WinsComponent.vue'
import BodyComponent from '../../components/charts/BodyComponent.vue'
import MapFreqComponent from '../../components/charts/MapFreqComponent.vue'
import FilterPanelComponent from '../../components/FilterPanelComponent.vue'

export default {
  components: {
    WinsComponent: WinsComponent,
    BodyComponent: BodyComponent,
    MapFreqComponent: MapFreqComponent,
    FilterPanel: FilterPanelComponent
  },
  data: function () {
    let timeOptions = {
      season: 'Current Season',
      month: 'Last Month',
      week: 'Last Week'
    }
    let playlistOptions = {
      all: 'All',
      ranked1v1: 'Ranked 1v1',
      ranked2v2: 'Ranked 2v2',
      ranked3v3: 'Ranked 3v3',
      ranked3v3solo: 'Ranked 3v3 Solo'
    }
    return {
      timeOptions: timeOptions,
      time: _.head(_.keys(timeOptions)),
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions))
    }
  },
  computed: {
    source: function () {
      return this.$store.dispatch('GET_STATS_SUMMARY', {
        time: this.time,
        playlist: this.playlist
      })
    }
  }
}
</script>

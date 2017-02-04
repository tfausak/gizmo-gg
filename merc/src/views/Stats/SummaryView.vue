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

import playlistOptions from '../../store/options/playlist.js'
import timeOptions from '../../store/options/time.js'

export default {
  components: {
    WinsComponent: WinsComponent,
    BodyComponent: BodyComponent,
    MapFreqComponent: MapFreqComponent,
    FilterPanel: FilterPanelComponent
  },
  data: function () {
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

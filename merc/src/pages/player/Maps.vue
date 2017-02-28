<template>
  <div class="container">
    <div class="columns">
      <div class="column is-one-quarter">
        <filter-panel-component v-model="time" title="Time" :options="timeOptions"></filter-panel-component>
      </div>
      <div class="column is-one-quarter">
        <filter-panel-component v-model="playlist" title="Playlist" :options="playlistOptions"></filter-panel-component>
      </div>
    </div>

    <loading-component :loading="loading"></loading-component>

    <div v-if="source">
      <h2 class="title">Map</h2>
      <map-table-component :source="source['byTemplate']"></map-table-component>

      <h2 class="title">Field</h2>
      <map-table-component :source="source['byModel']"></map-table-component>

      <h2 class="title">Variant</h2>
      <map-table-component :source="source['bySkin']"></map-table-component>
    </div>
  </div>
</template>

<script>
import FilterPanelComponent from '../components/FilterPanel'
import LoadingComponent from '../components/Loading'
import MapTableComponent from '../stats/components/MapTable'

import options from '../../store/options.js'
import { compileMapStats } from '../../store/scrubber.js'

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
      return this.GET_PLAYER_ARENAS === null || this.GET_ARENAS === null
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
      GET_PLAYER_ARENAS: null,
      GET_ARENAS: null,
      source: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_PLAYER_ARENAS = null
      vm.source = null
      vm.$store.dispatch('GET_PLAYER_ARENAS', {
        id: vm.playerId,
        playlist: vm.playlist,
        time: vm.time
      }).then(function (data) {
        vm.GET_PLAYER_ARENAS = data
        vm.compileData()
      })
    },
    compileData: function () {
      if (this.loading) {
        return
      }
      this.source = compileMapStats(this.GET_PLAYER_ARENAS, this.GET_ARENAS)
    }
  },
  props: [ 'playerId' ],
  watch: {
    playlist: function (val) { this.fetchData() },
    time: function (val) { this.fetchData() }
  }
}
</script>

<template>
  <div class="panel">
    <p class="panel-heading panel-squish">
      <span class="heading">Battle-Cars (this season)</span>
    </p>
    <p class="panel-tabs">
      <a v-for="(value, key) in playlistOptions" @click="setPlaylist(key)" :class="{ 'is-active': key === playlist }">{{ value }}</a>
    </p>
    <div class="panel-block" v-if="loading">
      <loading-component :loading="true"></loading-component>
    </div>
    <div v-if="source">
      TODO
    </div>
  </div>
</template>

<script>
import LoadingComponent from '../../components/Loading'
import options from '../../../store/options.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  computed: {
    loading: function () {
      return this.GET_STATS_BODIES === null
    }
  },
  components: {
    LoadingComponent
  },
  data: function () {
    let playlistOptions = options.shortPlaylists()
    return {
      GET_STATS_BODIES: null,
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      source: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_STATS_BODIES = null
      vm.source = null
      vm.$store.dispatch('GET_STATS_BODIES', {
        id: vm.playerId,
        playlist: vm.playlist,
        time: 'season'
      }).then(function (data) {
        vm.GET_STATS_BODIES = data
        vm.compileData()
      })
    },
    compileData: function () {
      if (this.loading) {
        return
      }
      this.source = {}
    },
    setPlaylist: function (key) {
      this.playlist = key
    }
  },
  props: [ 'playerId' ],
  watch: {
    playlist: function (val) { this.fetchData() }
  }
}
</script>

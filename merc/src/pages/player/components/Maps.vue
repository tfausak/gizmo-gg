<template>
  <div class="panel">
    <p class="panel-heading panel-squish">
      <span class="heading">Maps (this season)</span>
    </p>
    <p class="panel-tabs">
      <a v-for="(value, key) in playlistOptions" @click="setPlaylist(key)" :class="{ 'is-active': key === playlist }">{{ value }}</a>
    </p>
    <div class="panel-block" v-for="map in maps">
      <div class="level level-chained is-relative">
        <div class="level-item">
          <figure class="image is-48x48">
            <img :src="'/static/img/maps/' + map.icon + '.jpg'">
          </figure>
        </div>
        <div class="level-item">
          <div class="level level-stacked level-chained">
            <div class="level-item">
              <div class="heading is-medium is-bold no-margin">{{ map.map }}</div>
            </div>
            <div class="level-item text-small">
              <span>{{ map.wins + map.losses }} Games</span>
              <span class="text-spacer"></span>
              <span class="text-muted">{{ map.wins }}W {{ map.losses }}L</span>
              <span class="text-spacer"></span>
              <span>0%</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import options from '../../../store/options.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    let vm = this
    vm.fetchData()
    vm.$store.dispatch('GET_ARENAS').then(function (data) {
      vm.GET_ARENAS = data
    })
  },
  computed: {
    loading: function () {
      return this.GET_ARENAS === null || this.GET_ARENAS === null
    }
  },
  data: function () {
    let playlistOptions = options.shortPlaylists()
    return {
      GET_ARENAS: null,
      GET_PLAYER_MAPS: null,
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      maps: []
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_PLAYER_MAPS = null
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

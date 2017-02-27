<template>
  <div class="panel">
    <p class="panel-heading panel-squish">
      <span class="heading">Maps (this season)</span>
    </p>
    <p class="panel-tabs">
      <a v-for="(value, key) in playlistOptions" @click="setPlaylist(key)" :class="{ 'is-active': key === playlist }">{{ value }}</a>
    </p>
    <div class="panel-block" v-if="loading">
      <loading-component :loading="true"></loading-component>
    </div>
    <div v-if="source">
      <div class="panel-block" v-for="map in source['byTemplate']['data']">
        <div class="level level-chained is-relative">
          <div class="level-item">
            <figure class="image is-48x48">
              <img :src="'/static/img/maps/' + map.slug + '.jpg'">
            </figure>
          </div>
          <div class="level-item">
            <div class="level level-stacked level-chained">
              <div class="level-item">
                <div class="heading is-medium no-margin">{{ map.displayName }}</div>
              </div>
              <div class="level-item text-small">
                <span>{{ map.numGames }} Games</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import LoadingComponent from '../../components/Loading'
import options from '../../../store/options.js'
import { compileMapStats } from '../../../store/scrubber.js'

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
  computed: {
    loading: function () {
      return this.GET_ARENAS === null || this.GET_STATS_ARENAS === null
    }
  },
  components: {
    LoadingComponent
  },
  data: function () {
    let playlistOptions = options.shortPlaylists()
    return {
      GET_ARENAS: null,
      GET_STATS_ARENAS: null,
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      source: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_STATS_ARENAS = null
      vm.source = null
      vm.$store.dispatch('GET_PLAYER_ARENAS', {
        id: vm.playerId,
        playlist: vm.playlist,
        time: 'season'
      }).then(function (data) {
        vm.GET_STATS_ARENAS = data
        vm.compileData()
      })
    },
    compileData: function () {
      if (this.loading) {
        return
      }
      this.source = compileMapStats(this.GET_STATS_ARENAS, this.GET_ARENAS)
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

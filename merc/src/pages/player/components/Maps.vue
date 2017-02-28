<style lang="scss" scoped>
@import "~styles/vars.scss";

.mapStatsLevel {
  font-size: 13px;
  width: 100%;
  .mapPlayer {
    flex-grow: 1!important;
    text-align: center;
    .score {
      font-size: 15px;
    }
  }
  .mapDesc {
    width: 100px;
  }
  .mapRecord {
    width: 70px;
    text-align: center;
    .recordPct {
      font-size: 14px;
    }
  }
}
</style>

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
        <div class="level level-chained mapStatsLevel">
          <div class="level-item mapImage">
            <figure class="image is-48x48">
              <img :src="'/static/img/maps/' + map.slug + '.jpg'">
            </figure>
          </div>
          <div class="level-item is-block mapDesc">
            <div class="heading">{{ map.displayName }}</div>
            <div>{{ map.numGames }} Games</div>
          </div>
          <div class="level-item is-block mapPlayer">
            <div class="score">{{ map.perGame.score }}</div>
            <div class="text-muted">{{ map.perGame.goals }} / {{ map.perGame.assists }} / {{ map.perGame.saves }} / {{ map.perGame.shots }}</div>
          </div>
          <div class="level-item is-block mapRecord">
            <div class="recordPct">{{ map.winPct }}%</div>
            <div class="text-muted">{{ map.numWins }}W {{ map.numLosses }}L</div>
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

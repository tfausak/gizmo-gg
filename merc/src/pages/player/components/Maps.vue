<style lang="scss" scoped>
@import "~styles/vars.scss";

.mapStats {
  padding: 0;
  &:nth-child(odd) {
    background-color: rgba(0, 0, 0, 0.025);
  }
  &:nth-child(even) {
    background-color: rgba(0, 0, 0, 0.05);
  }
}
.mapStatsLevel {
  font-size: 13px;
  margin: 0!important;
  width: 100%;
  .column {
    padding: 0;
  }
  .mapImage {
    flex-grow: 0;
    padding-right: 5px;
  }
  .mapPlayer {
    padding-top: 3px;
    text-align: center;
    .score {
      font-size: 15px;
    }
  }
  .mapDesc {
    padding-top: 6px;
    .heading {
      font-weight: bold;
      margin-bottom: 0;
    }
  }
  .mapRecord {
    flex-grow: 1;
    padding-top: 3px;
    text-align: center;
    .recordPct {
      font-size: 14px;
    }
  }
}
</style>

<template>
  <div class="panel playerStatPanel">
    <p class="panel-heading panel-squish">
      <span class="heading">Maps (this season)  - <router-link :to="'/player/' + playerId + '/maps'">More</router-link></span>
    </p>
    <div class="panel-block" v-if="loading">
      <loading-component :loading="true"></loading-component>
    </div>
    <div class="panel-block" v-if="source && !source['byTemplate']['data'].length">
      <span class="text-muted text-italic">No results</span>
    </div>
    <div v-if="source">
      <div class="panel-block mapStats" v-for="map in source['byTemplate']['data']">
        <div class="columns mapStatsLevel">
          <div class="column mapImage">
            <figure class="image is-48x48">
              <img :src="'/static/img/maps/' + map.slug + '.jpg'">
            </figure>
          </div>
          <div class="column is-block mapDesc">
            <div class="heading">{{ map.displayName }}</div>
            <div class="text-muted2">{{ map.numGames }} Games</div>
          </div>
          <div class="column is-block mapPlayer">
            <div class="score">
              <span v-tooltip.top-center="'Points per Game'">{{ map.perGame.score }}</span>
            </div>
            <div class="text-muted2">
              <span v-tooltip.bottom-center="{ content: 'Per Game:<br>Goals / Assists / Saves / Shots', classes: 'bottom' }">
                {{ map.perGame.goals }} / {{ map.perGame.assists }} / {{ map.perGame.saves }} / {{ map.perGame.shots }}
              </span>
            </div>
          </div>
          <div class="column is-block mapRecord">
            <div class="recordPct">{{ map.winPct }}%</div>
            <div class="text-muted2">{{ map.numWins }}W {{ map.numLosses }}L</div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import LoadingComponent from '../../components/Loading'
import { compileMapStats } from '../../../store/scrubber.js'
import { EventBus } from '../../../store/event-bus.js'

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
      return this.GET_ARENAS === null || this.GET_PLAYER_ARENAS === null
    }
  },
  components: {
    LoadingComponent
  },
  created: function () {
    var vm = this
    EventBus.$on('player-updated', function () {
      vm.fetchData()
    })
  },
  data: function () {
    return {
      GET_ARENAS: null,
      GET_PLAYER_ARENAS: null,
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
        time: 'season'
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
      this.source['byTemplate']['data'] = _.filter(this.source['byTemplate']['data'], function (row) {
        return row.numGames
      })
    },
    setPlaylist: function (key) {
      this.playlist = key
    }
  },
  props: [ 'playerId', 'playlist' ],
  watch: {
    playlist: function (val) { this.fetchData() }
  }
}
</script>

<style scoped lang="scss">
@import "~styles/vars.scss";

.text-spacer {
  padding: 0 5px;
  text-align: center;
}
.panel {
  background-color: $white;
}
.playlist {
  width: 32px;
  text-align: center;
}
.heading.is-medium {
  font-size :12px;
}
.panel-tabs {
  padding: 0 1em;
  .tabs {
    margin-bottom: -1px;
  }
}

.panel-block .columns {
  width: 100%;
  align-items: center;
}
</style>

<template>
  <div>
    <div v-if="loading">
      <loading-component :loading="true"></loading-component>
    </div>
    <div v-else>
      <div class="panel">
        <div class="panel-block panel-tabs">
          <div class="tabs">
            <ul>
              <li v-for="(value, key) in playlistOptions" @click="setPlaylist(key)" :class="{ 'is-active': key === playlist }"><a>{{ value }}</a></li>
            </ul>
          </div>
        </div>
        <div class="panel-block">
          <div class="columns">
            <div class="column is-2">
              <chart-wins-component></chart-wins-component>
            </div>
            <div class="column is-2">
              {{ GET_PLAYER.numWins }}W {{ GET_PLAYER.numLosses }}L
            </div>
            <div class="column is-3 has-text-centered">
              <div class="title is-5">655 Pts/min</div>
              <div class="subtitle is-6">{{ GET_PLAYER.totalGoals }}/{{ GET_PLAYER.totalAssists }}/{{ GET_PLAYER.totalSaves }}/{{ GET_PLAYER.totalShots }} (..%)</div>
            </div>
          </div>
        </div>
      </div>

      <game-component v-for="game in GET_PLAYER.games" :game="game"></game-component>
    </div>
  </div>
</template>

<script>
import ChartWinsComponent from './ChartWins'
import GameComponent from './Game'
import LoadingComponent from '../../components/Loading'
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
  components: {
    ChartWinsComponent,
    GameComponent,
    LoadingComponent
  },
  computed: {
    loading: function () {
      return this.GET_ARENAS === null || this.GET_PLAYER === null
    }
  },
  data: function () {
    let playlistOptions = options.shortPlaylists()
    return {
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      GET_ARENAS: null,
      GET_PLAYER: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_PLAYER = null
      vm.$store.dispatch('GET_PLAYER', {
        id: vm.playerId,
        playlist: vm.playlist
      }).then(function (data) {
        vm.GET_PLAYER = data
      })
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

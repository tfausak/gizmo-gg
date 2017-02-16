<style scoped lang="scss">
@import "~styles/vars.scss";

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
              <chart-wins-component :wins="stats.wins" :losses="stats.losses"></chart-wins-component>
            </div>
            <div class="column is-2">
              {{ stats.wins }}W {{ stats.losses }}L
            </div>
            <div class="column is-3 has-text-centered">
              <div class="title is-5">{{ stats.ptsPerMin }} Pts/min</div>
              <div class="subtitle is-6">{{ stats.goals }}/{{ stats.assists }}/{{ stats.saves }}/{{ stats.shots }} ({{ stats.accuracy }}%)</div>
            </div>
            <div class="column is-3 has-text-centered">
              <div class="title is-5">{{ stats.goalsFor }} - {{ stats.goalsAgainst }}</div>
            </div>
          </div>
        </div>
      </div>

      <game-component v-for="game in GET_PLAYER.games" :game="game" :playerId="playerId"></game-component>
    </div>
  </div>
</template>

<script>
import ChartWinsComponent from './ChartWins'
import GameComponent from './Game'
import LoadingComponent from '../../components/Loading'
import options from '../../../store/options.js'
import { getPct } from '../../../store/scrubber.js'

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
    },
    stats: function () {
      let vm = this
      let stats = {
        duration: 0,
        wins: 0,
        losses: 0,
        points: 0,
        assists: 0,
        goals: 0,
        saves: 0,
        shots: 0,
        accuracy: 0,
        ptsPerMin: 0,
        goalsFor: 0,
        goalsAgainst: 0
      }
      if (vm.loading) {
        return stats
      }
      _.each(vm.GET_PLAYER.games, function (game) {
        _.each(game.players, function (player) {
          if (player.playerId === _.parseInt(vm.playerId)) {
            stats.duration += game.duration
            stats.points += player.score
            stats.assists += player.assists
            stats.goals += player.goals
            stats.saves += player.saves
            stats.shots += player.shots
            let didWin = player.isOnBlueTeam ? game.blueGoals > game.orangeGoals : game.blueGoals < game.orangeGoals
            stats.wins += didWin ? 1 : 0
            stats.losses += didWin ? 0 : 1
            stats.goalsFor += player.isOnBlueTeam ? game.blueGoals : game.orangeGoals
            stats.goalsAgainst += player.isOnBlueTeam ? game.orangeGoals : game.blueGoals
          }
        })
      })
      stats.accuracy = getPct(stats.goals, stats.shots)
      if (stats.duration) {
        stats.ptsPerMin = _.round(stats.points / (stats.duration / 60))
      }
      return stats
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

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

#gamesRecord {
  font-size: 12px;
  font-weight: bold;
  text-align: center;
}
#gamesDiff {
  color: $grey;
  text-align: center;
  font-size: 12px;
}
#gamesTable {
  font-size: 12px;
  th,
  td {
    padding: 0px 5px;
    text-align: right;
  }
}
.mapBlock {
  padding: 10px;
}
.mapBlockStats {
  padding-left: 10px;
  font-size: 12px;
}
.mapBlockName {
  font-weight: bold;
}
.mapBlockRecord {
  font-size: 11px;
}
</style>

<template>
  <div>
    <div class="panel">
      <div class="panel-block panel-tabs">
        <div class="tabs">
          <ul>
            <li v-for="(value, key) in playlistOptions" @click="setPlaylist(key)" :class="{ 'is-active': key === playlist }"><a>{{ value }}</a></li>
          </ul>
        </div>
      </div>
      <div class="panel-block is-block" v-if="!loading">
        <div class="level">
          <div>
            <chart-wins-component :wins="stats.wins" :losses="stats.losses"></chart-wins-component>
            <div id="gamesRecord">{{ stats.wins }}W {{ stats.losses }}L</div>
            <div id="gamesDiff">{{ stats.goalsFor }} - {{ stats.goalsAgainst }}</div>
          </div>
          <div>
            <table id="gamesTable">
              <thead>
                <tr><th>Total</th><th>Per min</th><th>Stat</th></tr>
              </thead>
              <tbody>
                <tr><td>{{ stats.points }}</td><td>{{ stats.perMin.points }}</td><td>Points</td></tr>
                <tr><td>{{ stats.goals }}</td><td>{{ stats.perMin.goals }}</td><td>Goals</td></tr>
                <tr><td>{{ stats.assists }}</td><td>{{ stats.perMin.assists }}</td><td>Assists</td></tr>
                <tr><td>{{ stats.saves }}</td><td>{{ stats.perMin.saves }}</td><td>Saves</td></tr>
                <tr><td>{{ stats.shots }}</td><td>{{ stats.perMin.shots }}</td><td>Shots</td></tr>
              </tbody>
            </table>
          </div>
          <div class="mapBlock" v-for="mapStats in stats.perMap">
            <div class="level level-chained">
              <figure class="image is-48x48">
                <img :src="'/static/img/maps/' + mapStats.nameSlug + '.jpg'" class="is-circle-dark">
              </figure>
              <div class="mapBlockStats">
                <div class="mapBlockName">{{ mapStats.name }}</div>
                <div class="mapBlockRecord">
                  {{ mapStats.winPct }}%
                  <br>
                  {{ mapStats.wins }}W {{ mapStats.losses }}L
                  <br>
                  {{ mapStats.goalsFor }} - {{ mapStats.goalsAgainst }}
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <div v-if="loading">
      <loading-component :loading="true"></loading-component>
    </div>
    <div v-else>
      <game-component v-for="game in GET_PLAYER.games" :game="game" :playerId="playerId"></game-component>
    </div>
  </div>
</template>

<script>
import ChartWinsComponent from './ChartWins'
import GameComponent from './Game'
import LoadingComponent from '../../components/Loading'
import options from '../../../store/options.js'
import slugger from '../../../store/slugger.js'
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
        perMin: {
          goals: 0,
          points: 0,
          assists: 0,
          saves: 0,
          shots: 0
        },
        perMap: {},
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
            let goalsFor = player.isOnBlueTeam ? game.blueGoals : game.orangeGoals
            let goalsAgainst = player.isOnBlueTeam ? game.orangeGoals : game.blueGoals
            stats.goalsFor += goalsFor
            stats.goalsAgainst += goalsAgainst
            if (!_.has(stats.perMap, game.arena.templateName)) {
              stats.perMap[game.arena.templateName] = {
                games: 0,
                wins: 0,
                losses: 0,
                goalsFor: 0,
                goalsAgainst: 0,
                name: game.arena.templateName,
                nameSlug: slugger.slugMap(game.arena.templateName)
              }
            }
            let tmp = stats.perMap[game.arena.templateName]
            tmp.games++
            tmp.wins += didWin ? 1 : 0
            tmp.losses += didWin ? 0 : 1
            tmp.winPct = getPct(tmp.wins, tmp.wins + tmp.losses)
            tmp.goalsFor += goalsFor
            tmp.goalsAgainst += goalsAgainst
          }
        })
      })
      stats.perMap = _.slice(_.reverse(_.sortBy(stats.perMap, 'games')), 0, 3)
      stats.accuracy = getPct(stats.goals, stats.shots)
      if (stats.duration) {
        stats.perMin.points = _.round(stats.points / (stats.duration / 60), 2)
        stats.perMin.goals = _.round(stats.goals / (stats.duration / 60), 2)
        stats.perMin.assists = _.round(stats.assists / (stats.duration / 60), 2)
        stats.perMin.saves = _.round(stats.saves / (stats.duration / 60), 2)
        stats.perMin.shots = _.round(stats.shots / (stats.duration / 60), 2)
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

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
  width: auto;
  margin: 0 auto;
  th,
  td {
    padding: 0px 5px;
    text-align: right;
  }
}
.gamesTableCol {
  background-color: #f5f5f5;
  td.has-text-left,
  th.has-text-left {
    text-align: left!important;
  }
}
.groupStats {
  padding-left: 10px;
  font-size: 11px;
}
.groupName {
  font-weight: bold;
}
.groupRecord {
  font-size: 11px;
}
</style>

<template>
  <div>
    <div class="panel">
      <div class="panel-block is-block" v-if="!loading">
        <div class="columns">
          <div class="column is-2">
            <chart-wins-component :wins="stats.wins" :losses="stats.losses"></chart-wins-component>
            <div id="gamesRecord">{{ stats.wins }}W {{ stats.losses }}L</div>
            <div id="gamesDiff">
              <span v-tooltip.bottom-center="{ content: 'Goals For - Goals Against', classes: 'bottom' }">{{ stats.goalsFor }} - {{ stats.goalsAgainst }}</span>
            </div>
          </div>
          <div class="column is-3 gamesTableCol">
            <table id="gamesTable">
              <thead>
                <tr><th class="has-text-left">Stat</th><th>Total</th><th>Per game</th></tr>
              </thead>
              <tbody>
                <tr><td class="has-text-left">Points</td><td>{{ stats.points }}</td><td>{{ stats.perGame.points }}</td></tr>
                <tr><td class="has-text-left">Goals</td><td>{{ stats.goals }}</td><td>{{ stats.perGame.goals }}</td></tr>
                <tr><td class="has-text-left">Assists</td><td>{{ stats.assists }}</td><td>{{ stats.perGame.assists }}</td></tr>
                <tr><td class="has-text-left">Saves</td><td>{{ stats.saves }}</td><td>{{ stats.perGame.saves }}</td></tr>
                <tr><td class="has-text-left">Shots</td><td>{{ stats.shots }}</td><td>{{ stats.perGame.shots }}</td></tr>
              </tbody>
            </table>
          </div>
          <div class="column is-7">
            <div>
              <div class="columns">
                <div class="column is-4" v-for="mapStats in stats.perMap">
                  <div class="level level-chained">
                    <figure class="image is-32x32">
                      <img :src="'/static/img/maps/' + mapStats.nameSlug + '.jpg'" class="is-circle-dark">
                    </figure>
                    <div class="groupStats">
                      <div class="groupName">{{ mapStats.name }}</div>
                      <div class="groupRecord">
                        {{ mapStats.winPct }}%
                        <br>
                        {{ mapStats.wins }}W {{ mapStats.losses }}L
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div>
              <div class="columns">
                <div class="column is-4" v-for="bodyStats in stats.perBody">
                  <div class="level level-chained">
                    <figure class="image is-32x32">
                      <img :src="'/static/img/bodies/' + bodyStats.nameSlug + '.png'" class="is-circle-dark">
                    </figure>
                    <div class="groupStats">
                      <div class="groupName">{{ bodyStats.name }}</div>
                      <div class="groupRecord">
                        {{ bodyStats.winPct }}%
                        <br>
                        {{ bodyStats.wins }}W {{ bodyStats.losses }}L
                      </div>
                    </div>
                  </div>
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
import slugger from '../../../store/slugger.js'
import { getPct } from '../../../store/scrubber.js'
import { EventBus } from '../../../store/event-bus.js'

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
        perGame: {
          goals: 0,
          points: 0,
          assists: 0,
          saves: 0,
          shots: 0
        },
        perMap: {},
        perBody: {},
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

            // map
            let mapName = game.arena.templateName
            if (!_.has(stats.perMap, mapName)) {
              stats.perMap[mapName] = {
                games: 0,
                wins: 0,
                losses: 0,
                goalsFor: 0,
                goalsAgainst: 0,
                name: mapName,
                nameSlug: slugger.slugMap(mapName)
              }
            }
            let tmp = stats.perMap[mapName]
            tmp.games++
            tmp.wins += didWin ? 1 : 0
            tmp.losses += didWin ? 0 : 1
            tmp.winPct = getPct(tmp.wins, tmp.wins + tmp.losses)
            tmp.goalsFor += goalsFor
            tmp.goalsAgainst += goalsAgainst

            // body
            let bodyName = player.loadout.bodyName
            if (!_.has(stats.perBody, bodyName)) {
              stats.perBody[bodyName] = {
                games: 0,
                wins: 0,
                losses: 0,
                goalsFor: 0,
                goalsAgainst: 0,
                name: bodyName,
                nameSlug: slugger.slugBody(bodyName)
              }
            }
            tmp = stats.perBody[bodyName]
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
      stats.perBody = _.slice(_.reverse(_.sortBy(stats.perBody, 'games')), 0, 3)
      stats.accuracy = getPct(stats.goals, stats.shots)
      if (stats.duration) {
        stats.perGame.points = _.round(stats.points / (stats.duration / 60) * 5, 2)
        stats.perGame.goals = _.round(stats.goals / (stats.duration / 60) * 5, 2)
        stats.perGame.assists = _.round(stats.assists / (stats.duration / 60) * 5, 2)
        stats.perGame.saves = _.round(stats.saves / (stats.duration / 60) * 5, 2)
        stats.perGame.shots = _.round(stats.shots / (stats.duration / 60) * 5, 2)
      }
      return stats
    }
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
    }
  },
  props: [ 'playerId', 'playlist' ],
  watch: {
    playlist: function (val) {
      this.fetchData()
    }
  }
}
</script>

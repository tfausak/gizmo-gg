<style lang="scss" scoped>
@import "~styles/vars.scss";
tr.is-label td {
  background-color: rgba(0, 0, 0, 0.1);
  font-weight: 400;
}
</style>

<template>
  <div class="container">
    <div v-if="loading">
      <loading-component :loading="true"></loading-component>
    </div>
    <div v-else>
      <table class="table">
        <thead>
          <tr>
            <th></th>
            <th v-for="player in game.players">
              {{ player.name }}
            </th>
          </tr>
        </thead>
        <tbody>

          <tr class="is-label">
            <td colspan="100%">Stats</td>
          </tr>
          <tr>
            <th>Goals</th>
            <td v-for="player in game.players">
              {{ player.goals }}
            </td>
          </tr>
          <tr>
            <th>Assists</th>
            <td v-for="player in game.players">
              {{ player.assists }}
            </td>
          </tr>
          <tr>
            <th>Saves</th>
            <td v-for="player in game.players">
              {{ player.saves }}
            </td>
          </tr>
          <tr>
            <th>Shots</th>
            <td v-for="player in game.players">
              {{ player.shots }}
            </td>
          </tr>

          <tr class="is-label">
            <td colspan="100%">Camera</td>
          </tr>
          <tr>
            <th>Camera.Angle</th>
            <td v-for="player in game.players">
              {{ player.camera.angle }}
            </td>
          </tr>
          <tr>
            <th>Camera.Distance</th>
            <td v-for="player in game.players">
              {{ player.camera.distance }}
            </td>
          </tr>
          <tr>
            <th>Camera.Fov</th>
            <td v-for="player in game.players">
              {{ player.camera.fov }}
            </td>
          </tr>
          <tr>
            <th>Camera.Height</th>
            <td v-for="player in game.players">
              {{ player.camera.height }}
            </td>
          </tr>
          <tr>
            <th>Camera.Stiffness</th>
            <td v-for="player in game.players">
              {{ player.camera.stiffness }}
            </td>
          </tr>
          <tr>
            <th>Camera.SwivelSpeed</th>
            <td v-for="player in game.players">
              {{ player.camera.swivelSpeed }}
            </td>
          </tr>

          <tr class="is-label">
            <td colspan="100%">Loadout</td>
          </tr>
          <tr>
            <th>Loadout.bodyName</th>
            <td v-for="player in game.players">
              {{ player.loadout.bodyName }}
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>

<script>
import { getTeamSize } from '../../store/scrubber.js'
import slugger from '../../store/slugger.js'
import ScoreboardComponent from '../player/components/Scoreboard'
import LoadingComponent from '../components/Loading'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  components: {
    LoadingComponent,
    ScoreboardComponent
  },
  computed: {
    loading: function () {
      return this.GET_GAME === null || this.compiled === null
    }
  },
  data: function () {
    return {
      GET_GAME: null,
      compiled: null
    }
  },
  methods: {
    compileData: function () {
      let vm = this
      vm.game = vm.GET_GAME
      let totalScore = 0
      let blueTeam = []
      let orangeTeam = []
      let maxPerf = 0
      let maxScore = 0
      let teamSize = getTeamSize(vm.game.playlistName)

      _.each(vm.game.players, function (gplayer) {
        gplayer.bodySlug = slugger.slugBody(gplayer.loadout.bodyName)
        totalScore += gplayer.score
        maxScore = _.max([maxScore, gplayer.score])
        if (gplayer.isOnBlueTeam) {
          blueTeam.push(gplayer)
        } else {
          orangeTeam.push(gplayer)
        }
      })
      if (maxScore) {
        maxPerf = _.round(maxScore / totalScore * 100, 2)
      }

      // sort teams by score desc
      orangeTeam = _.reverse(_.sortBy(orangeTeam, function (tplayer) {
        return tplayer.score
      }))
      blueTeam = _.reverse(_.sortBy(blueTeam, function (tplayer) {
        return tplayer.score
      }))

      let min = _.floor(vm.game.duration / 60)
      let sec = vm.game.duration - (min * 60)

      vm.compiled = {
        totalScore: totalScore,
        blueTeam: blueTeam,
        orangeTeam: orangeTeam,
        maxPerf: maxPerf,
        maxScore: maxScore,
        teamSize: teamSize,
        min: min,
        sec: sec
      }
    },
    fetchData: function () {
      let vm = this
      vm.GET_GAME = null
      vm.compiled = null
      vm.$store.dispatch('GET_GAME', {
        id: vm.gameId
      }).then(function (data) {
        vm.GET_GAME = data
        vm.compileData()
      })
    }
  },
  props: [ 'gameId' ]
}
</script>

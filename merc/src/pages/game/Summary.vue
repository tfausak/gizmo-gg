<style lang="scss" scoped>
</style>

<template>
  <div class="container">
    <div v-if="loading">
      <loading-component :loading="true"></loading-component>
    </div>
    <div v-else>
      <div class="scoreboards">
        <scoreboard-component :players="compiled.blueTeam" :team="'Blue'" :goals="game.blueGoals" :maxPerf="compiled.maxPerf" :totalScore="compiled.totalScore" :teamSize="compiled.teamSize" :playerId="0"></scoreboard-component>
        <scoreboard-component :players="compiled.orangeTeam" :team="'Orange'" :goals="game.orangeGoals" :maxPerf="compiled.maxPerf" :totalScore="compiled.totalScore" :teamSize="compiled.teamSize" :playerId="0"></scoreboard-component>
      </div>
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

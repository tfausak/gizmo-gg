<style scoped lang="scss">
@import "~styles/vars.scss";

.panel-block .columns {
  width: 100%;
  align-items: center;
}
.panel-win .panel-block,
.panel-loss .panel-block {
  hr {
    background-color: rgba(0, 0, 0, 0.2);
    margin: 4px auto 3px;
    max-width: 30px;
  }
}
.panel-win,
.panel-loss {
  margin-bottom: 10px!important;
}
.panel-win {
  background-color: lighten($solarized_blue, 30%);
}
.panel-loss {
  background-color: lighten($solarized_red, 35%);
}
.teamTable {
  font-size: 12px;
  width: auto;
  margin: 0 auto;
  th {
    width: 100px;
  }
  a {
    color: rgba(0, 0, 0, 0.6);
  }
  a:hover {
    color: rgba(0, 0, 0, 0.9);
  }
}
.summarySection {
  font-size: 12px;
  text-align: center;
  width: 70px;
}
.bodySection,
.mapSection {
  font-size: 12px;
  width: 80px;
  text-align: center;
  figure {
    margin: 0 auto;
  }
}
.statsSection {
  text-align: center;
  width: 140px;
  font-size: 12px;
}
.scoreSection {
  width: 100px;
  text-align: center;
  font-size: 20px;
  font-weight: bold;
}
.expander,
.gameDetails {
  background-color: rgba(0, 0, 0, 0.1);
}
.gameDetails {
  border: 0;
}
.expander {
  height: 100px;
  text-align: center;
  display: flex;
  justify-content: center;
  align-items: center;
  margin: -0.5em -0.75em;
  font-weight: bold;
  color: rgba(0, 0, 0, 0.7);
  &:hover {
    background-color: rgba(0, 0, 0, 0.15);
    cursor: pointer;
  }
}
.gameStuff {
  border-color: rgba(0, 0, 0, 0.1)!important;
}
</style>

<template>
  <div class="panel" :class="{ 'panel-win': isWin, 'panel-loss': !isWin }" v-if="player">
    <div class="panel-block gameStuff">
      <div class="columns is-gapless is-mobile">
        <div class="column summarySection is-narrow">
          <strong>{{ playlistSlug }}</strong>
          <br>{{ playedAt }}
          <hr>
          <strong v-if="isWin">Victory</strong>
          <strong v-else>Defeat</strong><br>
          {{ fDuration }}
        </div>
        <div class="column bodySection is-narrow">
          <figure class="image is-32x32 is-circle-dark">
            <img :src="'/static/img/bodies/' + player.bodySlug + '.png'">
          </figure>
          {{ player.loadout.bodyName }}
        </div>
        <div class="column mapSection is-narrow">
          <figure class="image is-32x32">
            <img :src="'/static/img/maps/' + templateSlug + '.jpg'"  class="is-circle-dark">
          </figure>
          {{ game.arena.templateName }}
        </div>
        <div class="column statsSection is-narrow">
          <div class="title is-5" style="margin-bottom: 0; font-weight: normal;">{{ player.score }} Points</div>
          {{ player.goals }} / {{ player.assists }} / {{ player.saves }} / {{ player.shots }}
        </div>
        <div class="column scoreSection is-narrow">
          {{ teamGoals }} - {{ oppGoals }}
        </div>
        <div class="column">
          <table class="teamTable">
            <thead>
              <tr>
                <th>Orange</th>
                <th>Blue</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="i in maxPlayers">
                <td v-if="i <= orangeTeam.length">
                  <a @click="goPlayer(orangeTeam[i - 1].playerId)">{{ orangeTeam[i - 1].name }}</a>
                </td>
                <td v-if="i <= blueTeam.length">
                  <router-link :to="'/player/' + blueTeam[i - 1].playerId">{{ blueTeam[i - 1].name }}</router-link>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
        <div class="column expandSection is-narrow">
          <div class="expander" @click="toggleDetails">
            <i class="fa fa-angle-up" v-if="expanded"></i>
            <i class="fa fa-angle-down" v-else></i>
          </div>
        </div>
      </div>
    </div>
    <div class="panel-block gameDetails" v-if="expanded">
      <p class="heading">Game Details</p>
    </div>
  </div>
</template>

<script>
import { getPct } from '../../../store/scrubber.js'
import slugger from '../../../store/slugger.js'

var moment = require('moment')
var _ = require('lodash')

export default {
  data: function () {
    let vm = this
    let player = null
    let totalScore = 0
    let blueTeam = []
    let orangeTeam = []
    _.each(vm.game.players, function (gplayer) {
      totalScore += gplayer.score
      if (gplayer.playerId === _.parseInt(vm.playerId)) {
        player = gplayer
      }
      if (gplayer.isOnBlueTeam) {
        blueTeam.push(gplayer)
      } else {
        orangeTeam.push(gplayer)
      }
    })
    player.bodySlug = slugger.slugBody(player.loadout.bodyName)
    if (player.score) {
      player.scorePct = 100
    }
    if (totalScore) {
      player.scorePct = _.round(player.score / totalScore * 100)
    }
    player.accuracy = getPct(player.goals, player.shots)
    let teamGoals = 0
    let oppGoals = 0
    if (player.isOnBlueTeam) {
      teamGoals = vm.game.blueGoals
      oppGoals = vm.game.orangeGoals
    } else {
      teamGoals = vm.game.orangeGoals
      oppGoals = vm.game.blueGoals
    }
    let min = _.floor(vm.game.duration / 60)
    let sec = vm.game.duration - (min * 60)
    return {
      playedAt: moment(vm.game.playedAt).fromNow(),
      fDuration: min + 'm ' + sec + 's',
      teamGoals: teamGoals,
      oppGoals: oppGoals,
      isWin: teamGoals > oppGoals,
      player: player,
      orangeTeam: orangeTeam,
      blueTeam: blueTeam,
      maxPlayers: _.max([orangeTeam.length, blueTeam.length]),
      templateSlug: slugger.slugMap(vm.game.arena.templateName),
      playlistSlug: slugger.slugPlaylist(vm.game.playlistName),
      expanded: false
    }
  },
  methods: {
    goPlayer: function (playerId) {
      console.log('goPlayer', playerId)
      this.$router.push({
        name: 'player.summary',
        params: { id: playerId }
      })
    },
    toggleDetails: function () {
      this.expanded = !this.expanded
    }
  },
  props: [ 'game', 'playerId' ]
}
</script>

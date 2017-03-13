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
  margin-bottom: 4px!important;
}
.panel-win {
  background-color: lighten($solarized_blue, 40%);
  .offcolor,
  a {
    color: #555;
  }
  a:hover {
    color: darken(saturate($solarized_blue, 10%), 20%);
  }
}
.panel-loss {
  background-color: lighten($solarized_red, 40%);
  .offcolor,
  a {
    color: #555;
  }
  a:hover {
    color: darken(saturate($solarized_red, 10%), 20%);
  }
}
.gameResult {
  font-weight: bold;
}
.teamTable {
  font-size: 12px;
  width: auto;
  margin: 0 auto;
  .playerName a {
    white-space: nowrap;
    overflow: hidden;
    width: 100px;
    display: block;
  }
}
.summarySection {
  font-size: 12px;
  line-height: 1.3;
  text-align: center;
  width: 80px;
  .playedAt {
    white-space: nowrap;
  }
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
.mapSection-multi {
  font-size: 10px;
  line-height: 1.1;
}

.statsSection {
  text-align: center;
  width: 140px;
  font-size: 12px;
}
.scoreSection {
  width: 90px;
  text-align: center;
  font-size: 23px;
  font-weight: thin;
}
.expander,
.gameDetails {
  background-color: rgba(0, 0, 0, 0.05);
}
.gameDetails {
  border: 0;
  display: block;
}
.expander {
  height: 75px;
  text-align: center;
  display: flex;
  justify-content: center;
  align-items: center;
  padding: 0 0.5em;
  font-weight: bold;
  color: rgba(0, 0, 0, 0.7);
  &:hover {
    background-color: rgba(0, 0, 0, 0.15);
    cursor: pointer;
  }
}
.gameStuff {
  border-color: rgba(0, 0, 0, 0.1)!important;
  padding: 0;
}
</style>

<template>
  <div class="panel" :class="{ 'panel-win': isWin, 'panel-loss': !isWin }" v-if="player">
    <div class="panel-block gameStuff">
      <div class="columns is-gapless is-mobile">
        <div class="column summarySection is-narrow">
          <strong>{{ playlistSlug }}</strong>
          <br><span class="playedAt">{{ playedAt }}</span>
          <hr>
          <div class="gameResult offcolor">
            <span v-if="isWin">Victory</span>
            <span v-else>Defeat</span>
          </div>
          {{ fDuration }}
        </div>
        <div class="column bodySection is-narrow">
          <figure class="image is-48x48 is-circle-dark">
            <img :src="'/static/img/bodies/' + player.bodySlug + '.png'">
          </figure>
          {{ player.loadout.bodyName }}
        </div>
        <div class="column mapSection is-narrow">

          <div v-if="game.arena.templateName === game.arena.modelName">
            <figure class="image is-48x48">
              <img :src="'/static/img/maps/' + templateSlug + '.jpg'"  class="is-circle-dark">
            </figure>
            {{ game.arena.templateName }}
          </div>
          <div v-else class="mapSection-multi">
            <figure class="image is-48x48">
              <img :src="'/static/img/maps/' + modelSlug + '.jpg'"  class="is-circle-dark">
            </figure>
            <div>{{ game.arena.templateName }}</div>
            <div>{{ game.arena.modelName }}</div>
          </div>
        </div>
        <div class="column statsSection is-narrow">
          <div class="title is-5" style="margin-bottom: 0; font-weight: normal;">{{ player.score }} Points</div>
          <span v-tooltip.bottom-center="{ content: 'Goals / Assists / Saves / Shots', classes: 'bottom' }">{{ player.goals }} / {{ player.assists }} / {{ player.saves }} / {{ player.shots }}</span>
        </div>
        <div class="column scoreSection is-narrow">
          <span v-tooltip.top-center="'Goals For - Goals Against'">{{ teamGoals }} - {{ oppGoals }}</span>
        </div>
        <div class="column">
          <table class="teamTable">
            <thead>
              <tr>
                <th>Blue</th>
                <th>Orange</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="i in maxPlayers">
                <td v-if="i <= blueTeam.length">
                  <span class="playerName">
                    <router-link :to="'/player/' + blueTeam[i - 1].playerId + '/summary'">
                      {{ blueTeam[i - 1].name }}
                    </router-link>
                  </span>
                </td>
                <td v-if="i <= orangeTeam.length">
                  <span class="playerName">
                    <router-link :to="'/player/' + orangeTeam[i - 1].playerId + '/summary'">
                      {{ orangeTeam[i - 1].name }}
                    </router-link>
                  </span>
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
      <scoreboard-component :players="blueTeam" :team="'Blue'" :goals="game.blueGoals" :maxPerf="maxPerf" :totalScore="totalScore" :teamSize="teamSize" :playerId="playerId"></scoreboard-component>
      <scoreboard-component :players="orangeTeam" :team="'Orange'" :goals="game.orangeGoals" :maxPerf="maxPerf" :totalScore="totalScore" :teamSize="teamSize" :playerId="playerId"></scoreboard-component>
    </div>
  </div>
</template>

<script>
import { getPct, getTeamSize } from '../../../store/scrubber.js'
import slugger from '../../../store/slugger.js'
import ScoreboardComponent from './Scoreboard'

var moment = require('moment')
var _ = require('lodash')

export default {
  components: {
    ScoreboardComponent
  },
  data: function () {
    let vm = this
    let player = null
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
      if (gplayer.playerId === _.parseInt(vm.playerId)) {
        player = gplayer
      }
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
      modelSlug: slugger.slugMapModel(vm.game.arena.modelName),
      playlistSlug: slugger.slugPlaylist(vm.game.playlistName),
      expanded: false,
      maxPerf: maxPerf,
      totalScore: totalScore,
      teamSize: teamSize
    }
  },
  methods: {
    toggleDetails: function () {
      this.expanded = !this.expanded
    }
  },
  props: [ 'game', 'playerId' ]
}
</script>

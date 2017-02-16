<style scoped lang="scss">
@import "~styles/vars.scss";

.panel-block .columns {
  width: 100%;
  align-items: center;
}
.panel-win .panel-block,
.panel-loss .panel-block {
  min-height: 100px;
  hr {
    background-color: rgba(0, 0, 0, 0.2);
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
  background-color: lighten($solarized_red, 40%);
}
</style>

<template>
  <div class="panel" :class="{ 'panel-win': isWin, 'panel-loss': !isWin }" v-if="player">
    <div class="panel-block">
      <div class="columns">
        <div class="column is-2 has-text-centered text-small">
          <strong>{{ game.playlistName }}</strong>
          <br>{{ playedAt }}
          <hr style="margin: 4px 0;">
          <strong v-if="isWin">Victory</strong>
          <strong v-else>Defeat</strong><br>
          {{ fDuration }}
        </div>
        <div class="column is-2">
          <figure class="image is-48x48 is-circle-dark">
            <img :src="'/static/img/bodies/octane.png'">
          </figure>
          {{ game.bodyName }}
        </div>
        <div class="column is-2 has-text-centered">
          <div class="title is-5" style="margin-bottom: 0; font-weight: normal;">{{ player.score }} Points</div>
          <div>{{ player.goals }}/{{ player.assists }}/{{ player.saves }}/{{ player.shots }} ({{ player.accuracy }}%)</div>
        </div>
        <div class="column is-2 has-text-centered">
          <div class="title is-4">{{ teamGoals }} - {{ oppGoals }}</div>
        </div>
        <div class="column is-2">
          {{ game.arena.templateName }}
        </div>
        <div class="column is-2">
          <div v-for="gplayer in game.players">
            <span>{{ gplayer.name }}</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { getPct } from '../../../store/scrubber.js'

var moment = require('moment')
var _ = require('lodash')

export default {
  data: function () {
    let vm = this
    let player = null
    _.each(vm.game.players, function (gplayer) {
      if (gplayer.playerId === _.parseInt(vm.playerId)) {
        player = gplayer
      }
    })
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
      player: player
    }
  },
  props: [ 'game', 'playerId' ]
}
</script>

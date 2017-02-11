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
  <div class="panel" :class="{ 'panel-win': isWin, 'panel-loss': !isWin }">
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
          <figure class="image is-48x48 is-circle-dark-128x128">
            <img :src="'/static/img/bodies/' + fBody + '.png'">
          </figure>
          {{ game.bodyName }}
        </div>
        <div class="column is-2 has-text-centered">
          <div class="title is-5" style="margin-bottom: 0; font-weight: normal;">{{ game.score }} Points</div>
          <div>{{ game.goals }}/{{ game.assists }}/{{ game.saves }}/{{ game.shots }} ({{ accuracy }}%)</div>
        </div>
        <div class="column is-2 has-text-centered">
          <div class="title is-4">{{ game.yourScore }} - {{ game.theirScore }}</div>
        </div>
        <div class="column is-3">
          <div class="level">
            <div class="level-item">
              <figure class="image is-48x48">
                <img :src="'/static/img/maps/standard.jpg'">
              </figure>
            </div>
            <div class="level-item">
              <div>
                <div class="heading">Standard</div>
                <div>{{ game.arenaName }}</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
var moment = require('moment')
var _ = require('lodash')

export default {
  props: [ 'game' ],
  data: function () {
    let min = _.floor(this.game.duration / 60)
    let sec = this.game.duration - (min * 60)
    let accuracy = 0
    if (this.game.goals > 0) {
      accuracy = 100
    }
    if (this.game.shots > 0) {
      accuracy = _.min([100, _.round(this.game.goals / this.game.shots * 100)])
    }
    let fBody = _.lowerCase(this.game.bodyName)
    fBody = _.replace(fBody, / /g, '-')
    return {
      playedAt: moment(this.game.playedAt).fromNow(),
      fDuration: min + 'm ' + sec + 's',
      accuracy: parseInt(accuracy),
      fBody: fBody,
      isWin: this.game.yourScore > this.game.theirScore
    }
  }
}
</script>

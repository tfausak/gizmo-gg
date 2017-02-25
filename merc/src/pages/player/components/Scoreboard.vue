<style lang="scss" scoped>
.scoreboard {
  table {
    margin: 0 auto 10px;
    background-color: transparent;
    font-size: 12px;
    width: auto;
    border: 1px solid rgba(255, 255, 255, 0.7);
    .progress {
      width: 100px;
    }
    th {
      background-color: rgba(255, 255, 255, 0.2);
      font-weight: normal;
      width: 50px;
      white-space: nowrap;
      overflow: hidden;
      color: rgba(0, 0, 0, 0.7);
      vertical-align: bottom;
    }
    .perfTh {
      text-align: left;
    }
    .teamTh {
      width: 300px;
      font-size: 21px;
      text-transform: uppercase;
      letter-spacing: 1px;
      font-weight: normal;
      text-align: left;
    }
    .teamScore {
      display: inline-block;
    }
    .playerBody {
      padding-left: 1em;
      width: 50px;
    }
    .playerName {
      text-align: left;
    }
    a {
      color: #666;
      font-weight: normal;
      &:hover {
        color: #000;
      }
    }
    td {
      background-color: rgba(255, 255, 255, 0.4);
      font-size: 14px;
    }
    td, th {
      border-color: rgba(0, 0, 0, 0.1);
      border-left: 0;
      border-right: 0;
      padding: 2px 6px;
      text-align: right;
    }
    tr:hover {
      background-color: inherit;
    }
  }
}
</style>

<template>
  <div class="scoreboard">
    <table class="table is-narrow scoreboard">
      <thead>
        <tr>
          <th colspan="2" class="teamTh">
            <span class="teamScore">{{ goals }}</span>
            <span class="teamName">{{ team }}</span>
          </th>
          <th>Goals</th>
          <th>Assists</th>
          <th>Saves</th>
          <th>Shots</th>
          <th>Points</th>
          <th>Accuracy</th>
          <th class="perfTh">Performance</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="gplayer in players">
          <td class="playerBody">
            <figure class="image is-24x24 is-circle-dark">
              <img :src="'/static/img/bodies/' + gplayer.bodySlug + '.png'">
            </figure>
          </td>
          <td class="playerName">
            <router-link :to="'/player/' + gplayer.playerId">
              {{ gplayer.name }}
            </router-link>
          </td>
          <td>{{ gplayer.goals }}</td>
          <td>{{ gplayer.assists }}</td>
          <td>{{ gplayer.saves }}</td>
          <td>{{ gplayer.shots }}</td>
          <td>{{ gplayer.score }}</td>
          <td>{{ gplayer.accuracy }}%</td>
          <td>
            <div class="level level-chained">
              <div class="level-item">
                <progress class="progress is-small" :class="{ 'is-primary': gplayer.perf >= regularPerfPct, 'is-success': gplayer.perf >= goodPerfPct, 'is-danger': gplayer.perf >= amazePerfPct }" :value="gplayer.perf" :max="maxPerf"></progress>
              </div>
              <div class="level-item">
                {{ gplayer.perf }}%
              </div>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
import { getPct } from '../../../store/scrubber.js'

var _ = require('lodash')

export default {
  data: function () {
    let vm = this
    let accuracy = 0
    _.each(vm.players, function (player) {
      player.accuracy = getPct(player.goals, player.shots)
      player.perf = getPct(player.score, vm.totalScore)
    })
    let regularPerfPct = 100 / (vm.teamSize * 2)
    let goodPerfPct = 200 / (vm.teamSize * 2)
    let amazePerfPct = 300 / (vm.teamSize * 2)
    if (vm.teamSize === 1) {
      regularPerfPct = 50
      goodPerfPct = 62.5
      amazePerfPct = 75
    } else if (vm.teamSize === 2) {
      regularPerfPct = 25
      goodPerfPct = 33.3
      amazePerfPct = 50
    } else if (vm.teamSize === 3) {
      regularPerfPct = 16.6
      goodPerfPct = 25
      amazePerfPct = 33.3
    } else if (vm.teamSize === 4) {
      regularPerfPct = 12.5
      goodPerfPct = 16.6
      amazePerfPct = 25
    }
    return {
      accuracy: accuracy,
      regularPerfPct: regularPerfPct,
      goodPerfPct: goodPerfPct,
      amazePerfPct: amazePerfPct
    }
  },
  props: [ 'players', 'goals', 'team', 'maxPerf', 'totalScore', 'teamSize' ]
}
</script>

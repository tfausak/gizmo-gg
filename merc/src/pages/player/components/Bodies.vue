<style lang="scss" scoped>
@import "~styles/vars.scss";

.mapStats {
  padding: 0;
}
.mapStatsLevel {
  font-size: 13px;
  margin: 0!important;
  width: 100%;
  .column {
    padding: 0;
  }
  .mapImage {
    flex-grow: 0;
    padding-right: 5px;
  }
  .mapPlayer {
    padding-top: 3px;
    text-align: center;
    .score {
      font-size: 15px;
    }
  }
  .mapDesc {
    padding-top: 6px;
    .heading {
      margin-bottom: 0;
    }
  }
  .mapRecord {
    flex-grow: 1;
    padding-top: 3px;
    text-align: center;
    .recordPct {
      font-size: 14px;
    }
  }
}
</style>

<template>
  <div class="panel">
    <p class="panel-heading panel-squish">
      <span class="heading">Battle-Cars (this season) - <router-link :to="'/player/' + playerId + '/battle-cars'">More</router-link></span>
    </p>
    <div class="panel-block" v-if="loading">
      <loading-component :loading="true"></loading-component>
    </div>
    <div class="panel-block" v-if="source && !source.length">
      <span class="text-muted text-italic">No results</span>
    </div>
    <div v-if="source">
      <div class="panel-block mapStats" v-for="body in source">
        <div class="columns mapStatsLevel">
          <div class="column mapImage">
            <figure class="image is-48x48">
              <img :src="'/static/img/bodies/' + body.slug + '.png'">
            </figure>
          </div>
          <div class="column is-block mapDesc">
            <div class="heading">{{ body.bodyName }}</div>
            <div>{{ body.numGames }} Games</div>
          </div>
          <div class="column is-block mapPlayer">
            <div class="score">
              <span v-tooltip.top-center="'Points per Game'">
                {{ body.perGame.score }}
              </span>
            </div>
            <div class="text-muted">
              <span v-tooltip.bottom-center="{ content: 'Per Game:<br>Goals / Assists / Saves / Shots', classes: 'bottom' }">
                {{ body.perGame.goals }} / {{ body.perGame.assists }} / {{ body.perGame.saves }} / {{ body.perGame.shots }}
              </span>
            </div>
          </div>
          <div class="column is-block mapRecord">
            <div class="recordPct">{{ body.winPct }}%</div>
            <div class="text-muted">{{ body.numWins }}W {{ body.numLosses }}L</div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import LoadingComponent from '../../components/Loading'
import slugger from '../../../store/slugger.js'
import { getPct } from '../../../store/scrubber.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  computed: {
    loading: function () {
      return this.GET_PLAYER_BODIES === null
    }
  },
  components: {
    LoadingComponent
  },
  data: function () {
    return {
      GET_PLAYER_BODIES: null,
      source: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_PLAYER_BODIES = null
      vm.source = null
      vm.$store.dispatch('GET_PLAYER_BODIES', {
        id: vm.playerId,
        playlist: vm.playlist,
        time: 'season'
      }).then(function (data) {
        vm.GET_PLAYER_BODIES = data
        vm.compileData()
      })
    },
    compileData: function () {
      let vm = this
      if (vm.loading) {
        return
      }
      vm.source = []
      _.each(vm.GET_PLAYER_BODIES, function (row) {
        row.slug = slugger.slugBody(row.bodyName)
        row.winPct = _.round(getPct(row.numWins, row.numGames))
        row.perGame = {
          score: 0,
          goals: 0,
          assists: 0,
          saves: 0,
          shots: 0
        }
        if (row.totalDuration > 0) {
          row.perGame = {
            score: _.round(row.totalScore / (row.totalDuration / 60) * 5),
            goals: _.round(row.totalGoals / (row.totalDuration / 60) * 5, 1).toFixed(1),
            assists: _.round(row.totalAssists / (row.totalDuration / 60) * 5, 1).toFixed(1),
            saves: _.round(row.totalSaves / (row.totalDuration / 60) * 5, 1).toFixed(1),
            shots: _.round(row.totalShots / (row.totalDuration / 60) * 5, 1).toFixed(1)
          }
        }
        vm.source.push(row)
      })

      vm.source = _.filter(vm.source, function (row) {
        return row.numGames
      })

      vm.source = _.slice(_.reverse(_.sortBy(vm.source, function (row) {
        return row.numGames
      })), 0, 5)
    },
    setPlaylist: function (key) {
      this.playlist = key
    }
  },
  props: [ 'playerId', 'playlist' ],
  watch: {
    playlist: function (val) { this.fetchData() }
  }
}
</script>

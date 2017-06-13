<style lang="scss" scoped>
@import "~styles/vars.scss";
.panel-chart {
  background-color: $white-ter;
  border-radius: 3px;
}
.panel-options {
  border-bottom: 1px solid #ddd;
  .tabs {
    margin-bottom: -1px;
  }
}
.echarts {
  width: 750px!important;
  height: 350px!important;
  margin: 0 auto!important;
}
</style>

<template>
  <div class="container">
    <div class="panel panel-chart">
      <div class="panel-options">
        <div class="level">
          <div class="level-left">
            <div class="level-item">
              <div class="tabs">
                <ul>
                  <li v-for="(value, key) in playlistOptionsSpec" :class="{ 'is-active': key == playlist }" @click="setPlaylist(key)"><a>{{ value }}</a></li>
                </ul>
              </div>
            </div>
          </div>
          <div class="level-right">
            <div class="level-item">
              <!--<div class="tabs">
                <ul>
                  <li v-for="(value, key) in shortTimeOptions" :class="{ 'is-active': key == time }" @click="setTime(key)"><a>{{ value }}</a></li>
                </ul>
              </div>-->
            </div>
          </div>
        </div>
      </div>
      <div class="panel-block">
        <loading-component :loading="loading"></loading-component>
        <echart :options="chartOptions" v-if="chartOptions && !loading"></echart>
      </div>
    </div>
  </div>
</template>

<script>
import LoadingComponent from '../components/Loading'
import FilterTimeMixin from '../mixins/FilterTimeMixin'
import PlaylistOptionsMixin from '../mixins/PlaylistOptionsMixin'
import ranks from '../../store/ranks.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
    this.updateChartOptions()
  },
  components: {
    LoadingComponent
  },
  computed: {
    loading: function () {
      return this.GET_PLAYER_RANK === null
    }
  },
  data: function () {
    return {
      GET_PLAYER_RANK: null,
      playlist: 'ranked2v2',
      time: 'season',
      chartOptions: null
    }
  },
  methods: {
    setPlaylist: function (p) {
      this.playlist = p
      this.updateChartOptions()
    },
    setTime: function (t) {
      this.time = t
    },
    compileData: function () {
    },
    fetchData: function () {
      var vm = this
      vm.GET_PLAYER_RANK = null
      vm.$store.dispatch('GET_PLAYER_RANK', {
        id: vm.playerId,
        time: vm.time
      }).then(function (data) {
        vm.GET_PLAYER_RANK = data
        vm.compileData()
      })
    },
    updateChartOptions: function () {
      if (this.loading) {
        return {}
      }
      let arr = this.generateChartSeries(this.playlist)
      let series = arr[0]
      let visualMap = arr[1]
      this.chartOptions = {
        xAxis: {
          type: 'time',
          splitLine: { show: false },
          axisTick: { show: false },
          axisLabel: { show: false },
          axisPointer: {
            show: true,
            snap: true,
            lineStyle: {
              type: 'dashed'
            }
          }
        },
        yAxis: {
          type: 'value',
          name: 'MMR',
          min: 'dataMin',
          max: 'dataMax',
          splitLine: { show: false }
        },
        visualMap: visualMap,
        series: [ series ]
      }
    },
    generateChartSeries: function (playlist) {
      if (this.loading) {
        return {}
      }
      let visualMap = {
        type: 'piecewise',
        outOfRange: { color: '#000' },
        pieces: [],
        top: 10,
        right: 10
      }
      let brackets = {}
      let data = []
      _.each(this.GET_PLAYER_RANK[playlist], function (pt, key) {
        if (!_.has(brackets, pt.tier)) {
          brackets[pt.tier] = {
            gt: pt.mmr,
            lte: pt.mmr
          }
        }
        brackets[pt.tier].gt = Math.min(brackets[pt.tier].gt, pt.mmr)
        brackets[pt.tier].lte = Math.max(brackets[pt.tier].lte, pt.mmr)
        data.push([pt.at, pt.mmr])
      })
      _.each(brackets, function (bracket, tier) {
        let rankObj = ranks.getRankObjFromTier(tier)
        if (!rankObj) return
        visualMap.pieces.push({
          min: bracket.gt,
          max: bracket.lte,
          label: rankObj.tiers[tier].name,
          color: rankObj.color
        })
      })
      let series = {
        animation: false,
        type: 'line',
        data: data
      }
      return [series, visualMap]
    }
  },
  mixins: [ FilterTimeMixin, PlaylistOptionsMixin ],
  props: [ 'playerId' ],
  watch: {
    GET_PLAYER_RANK: function (val) {
      this.updateChartOptions()
    }
  }
}
</script>

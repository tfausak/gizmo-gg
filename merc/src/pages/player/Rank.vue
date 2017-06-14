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
import chartFuncs from '../../store/chart-funcs.js'

var moment = require('moment')
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
      let visualMap = {
        type: 'piecewise',
        outOfRange: { color: '#000' },
        pieces: [],
        top: 10,
        right: 10
      }
      let brackets = {}
      let byDate = {}
      _.each(this.GET_PLAYER_RANK[this.playlist], function (pt, key) {
        if (!_.has(brackets, pt.tier)) {
          brackets[pt.tier] = {
            gt: pt.mmr,
            lte: pt.mmr
          }
        }
        brackets[pt.tier].gt = Math.min(brackets[pt.tier].gt, pt.mmr)
        brackets[pt.tier].lte = Math.max(brackets[pt.tier].lte, pt.mmr)
        let date = moment(pt.at).format('YYYY-MM-DD')
        if (!_.has(byDate, date)) {
          byDate[date] = [pt.mmr, pt.mmr, pt.mmr, pt.mmr]
        }
        byDate[date][1] = pt.mmr
        byDate[date][2] = Math.min(byDate[date][2], pt.mmr)
        byDate[date][3] = Math.max(byDate[date][3], pt.mmr)
      })
      byDate = _(byDate).toPairs().sortBy(0).fromPairs().value()
      let data0 = []
      _.each(byDate, function (value, date) {
        data0.push([date, value[0], value[1], value[2], value[3]])
      })
      data0 = chartFuncs.splitData(data0)
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

      // -------------
      // CHART OPTIONS
      // -------------

      this.chartOptions = {
        xAxis: {
          type: 'category',
          data: data0.categoryData,
          scale: true,
          boundaryGap: false,
          axisLine: { onZero: false },
          splitLine: { show: false },
          splitNumber: 20,
          min: 'dataMin',
          max: 'dataMax'
        },
        yAxis: {
          scale: true,
          splitArea: { show: true }
        },
        dataZoom: [
          {
            type: 'inside',
            start: 50,
            end: 100
          },
          {
            show: true,
            type: 'slider',
            y: '90%',
            start: 50,
            end: 100
          }
        ],
        series: [
          {
            animation: false,
            type: 'candlestick',
            data: data0.values,
            markLine: {
              symbol: [ 'none', 'none' ],
              lineStyle: {
                normal: { color: '#444' }
              },
              data: [
                {
                  name: 'min line at end',
                  type: 'min',
                  valueDim: 'close'
                },
                {
                  name: 'max line at end',
                  type: 'max',
                  valueDim: 'close'
                }
              ]
            },
            markArea: {
              silent: true,
              data: [
                [ { yAxis: 1000, itemStyle: { normal: { color: '#204A87' } } }, { yAxis: 1200 } ],
                [ { yAxis: 1200, itemStyle: { normal: { color: '#5c3566' } } }, { yAxis: 1400 } ]
              ]
            }
          },
          {
            type: 'line',
            data: chartFuncs.calculateMA(data0, 1),
            smooth: true,
            lineStyle: {
              normal: { color: 'rgba(0, 0, 0, 0.4)' }
            }
          },
          {
            type: 'line',
            data: chartFuncs.calculateMA(data0, 5),
            smooth: true,
            lineStyle: {
              normal: { color: 'rgba(255, 255, 255, 0.4)' }
            }
          }
        ]
      }
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

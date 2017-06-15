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
// import ranks from '../../store/ranks.js'

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

      // sort oldest first
      let raw = _.sortBy(this.GET_PLAYER_RANK[this.playlist], 'at')

      // group by date
      let byDate = {}
      _.each(raw, function (pt, key) {
        let date = moment(pt.at).format('YYYY-MM-DD')
        if (!_.has(byDate, date)) {
          byDate[date] = []
        }
        byDate[date].push(pt.mmr)
      })

      let data0 = []
      _.each(byDate, function (mmrs, date) {
        _.each(mmrs, function (mmr) {
          data0.push([date, mmr])
        })
      })

      // create categories
      let categories = _.keys(byDate)

      // -------------
      // CHART OPTIONS
      // -------------

      this.chartOptions = {
        xAxis: {
          type: 'category',
          data: categories,
          splitLine: { show: false }
        },
        yAxis: {
          type: 'value',
          splitArea: { show: true },
          scale: true
        },
        series: [
          {
            type: 'line',
            data: data0,
            symbol: 'circle',
            symbolSize: 5,
            showAllSymbol: true,
            lineStyle: {
              normal: {
                color: 'rgba(0, 0, 0, 0.3)'
              }
            },
            itemStyle: {
              normal: {
                color: 'rgba(0, 0, 0, 0.4)'
              }
            }
          }
        ],
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

<style lang="scss" scoped>
@import "~styles/vars.scss";
.panel-chart {
  background-color: $white-ter;
}
.panel-options {
  border-bottom: 1px solid #ddd;
  .tabs {
    margin-bottom: -1px;
  }
}
.echarts {
  width: 650px!important;
  height: 350px!important;
  margin: 0 auto!important;
}
</style>

<template>
  <div class="container">
    <loading-component :loading="loading"></loading-component>
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
              <div class="tabs">
                <ul>
                  <li v-for="(value, key) in shortTimeOptions" :class="{ 'is-active': key == time }" @click="setTime(key)"><a>{{ value }}</a></li>
                </ul>
              </div>
            </div>
          </div>
        </div>
      </div>
      <div class="panel-block">
        <echart :options="chartOptions" v-if="chartOptions && !loading"></echart>
      </div>
    </div>
  </div>
</template>

<script>
import LoadingComponent from '../components/Loading'
import FilterTimeMixin from '../mixins/FilterTimeMixin'
import FilterPlaylistMixin from '../mixins/FilterPlaylistMixin'

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
      let data = _
        .chain(this.GET_PLAYER_RANK[this.playlist])
        .reverse()
        .map((value, index) => [index, value.mmr])
        .value()
      this.chartOptions = {
        xAxis: {
          type: 'value',
          name: ''
        },
        yAxis: {
          type: 'value',
          name: 'MMR',
          min: _.chain(data).map(value => value[1]).min().value() - 10,
          max: _.chain(data).map(value => value[1]).max().value() + 10
        },
        series: [
          {
            animation: false,
            type: 'line',
            data: data
          }
        ]
      }
    }
  },
  mixins: [ FilterTimeMixin, FilterPlaylistMixin ],
  props: [ 'playerId' ],
  watch: {
    GET_PLAYER_RANK: function (val) {
      this.updateChartOptions()
    }
  }
}
</script>

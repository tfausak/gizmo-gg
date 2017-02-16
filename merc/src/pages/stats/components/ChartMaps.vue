<style scoped lang="scss">
.echarts {
  width: 650px!important;
  height: 250px!important;
  margin: 0 auto!important;
}
</style>

<template>
  <div>
    <loading-component :loading="loading"></loading-component>
    <echart :options="chartOptions" v-if="chartOptions && !loading"></echart>
  </div>
</template>

<script>
import LoadingComponent from '../../components/Loading'
import { scrub } from '../../../store/scrubber.js'

var _ = require('lodash')

export default {
  beforeMount () {
    let vm = this
    vm.$store.dispatch('GET_ARENAS').then(function (data) {
      vm.GET_ARENAS = data
      vm.updateChartOptions()
    })
  },
  components: {
    LoadingComponent
  },
  computed: {
    loading: function () {
      return this.GET_STATS_SUMMARY === null || this.GET_ARENAS === null
    }
  },
  data: function () {
    return {
      chartOptions: null,
      GET_ARENAS: null
    }
  },
  methods: {
    updateChartOptions: function () {
      let vm = this
      if (vm.loading) {
        return {}
      }
      let byTemplate = {}
      _.each(vm.GET_STATS_SUMMARY.mapFreqPct, function (value, key) {
        _.each(vm.GET_ARENAS, function (arena) {
          if (arena.name === key) {
            if (!_.has(byTemplate, arena.templateName)) {
              byTemplate[arena.templateName] = 0
            }
            byTemplate[arena.templateName] += value
          }
        })
      })
      vm.chartOptions = {
        series: [
          {
            animation: false,
            startAngle: 0,
            type: 'pie',
            radius: ['45%', '75%'],
            label: { normal: { formatter: `{d}% {b}` } },
            data: scrub(byTemplate, 0.01)
          }
        ]
      }
    }
  },
  props: [ 'GET_STATS_SUMMARY' ],
  watch: {
    GET_STATS_SUMMARY: function (val) {
      this.updateChartOptions()
    }
  }
}
</script>

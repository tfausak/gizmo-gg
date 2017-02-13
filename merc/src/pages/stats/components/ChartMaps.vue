<style scoped lang="scss">
.echart-loading,
.echarts {
  width: 650px!important;
  height: 250px!important;
  margin: 0 auto!important;
}
</style>

<template>
  <div>
    <echart :options="chartOptions" v-if="chartOptions"></echart>
  </div>
</template>

<script>
import { scrub } from '../../../store/scrubber.js'

var _ = require('lodash')

export default {
  beforeMount () {
    this.updateChartOptions()
  },
  data: function () {
    return {
      chartOptions: null
    }
  },
  methods: {
    updateChartOptions: function () {
      let byTemplate = {}
      let vm = this
      _.each(vm.source, function (value, key) {
        _.each(vm.arenas, function (arena) {
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
  props: [ 'arenas', 'source' ],
  watch: {
    source: function (val) {
      this.updateChartOptions()
    }
  }
}
</script>

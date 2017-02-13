<style scoped lang="scss">
.echart-loading,
.echarts {
  width: 450px!important;
  height: 150px!important;
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
      this.chartOptions = {
        series: [
          {
            animation: false,
            startAngle: 0,
            type: 'pie',
            radius: ['45%', '75%'],
            label: { normal: { formatter: function (params) {
              return _.round(params.value * 100) + '% ' + params.name
            } } },
            data: scrub(this.source, 0.05)
          }
        ]
      }
    }
  },
  props: [ 'source' ],
  watch: {
    source: function (val) {
      this.updateChartOptions()
    }
  }
}
</script>

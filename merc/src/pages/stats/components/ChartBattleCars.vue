<style scoped lang="scss">
.echarts {
  width: 450px!important;
  height: 150px!important;
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
    this.updateChartOptions()
  },
  components: {
    LoadingComponent
  },
  computed: {
    loading: function () {
      return this.GET_STATS_SUMMARY === null
    }
  },
  data: function () {
    return {
      chartOptions: null
    }
  },
  methods: {
    updateChartOptions: function () {
      if (this.loading) {
        return {}
      }
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
            data: scrub(this.GET_STATS_SUMMARY.bodyFreqPct, 0.05)
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

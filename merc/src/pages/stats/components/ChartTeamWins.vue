<template>
  <div>
    <loading-component :loading="loading"></loading-component>
    <echart :options="chartOptions" v-if="chartOptions && !loading"></echart>
  </div>
</template>

<script>
import LoadingComponent from '../../components/Loading'

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
            type: 'pie',
            radius: ['45%', '75%'],
            label: { normal: { formatter: `{b}\n{d}%` } },
            data: [
              {
                name: 'Orange',
                value: this.GET_STATS_SUMMARY.winPct.orange,
                itemStyle: { normal: { color: '#CB4B16' } }
              },
              {
                name: 'Blue',
                value: this.GET_STATS_SUMMARY.winPct.blue,
                itemStyle: { normal: { color: '#268BD2' } }
              }
            ]
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

<style scoped lang="scss">
.echart-loading,
.echarts {
  width: 450px!important;
  height: 250px!important;
  margin: 0 auto!important;
}
</style>

<template>
  <article class="message">
    <div class="message-body">
      <p class="heading has-text-centered">
        Map Frequency
        - <router-link to="/stats/maps">more</router-link>
      </p>
      <div style="margin-top: 20px;"></div>
      <echart :options="chartOptions" v-if="chartOptions && !loading"></echart>
      <loading-component :loading="loading"></loading-component>
    </div>
  </article>
</template>

<script>
import LoadingComponent from './LoadingComponent.vue'
import { scrub } from '../../lib/basic-chart-data-scrubber.js'

export default {
  components: {
    LoadingComponent: LoadingComponent
  },
  props: [ 'source', 'loading' ],
  data: function () {
    return {
      chartOptions: null
    }
  },
  watch: {
    source: function (val) {
      this.updateChartOptions()
    }
  },
  methods: {
    updateChartOptions: function () {
      var vm = this
      this.source.then(function (result) {
        let chartData = scrub(result.map_freq_pct, 0.05)
        vm.chartOptions = {
          series: [
            {
              type: 'pie',
              radius: ['45%', '75%'],
              data: chartData,
              label: { normal: { formatter: `{b}\n{d}%` } }
            }
          ]
        }
      })
    }
  },
  beforeMount () {
    this.updateChartOptions()
  }
}
</script>

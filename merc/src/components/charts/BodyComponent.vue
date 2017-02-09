<template>
  <article class="message">
    <div class="message-body">
      <p class="heading">
        Battle-Car Usage
        - <router-link to="/stats/battle-cars">more</router-link>
      </p>
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
        let chartData = scrub(result.body_freq_pct)
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

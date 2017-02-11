<template>
  <article class="message">
    <div class="message-body">
      <p class="heading is-1">Win Pct by Team</p>
      <echart :options="chartOptions" v-if="chartOptions && !loading"></echart>
      <loading-component :loading="loading"></loading-component>
    </div>
  </article>
</template>

<script>
import LoadingComponent from '../../components/Loading.vue'

export default {
  components: {
    LoadingComponent
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
        let chartData = []
        for (let key in result.winPct) {
          let color = '#000000'
          if (key === 'orange') {
            color = '#CB4B16'
          } else {
            color = '#268BD2'
          }
          chartData.push({
            name: key,
            value: result.winPct[key],
            itemStyle: { normal: { color: color } }
          })
        }
        vm.chartOptions = {
          series: [
            {
              animation: false,
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

<template>
  <article class="message">
    <div class="message-body">
      <p class="heading is-1">Win Pct by Team</p>
      <echart :options="chartOptions" v-if="chartOptions"></echart>
    </div>
  </article>
</template>

<script>
export default {
  props: [ 'source' ],
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
      this.source.then(function (source) {
        let chartData = []
        for (let key in source.win_pct) {
          let color = '#000000'
          if (key === 'orange') {
            color = '#CB4B16'
          } else {
            color = '#268BD2'
          }
          chartData.push({
            name: key,
            value: source.win_pct[key],
            itemStyle: { normal: { color: color } }
          })
        }
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

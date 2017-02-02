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
  data: function () {
    return {
      loading: true,
      chartOptions: null
    }
  },
  beforeMount () {
    var vm = this
    this.$store.dispatch('GET_STATS_SUMMARY').then(function (data) {
      vm.loading = false
      let chartData = []
      for (let key in data.win_pct) {
        let color = '#000000'
        if (key === 'orange') {
          color = '#CB4B16'
        } else {
          color = '#268BD2'
        }
        chartData.push({
          value: data.win_pct[key],
          name: key,
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
}
</script>

<template>
  <div>
    <echart :options="chartOptions" v-if="chartOptions"></echart>
  </div>
</template>

<script>
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
            type: 'pie',
            radius: ['45%', '75%'],
            label: { normal: { formatter: `{b}\n{d}%` } },
            data: [
              {
                name: 'Orange',
                value: this.source.orange,
                itemStyle: { normal: { color: '#CB4B16' } }
              },
              {
                name: 'Blue',
                value: this.source.blue,
                itemStyle: { normal: { color: '#268BD2' } }
              }
            ]
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

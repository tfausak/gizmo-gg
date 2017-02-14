<style scoped lang="scss">
.echarts {
  width: 100px!important;
  height: 80px!important;
  margin: 0 auto!important;
}
</style>

<template>
  <echart :options="chartOptions" v-if="chartOptions"></echart>
</template>

<script>
export default {
  data: function () {
    return {
      chartOptions: {
        graphic: {
          elements: [
            {
              type: 'text',
              left: 'center',
              top: 'middle',
              style:
              {
                'text': '50%',
                fill: '#777'
              }
            }
          ]
        },
        series: [
          {
            hoverAnimation: false,
            animation: false,
            type: 'pie',
            radius: ['55%', '80%'],
            data: [
              {
                name: 'losses',
                value: 1
              },
              {
                name: 'wins',
                value: 1
              }
            ],
            label: { normal: { show: false } }
          }
        ]
      }
    }
  }
}
</script>

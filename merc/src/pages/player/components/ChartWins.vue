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
import { getPct } from '../../../store/scrubber.js'

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
                'text': getPct(this.wins, this.wins + this.losses, 0) + '%',
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
                value: this.losses
              },
              {
                name: 'wins',
                value: this.wins,
                itemStyle: { normal: { color: '#268BD2' } }
              }
            ],
            label: { normal: { show: false } }
          }
        ]
      }
    }
  },
  props: [ 'wins', 'losses' ]
}
</script>

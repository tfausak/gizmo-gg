<style scoped lang="scss">
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
        Map Frequency (competitive)
        - <router-link to="/stats/maps">more</router-link>
      </p>
      <div style="margin-top: 20px;"></div>
      <echart :options="chart"></echart>
    </div>
  </article>
</template>

<script>
export default {
  data: function () {
    let source = {
      'Standard': {
        'pct': 70.0,
        'maps': {
          'DFH Stadium': 12,
          'Urban Central': 13,
          'Mannfield': 10,
          'Beckwith Park': 10,
          'Utopia Coliseum': 12,
          'Aquadome': 13
        }
      },
      'Wasteland': {
        'pct': 10.0,
        'maps': {
          'Wasteland': 10.0
        }
      },
      'Starbase ARC': {
        'pct': 10.0,
        'maps': {
          'Starbase ARC': 10.0
        }
      },
      'Neo Tokyo': {
        'pct': 10.0,
        'maps': {
          'Neo Tokyo': 10.0
        }
      }
    }
    let colorGroups = [
      {
        parent: '#c23531',
        children: [
          '#c23531',
          '#ce4a48',
          '#db5e5c',
          '#db716f',
          '#e29493',
          '#eaaead',
          '#f2d0d0'
        ]
      },
      {
        parent: '#2f4554',
        children: ['#2f4554']
      },
      {
        parent: '#61a0a8',
        children: ['#61a0a8']
      },
      {
        parent: '#d48265',
        children: ['#d48265']
      }
    ]
    let dataByGroup = []
    let i = 0
    for (let group in source) {
      source[group].colorGroup = colorGroups[i]
      dataByGroup.push({
        value: source[group].pct,
        name: group,
        itemStyle: { normal: { color: colorGroups[i].parent } }
      })
      i++
    }
    let dataByMap = []
    for (let group in source) {
      let j = 0
      for (let map in source[group]['maps']) {
        let colors = source[group].colorGroup.children
        let color = colors[j % colors.length]
        dataByMap.push({
          value: source[group]['maps'][map],
          name: map,
          itemStyle: { normal: { color: color } }
        })
        j++
      }
    }
    return {
      chart: {
        series: [
          {
            type: 'pie',
            radius: '40%',
            data: dataByGroup,
            slient: true,
            animation: false,
            label: { normal: { show: false } }
          },
          {
            type: 'pie',
            radius: ['55%', '85%'],
            data: dataByMap,
            label: { normal: { formatter: `{b}\n{d}%` } }
          }
        ]
      }
    }
  }
}
</script>

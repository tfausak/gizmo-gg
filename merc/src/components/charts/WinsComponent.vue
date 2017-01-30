<template>
  <article class="message">
    <div class="message-body">
      <p class="heading is-1">Win Pct by Team</p>
      <echart :options="chart"></echart>
    </div>
  </article>
</template>

<script>
export default {
  data: function () {
    let source = {
      'Orange': 48.1,
      'Blue': 51.9
    }
    let data = []
    for (let key in source) {
      let color = '#000000'
      if (key === 'Orange') {
        color = '#CB4B16'
      } else {
        color = '#268BD2'
      }
      data.push({
        value: source[key],
        name: key,
        itemStyle: { normal: { color: color } }
      })
    }
    return {
      chart: {
        series: [
          {
            type: 'pie',
            radius: ['45%', '75%'],
            data: data,
            label: { normal: { formatter: `{b}\n{d}%` } }
          }
        ]
      }
    }
  }
}
</script>

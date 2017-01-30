<template>
  <article class="message">
    <div class="message-body">
      <p class="heading">
        Body Usage
        - <router-link to="/stats/bodies">more</router-link>
      </p>
      <echart :options="chart"></echart>
    </div>
  </article>
</template>

<script>
export default {
  data: function () {
    let source = {
      'Octane': 65.4,
      'Dominus': 11.2,
      'Breakout-S': 7.8,
      'Batmobile': 5.5,
      'Other': 10.1
    }
    let data = []
    for (let key in source) {
      data.push({
        value: source[key],
        name: key
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

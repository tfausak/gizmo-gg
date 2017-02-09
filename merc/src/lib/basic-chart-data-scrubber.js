var _ = require('lodash')

export function scrub (data, tolerance) {
  tolerance = tolerance || 0.05
  let chartData = []
  let fixed = []
  for (let key in data) {
    fixed.push({
      name: key,
      value: data[key]
    })
  }
  fixed = _.filter(
    _.reverse(_.sortBy(fixed, 'value')),
    function (o) {
      return o.value > tolerance
    }
  )
  let total = _.reduce(fixed, function (result, value, key) {
    return result + value.value
  }, 0)
  for (let i in fixed) {
    chartData.push({
      name: fixed[i].name,
      value: fixed[i].value
    })
  }
  chartData.push({
    name: 'Other',
    value: 1 - total
  })
  return chartData
}

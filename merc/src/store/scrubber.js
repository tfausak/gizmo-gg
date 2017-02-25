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
  if (total < 0.99) {
    chartData.push({
      name: 'Other',
      value: 1 - total
    })
  }
  return chartData
}

export function getPct (top, bot, precision = 2) {
  if (top) {
    if (bot) {
      return _.round(top / bot * 100, precision)
    }
    return 100
  }
  return 0
}

export function getTeamSize (playlistName) {
  if (!playlistName) {
    return 0
  }
  if (playlistName.match(/doubles/i)) {
    return 2
  }
  if (playlistName.match(/solo duel/i)) {
    return 1
  }
  if (playlistName.match(/standard/i)) {
    return 3
  }
  if (playlistName.match(/solo standard/i)) {
    return 3
  }
  if (playlistName.match(/rumble/i)) {
    return 3
  }
  if (playlistName.match(/chaos/i)) {
    return 4
  }
  return 0
}

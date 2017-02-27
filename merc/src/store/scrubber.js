var _ = require('lodash')
import slugger from './slugger.js'

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

export function compileMapStats (STATS_ARENAS, ARENAS) {
  let types = [ 'byTemplate', 'byModel', 'bySkin' ]
  let data = {}
  _.each(types, function (type) {
    data[type] = { maxFreqPct: 0, maxScore: 0, data: {} }
  })

  let totalGames = 0
  _.each(STATS_ARENAS, function (value, key) {
    totalGames += value.numGames
  })

  let sumCols = [
    'numGames',
    'totalShots',
    'totalGoals',
    'totalScore',
    'totalSaves',
    'totalAssists'
  ]
  _.each(STATS_ARENAS, function (value, key) {
    _.each(ARENAS, function (arena) {
      if (arena.name === value.arenaName) {
        // Template
        if (!_.has(data['byTemplate']['data'], arena.templateName)) {
          data['byTemplate']['data'][arena.templateName] = {
            displayName: arena.templateName,
            slug: slugger.slugMap(arena.templateName)
          }
          _.each(sumCols, function (col) {
            data['byTemplate']['data'][arena.templateName][col] = 0
          })
        }
        _.each(sumCols, function (col) {
          data['byTemplate']['data'][arena.templateName][col] += value[col]
        })

        // Model
        if (!_.has(data['byModel']['data'], arena.modelName)) {
          data['byModel']['data'][arena.modelName] = {
            displayName: arena.modelName
          }
          _.each(sumCols, function (col) {
            data['byModel']['data'][arena.modelName][col] = 0
          })
        }
        _.each(sumCols, function (col) {
          data['byModel']['data'][arena.modelName][col] += value[col]
        })

        // Skin
        let fullSkinName = arena.modelName
        if (arena.skinName) {
          fullSkinName += ' (' + arena.skinName + ')'
        }
        if (!_.has(data['bySkin']['data'], fullSkinName)) {
          data['bySkin']['data'][fullSkinName] = {
            displayName: fullSkinName
          }
          _.each(sumCols, function (col) {
            data['bySkin']['data'][fullSkinName][col] = 0
          })
        }
        _.each(sumCols, function (col) {
          data['bySkin']['data'][fullSkinName][col] += value[col]
        })
      }
    })
  })

  _.each(types, function (type) {
    data[type]['maxFreqPct'] = 0
    data[type]['maxScore'] = 0
    data[type]['data'] = _.map(data[type]['data'], function (value, key) {
      value.accuracy = 0
      if (value.totalGoals > 0) {
        value.accuracy = 100
      }
      if (value.totalShots > 0) {
        value.accuracy = _.round(value.totalGoals / value.totalShots * 100, 2)
      }
      value.avgScore = _.round(value.totalScore / value.numGames, 2)
      value.freqPct = _.round(value.numGames / totalGames * 100, 2)
      data[type]['maxFreqPct'] = _.max([value.freqPct, data[type]['maxFreqPct']])
      data[type]['maxScore'] = _.max([value.avgScore, data[type]['maxScore']])
      return value
    })
  })

  _.each(types, function (type) {
    data[type]['data'] = _.reverse(_.sortBy(data[type]['data'], function (o) {
      return o.freqPct
    }))
  })

  return data
}

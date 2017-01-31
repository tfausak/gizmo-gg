import bodies from '../assets/bodies.js'
import maps from '../assets/maps.js'
var _ = require('lodash')

let statBodies = []
let winPct = 55
for (let i in bodies.list) {
  statBodies.push({
    body: bodies.list[i],
    winPct: winPct,
    games: _.random(10, 500),
    pts: _.random(300, 1000),
    acc: _.round(_.random(35, 55, true), 2)
  })
  winPct = _.round(winPct - _.random(0, 1, true), 2)
}

let freqPct = 12
let statMaps = []
for (let template in maps.list) {
  for (let i in maps.list[template]) {
    statMaps.push({
      map: maps.list[template][i],
      freqPct: freqPct,
      games: _.round(freqPct * 100),
      pts: _.random(300, 1000),
      acc: _.round(_.random(35, 55, true), 2)
    })
    freqPct = _.round(freqPct - _.random(0, 1, true), 2)
  }
}

export default {
  getStatBodies: function () {
    return statBodies
  },
  getStatMaps: function () {
    return statMaps
  }
}

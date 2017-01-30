import bodies from '../assets/bodies.js'
var _ = require('lodash')

let rows = []
let winPct = 55
for (let i in bodies.list) {
  rows.push({
    body: bodies.list[i],
    winPct: winPct,
    games: _.random(10, 500),
    pts: _.random(300, 1000),
    acc: _.round(_.random(35, 55, true), 2)
  })
  winPct = _.round(winPct - _.random(0, 1, true), 2)
}

export default rows

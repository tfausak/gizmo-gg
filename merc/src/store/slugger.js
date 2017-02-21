var _ = require('lodash')

export default {
  slugBody: function (body) {
    if (!body) {
      return null
    }
    if (body.match(/delorean/i)) {
      return 'delorean'
    }
    body = _.toLower(body)
    body = _.replace(body, /\s+/g, '-')
    return body
  },
  slugMap: function (name) {
    if (!name) {
      return null
    }
    if (name.match(/starbase/i)) {
      return 'arc'
    }
    return _.toLower(name)
  },
  slugPlaylist: function (name) {
    if (!name) {
      return null
    }
    if (name.match(/doubles/i)) {
      return '2v2'
    }
    if (name.match(/solo duel/i)) {
      return '1v1'
    }
    if (name.match(/standard/i)) {
      return '3v3'
    }
    if (name.match(/solo standard/i)) {
      return '3v3 Solo'
    }
    return _.toLower(name)
  }
}

var _ = require('lodash')

var slug = function (str) {
  if (!str) {
    return ''
  }
  str = _.toLower(str)
  str = _.replace(str, /\s+/g, '-')
  return str
}

export default {
  slug: slug,
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
    name = _.toLower(name)
    name = _.replace(name, /\s+/g, '-')
    return name
  },
  slugMapModel: function (name) {
    return slug(name)
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
    if (name.match(/solo standard/i)) {
      return '3v3 Solo'
    }
    if (name.match(/standard/i)) {
      return '3v3'
    }
    return _.toLower(name)
  }
}

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
  }
}

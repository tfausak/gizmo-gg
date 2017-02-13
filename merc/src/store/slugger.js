var _ = require('lodash')

export default {
  slugBody: function (body) {
    if (body.match(/delorean/i)) {
      return 'delorean'
    }
    body = _.toLower(body)
    body = _.replace(body, /\s+/g, '-')
    return body
  }
}

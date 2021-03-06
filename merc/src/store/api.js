import axios from 'axios'

export function getResource (endpoint) {
  var url = getEndpointUrl(endpoint)
  return getRaw(url)
}

export function getRaw (url) {
  var delay = process.env.API_DELAY_MS
  return new Promise(function (resolve, reject) {
    axios.get(url)
      .then(function (response) {
        setTimeout(function () {
          resolve(response.data)
        }, delay)
      })
      .catch(function (error) {
        setTimeout(function () {
          reject(error.message)
        }, delay)
      })
  })
}

export function getEndpointUrl (endpoint) {
  var url = process.env.API_URL.replace(/\/+$/, '') // remove trailing slashes
  endpoint = endpoint.replace(/^\/+/, '') // remove leading slashes
  return url + '/' + endpoint // add slash back in the middle
}

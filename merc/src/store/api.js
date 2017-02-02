import axios from 'axios'

export function getResource (endpoint) {
  var url = getEndpointUrl(endpoint)
  return new Promise(function (resolve, reject) {
    axios.get(url)
      .then(function (response) {
        // handle successful api response
        resolve(response.data)
      })
      .catch(reject)
  })
}

export function getEndpointUrl (endpoint) {
  var url = process.env.API_URL.replace(/\/+$/, '')
  endpoint = endpoint.replace(/^\/+/, '')
  return url + '/' + endpoint
}

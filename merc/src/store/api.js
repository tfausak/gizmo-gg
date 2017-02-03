import axios from 'axios'

export function getResource (endpoint) {
  var url = getEndpointUrl(endpoint)
  return new Promise(function (resolve, reject) {
    axios.get(url)
      .then(function (response) {
        resolve(response.data)
      })
      .catch(function (error) {
        reject(error.message)
      })
  })
}

export function getEndpointUrl (endpoint) {
  var url = process.env.API_URL.replace(/\/+$/, '') // remove trailing slashes
  endpoint = endpoint.replace(/^\/+/, '') // remove leading slashes
  return url + '/' + endpoint // add slash back in the middle
}

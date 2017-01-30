import Vue from 'vue'

export function getResource (endpoint, callback) {
  var url = process.env.API_URL.replace(/\/+$/, '')
  endpoint = endpoint.replace(/^\/+/, '')
  return Vue.http.get(url + '/' + endpoint)
    .then(callback, (response) => {
      console.log('error', response)
    })
}

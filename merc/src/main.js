import Vue from 'vue'
import App from './App'
import store from './store'
import router from './router'
import ECharts from 'vue-echarts'
import axios from 'axios'
import './assets/styles/app.scss'

Vue.prototype.$http = axios
Vue.component('echart', ECharts)

// create the app instance.
// here we inject the router and store to all child components,
// making them available everywhere as `this.$router` and `this.$store`.
const app = new Vue({
  router: router,
  store: store,
  template: '<App/>',
  components: { App }
})

// actually mount to DOM
app.$mount('#app')

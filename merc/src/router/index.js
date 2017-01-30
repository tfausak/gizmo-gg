import Vue from 'vue'
import Router from 'vue-router'

Vue.use(Router)

export default new Router({
  mode: 'history',
  scrollBehavior: () => ({ y: 0 }),
  linkActiveClass: 'is-active',
  routes: [
    { path: '/upload', component: require('../views/UploadView.vue') },
    { path: '/app', component: require('../views/AppView.vue') },
    {
      path: '/stats',
      component: require('../views/StatsView.vue'),
      children: [
        {
          path: '',
          component: require('../views/Stats/SummaryView')
        },
        {
          path: 'bodies',
          component: require('../views/Stats/BodiesView')
        },
        {
          path: 'wheels',
          component: require('../views/Stats/WheelsView')
        },
        {
          path: 'toppers',
          component: require('../views/Stats/ToppersView')
        },
        {
          path: 'boosts',
          component: require('../views/Stats/BoostsView')
        },
        {
          path: 'maps',
          component: require('../views/Stats/MapsView')
        }
      ]
    },
    { path: '/', component: require('../views/IndexView.vue') },
    { path: '*', redirect: '/' }
  ]
})

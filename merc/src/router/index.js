import Vue from 'vue'
import Router from 'vue-router'

Vue.use(Router)

export default new Router({
  mode: 'history',
  scrollBehavior: () => ({ y: 0 }),
  linkActiveClass: 'is-active',
  routes: [
    { path: '/upload', component: require('../views/UploadView.vue') },
    { path: '/desktop-app', component: require('../views/DesktopView.vue') },
    { path: '/about', component: require('../views/AboutView.vue') },
    {
      name: 'search',
      path: '/search',
      component: require('../views/SearchView.vue'),
      props: true
    },
    {
      path: '/stats',
      component: require('../views/StatsView.vue'),
      children: [
        {
          path: '',
          component: require('../views/Stats/SummaryView')
        },
        {
          path: 'battle-cars',
          component: require('../views/Stats/BodiesView')
        },
        {
          path: 'maps',
          component: require('../views/Stats/MapsView')
        }
      ]
    },
    {
      path: '/player/:platform/:id',
      component: require('../views/PlayerView'),
      props: true,
      children: [
        {
          path: '',
          name: 'player',
          component: require('../views/Player/HomeView')
        }
      ]
    },
    {
      path: '/player-example',
      component: require('../views/PlayerExample.vue'),
      children: [
        {
          path: '',
          component: require('../views/Player/SummaryView.vue')
        }
      ]
    },
    { path: '/', component: require('../views/IndexView.vue') },
    { path: '*', redirect: '/' }
  ]
})

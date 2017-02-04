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
          component: require('../views/Player/HomeView')
        }
      ]
    },
    { path: '/', component: require('../views/IndexView.vue') },
    { path: '*', redirect: '/' }
  ]
})

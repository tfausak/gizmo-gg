import Vue from 'vue'
import Router from 'vue-router'

import DefaultPage from '../pages/templates/Default'

import DesktopAppPage from '../pages/DesktopApp'
import GamePage from '../pages/Game'
import GameSummaryPage from '../pages/game/Summary'
import IndexPage from '../pages/Index'
import PlayerPage from '../pages/Player'
import PlayerSummaryPage from '../pages/player/Summary'
import PlayerRankPage from '../pages/player/Rank'
import PlayerBattleCarsPage from '../pages/player/BattleCars'
import PlayerMapsPage from '../pages/player/Maps'
import SearchPage from '../pages/Search'
import StatsPage from '../pages/Stats'
import StatsSummaryPage from '../pages/stats/Summary'
import StatsBattleCarsPage from '../pages/stats/BattleCars'
import StatsMapsPage from '../pages/stats/Maps'
import UploadPage from '../pages/Upload'
import UploadStatusPage from '../pages/UploadStatus'

Vue.use(Router)

export default new Router({
  mode: 'history',
  scrollBehavior: () => ({ y: 0 }),
  linkActiveClass: 'is-active',
  routes: [
    {
      path: '/',
      component: DefaultPage,
      children: [
        {
          path: '',
          name: 'index',
          component: IndexPage
        },
        {
          path: 'desktop-app',
          name: 'desktop-app',
          component: DesktopAppPage
        },
        {
          path: '/player/:id',
          component: PlayerPage,
          props: true,
          children: [
            {
              path: '',
              redirect: 'summary'
            },
            {
              path: 'summary',
              name: 'player.summary',
              component: PlayerSummaryPage
            },
            {
              path: 'rank',
              name: 'player.rank',
              component: PlayerRankPage
            },
            {
              path: 'battle-cars',
              name: 'player.battle-cars',
              component: PlayerBattleCarsPage
            },
            {
              path: 'maps',
              name: 'player.maps',
              component: PlayerMapsPage
            }
          ]
        },
        {
          path: '/game/:id',
          component: GamePage,
          props: true,
          children: [
            {
              path: '',
              redirect: 'summary'
            },
            {
              path: 'summary',
              name: 'game.summary',
              component: GameSummaryPage
            }
          ]
        },
        {
          path: 'search',
          name: 'search',
          component: SearchPage,
          props: true
        },
        {
          path: 'stats',
          component: StatsPage,
          children: [
            {
              path: 'summary',
              name: 'stats',
              component: StatsSummaryPage
            },
            {
              path: 'battle-cars',
              name: 'stats.battle-cars',
              component: StatsBattleCarsPage
            },
            {
              path: 'maps',
              name: 'stats.maps',
              component: StatsMapsPage
            }
          ]
        },
        {
          path: 'upload',
          name: 'upload',
          component: UploadPage
        },
        {
          path: 'upload/status/:id',
          name: 'upload-status',
          component: UploadStatusPage
        }
      ]
    },
    { path: '*', redirect: '/' }
  ]
})

<style scoped lang="scss">
@import "~styles/vars.scss";

#playerHero {
  background-color: #eee;
  border-bottom: 1px solid rgba(0, 0, 0, 0.2);
  #playerBody {
    margin-right: 0.5em;
  }
}
#playerData {
  background-color: #fff;
}

.noBodyIcon {
  font-size: 45px;
  height: 96px;
  width: 96px;
  text-align: center;
  line-height: 90px;
  color: $grey-light;
}
.platformIcon {
  font-size: 30px;
  width: 40px;
  .icon-xbox {
    font-size: 20px;
  }
}
.rankBoxes {
  flex-grow: 1!important;
  justify-content: flex-end!important;
}
.rankBox {
  background-color: #fff;
  border: 1px solid #ddd;
  border-right: 0;
  width: 100px;
  font-size: 11px;
  letter-spacing: 1px;
  text-align: center;
}
.rankBox:last-child {
  border-right: 1px solid #ddd;
}
.rankPlaylist {
  background-color: #f4f4f4;
  padding: 0.4em 1.5em;
  font-size: 12px;
  font-weight: bold;
}
.rankImage .rank-icon {
  height: 64px;
}
.rankGames {
  padding-bottom: 0.5em;
}
</style>

<template>
  <div>
    <div v-if="loading || missing">
      <section class="hero is-medium">
        <div class="hero-body">

          <div class="container has-text-centered" v-if="!missing">
            <h1 class="title">
              <div>
                <span class="icon is-large">
                  <i class="fa fa-circle-o-notch fa-spin"></i>
                </span>
              </div>
              Loading..
            </h1>
            <h2 class="subtitle">
              player ID#{{ id }}
            </h2>
          </div>

          <div class="container has-text-centered" v-else>
            <h1 class="title">
              <div>
                <span class="icon is-large is-muted">
                  <i class="fa fa-exclamation-triangle"></i>
                </span>
              </div>
              Player not found!
            </h1>
            <h2 class="subtitle">
              Players only show up if they appear in at least one replay.<br><router-link to="/upload">Upload a replay</router-link> to get your own player page.
            </h2>
            <h2 class="subtitle">
              Or you can <router-link to="/">search for another player</router-link>.
            </h2>
          </div>

        </div>
      </section>
    </div>
    <div v-else>
      <section class="hero" id="playerHero">
        <div class="hero-body">
          <div class="container">
            <div class="level level-chained">
              <div class="level-item" id="playerBody">
                <figure class="image is-96x96 is-circle-dark">
                  <img :src="'/static/img/bodies/' + bodyName + '.png'" v-if="bodyName">
                  <i class="fa fa-question noBodyIcon" v-else></i>
                </figure>
              </div>
              <div class="level-item platformIcon">
                <i class="fa fa-steam" v-if="isPlatform('Steam')"></i>
                <i class="icon-playstation" v-if="isPlatform('PlayStation')"></i>
                <i class="icon-xbox" v-if="isPlatform('Xbox')"></i>
              </div>
              <div class="level-item">
                <div class="is-block">
                  <h1 class="title no-margin-top">
                    {{ GET_PLAYER.name }}
                  </h1>
                  <h2 class="subtitle no-margin-top">
                    last played {{ lastUpdated }}
                  </h2>
                </div>
              </div>
              <div class="level-item rankBoxes">
                <div class="rankBox" v-for="(skill, key) in skills">
                  <div class="rankPlaylist">{{ key }}</div>
                  <div v-if="skill">
                    <div class="rankImage">
                      <rank-icon :rank="skill.tier"></rank-icon>
                    </div>
                    <div class="rankDivision">DIV {{ skill.division + 1 }}</div>
                    <div class="rankGames">{{ skill.matchesPlayed }} Games</div>
                  </div>
                  <div v-else>
                    <div class="rankImage">
                      <rank-icon :rank="0"></rank-icon>
                    </div>
                    <div class="rankDivision">Unranked</div>
                    <div class="rankGames">0 Games</div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="hero-foot">
          <div class="container">
            <ul class="playerTabs">
              <router-link :to="{ name: 'player.summary', params: { playerId: id } }" tag="li"><a>Summary</a></router-link>
              <router-link :to="'/player/' + id + '/rank'" tag="li"><a>Rank</a></router-link>
              <router-link :to="'/player/' + id + '/battle-cars'" tag="li"><a>Battle-Cars</a></router-link>
              <router-link :to="'/player/' + id + '/maps'" tag="li"><a>Maps</a></router-link>
            </ul>
          </div>
        </div>
      </section>
      <section class="section" id="playerData">
        <router-view :playerId="id"></router-view>
      </section>
    </div>
  </div>
</template>

<script>
import RankIcon from './components/RankIcon'
import slugger from '../store/slugger.js'
import { EventBus } from '../store/event-bus.js'

var moment = require('moment')
var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  components: {
    RankIcon
  },
  computed: {
    bodyName: function () {
      let vm = this
      let game = _.head(vm.GET_PLAYER.games)
      let bodyName = null
      if (game) {
        _.each(game.players, function (player) {
          if (player.playerId === _.parseInt(vm.id)) {
            bodyName = player.loadout.bodyName
          }
        })
      }
      return slugger.slugBody(bodyName)
    },
    lastUpdated: function () {
      return moment(this.GET_PLAYER.lastPlayedAt).fromNow()
    },
    loading: function () {
      return this.GET_PLAYER === null
    }
  },
  created: function () {
    var vm = this
    vm.pollInterval = setInterval(function () {
      vm.poll()
    }, 10000)
    EventBus.$on('player-updated', function () {
      vm.$store.dispatch('CLEAR_ENDPOINT', 'stats/players/' + vm.id)
      vm.fetchData(true)
    })
  },
  destroyed: function () {
    if (this.pollInterval) {
      clearInterval(this.pollInterval)
    }
  },
  data: function () {
    let skills = {
      '1v1': null,
      '2v2': null,
      '3v3': null,
      '3v3 Solo': null
    }
    return {
      GET_PLAYER: null,
      missing: false,
      pollInterval: null,
      baseSkills: _.clone(skills),
      skills: skills
    }
  },
  methods: {
    isPlatform: function (platform) {
      return this.GET_PLAYER && this.GET_PLAYER.platform.name === platform
    },
    compileData: function () {
      var vm = this
      _.each(vm.GET_PLAYER.skills, function (value, key) {
        vm.skills[slugger.slugPlaylist(key)] = value
      })
    },
    fetchData: function (seamless = false) {
      var vm = this
      if (!seamless) {
        vm.GET_PLAYER = null
        vm.skills = _.clone(vm.baseSkills)
        vm.missing = false
      }
      vm.$store.dispatch('GET_PLAYER', {
        id: vm.id,
        playlist: 'all'
      }).then(function (data) {
        vm.GET_PLAYER = data
        // add this player to the recent players stored in a cookie
        let recent = []
        let cookie = vm.$cookie.get('recent')
        if (cookie) {
          recent = JSON.parse(cookie)
        }
        let newRecent = []
        let myId = vm.id
        newRecent.push({
          id: myId,
          name: vm.GET_PLAYER.name
        })
        _.each(recent, function (value) {
          if (value.id === myId) {
            return
          }
          newRecent.push(value)
        })
        newRecent = _.slice(newRecent, 0, 5)
        vm.$cookie.set('recent', JSON.stringify(newRecent))
        EventBus.$emit('recent-updated')

        vm.compileData()

        if (seamless) {
          vm.$forceUpdate()
        }
      }).catch(function () {
        vm.missing = true
      })
    },
    poll: function () {
      var vm = this
      vm.$store.dispatch('GET_PLAYER_POLL', {
        id: vm.id
      }).then(function (data) {
        if (data && vm.GET_PLAYER && vm.GET_PLAYER.lastPlayedAt !== data) {
          EventBus.$emit('player-updated')
        }
      })
    }
  },
  props: [ 'id' ],
  watch: {
    id: function () {
      this.fetchData()
    }
  }
}
</script>

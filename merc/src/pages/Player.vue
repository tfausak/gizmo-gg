<style scoped lang="scss">
@import "~styles/vars.scss";

$data_bg: #fff;

#playerHero {
  background-color: #eee;
  border-bottom: 1px solid rgba(0, 0, 0, 0.2);
  #playerBody {
    margin-right: 0.5em;
  }
}
#playerData {
  background-color: $data_bg;
}
.playerTabs {
  padding: 0;
  margin: 0;
  display: flex;
  li {
    display: block;
    padding: 0;
    margin: 0;
  }
  li > a {
    background-color: #f4f4f4;
    border: 1px solid rgba(0, 0, 0, 0.2);
    border-left: 0;
    margin-bottom: -1px;
    padding: 0.2em 1em;
    display: block;
    color: #444;
    &:hover {
      background-color: #eee;
    }
  }
  li:first-child a {
    border-left: 1px solid rgba(0, 0, 0, 0.2);
    border-top-left-radius: 3px;
  }
  li:last-child a {
    border-top-right-radius: 3px;
  }
  li.is-active > a {
    color: $primary;
    z-index: 999;
    position: relative;
    border-bottom: 1px solid transparent;
    background-color: $data_bg;
  }
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
              for player ID#{{ id }}
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
      <div>
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
              </div>
            </div>
          </div>
          <div class="hero-foot">
              <div class="container">
                <ul class="playerTabs">
                  <router-link :to="{ name: 'player.summary', params: { playerId: id } }" tag="li"><a>Summary</a></router-link>
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
  </div>
</template>

<script>
import slugger from '../store/slugger.js'
import { EventBus } from '../store/event-bus.js'

var moment = require('moment')
var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
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
    setInterval(function () {
      vm.poll()
    }, 10000)
    EventBus.$on('player-updated', function () {
      vm.$store.dispatch('CLEAR_ENDPOINT', 'stats/players/' + vm.id)
      vm.fetchData(true)
    })
  },
  data: function () {
    return {
      GET_PLAYER: null,
      missing: false
    }
  },
  methods: {
    isPlatform: function (platform) {
      return this.GET_PLAYER && this.GET_PLAYER.platform.name === platform
    },
    fetchData: function (seamless = false) {
      var vm = this
      if (!seamless) {
        this.GET_PLAYER = null
        this.missing = false
      }
      vm.$store.dispatch('GET_PLAYER', {
        id: vm.id,
        playlist: 'all'
      }).then(function (data) {
        vm.GET_PLAYER = data
        let recent = []
        let cookie = vm.$cookie.get('recent')
        if (cookie) {
          recent = JSON.parse(cookie)
          console.log(recent)
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

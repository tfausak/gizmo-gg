<style scoped lang="scss">
@import "~styles/vars.scss";

#playerHero {
  li.is-active a {
    background-color: $white-like!important;
  }
  #playerBody {
    margin-right: 1.5em;
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
              &nbsp;Searching..
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
                    <img :src="'/static/img/bodies/octane.png'">
                    bodyId: {{ bodyId }}
                  </figure>
                </div>
                <div class="level-item">
                  <div class="is-block">
                    <h1 class="title no-margin-top">
                      <span class="icon is-medium">
                        <i class="fa fa-steam"></i>
                      </span>
                      <span>
                        {{ GET_PLAYER.name }}
                      </span>
                    </h1>
                    <h2 class="subtitle no-margin-top">
                      last played {{ lastUpdated }}
                    </h2>
                    <div class="is-muted" v-if="GET_PLAYER.aliases">
                      Aliases: <span v-for="alias in GET_PLAYER.aliases">{{ alias }}</span>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div class="hero-foot">
              <div class="container">
                <nav class="tabs is-boxed">
                  <ul>
                    <router-link :to="'/player/' + id" tag="li" exact><a>Summary</a></router-link>
                    <router-link :to="'/player/' + id + '/battle-cars'" tag="li"><a>Battle-Cars</a></router-link>
                    <router-link :to="'/player/' + id + '/maps'" tag="li"><a>Maps</a></router-link>
                  </ul>
                </nav>
              </div>
            </div>
        </section>
        <section class="section bg-offwhite">
          <router-view :playerId="id"></router-view>
        </section>
      </div>
    </div>
  </div>
</template>

<script>
var moment = require('moment')
var _ = require('lodash')

export default {
  beforeMount: function () {
    var vm = this
    vm.$store.dispatch('GET_PLAYER', {
      id: vm.id
    }).then(function (data) {
      vm.GET_PLAYER = data
    }).catch(function () {
      vm.missing = true
    })
  },
  computed: {
    bodyId: function () {
      let vm = this
      let game = _.head(vm.GET_PLAYER.games)
      let bodyId = null
      if (game) {
        _.each(game.players, function (player) {
          if (player.playerId === _.parseInt(vm.id)) {
            bodyId = player.loadout.bodyId
          }
        })
      }
      return bodyId
    },
    lastUpdated: function () {
      return moment(this.GET_PLAYER.lastPlayedAt).fromNow()
    },
    loading: function () {
      return this.GET_PLAYER === null
    }
  },
  data: function () {
    return {
      GET_PLAYER: null,
      missing: false
    }
  },
  props: [ 'id' ]
}
</script>

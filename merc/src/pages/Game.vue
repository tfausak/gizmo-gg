<style scoped lang="scss">
@import "~styles/vars.scss";

#gameHero {
  background-color: #eee;
  border-bottom: 1px solid rgba(0, 0, 0, 0.2);
  text-align: center;
}
#gameData {
  background-color: #fff;
}
.gameMap {
  figure {
    margin: 0 auto 1em;
  }
  .mapInfo {
    line-height: 1.2;
    .mapName {
      font-size: 24px;
    }
    .mapSkin,
    .mapSub {
      color: $grey;
    }
    .mapSkin {
      margin-top: 4px;
    }
  }
}
.gameResult {
  display: flex;
  justify-content: center;
  font-size: 14px;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-top: 3px;
}
.resultSeparator {
  width: 20px;
  text-align: center;
}
.blueResult {
  color: $solarized_blue;
}
.orangeResult {
  color: $solarized_orange;
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
              game ID#{{ id }}
            </h2>
          </div>

          <div class="container has-text-centered" v-else>
            <h1 class="title">
              <div>
                <span class="icon is-large is-muted">
                  <i class="fa fa-exclamation-triangle"></i>
                </span>
              </div>
              Game not found!
            </h1>
            <h2 class="subtitle">
              This game is not found in our system.<br><router-link to="/upload">Upload a replay</router-link> to get a game page.
            </h2>
            <h2 class="subtitle">
              Or you can <router-link to="/">search for a player</router-link>.
            </h2>
          </div>

        </div>
      </section>
    </div>
    <div v-else>
      <section class="hero" id="gameHero">
        <div class="hero-body">
          <div class="container">
            <div class="gameMap">
              <div v-if="game.arena.templateName === game.arena.modelName">
                <figure class="image is-128x128">
                  <img :src="'/static/img/maps/' + game.templateSlug + '.jpg'"  class="is-circle-dark">
                </figure>
                <div class="mapInfo">
                  <div class="mapName">{{ game.arena.templateName }}</div>
                  <div class="mapSkin">{{ game.arena.skinName }}</div>
                </div>
              </div>
              <div v-else>
                <figure class="image is-128x128">
                  <img :src="'/static/img/maps/' + game.modelSlug + '.jpg'"  class="is-circle-dark">
                </figure>
                <div class="mapInfo">
                  <div class="mapSub">{{ game.arena.templateName }}</div>
                  <div class="mapName">{{ game.arena.modelName }}</div>
                  <div class="mapSkin">{{ game.arena.skinName }}</div>
                </div>
              </div>
            </div>
            <div>{{ game.playlistSlug }} {{ game.playedAgo }}</div>
            <div class="gameResult">
              <div class="blueResult">
                Blue {{ game.blueGoals }}
              </div>
              <div class="resultSeparator">-</div>
              <div class="orangeResult">
                Orange {{ game.orangeGoals }}
              </div>
            </div>
            <div class="gameDuration">{{ game.fDuration }}</div>
          </div>
        </div>
        <div class="hero-foot">
          <div class="container">
            <ul class="playerTabs">
              <router-link :to="{ name: 'game.summary', params: { gameId: id } }" tag="li"><a>Summary</a></router-link>
            </ul>
          </div>
        </div>
      </section>
      <section class="section" id="gameData">
        <router-view :gameId="id"></router-view>
      </section>
    </div>
  </div>
</template>

<script>
import slugger from '../store/slugger.js'
var moment = require('moment')
var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  computed: {
    loading: function () {
      return this.GET_GAME === null
    }
  },
  data: function () {
    return {
      GET_GAME: null,
      game: null,
      missing: false
    }
  },
  methods: {
    compileData: function () {
      this.game = this.GET_GAME.games[0]
      this.game.templateSlug = slugger.slugMap(this.game.arena.templateName)
      this.game.modelSlug = slugger.slugMapModel(this.game.arena.modelName)
      this.game.playlistSlug = slugger.slugPlaylist(this.game.playlistName)
      this.game.playedAgo = moment(this.game.playedAt).fromNow()
      let min = _.floor(this.game.duration / 60)
      let sec = this.game.duration - (min * 60)
      this.game.fDuration = min + 'm ' + sec + 's'
    },
    fetchData: function () {
      var vm = this
      vm.GET_GAME = null
      vm.game = null
      vm.missing = false
      vm.$store.dispatch('GET_GAME', {
        id: vm.id
      }).then(function (data) {
        vm.GET_GAME = data
        vm.compileData()
      }).catch(function () {
        vm.missing = true
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

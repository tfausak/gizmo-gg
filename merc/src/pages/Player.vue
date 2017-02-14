<style scoped lang="scss">
@import "~styles/vars.scss";

#playerHero {
  li.is-active a {
    background-color: $white-ish!important;
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
                  </figure>
                </div>
                <div class="level-item">
                  <div class="is-block">
                    <h1 class="title">
                      <span class="icon" style="margin-top: 8px;">
                        <i class="fa fa-steam"></i>
                      </span>
                      <span>
                        player #{{ id }}
                      </span>
                    </h1>
                    <h2 class="subtitle" style="margin-top: 0;">
                      last updated 29 minutes ago
                    </h2>
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

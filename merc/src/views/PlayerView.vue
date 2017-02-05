<template>
  <div>
    <div v-if="!player">
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
              for player {{ platform }}.{{ id }}
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
    <router-view v-if="player"></router-view>
  </div>
</template>

<script>
import PlayerSearchComponent from '../components/PlayerSearchComponent.vue'

export default {
  components: {
    PlayerSearchComponent
  },
  props: [ 'platform', 'id' ],
  data: function () {
    return {
      player: null,
      missing: false
    }
  },
  beforeMount: function () {
    var vm = this
    this.$store.dispatch('FIND_PLAYER', {
      platform: this.platform,
      id: this.id
    }).then(function (result) {
      vm.player = result
    })
    .catch(function () {
      vm.missing = true
    })
  }
}
</script>

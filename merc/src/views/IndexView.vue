<style scoped lang="scss">
.mybox h2.subtitle {
  margin-bottom: 5px;
}
.gizmo {
  background-image: url('/static/img/gizmo/gizmo-green-hq.png');
  background-position: center center;
  height: 150px;
  background-repeat: no-repeat;
  background-size: contain;
  position: relative;
}
</style>

<template>
  <div>
    <section class="hero is-primary is-medium has-text-centered">
      <div class="hero-body">
        <div class="container">
          <h1 class="title">
            <div class="gizmo"></div>
            gizmo.gg
          </h1>
          <div>
            <player-search-component></player-search-component>
          </div>
        </div>
      </div>
    </section>
    <section class="hero is-light">
      <div class="hero-body">
        <div class="container">
          <nav class="level">
            <div class="level-item has-text-centered">
              <div>
                <p class="heading">Replays</p>
                <p class="title">3,456</p>
              </div>
            </div>
            <div class="level-item has-text-centered">
              <div>
                <p class="heading">Players</p>
                <p class="title">
                  <router-link to="/player/steam/12345" exact>123</router-link>
                </p>
              </div>
            </div>
            <div class="level-item has-text-centered">
              <div>
                <p class="heading">Wasteland</p>
                <p class="title">6.4%</p>
              </div>
            </div>
            <div class="level-item has-text-centered">
              <div>
                <p class="heading">Tokyo</p>
                <p class="title">5.5%</p>
              </div>
            </div>
          </nav>
        </div>
      </div>
    </section>
    <section class="hero is-medium">
      <div class="hero-body">
        <div class="container">
          <div class="columns">
            <div class="column">
              <div class="mybox has-text-centered">
                <span class="icon is-large">
                  <i class="fa fa-car"></i>
                </span>
                <div>
                  <h2 class="subtitle">Powered by <a target="_blank" href="https://github.com/tfausak/rattletrap">Rattletrap</a></h2>
                  <p>Best in class replay parsing. Fastest in it's sub-phylum.</p>
                </div>
              </div>
            </div>
            <div class="column">
              <div class="mybox has-text-centered">
                <span class="icon is-large">
                  <i class="fa fa-area-chart"></i>
                </span>
                <div>
                  <h2 class="subtitle">Track Your Progress</h2>
                  <p>Download our <router-link to="/desktop-app">desktop app</router-link> to automatically upload your replays. Your stats will be generated as you play.</p>
                </div>
              </div>
            </div>
            <div class="column">
              <div class="mybox has-text-centered">
                <span class="icon is-large">
                  <i class="fa fa-comments-o"></i>
                </span>
                <div>
                  <h2 class="subtitle">Join the Community</h2>
                  <p>Have a good idea? Want to help us lower the salt levels? <a target="_blank" href="https://discord.gg/nqZHDpy">Join us on discord</a>.</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
  </div>
</template>

<script>
import PlayerSearchComponent from '../components/PlayerSearchComponent.vue'

export default {
  components: {
    PlayerSearchComponent
  }
}
</script>

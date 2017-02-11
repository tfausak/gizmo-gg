<style scoped lang="scss">
#gizmo-splash {
  background-image: url('/static/img/gizmo/gizmo-green-hq.png');
  background-position: center center;
  height: 150px;
  background-repeat: no-repeat;
  background-size: contain;
  position: relative;
}
#gizmo-snippets {
  h2 {
    margin-bottom: 5px;
  }
}
</style>

<template>
  <div>
    <section class="hero is-primary is-bold is-large has-text-centered">
      <div class="hero-body">
        <div class="container">
          <h1 class="title">
            <div id="gizmo-splash"></div>
            gizmo.gg
          </h1>
          <div class="text-normal">
            <player-search-component></player-search-component>
          </div>
        </div>
      </div>
    </section>

    <section class="hero is-light">
      <div class="hero-body">
        <div class="container">
          <div class="level has-text-centered">
            <div class="level-item">
              <div>
                <p class="heading">Replays</p>
                <p class="title">3,456</p>
              </div>
            </div>
            <div class="level-item">
              <div>
                <p class="heading">Players</p>
                <p class="title">123</p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

    <section class="section">
      <div class="container">
        <div class="columns has-text-centered" id="gizmo-snippets">

          <div class="column">
            <span class="icon is-large">
              <i class="fa fa-car"></i>
            </span>
            <div>
              <h2 class="subtitle">Powered by <a target="_blank" href="https://github.com/tfausak/rattletrap">Rattletrap</a></h2>
              <p>Best in class replay parsing. Fastest in it's sub-phylum.</p>
            </div>
          </div>

          <div class="column">
            <span class="icon is-large">
              <i class="fa fa-area-chart"></i>
            </span>
            <div>
              <h2 class="subtitle">Track Your Progress</h2>
              <p>Download our <router-link to="/desktop-app">desktop app</router-link> to automatically upload your replays. Your stats will be generated as you play.</p>
            </div>
          </div>

          <div class="column">
            <span class="icon is-large">
              <i class="fa fa-comments-o"></i>
            </span>
            <div>
              <h2 class="subtitle">Join the Community</h2>
              <p>Have a good idea? Want to help us lower the salt levels? <a target="_blank" href="https://discord.gg/nqZHDpy">Join us on discord</a>.</p>
            </div>
          </div>

          <div class="column">
            <span class="icon is-large">
              <i class="fa fa-gears"></i>
            </span>
            <div>
              <h2 class="subtitle">Test</h2>
              <ul>
                <li><router-link to="/dev">dev</router-link></li>
              </ul>
            </div>
          </div>

        </div>
      </div>
    </section>

  </div>
</template>

<script>
import PlayerSearchComponent from './components/PlayerSearch'

export default {
  components: {
    PlayerSearchComponent
  }
}
</script>

<style scoped lang="scss">
#gizmo-splash {
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
.splashDesc {
  font-size: 15px;
  p {
    margin: 0 0 10px;
  }
}
</style>

<template>
  <div>
    <section class="hero is-primary is-medium has-text-centered">
      <div class="hero-body">
        <div class="container">
          <h1 class="title">
            <div id="gizmo-splash" v-bind:style="{ backgroundImage: backgroundImage }"></div>
            gizmo.gg
          </h1>
          <div class="text-normal">
            <player-search-component></player-search-component>
          </div>
        </div>
      </div>
    </section>

    <section class="section">
      <div class="container has-text-centered">
        <h1 class="title is-3">Keep track of your Rocket League games.</h1>
        <hr>
        <div class="columns">
          <div class="column is-half is-offset-one-quarter splashDesc">
            <p>gizmo.gg is a Rocket League statistics site that helps you keep track of your games. We analyze your replays and provide you with data about your wins, losses, goals, assists, and everything else. Use our simple open-source tool to automatically upload your replays after each game.</p>
          </div>
        </div>
      </div>
    </section>

    <section class="section bg-offwhite">
      <div class="container">
        <div class="columns has-text-centered" id="gizmo-snippets">
          <div class="column is-4 is-offset-1">
            <span class="icon is-large">
              <i class="fa fa-cloud-upload"></i>
            </span>
            <h2 class="title is-3">Automatically Upload Replays</h2>
            <p class="splashDesc">Download our <router-link to="/desktop-app">desktop app</router-link> to automatically upload your replays. Your stats will be generated as you play.</p>
          </div>
          <div class="column is-3 is-offset-2">
            <span class="icon is-large">
              <i class="fa fa-comments-o"></i>
            </span>
            <h2 class="title is-3">Join the Community</h2>
            <p class="splashDesc">Have a good idea or feature request? Found a problem? <a target="_blank" href="https://discord.gg/nqZHDpy">Join us on discord</a>.</p>
          </div>
        </div>
      </div>
    </section>

  </div>
</template>

<script>
import PlayerSearchComponent from './components/PlayerSearch'

export default {
  data: () => {
    const colors = ['blue', 'green', 'orange'];
    const color = colors[Math.floor(Math.random() * colors.length)];
    return {
      backgroundImage: `url('/static/img/gizmo/gizmo-${color}-hq.png')`
    };
  },
  components: {
    PlayerSearchComponent
  }
}
</script>

<template>
  <div>
    <section class="hero is-primary">
      <div class="hero-body">
        <div class="container">
          <div class="level">
            <div class="level-left">
              <h1 class="title">
                Search
              </h1>
            </div>
            <div class="level-item">
              <player-search-component :platform="platform" :search="search"></player-search-component>
            </div>
          </div>
        </div>
      </div>
    </section>
    <section class="section">
      <div class="container">
        <div v-if="!results.length">
          <div class="container has-text-centered">
            <h1 class="title">
              No results!
            </h1>
            <hr>
            <h2 class="subtitle">
              Players only show up if they appear in at least one uploaded replay.<br><router-link to="/upload">Upload a replay</router-link> to get your own player page.
            </h2>
          </div>
        </div>
        <div v-else>
          <table class="table">
            <thead>
              <tr>
                <th>Platform</th>
                <th>Avatar</th>
                <th>Name</th>
                <th>Aliases</th>
                <th>Rank</th>
              </tr>
            </thead>
          </table>
        </div>
      </div>
    </section>
  </div>
</template>

<script>
import PlayerSearchComponent from '../components/PlayerSearchComponent.vue'

export default {
  data: function () {
    return {
      platform: this.$route.query.platform,
      search: this.$route.query.search,
      results: []
    }
  },
  components: {
    PlayerSearchComponent
  }
}
</script>

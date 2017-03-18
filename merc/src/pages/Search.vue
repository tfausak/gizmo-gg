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
              <player-search-component :search="search"></player-search-component>
            </div>
          </div>
        </div>
      </div>
    </section>
    <loading-component :loading="loading"></loading-component>
    <section v-if="!loading" class="section">
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
                <th>Name</th>
                <th>Last seen</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="player in results">
                <td>{{ player.platformName }}</td>
                <td>
                  <router-link :to="'/player/' + player.id + '/summary'">
                    {{ player.name }}
                  </router-link>
                </td>
                <td>{{ player.lastSeen }}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </section>
  </div>
</template>

<script>
import LoadingComponent from './components/Loading'
import PlayerSearchComponent from './components/PlayerSearch'

export default {
  data: function () {
    return {
      loading: true,
      results: []
    }
  },
  computed: {
    search: function () {
      return this.$route.query.search
    },
    platform: function () {
      return this.$route.query.platform
    }
  },
  methods: {
    performSearch: function () {
      const vm = this
      vm.loading = true
      this.$store.dispatch('GET_SEARCH', {
        name: this.search,
        platform: this.platform
      }).then(function (players) {
        vm.loading = false
        if (players.length === 1) {
          vm.$router.push({
            name: 'player.summary',
            params: {
              id: players[0].id
            }
          })
        } else {
          vm.results = players
        }
      }).catch(function (message) {
        vm.loading = false
        vm.results = []
      })
    }
  },
  beforeMount: function () { this.performSearch() },
  watch: {
    search: function () { this.performSearch() },
    platform: function () { this.performSearch() }
  },
  components: {
    LoadingComponent,
    PlayerSearchComponent
  }
}
</script>

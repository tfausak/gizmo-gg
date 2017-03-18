<style scoped lang="scss">
@import "~styles/vars.scss";

.brand {
  font-weight: bold;
  background-color: transparent!important;
}
#header {
  background-color: $dark;
}
#header a.nav-item {
  color: $white;
  border: 0;
  &:hover {
    background-color: rgba(255, 255, 255, 0.3);
    border: 0;
  }
  &.is-active {
    border: 0;
    padding-bottom: calc(0.5rem - 1px);
    background-color: rgba(255, 255, 255, 0.1);
  }
}
#header a.nav-item {
  color: $white;
}
#header .topnav {
  background-color: transparent;
  padding: 0.5rem 1rem;
}
.nav-item a.is-tab:hover, a.nav-item.is-tab:hover,
.nav-item a.is-tab.is-active, a.nav-item.is-tab.is-active {
  border-bottom-color: #fff;
}
</style>

<template>
  <div id="header" >
    <div class="container">
      <div class="level topnav">
        <div class="level-left">
          <div class="level-item">
            <router-link to="/" class="nav-item brand">gizmo.gg</router-link>
          </div>
        </div>
        <div class="level-right">
          <div class="level-item search-wrapper" v-if="showSearch()">
            <player-search-component :small="true"></player-search-component>
          </div>
          <div class="level-item">
            <router-link :to="{ name: 'stats' }" class="nav-item is-tab">Statistics</router-link>
            <router-link to="/desktop-app" class="nav-item is-tab">Desktop App</router-link>
            <router-link to="/upload" class="nav-item is-tab">Upload Replay</router-link>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import PlayerSearchComponent from '../../components/PlayerSearch'

export default {
  components: {
    PlayerSearchComponent
  },
  methods: {
    showSearch: function () {
      return this.$route.name !== 'index' && this.$route.name !== 'search'
    }
  }
}
</script>

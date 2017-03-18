<style lang="scss">
@import "~styles/vars.scss";

.panel {
  background-color: $white;
}
#playlistTabs {
  margin-top: -1.4rem;
  li {
    background-color: #fff;
  }
}
.playerStatPanel {
  & > .panel-heading {
    border-bottom: 0;
    & > .heading {
      color: rgba(0, 0, 0, 0.5)!important;
      font-weight: normal;
      letter-spacing: 0;
    }
  }
}
</style>

<template>
  <div class="container">
    <div class="tabs is-toggle is-small" id="playlistTabs">
      <ul>
        <li v-for="(value, key) in playlistOptions" @click="setPlaylist(key)" :class="{ 'is-active': key === playlist }"><a>{{ value }}</a></li>
      </ul>
    </div>
    <div class="columns">
      <div class="column is-one-third">
        <!-- <ranks-component :playerId="playerId"></ranks-component> -->
        <maps-component :playerId="playerId" :playlist="playlist"></maps-component>
        <bodies-component :playerId="playerId" :playlist="playlist"></bodies-component>
      </div>
      <div class="column">
        <games-component :playerId="playerId" :playlist="playlist"></games-component>
      </div>
    </div>
  </div>
</template>

<script>
import GamesComponent from './components/Games'
import MapsComponent from './components/Maps'
import BodiesComponent from './components/Bodies'
import RanksComponent from './components/Ranks'
import FilterPlaylistMixin from '../mixins/FilterPlaylistMixin'

export default {
  components: {
    GamesComponent,
    MapsComponent,
    BodiesComponent,
    RanksComponent
  },
  methods: {
    fetchData: function () {},
    setPlaylist: function (key) {
      this.playlist = key
    }
  },
  mixins: [ FilterPlaylistMixin ],
  props: [ 'playerId' ]
}
</script>

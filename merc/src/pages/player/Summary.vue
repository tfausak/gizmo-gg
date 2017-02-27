<style lang="scss">
@import "~styles/vars.scss";

.panel {
  background-color: $white;
}
</style>

<template>
  <div class="container">
    <div class="columns">
      <div class="column is-one-third">
        <!-- <ranks-component :playerId="playerId"></ranks-component> -->
        <maps-component :playerId="playerId"></maps-component>
      </div>
      <div class="column">
        <games-component :playerId="playerId"></games-component>
      </div>
    </div>
  </div>
</template>

<script>
import GamesComponent from './components/Games'
import MapsComponent from './components/Maps'
import RanksComponent from './components/Ranks'

export default {
  components: {
    GamesComponent,
    MapsComponent,
    RanksComponent
  },
  props: [ 'playerId' ]
}
</script>

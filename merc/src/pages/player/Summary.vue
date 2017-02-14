<style scoped lang="scss">
@import "~styles/vars.scss";

.text-spacer {
  padding: 0 5px;
  text-align: center;
}
.panel {
  background-color: $white;
}
.playlist {
  width: 32px;
  text-align: center;
}
.heading.is-medium {
  font-size :12px;
}
.panel-tabs {
  padding: 0 1em;
  .tabs {
    margin-bottom: -1px;
  }
}
.echarts {
  width: 100px!important;
  height: 80px!important;
  margin: 0 auto!important;
}
.panel-block .columns {
  width: 100%;
  align-items: center;
}
</style>

<template>
  <div class="container">
    <div class="columns">
      <div class="column is-one-third">
        <ranks-component :playerId="playerId"></ranks-component>
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

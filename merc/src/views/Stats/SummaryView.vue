<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-one-quarter">
          <filter-panel title="Time" :options="[ 'Current Season', 'Last Month', 'Last Week' ]"></filter-panel>
        </div>
        <div class="column is-one-quarter">
          <filter-panel title="Playlist" :options="[ 'All', 'Ranked 1v1', 'Ranked 2v2', 'Ranked 3v3', 'Ranked 3v3 Solo' ]"></filter-panel>
        </div>
      </div>

      <div class="columns">
        <div class="column">
          <wins-component></wins-component>
        </div>
        <div class="column">
          <body-component></body-component>
        </div>
      </div>
      <map-freq-component></map-freq-component>
    </div>
  </section>
</template>

<script>
import WinsComponent from '../../components/charts/WinsComponent.vue'
import BodyComponent from '../../components/charts/BodyComponent.vue'
import MapFreqComponent from '../../components/charts/MapFreqComponent.vue'
import FilterPanelComponent from '../../components/FilterPanelComponent.vue'

export default {
  components: {
    WinsComponent: WinsComponent,
    BodyComponent: BodyComponent,
    MapFreqComponent: MapFreqComponent,
    FilterPanel: FilterPanelComponent
  }
}
</script>

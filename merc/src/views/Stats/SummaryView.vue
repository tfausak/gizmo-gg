<template>
  <section class="section">
    <div class="container">
      <h1 class="title">Summary</h1>
      <hr>
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

export default {
  components: {
    WinsComponent: WinsComponent,
    BodyComponent: BodyComponent,
    MapFreqComponent: MapFreqComponent
  }
}
</script>

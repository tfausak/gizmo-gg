<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <!--
        <div class="column is-one-quarter">
          <filter-panel-component v-model="tier" title="Tier" :options="tierOptions" :sync="tier"></filter-panel-component>
        </div>
        -->
        <div class="column is-one-quarter">
          <filter-panel-component v-model="time" title="Time" :options="timeOptions" :sync="time"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="playlist" title="Playlist" :options="playlistOptions" :sync="playlist"></filter-panel-component>
        </div>
      </div>

      <loading-component :loading="loading"></loading-component>

      <div v-if="source">
        <h2 class="title">Map</h2>
        <map-table-component :source="source['byTemplate']"></map-table-component>

        <h2 class="title">Field</h2>
        <map-table-component :source="source['byModel']"></map-table-component>

        <h2 class="title">Variant</h2>
        <map-table-component :source="source['bySkin']"></map-table-component>
      </div>
    </div>
  </section>
</template>

<script>
import FilterPanelComponent from '../components/FilterPanel'
import LoadingComponent from '../components/Loading'
import MapTableComponent from './components/MapTable'
import FilterTierMixin from '../mixins/FilterTierMixin'
import FilterPlaylistMixin from '../mixins/FilterPlaylistMixin'
import FilterTimeMixin from '../mixins/FilterTimeMixin'

import { compileMapStats } from '../../store/scrubber.js'

export default {
  beforeMount: function () {
    let vm = this
    vm.fetchData()
    vm.$store.dispatch('GET_ARENAS').then(function (data) {
      vm.GET_ARENAS = data
      vm.compileData()
    })
  },
  components: {
    FilterPanelComponent,
    LoadingComponent,
    MapTableComponent
  },
  computed: {
    loading: function () {
      return this.GET_STATS_ARENAS === null || this.GET_ARENAS === null
    }
  },
  data: function () {
    return {
      GET_STATS_ARENAS: null,
      GET_ARENAS: null,
      source: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_STATS_ARENAS = null
      vm.source = null
      vm.$store.dispatch('GET_STATS_ARENAS', {
        playlist: vm.playlist,
        tier: vm.tier,
        time: vm.time
      }).then(function (data) {
        vm.GET_STATS_ARENAS = data
        vm.compileData()
      })
    },
    compileData: function () {
      if (this.loading) {
        return
      }
      this.source = compileMapStats(this.GET_STATS_ARENAS, this.GET_ARENAS)
    }
  },
  mixins: [ FilterTierMixin, FilterPlaylistMixin, FilterTimeMixin ]
}
</script>

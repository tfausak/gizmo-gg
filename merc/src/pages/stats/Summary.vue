<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-one-quarter">
          <filter-panel-component v-model="time" title="Time" :options="timeOptions" :sync="time"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="playlist" title="Playlist" :options="playlistOptions" :sync="playlist"></filter-panel-component>
        </div>
      </div>

      <div class="columns">
        <div class="column">
          <article class="message">
            <div class="message-body">
              <p class="heading is-1">Win Pct by Team</p>
              <chart-team-wins-component :GET_STATS_SUMMARY="GET_STATS_SUMMARY"></chart-team-wins-component>
            </div>
          </article>
        </div>
        <div class="column">
          <article class="message">
            <div class="message-body">
              <p class="heading is-1">
                Battle-Car Use
                - <router-link to="/stats/battle-cars">more</router-link>
              </p>
              <chart-battle-cars-component :GET_STATS_SUMMARY="GET_STATS_SUMMARY"></chart-battle-cars-component>
            </div>
          </article>
        </div>
      </div>

      <article class="message">
        <div class="message-body">
          <p class="heading has-text-centered">
            Map Frequency
            - <router-link to="/stats/maps">more</router-link>
          </p>
          <div style="margin-top: 20px;"></div>
          <chart-maps-component :GET_STATS_SUMMARY="GET_STATS_SUMMARY"></chart-maps-component>
        </div>
      </article>
    </div>
  </section>
</template>

<script>
import ChartBattleCarsComponent from './components/ChartBattleCars'
import ChartMapsComponent from './components/ChartMaps'
import ChartTeamWinsComponent from './components/ChartTeamWins'
import FilterPanelComponent from '../components/FilterPanel'
import FilterPlaylistMixin from '../mixins/FilterPlaylistMixin'
import FilterTimeMixin from '../mixins/FilterTimeMixin'

export default {
  beforeMount: function () {
    this.fetchData()
  },
  components: {
    ChartBattleCarsComponent,
    ChartMapsComponent,
    ChartTeamWinsComponent,
    FilterPanelComponent
  },
  computed: {
    loading: function () {
      return this.GET_STATS_SUMMARY === null
    }
  },
  data: function () {
    return {
      GET_STATS_SUMMARY: null
    }
  },
  methods: {
    fetchData: function () {
      let vm = this
      vm.GET_STATS_SUMMARY = null
      vm.$store.dispatch('GET_STATS_SUMMARY', {
        playlist: vm.playlist,
        time: vm.time
      }).then(function (data) {
        vm.GET_STATS_SUMMARY = data
      })
    }
  },
  mixins: [ FilterPlaylistMixin, FilterTimeMixin ]
}
</script>

<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-one-quarter">
          <filter-panel v-model="tier" title="Tier" :options="tierOptions"></filter-panel>
        </div>
        <div class="column is-one-quarter">
          <filter-panel v-model="time" title="Time" :options="timeOptions"></filter-panel>
        </div>
        <div class="column is-one-quarter">
          <filter-panel v-model="playlist" title="Playlist" :options="playlistOptions"></filter-panel>
        </div>
      </div>

      <table class="table is-striped table-outerborder table-stats">
        <thead>
          <tr>
            <th-sortable v-for="col in cols" @orderByCol="orderByCol" :sort="sort" :col="col.key">{{ col.name }}</th-sortable>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in sortedRows">
            <td>{{ row.map }}</td>
            <td>
              <div class="level">
                <div class="level-item">
                  <progress class="progress is-small" :value="row.freqPct" :max="maxFreqPct"></progress>
                </div>
                <div class="level-item">
                  {{ row.freqPct }}%
                </div>
              </div>
            </td>
            <td class="has-text-right">{{ row.games }}</td>
            <td>
              <div class="level">
                <div class="level-item">
                  <progress class="progress is-small is-success" :value="row.pts" :max="maxPts"></progress>
                </div>
                <div class="level-item">
                  {{ row.pts }}
                </div>
              </div>
            </td>
            <td>{{ row.acc }}%</td>
          </tr>
        </tbody>
      </table>
    </div>
  </section>
</template>

<script>
import mock from '../../mock/index.js'
import bodies from '../../assets/bodies.js'
import SortableThComponent from '../../components/SortableThComponent.vue'
import FilterPanelComponent from '../../components/FilterPanelComponent.vue'

import playlistOptions from '../../store/options/playlist.js'
import timeOptions from '../../store/options/time.js'
import tierOptions from '../../store/options/tier.js'

var _ = require('lodash')

export default {
  components: {
    ThSortable: SortableThComponent,
    FilterPanel: FilterPanelComponent
  },
  computed: {
    sortedRows: function () {
      var sorted = _.sortBy(this.rows, [this.sort])
      if (!this.dir) {
        _.reverse(sorted)
      }
      return sorted
    }
  },
  data: function () {
    let rows = mock.getStatMaps()
    let maxFreqPct = 0
    let maxPts = 0
    for (let i in rows) {
      let row = rows[i]
      maxFreqPct = Math.max(maxFreqPct, row.freqPct)
      maxPts = Math.max(maxPts, row.pts)
    }
    return {
      tierOptions: tierOptions,
      tier: _.head(_.keys(tierOptions)),
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      timeOptions: timeOptions,
      time: _.head(_.keys(timeOptions)),
      rows: rows,
      maxFreqPct: maxFreqPct,
      maxPts: maxPts,
      sort: null,
      dir: 1,
      cols: [
        { key: 'map', name: 'Map' },
        { key: 'freqPct', name: 'Freq. Pct' },
        { key: 'games', name: 'Games Played' },
        { key: 'pts', name: 'Avg Pts' },
        { key: 'acc', name: 'Accuracy' }
      ]
    }
  },
  methods: {
    slug: bodies.slug,
    orderByCol: function (col, dir) {
      this.sort = col
      this.dir = dir
    }
  }
}
</script>

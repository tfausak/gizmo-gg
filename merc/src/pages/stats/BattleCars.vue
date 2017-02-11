<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-one-quarter">
          <filter-panel-component v-model="tier" title="Tier" :options="tierOptions"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="time" title="Time" :options="timeOptions"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="map" title="Map" :options="mapOptions"></filter-panel-component>
        </div>
        <div class="column is-one-quarter">
          <filter-panel-component v-model="playlist" title="Playlist" :options="playlistOptions"></filter-panel-component>
        </div>
      </div>

      <table class="table is-striped table-outerborder table-stats">
        <thead>
          <tr>
            <sortable-th-component v-for="col in cols" @orderByCol="orderByCol" :sort="sort" :col="col.key">{{ col.name }}</sortable-th-component>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in sortedRows">
            <td>
              <div class="level level-chained">
                <div class="level-item">
                  <figure class="image is-32x32 is-circle-32x32">
                    <img :src="'/static/img/bodies/' + slug(row.body) + '.png'">
                  </figure>
                </div>
                <div class="level-item">
                  <strong>{{ row.body }}</strong>
                </div>
              </div>
            </td>
            <td>
              <div class="level level-chained">
                <div class="level-item">
                  <progress class="progress is-small" :class="{ 'is-primary': row.winPct >= 50 }" :value="row.winPct" :max="maxWinPct"></progress>
                </div>
                <div class="level-item">
                  {{ row.winPct }}%
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
import SortableThComponent from '../components/SortableTh'
import FilterPanelComponent from '../components/FilterPanel'

import playlistOptions from '../../store/options/playlist.js'
import timeOptions from '../../store/options/time.js'
import mapOptions from '../../store/options/map.js'
import tierOptions from '../../store/options/tier.js'

var _ = require('lodash')

export default {
  components: {
    SortableThComponent,
    FilterPanelComponent
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
    let rows = mock.getStatBodies()
    let maxWinPct = 0
    let maxPts = 0
    for (let i in rows) {
      let row = rows[i]
      maxWinPct = Math.max(maxWinPct, row.winPct)
      maxPts = Math.max(maxPts, row.pts)
    }
    return {
      tierOptions: tierOptions,
      tier: _.head(_.keys(tierOptions)),
      playlistOptions: playlistOptions,
      playlist: _.head(_.keys(playlistOptions)),
      timeOptions: timeOptions,
      time: _.head(_.keys(timeOptions)),
      mapOptions: mapOptions,
      map: _.head(_.keys(mapOptions)),
      rows: rows,
      maxWinPct: maxWinPct,
      maxPts: maxPts,
      sort: null,
      dir: 1,
      cols: [
        { key: 'body', name: 'Battle-Car' },
        { key: 'winPct', name: 'Win Pct' },
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

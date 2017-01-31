<style scoped lang="scss">
@import "~styles/vars.scss";

.table-bodies {
  th {
    font-weight: normal;
    .has-text-right {
      text-align: right;
    }
  }

  .level {
    justify-content: flex-start;
  }
  .level > .level-item {
    flex-grow: 0;
    padding: 0 0 0 10px;
  }

  progress {
    width: 100px;
    border-radius: 0;
  }
}
.is-circle-32x32 {
  background-color: $grey-lighter;
  border-radius: 16px;
}
</style>

<template>
  <section class="section">
    <div class="container">
      <div class="columns">

        <div class="column is-one-quarter">
          <filter-panel title="Tier" :options="[ 'All', 'Prospect', 'Challenger', 'Star', 'Champion' ]"></filter-panel>
        </div>

        <div class="column is-one-quarter">
          <filter-panel title="Time" :options="[ 'Current Season', 'Last Month', 'Last Week' ]"></filter-panel>
        </div>

        <div class="column is-one-quarter">
          <filter-panel title="Map" :options="[ 'All', 'Standard', 'Wasteland', 'ARC' ]"></filter-panel>
        </div>

        <div class="column is-one-quarter">
          <filter-panel title="Playlist" :options="[ 'All', 'Ranked 1v1', 'Ranked 2v2', 'Ranked 3v3', 'Ranked 3v3 Solo' ]"></filter-panel>
        </div>

      </div>

      <table class="table is-striped table-outerborder table-bodies">
        <thead>
          <tr>
            <th-sortable @orderByCol="orderByCol" :sort="sort" :col="'body'">Body</th-sortable>
            <th-sortable @orderByCol="orderByCol" :sort="sort" :col="'winPct'">Win Pct</th-sortable>
            <th-sortable @orderByCol="orderByCol" :sort="sort" :col="'games'">Games Played</th-sortable>
            <th-sortable @orderByCol="orderByCol" :sort="sort" :col="'pts'">Avg Pts</th-sortable>
            <th-sortable @orderByCol="orderByCol" :sort="sort" :col="'acc'">Accuracy</th-sortable>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in sortedRows">
            <td>
              <div class="level">
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
              <div class="level">
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
import mock from '../../mock/stats-bodies.js'
import bodies from '../../assets/bodies.js'
import SortableThComponent from '../../components/SortableThComponent.vue'
import FilterPanelComponent from '../../components/FilterPanelComponent.vue'
var _ = require('lodash')

export default {
  components: {
    ThSortable: SortableThComponent,
    FilterPanel: FilterPanelComponent
  },
  computed: {
    sortedRows: function () {
      var sorted = _.sortBy(this.$data.rows, [this.$data.sort])
      if (!this.$data.dir) {
        _.reverse(sorted)
      }
      return sorted
    }
  },
  data: function () {
    let rows = mock
    let maxWinPct = 0
    let maxPts = 0
    for (let i in rows) {
      let row = rows[i]
      maxWinPct = Math.max(maxWinPct, row.winPct)
      maxPts = Math.max(maxPts, row.pts)
    }
    return {
      rows: rows,
      maxWinPct: maxWinPct,
      maxPts: maxPts,
      sort: null,
      dir: 1
    }
  },
  methods: {
    slug: bodies.slug,
    orderByCol: function (col, dir) {
      this.$data.sort = col
      this.$data.dir = dir
    }
  }
}
</script>

<template>
  <table class="table is-striped table-outerborder table-stats">
    <thead>
      <tr>
        <sortable-th-component v-for="col in cols" @orderByCol="orderByCol" :sort="sort" :col="col.key">{{ col.name }}</sortable-th-component>
      </tr>
    </thead>
    <tbody>
      <tr v-for="row in sortedRows">
        <td>{{ row.displayName }}</td>
        <td>
          <div class="level level-chained">
            <div class="level-item">
              <progress class="progress is-small" :value="row.freqPct" :max="source['maxFreqPct']"></progress>
            </div>
            <div class="level-item">
              {{ row.freqPct }}%
            </div>
          </div>
        </td>
        <td class="has-text-right">{{ row.numGames }}</td>
        <td class="has-text-right" v-if="hasCol('winPct')">
          <div class="level">
            <div class="level-left">
              <div class="level-item">{{ row.winPct }}%</div>
            </div>
            <div class="level-right">
              <div class="level-item">{{ row.numWins }}W - {{ row.numLosses }}L</div>
            </div>
          </div>
        </td>
        <td>
          <div class="level level-chained">
            <div class="level-item">
              <progress class="progress is-small is-success" :value="row.avgScore" :max="source['maxScore']"></progress>
            </div>
            <div class="level-item">
              {{ row.avgScore }}
            </div>
          </div>
        </td>
        <td>{{ row.accuracy }}%</td>
      </tr>
    </tbody>
  </table>
</template>

<script>
import SortableThComponent from '../../components/SortableTh'

var _ = require('lodash')

export default {
  components: {
    SortableThComponent
  },
  computed: {
    sortedRows: function () {
      if (!this.source) {
        return []
      }
      var sorted = _.sortBy(this.source['data'], [this.sort])
      if (!this.dir) {
        _.reverse(sorted)
      }
      return sorted
    }
  },
  data: function () {
    let cols
    if (this.scope === 'map') {
      cols = [
        { key: 'displayName', name: 'Map' },
        { key: 'freqPct', name: 'Freq. Pct' },
        { key: 'numGames', name: 'Games Played' },
        { key: 'avgScore', name: 'Avg Score' },
        { key: 'accuracy', name: 'Accuracy' }
      ]
    } else if (this.scope === 'player') {
      cols = [
        { key: 'displayName', name: 'Map' },
        { key: 'freqPct', name: 'Freq. Pct' },
        { key: 'numGames', name: 'Games Played' },
        { key: 'winPct', name: 'Win Pct' },
        { key: 'avgScore', name: 'Avg Score' },
        { key: 'accuracy', name: 'Accuracy' }
      ]
    }
    return {
      sort: null,
      dir: 1,
      cols: cols
    }
  },
  methods: {
    orderByCol: function (col, dir) {
      this.sort = col
      this.dir = dir
    },
    hasCol: function (col) {
      let found = false
      _.each(this.cols, function (o) {
        if (o.key === col) {
          found = true
          return
        }
      })
      return found
    }
  },
  props: {
    source: {
      required: true
    },
    scope: {
      default: 'map'
    }
  }
}
</script>

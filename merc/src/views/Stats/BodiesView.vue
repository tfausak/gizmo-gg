<style scoped lang="scss">
@import "~styles/vars.scss";

.table-bodies {
  border: 1px solid $border;

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
.is-sort {
  font-size: 12px;
  width: 12px;
  height: auto;
  margin-top: 1px;
  margin-bottom: -1px;
}
.is-circle-32x32 {
  background-color: $grey-lighter;
  border-radius: 16px;
  padding: 2px;
}
.level-split {
  justify-content: space-between!important;
}
.level-split > .level-item {
  flex-grow: 0!important;
}
</style>

<template>
  <section class="section">
    <div class="container">
      <h1 class="title">Bodies</h1>
      <hr>
      <table class="table is-striped table-bodies">
        <thead>
          <tr>
            <th class="has-text-right" style="width: 60px;">
              #
              <span class="icon is-sort">
                <i class="fa fa-sort"></i>
              </span>
            </th>
            <th>Body</th>
            <th>Win Rate</th>
            <th class="has-text-right" style="width: 130px;">
              Games Played
              <span class="icon is-sort">
                <i class="fa fa-sort"></i>
              </span>
            </th>
            <th>
              <div class="level level-split">
                <div class="level-item">Avg Points</div>
                <div class="level-item">
                  <span class="icon is-sort">
                    <i class="fa fa-sort"></i>
                  </span>
                </div>
              </div>
            </th>
            <th>Accuracy</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in rows">
            <td class="has-text-right">{{ row.id }}</td>
            <td>
              <div class="level">
                <div class="level-item">
                  <figure class="image is-32x32 is-circle-32x32">
                    <img :src="'/static/img/bodies/' + row.body.toLowerCase() + '.png'">
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
      maxWinPct: {{ maxWinPct }}<br>
      maxPts: {{ maxPts }}
    </div>
  </section>
</template>

<script>
export default {
  data: function () {
    let rows = [
      { id: 1, body: 'Octane', winPct: '52.55', games: '183', pts: '655', acc: '44.8' },
      { id: 2, body: 'Dominus', winPct: '52.25', games: '96', pts: '683', acc: '45.2' }
    ]
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
      maxPts: maxPts
    }
  }
}
</script>

<style scoped lang="scss">
.level-progress {
  flex-grow: 5!important;
}
th {
  font-weight: normal;
}
.is-sort {
  font-size: 12px;
  width: 12px;
  height: auto;
  margin-top: 1px;
  margin-bottom: -1px;
}
th.has-text-right {
  text-align: right;
}
table {
  border: 1px solid #dbdbdb;
}
</style>

<template>
  <section class="section">
    <div class="container">
      <h1 class="title">Bodies</h1>
      <hr>
      <table class="table is-striped">
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
            <th>Points</th>
            <th>Accuracy</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in rows">
            <td class="has-text-right">{{ row.id }}</td>
            <td>
              <figure class="image is-24x24">
                <img :src="'/static/img/bodies/' + row.body.toLowerCase() + '.png'">
              </figure>
              <strong>{{ row.body }}</strong>
            </td>
            <td>
              <div class="level">
                <div class="level-item level-progress">
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
                <div class="level-item level-progress">
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

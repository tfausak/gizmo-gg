<style lang="scss" scoped>
.scoreboard {
  table {
    margin: 0 auto;
    background-color: transparent;
    font-size: 12px;
    width: auto;
    border: 1px solid rgba(0, 0, 0, 0.1);
    th {
      background-color: rgba(255, 255, 255, 0.2);
      font-weight: normal;
      width: 50px;
      white-space: nowrap;
      overflow: hidden;
      color: rgba(0, 0, 0, 0.7);
      vertical-align: bottom;
    }
    .teamTh {
      width: 300px;
      font-size: 21px;
      text-transform: uppercase;
      letter-spacing: 1px;
      font-weight: normal;
      text-align: left;
    }
    .teamScore {
      display: inline-block;
    }
    .playerBody {
      padding-left: 1em;
      width: 50px;
    }
    .playerName {
      text-align: left;
    }
    a {
      color: inherit;
      font-weight: normal;
      &:hover {
        color: #fff;
      }
    }
    td {
      background-color: rgba(255, 255, 255, 0.4);
      font-size: 14px;
    }
    td, th {
      border-color: rgba(0, 0, 0, 0.1);
      border-left: 0;
      border-right: 0;
      padding: 2px 6px;
      text-align: right;
    }
    tr:hover {
      background-color: inherit;
    }
  }
}
</style>

<template>
  <div class="scoreboard">
    <table class="table is-narrow scoreboard">
      <thead>
        <tr>
          <th colspan="2" class="teamTh">
            <span class="teamScore">{{ goals }}</span>
            <span class="teamName">{{ team }}</span>
          </th>
          <th>Goals</th>
          <th>Assists</th>
          <th>Saves</th>
          <th>Shots</th>
          <th>Points</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="gplayer in players">
          <td class="playerBody">
            <figure class="image is-24x24 is-circle-dark">
              <img :src="'/static/img/bodies/' + gplayer.bodySlug + '.png'">
            </figure>
          </td>
          <td class="playerName">
            <router-link :to="'/player/' + gplayer.playerId">
              {{ gplayer.name }}
            </router-link>
          </td>
          <td>{{ gplayer.goals }}</td>
          <td>{{ gplayer.assists }}</td>
          <td>{{ gplayer.saves }}</td>
          <td>{{ gplayer.shots }}</td>
          <td>{{ gplayer.score }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
export default {
  props: [ 'players', 'goals', 'team' ]
}
</script>

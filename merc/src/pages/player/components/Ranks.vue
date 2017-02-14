<template>
  <div class="panel">
    <p class="panel-heading panel-squish">
      <span class="heading">Ranks</span>
    </p>
    <div class="panel-block" v-for="rank in ranks">
      <div class="level level-chained is-relative">
        <div class="level-item playlist text-small">
          {{ rank.playlist }}
        </div>
        <div class="level-item">
          <figure class="image is-32x32">
            <img :src="'/static/img/ranks/s3/' + rank.icon + '.png'">
          </figure>
        </div>
        <div class="level-item">
          <div class="level level-stacked level-chained">
            <div class="level-item">
              <div class="heading is-medium is-bold no-margin text-primary">{{ rank.rank }}</div>
            </div>
            <div class="level-item text-small">
              <span>{{ rank.rating }} MMR</span>
            </div>
            <div class="level-item text-small">
              <span class="text-muted">{{ rank.games }} Games</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  data: function () {
    return {
      ranks: [
        { rating: 882, playlist: '1v1', rank: 'All-Star Div 4', icon: 'all-star', games: 127 },
        { rating: 956, playlist: '2v2', rank: 'Superstar Div 3', icon: 'superstar', games: 1745 },
        { rating: 789, playlist: '3v3', rank: 'Shooting Star Div 5', icon: 'shooting-star', games: 160 },
        { rating: 900, playlist: '3v3 Solo', rank: 'All-Star Div 5', icon: 'all-star', games: 98 }
      ]
    }
  },
  props: [ 'playerId' ]
}
</script>

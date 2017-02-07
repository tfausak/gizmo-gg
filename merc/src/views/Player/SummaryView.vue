<style scoped lang="scss">
@import "~styles/vars.scss";

.text-spacer {
  padding: 0 5px;
  text-align: center;
}
.panel {
  background-color: $white;
}
.playlist {
  width: 32px;
  text-align: center;
}
</style>

<template>
  <div class="container">
    <div class="columns">
      <div class="column is-one-third">
        <div class="panel is-light">
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
                    <div class="heading is-bold no-margin text-primary">{{ rank.rank }}</div>
                  </div>
                  <div class="level-item text-small">
                    <span>{{ rank.rating }} MMR</span>
                    <span class="text-spacer"></span>
                    <span class="text-muted">{{ rank.wins }}W {{ rank.losses }}L</span>
                    <span class="text-spacer"></span>
                    <span>{{ winPct(rank.wins, rank.losses) }}%</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>

        <div class="panel is-light">
          <div class="panel-block" v-for="map in maps">
            <div class="level level-chained is-relative">
              <div class="level-item">
                <figure class="image is-48x48">
                  <img :src="'/static/img/maps/' + map.icon + '.jpg'">
                </figure>
              </div>
              <div class="level-item">
                <div class="level level-stacked level-chained">
                  <div class="level-item">
                    <div class="heading is-bold no-margin">{{ map.map }}</div>
                  </div>
                  <div class="level-item text-small">
                    <span>{{ map.wins + map.losses }} Games</span>
                    <span class="text-spacer"></span>
                    <span class="text-muted">{{ map.wins }}W {{ map.losses }}L</span>
                    <span class="text-spacer"></span>
                    <span>{{ winPct(map.wins, map.losses) }}%</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
      <div class="column">
        asdf
      </div>
    </div>
  </div>
</template>

<script>
var _ = require('lodash')

export default {
  data: function () {
    return {
      ranks: [
        { rating: 882, playlist: '1v1', rank: 'All-Star Div 4', icon: 'all-star', wins: 88, losses: 71 },
        { rating: 956, playlist: '2v2', rank: 'Superstar Div 3', icon: 'superstar', wins: 88, losses: 71 },
        { rating: 789, playlist: '3v3', rank: 'Shooting Star Div 5', icon: 'shooting-star', wins: 88, losses: 71 },
        { rating: 900, playlist: '3v3 Solo', rank: 'All-Star Div 5', icon: 'all-star', wins: 88, losses: 71 }
      ],
      maps: [
        { map: 'Standard', icon: 'standard', pts: 611, acc: 49.87, wins: 201, losses: 143 },
        { map: 'Wasteland', icon: 'wasteland', pts: 611, acc: 49.87, wins: 201, losses: 143 },
        { map: 'Starbase ARC', icon: 'arc', pts: 611, acc: 49.87, wins: 201, losses: 143 },
        { map: 'Neo Tokyo', icon: 'neotokyo', pts: 611, acc: 49.87, wins: 201, losses: 143 }
      ]
    }
  },
  methods: {
    winPct: function (wins, losses) {
      if (!wins) {
        return 0
      } else if (!losses) {
        return 100
      }
      return _.round(wins / (losses + wins) * 100)
    }
  }
}
</script>

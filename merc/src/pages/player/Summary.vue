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
.heading.is-medium {
  font-size :12px;
}
.panel-tabs {
  padding: 0 1em;
  .tabs {
    margin-bottom: -1px;
  }
}
.echarts {
  width: 100px!important;
  height: 80px!important;
  margin: 0 auto!important;
}
.panel-block .columns {
  width: 100%;
  align-items: center;
}
</style>

<template>
  <div class="container">
    <div class="columns">
      <div class="column is-one-third">
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

        <div class="panel">
          <p class="panel-heading panel-squish">
            <span class="heading">Maps</span>
          </p>
          <p class="panel-tabs">
            <a class="is-active">All</a>
            <a v-for="playlist in playlists">{{ playlist }}</a>
          </p>
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
                    <div class="heading is-medium is-bold no-margin">{{ map.map }}</div>
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
        <div class="panel">
          <div class="panel-block panel-tabs">
            <div class="tabs">
              <ul>
                <li class="is-active"><a>All</a></li>
                <li><a>1v1</a></li>
                <li><a>2v2</a></li>
                <li><a>3v3</a></li>
                <li><a>3v3 Solo</a></li>
              </ul>
            </div>
          </div>
          <div class="panel-block">
            <div class="columns">
              <div class="column is-2">
                <echart :options="chartOptions" v-if="chartOptions"></echart>
              </div>
              <div class="column is-2">
                {{ source.numWins }}W {{ source.numLosses }}L
              </div>
              <div class="column is-3 has-text-centered">
                <div class="title is-5">655 Pts/min</div>
                <div class="subtitle is-6">{{ source.totalGoals }}/{{ source.totalAssists }}/{{ source.totalSaves }}/{{ source.totalShots }} (..%)</div>
              </div>
            </div>
          </div>
        </div>

        <game-component v-for="game in source.games" :game="game"></game-component>
      </div>
    </div>
  </div>
</template>

<script>
import GameComponent from './components/Game'
var _ = require('lodash')

export default {
  components: {
    GameComponent
  },
  props: [ 'source' ],
  data: function () {
    return {
      ranks: [
        { rating: 882, playlist: '1v1', rank: 'All-Star Div 4', icon: 'all-star', games: 127 },
        { rating: 956, playlist: '2v2', rank: 'Superstar Div 3', icon: 'superstar', games: 1745 },
        { rating: 789, playlist: '3v3', rank: 'Shooting Star Div 5', icon: 'shooting-star', games: 160 },
        { rating: 900, playlist: '3v3 Solo', rank: 'All-Star Div 5', icon: 'all-star', games: 98 }
      ],
      maps: [
        { map: 'Standard', icon: 'standard', pts: 611, acc: 49.87, wins: 201, losses: 143 },
        { map: 'Wasteland', icon: 'wasteland', pts: 611, acc: 49.87, wins: 201, losses: 143 },
        { map: 'Starbase ARC', icon: 'arc', pts: 611, acc: 49.87, wins: 201, losses: 143 },
        { map: 'Neo Tokyo', icon: 'neotokyo', pts: 611, acc: 49.87, wins: 201, losses: 143 }
      ],
      playlists: ['1v1', '2v2', '3v3', '3v3 Solo'],
      chartOptions: {
        graphic: {
          elements: [
            {
              type: 'text',
              left: 'center',
              top: 'middle',
              style:
              {
                'text': _.round(this.source.winPct * 100) + '%',
                fill: '#777'
              }
            }
          ]
        },
        series: [
          {
            hoverAnimation: false,
            animation: false,
            type: 'pie',
            radius: ['55%', '80%'],
            data: [
              {
                name: 'losses',
                value: this.source.numLosses
              },
              {
                name: 'wins',
                value: this.source.numWins
              }
            ],
            label: { normal: { show: false } }
          }
        ]
      }
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

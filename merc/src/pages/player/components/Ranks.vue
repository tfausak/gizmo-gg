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
      ranks: []
    }
  },
  props: [ 'playerId' ]
}
</script>

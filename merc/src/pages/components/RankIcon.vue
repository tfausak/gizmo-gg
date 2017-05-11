<style scoped lang="scss">
.rank-icon {
  display: inline-block;
  height: 1.5em;
  vertical-align: bottom;
}
</style>

<template>
  <img
    :class="'rank-icon rank-' + rank"
    :src="'/static/img/ranks/season-4/' + rank + '.svg'"
    type="image/svg+xml"
    >
</template>

<script>
  export default {
    props: [ 'rank' ]
  }
</script>

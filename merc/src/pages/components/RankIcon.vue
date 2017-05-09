<style scoped lang="scss">
  .rank-icon {
    display: inline-block;
    height: 2em;
    vertical-align: middle;
  }
</style>

<template>
  <object
    :class="'rank-icon rank-' + rank"
    :data="'/static/img/ranks/season-4/' + rank + '.svg'"
    type="image/svg+xml"
  >
  </object>
</template>

<script>
  export default {
    props: [ 'rank' ]
  }
</script>

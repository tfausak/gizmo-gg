<style scoped lang="scss">
@import "~styles/vars.scss";

.loading-component {
  width: 300px;
  height: 150px;
  margin: 0 auto;
  text-align: center;
  padding-top: 45px;
  color: $grey-light;
  .fa {
    font-size: 65px;
  }
}
</style>

<template>
  <div v-if="loading" class="loading-component">
    <i class="fa fa-circle-o-notch fa-spin"></i>
  </div>
</template>

<script>
export default {
  props: [ 'loading' ]
}
</script>

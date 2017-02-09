<template>
  <div v-if="loading" class="echart-loading">
    <i class="fa fa-circle-o-notch fa-spin"></i>
  </div>
</template>

<script>
export default {
  props: [ 'loading' ]
}
</script>

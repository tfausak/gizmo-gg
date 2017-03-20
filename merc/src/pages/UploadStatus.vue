<template>
  <div>
    {{ GET_UPLOAD }}
  </div>
</template>

<script>
export default {
  beforeMount: function () {
    this.fetchData()
  },
  data: function () {
    return {
      GET_UPLOAD: null
    }
  },
  methods: {
    fetchData: function () {
      var vm = this
      vm.GET_UPLOAD = null
      vm.$store.dispatch('GET_UPLOAD', {
        id: vm.id
      }).then(function (data) {
        vm.GET_UPLOAD = data
      }).catch(function () {
        vm.missing = true
      })
    }
  },
  props: [ 'id' ],
  watch: {
    id: function () {
      this.fetchData()
    }
  }
}
</script>

<template>
  <section class="section">
    <div class="container">
      <loading-component :loading="loading"></loading-component>
      <table class="table" v-if="source && !loading">
        <thead>
          <tr>
            <th>ID</th>
            <th>created</th>
            <th>platformID</th>
            <th>remoteID</th>
            <th>localID</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="player in source">
            <td><router-link :to="'/player/' + player.id ">{{ player.id }}</router-link></td>
            <td>{{ player.createdAt }}</td>
            <td>{{ player.platformId }}</td>
            <td>{{ player.remoteId }}</td>
            <td>{{ player.localId }}</td>
          </tr>
        </tbody>
      </table>
    </div>
  </section>
</template>

<script>
import LoadingComponent from '../components/Loading'

export default {
  components: {
    LoadingComponent
  },
  data: function () {
    return {
      source: null,
      loading: true
    }
  },
  beforeMount: function () {
    let vm = this
    this.$store.dispatch('GET_PLAYERS').then(function (data) {
      vm.loading = false
      vm.source = data
    })
  }
}
</script>

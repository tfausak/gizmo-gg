<script>
import PlaylistOptionsMixin from './PlaylistOptionsMixin'

export default {
  computed: {
    qPlaylist: function () {
      return this.$route.query.playlist
    }
  },
  data: function () {
    return {
      playlist: this.$route.query.playlist || this.playlistDefault
    }
  },
  mixins: [ PlaylistOptionsMixin ],
  watch: {
    playlist: function (val) {
      let data = Object.assign({}, this.$route.query)
      data['playlist'] = val
      this.$router.replace({
        name: this.$route.name,
        query: data
      })
    },
    qPlaylist: function (val) {
      this.playlist = this.$route.query.playlist || this.playlistDefault
      this.fetchData()
    }
  }
}
</script>

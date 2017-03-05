<script>
import options from '../../store/options.js'

var _ = require('lodash')

export default {
  computed: {
    qPlaylist: function () {
      return this.$route.query.playlist
    }
  },
  data: function () {
    let playlistOptions = options.playlists()
    let playlistDefault = _.head(_.keys(playlistOptions))
    return {
      playlistOptions: playlistOptions,
      playlistDefault: playlistDefault,
      playlist: this.$route.query.playlist || playlistDefault
    }
  },
  watch: {
    playlist: function (val) {
      let data = Object.assign({}, this.$route.query)
      data['playlist'] = val
      this.$router.replace({
        name: 'stats',
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

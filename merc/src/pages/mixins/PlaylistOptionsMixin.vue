<script>
import options from '../../store/options.js'

var _ = require('lodash')

export default {
  data: function () {
    let playlistOptions = options.playlists()
    let playlistOptionsSpec = _.omit(playlistOptions, 'all')
    let playlistDefault = _.head(_.keys(playlistOptions))
    return {
      playlistOptions: playlistOptions,
      playlistOptionsSpec: playlistOptionsSpec,
      playlistDefault: playlistDefault
    }
  }
}
</script>

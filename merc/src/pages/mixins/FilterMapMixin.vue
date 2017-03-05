<script>
import options from '../../store/options.js'

var _ = require('lodash')

export default {
  computed: {
    qMap: function () {
      return this.$route.query.map
    }
  },
  data: function () {
    let mapOptions = options.mapTemplates()
    let mapDefault = _.head(_.keys(mapOptions))
    return {
      mapOptions: mapOptions,
      mapDefault: mapDefault,
      map: this.$route.query.map || mapDefault
    }
  },
  watch: {
    map: function (val) {
      let data = Object.assign({}, this.$route.query)
      data['map'] = val
      this.$router.push({
        name: this.$route.name,
        query: data
      })
    },
    qMap: function (val) {
      this.map = this.$route.query.map || this.mapDefault
      this.fetchData()
    }
  }
}
</script>

<script>
import options from '../../store/options.js'

var _ = require('lodash')

export default {
  computed: {
    qTier: function () {
      return this.$route.query.tier
    }
  },
  data: function () {
    let tierOptions = options.tiers()
    let tierDefault = _.head(_.keys(tierOptions))
    return {
      tierOptions: tierOptions,
      tierDefault: tierDefault,
      tier: this.$route.query.tier || tierDefault
    }
  },
  watch: {
    tier: function (val) {
      let data = Object.assign({}, this.$route.query)
      data['tier'] = val
      this.$router.replace({
        name: this.$route.name,
        query: data
      })
    },
    qTier: function (val) {
      this.tier = this.$route.query.tier || this.tierDefault
      this.fetchData()
    }
  }
}
</script>

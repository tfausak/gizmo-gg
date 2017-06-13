<script>
import options from '../../store/options.js'

var _ = require('lodash')

export default {
  computed: {
    qTime: function () {
      return this.$route.query.time
    }
  },
  data: function () {
    let timeOptions = options.times()
    let shortTimeOptions = options.shortTimes()
    let timeDefault = _.head(_.keys(timeOptions))
    return {
      timeOptions: timeOptions,
      shortTimeOptions: shortTimeOptions,
      timeDefault: timeDefault,
      time: this.$route.query.time || timeDefault
    }
  },
  watch: {
    time: function (val) {
      let data = Object.assign({}, this.$route.query)
      data['time'] = val
      this.$router.replace({
        name: this.$route.name,
        query: data
      })
    },
    qTime: function (val) {
      this.time = this.$route.query.time || this.timeDefault
      this.fetchData()
    }
  }
}
</script>

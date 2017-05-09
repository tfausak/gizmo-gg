<style scoped lang="scss">
.rankBoxes {
  flex-grow: 1!important;
  justify-content: center!important;
  display: flex;
}
.rankBox {
  background-color: #fff;
  border: 1px solid #ddd;
  border-right: 0;
  width: 100px;
  font-size: 11px;
  letter-spacing: 1px;
  text-align: center;
}
.rankBox:last-child {
  border-right: 1px solid #ddd;
}
.rankPlaylist {
  background-color: #f4f4f4;
  padding: 0.4em 1.5em;
  font-size: 12px;
  font-weight: bold;
}
.rankImage {
  padding: 0.5em;
}
.rankImage img {
  width: 30px;
}
.rankGames {
  padding-bottom: 0.5em;
}
</style>

<template>
  <div class="container">
    <loading-component :loading="loading"></loading-component>
    <div class="rankBoxes">
      <div class="rankBox" v-for="(skill, key) in skills">
        <div class="rankPlaylist">{{ key }}</div>
        <div v-if="skill">
          <div class="rankImage">
            <img :src="'/static/img/tiers/' + skill.tier + '.png'">
          </div>
          <div class="rankDivision">DIV {{ skill.division + 1 }}</div>
          <div class="rankGames">{{ skill.matchesPlayed }} Games</div>
        </div>
        <div v-else>
          <div class="rankImage">
            <i class="fa fa-question"></i>
          </div>
          <div class="rankGames">Unknown</div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import LoadingComponent from '../components/Loading'
import slugger from '../../store/slugger.js'

var _ = require('lodash')

export default {
  beforeMount: function () {
    this.fetchData()
  },
  components: {
    LoadingComponent
  },
  computed: {
    loading: function () {
      return this.GET_PLAYER === null
    }
  },
  data: function () {
    let skills = {
      '1v1': null,
      '2v2': null,
      '3v3': null,
      '3v3 Solo': null
    }
    return {
      GET_PLAYER: null,
      baseSkills: _.clone(skills),
      skills: skills
    }
  },
  methods: {
    compileData: function () {
      var vm = this
      _.each(vm.GET_PLAYER.skills, function (value, key) {
        vm.skills[slugger.slugPlaylist(key)] = value
      })
    },
    fetchData: function () {
      var vm = this
      vm.GET_PLAYER = null
      vm.skills = _.clone(vm.baseSkills)
      vm.$store.dispatch('GET_PLAYER', {
        id: vm.playerId,
        playlist: 'all'
      }).then(function (data) {
        vm.GET_PLAYER = data
        vm.compileData()
      })
    }
  },
  props: [ 'playerId' ]
}
</script>

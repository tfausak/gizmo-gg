<style scoped lang="scss">
@import "~styles/vars.scss";

#player:focus {
  border-color: $grey-light;
}
.text-smaller {
  font-size: 14px!important;
}
.button {
  border: 0;
  border-top-right-radius: 3px!important;
  border-bottom-right-radius: 3px!important;
}
.input {
  min-width: 210px;
  box-sizing: content-box;
  border: 0;
  box-shadow: none;
  font-size: 14px!important;
  height: auto;
}
.search-small {
  .input {
    font-size: 12px!important;
    padding: 0px 15px;
    min-width: 90px;
  }
  .button {
    font-size: 12px;
  }
}
.search-container {
  position: relative;
  display: flex;
  z-index: 9999;
  border-radius: 0!important;
  margin: 1px 3px;
}
.search-history {
  position: absolute;
  top: 100%;
  width: 100%;
  background-color: lighten($primary, 20%);
  z-index: 99999;
}
.search-history-heading {
  background-color: rgba(0, 0, 0, 0.05);
  padding: 0.5em 1em;
  color: rgba(0, 0, 0, 0.4);
  .icon {
    vertical-align: 0px;
  }
}
.search-history-content {
  .search-item a {
    padding: 3px 10px;
    font-size: 12px;
    display: block;
    color: #fff;
    &:hover {
      text-decoration: underline;
    }
  }
}
</style>

<template>
  <form @submit.prevent="submit" role="form" >
    <div class="control has-addons has-addons-centered">
      <div class="search-container" :class="{ 'search-small': small }">
        <input v-model="mysearch" id="player" class="input" :class="{ 'is-small': small, 'is-medium': !small }" type="text" v-focus="focused" @focus="focused = true" @blur="focused = false" @click="toggleRecent" placeholder="Search by in-game name">
        <button type="submit" class="button" :class="{ 'is-small': small, 'is-medium': !small }">
          <span class="icon is-small">
            <i class="fa fa-search"></i>
          </span>
        </button>
        <div class="search-history" v-if="showRecent && anyRecent()">
          <div class="search-history-heading">
            <span class="icon is-small">
              <i class="fa fa-clock-o"></i>
            </span>
            <span>Recent</span>
          </div>
          <div class="search-history-content">
            <ul>
              <li v-for="item in recent" class="search-item">
                <router-link :to="'/player/' + item.id + '/summary'">
                  {{ item.name }}
                </router-link>
              </li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  </form>
</template>

<script>
import { EventBus } from '../../store/event-bus.js'
import { focus } from 'vue-focus'
var _ = require('lodash')

export default {
  props: [ 'search', 'small' ],
  created: function () {
    var vm = this
    EventBus.$on('recent-updated', function () {
      vm.recent = vm.getRecent()
    })
  },
  directives: { focus: focus },
  data: function () {
    return {
      focused: true,
      showDropdown: false,
      mysearch: this.search,
      showRecent: false,
      recent: this.getRecent()
    }
  },
  methods: {
    getRecent: function () {
      let tmp = []
      let cookie = this.$cookie.get('recent')
      if (cookie) {
        tmp = JSON.parse(cookie)
      }
      return tmp
    },

    submit: function () {
      this.$router.push({
        name: 'search',
        query: {
          search: this.mysearch
        }
      })
    },

    toggleRecent: function () {
      var vm = this
      vm.showRecent = !vm.showRecent
      if (vm.showRecent) {
        setTimeout(function () {
          document.addEventListener('click', vm.toggleRecent)
        }, 0)
      } else {
        document.removeEventListener('click', vm.toggleRecent)
      }
    },

    anyRecent: function () {
      return _.size(this.recent)
    },

    focusInput: function () {
      this.focused = true
    }
  }
}
</script>

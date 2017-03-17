<style scoped lang="scss">
@import "~styles/vars.scss";

.dropdown {
  position: absolute;
  top: 100%;
  left: 0;
  background-color: #fff;
  border: 1px solid rgba(0, 0, 0, 0.1);
  z-index: 999;
}
.platform {
  text-align: right;
  min-width: 140px;
}
.angle {
  flex-grow: 2;
  text-align: right!important;
}
.input {
  min-width: 210px;
}
aside > .menu-label {
  padding: 10px 20px;
  margin: 0;
}
aside > ul {
  padding: 0;
}
#player:focus {
  border-color: $grey-light;
}
.text-smaller {
  font-size: 14px!important;
}
.button {
  border: 0;
}
.input {
  font-size: 14px!important;
  padding: 4px 10px 4px 10px;
  box-sizing: content-box;
  border-radius: 0!important;
  border: 0;
  box-shadow: none;
}
.search-container {
  position: relative;
  display: flex;
}
.search-history {
  position: absolute;
  top: 100%;
  width: 100%;
  background-color: lighten($primary, 20%);
  z-index: 999;
}
.search-history-heading {
  z-index: 999;
  background-color: rgba(0, 0, 0, 0.05);
  padding: 0.5em 1em;
  color: rgba(0, 0, 0, 0.4);
  .icon {
    vertical-align: 0px;
  }
}
.search-history-content {
  .search-item a {
    padding: 5px 10px;
    font-size: 12px;
    display: block;
    &:hover {
      text-decoration: underline;
    }
  }
}
</style>

<template>
  <form @submit.prevent="submit" role="form">
    <div class="control has-addons has-addons-centered">
      <div class="search-container">
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
        <span class="platform button is-medium" @click="toggleDropdown">
          <span class="icon is-small">
            <i :class="selected_platform.icon"></i>
          </span>
          <span class="text-smaller">{{ selected_platform.text }}</span>
          <span class="angle icon is-small">
            <i class="fa" :class="{ 'fa-angle-down': show_dropdown, 'fa-angle-right': !show_dropdown }"></i>
          </span>
          <div class="dropdown has-text-left" v-show="show_dropdown">
            <aside class="menu">
              <p class="menu-label">
                Choose Platform
              </p>
              <ul class="menu-list" v-for="p in platforms">
                <li>
                  <a @click="choosePlatform(p)">
                    <span class="icon is-small">
                      <i :class="p.icon"></i>
                    </span>
                    <span>{{ p.text }}</span>
                  </a>
                </li>
              </ul>
            </aside>
          </div>
        </span>
        <input v-model="mysearch" id="player" class="input is-medium" type="text" v-focus="focused" @focus="focused = true" @blur="focused = false" :placeholder="selected_platform.placeholder" @click="toggleRecent">
        <button type="submit" class="button is-medium">
          <span class="icon is-small">
            <i class="fa fa-search"></i>
          </span>
        </button>
      </div>
    </div>
  </form>
</template>

<script>
import platforms from '../../store/platforms.js'
import { focus } from 'vue-focus'
var _ = require('lodash')

export default {
  props: [ 'platform', 'search' ],
  directives: { focus: focus },
  data: function () {
    let recent = []
    let cookie = this.$cookie.get('recent')
    if (cookie) {
      recent = JSON.parse(cookie)
    }
    return {
      focused: true,
      show_dropdown: false,
      selected_platform: platforms[0],
      platforms: platforms,
      mysearch: this.search,
      showRecent: false,
      recent: recent
    }
  },
  beforeMount: function () {
    let result = _.find(this.platforms, [ 'slug', this.platform ])
    if (result) {
      this.selected_platform = result
    }
  },
  methods: {
    submit: function () {
      this.$router.push({
        name: 'search',
        query: {
          platform: this.selected_platform.slug,
          search: this.mysearch
        }
      })
    },

    toggleRecent: function () {
      this.showRecent = !this.showRecent
    },

    anyRecent: function () {
      return _.size(this.recent)
    },

    focusInput: function () {
      this.focused = true
    },

    toggleDropdown: function () {
      var vm = this
      this.show_dropdown = !this.show_dropdown
      if (this.show_dropdown) {
        setTimeout(function () {
          document.addEventListener('click', vm.toggleDropdown)
        }, 0)
      } else {
        document.removeEventListener('click', vm.toggleDropdown)
        this.focused = true
      }
    },

    choosePlatform: function (p) {
      this.selected_platform = p
      this.focused = true
    }
  }
}
</script>

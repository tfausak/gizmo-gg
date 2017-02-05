<style scoped lang="scss">
@import "~styles/vars.scss";

.dropdown {
  position: absolute;
  top: 100%;
  left: 0;
  background-color: #fff;
  border: 1px solid rgba(0, 0, 0, 0.1);
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
  padding: 6px 10px 2px 10px;
  box-sizing: content-box;
  border-radius: 0!important;
  border: 0;
  box-shadow: none;
}
.control-input-container {
  position: relative;
  z-index: 111;
}
.control-placeholder {
  position: absolute;
  top: 10px;
  left: 11px;
  z-index: 999;
  color: #aaa;
  font-size: 14px;
  pointer-events: none;
  transition: top .2s ease-in-out;
  transition: font-size .1s ease-in-out;

  &.mini {
    font-size: 11px;
    top: 0;
    left: 10px;
  }
}
</style>

<template>
  <form @submit.prevent="submit" role="form">
    <p class="control has-addons has-addons-centered">
      <span class="platform button is-medium" @click="toggleDropdown">
        <span class="icon is-small">
          <i :class="platform.icon"></i>
        </span>
        <span class="text-smaller">{{ platform.text }}</span>
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
      <span class="control-input-container">
        <span class="control-placeholder" :class="{ 'mini': search }" @click="focusInput">{{ platform.placeholder }}</span>
        <input v-model="search" id="player" class="input is-medium" type="text" v-focus="focused" @focus="focused = true" @blur="focused = false">
      </span>
      <button type="submit" class="button is-medium">
        <span class="icon is-small">
          <i class="fa fa-search"></i>
        </span>
      </button>
    </p>
  </form>
</template>

<script>
import platforms from '../store/options/platforms.js'
import { focus } from 'vue-focus'

export default {
  directives: { focus: focus },
  data: function () {
    return {
      search: '',
      focused: true,
      show_dropdown: false,
      platform: platforms[0],
      platforms: platforms
    }
  },
  methods: {
    submit: function () {
      this.$router.push({
        name: 'player',
        params: {
          platform: this.platform.slug,
          id: this.search
        }
      })
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
      this.platform = p
      this.focused = true
    }
  }
}
</script>

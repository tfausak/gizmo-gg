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
  min-width: 160px;
}
.angle {
  flex-grow: 2;
  text-align: right!important;
}
.input {
  min-width: 200px;
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
</style>

<template>
  <p class="control has-addons has-addons-centered">
    <span class="platform button" @click="toggleDropdown">
      <span class="icon is-small">
        <i :class="platform.icon"></i>
      </span>
      <span>{{ platform.text }}</span>
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
    <input id="player" class="input" type="text" :placeholder="platform.placeholder" v-focus="focused" @focus="focused = true" @blur="focused = false">
    <span class="button">
      <span class="icon is-small">
        <i class="fa fa-search"></i>
      </span>
    </span>
  </p>
</template>

<script>
import { focus } from 'vue-focus'

export default {
  directives: { focus: focus },
  data: function () {
    var platforms = [
      { icon: 'fa fa-steam', text: 'Steam', placeholder: 'Steam profile URL or ID' },
      { icon: 'icon-xbox', text: 'Xbox', placeholder: 'Xbox gamertag' },
      { icon: 'icon-playstation', text: 'Playstation', placeholder: 'Playstation username' }
    ]
    return {
      focused: true,
      show_dropdown: false,
      platform: platforms[0],
      platforms: platforms
    }
  },
  methods: {
    toggleDropdown: function () {
      var vm = this
      this.$data.show_dropdown = !this.$data.show_dropdown
      if (this.$data.show_dropdown) {
        setTimeout(function () {
          document.addEventListener('click', vm.toggleDropdown)
        }, 0)
      } else {
        document.removeEventListener('click', vm.toggleDropdown)
        this.$data.focused = true
      }
    },
    choosePlatform: function (p) {
      this.$data.platform = p
      this.$data.focused = true
    }
  }
}
</script>

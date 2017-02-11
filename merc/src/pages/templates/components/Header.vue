<style scoped lang="scss">
@import "~styles/vars.scss";

.brand {
  font-weight: bold;
}
#header {
  background-color: $dark;
}
#header a.nav-item {
  color: $white;
}
#header .nav {
  background-color: transparent;
}
.nav-item a.is-tab:hover, a.nav-item.is-tab:hover,
.nav-item a.is-tab.is-active, a.nav-item.is-tab.is-active {
  border-bottom-color: #fff;
}
</style>

<template>
  <div id="header">
    <div class="container">
      <nav class="nav">
        <div class="nav-left">
         <router-link to="/" class="nav-item brand">gizmo.gg</router-link>
        </div>
        <div class="nav-right">
          <router-link to="/stats" class="nav-item is-tab">Statistics</router-link>
          <router-link to="/desktop-app" class="nav-item is-tab">Desktop App</router-link>
          <router-link to="/upload" class="nav-item is-tab">Upload Replay</router-link>
        </div>
      </nav>
    </div>
  </div>
</template>

<style scoped lang="scss">
@import "~styles/vars.scss";

#footer .footer {
  background-color: $white-ish;
}
#footer {
  background-color: $white-ish;
  box-shadow: 0px 500px 0px 500px $white-ish;
}
</style>

<template>
  <div id="footer">
    <footer class="footer">
      <div class="container">
        <div class="content has-text-centered">
          <p>
            &copy; 2017 <strong>gizmo.gg</strong>. Data from Rocket League replays.
          </p>
          <p><router-link to="/about">About</router-link> - <a target="_blank" href="https://discord.gg/nqZHDpy">Contact</a></p>
        </div>
      </div>
    </footer>
  </div>
</template>

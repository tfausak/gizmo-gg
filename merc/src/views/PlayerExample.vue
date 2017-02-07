<style scoped lang="scss">
@import "~styles/vars.scss";

.level-player {
  justify-content: flex-start;
}
.level-player > .level-item {
  flex-grow: 0!important;
}
.level-spacer {
  width: 20px;
}
.normal {
  display: block;
}
.hero-main .hero-body {
  padding-bottom: 0;
}
.hero-main .hero-foot {
  margin-top: 10px;
}
.is-offwhite,
.hero-main li.is-active a {
  background-color: $white-ter!important;
}
</style>

<template>
  <div>
    <section class="hero hero-main is-primary">
      <div class="hero-body">
        <div class="container">
          <div class="level level-player">
            <div class="level-item">
              <figure class="image is-128x128 is-circle-light-128x128">
                <img :src="'/static/img/bodies/octane.png'">
              </figure>
            </div>
            <div class="level-item level-spacer">
            </div>
            <div class="level-item">
              <div class="normal">
                <h1 class="title">
                  <span class="icon" style="margin-top: 8px;">
                    <i class="fa fa-steam"></i>
                  </span>
                  <span>
                    rustixote
                  </span>
                </h1>
                <h2 class="subtitle" style="margin-top: 0;">
                  last updated 29 minutes ago
                </h2>
              </div>
            </div>
          </div>
        </div>
      </div>
      <div class="hero-foot">
          <div class="container">
            <nav class="tabs is-boxed">
              <ul>
                <router-link to="/player-example" tag="li"><a>Summary</a></router-link>
                <router-link to="/player-example/battle-cars" tag="li"><a>Battle-Cars</a></router-link>
                <router-link to="/player-example/maps" tag="li"><a>Maps</a></router-link>
              </ul>
            </nav>
          </div>
        </div>
    </section>
    <section class="section is-offwhite">
      <router-view></router-view>
    </section>
  </div>
</template>

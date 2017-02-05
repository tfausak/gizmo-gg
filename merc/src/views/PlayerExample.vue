<style scoped lang="scss">
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
</style>

<template>
  <section class="hero hero-main">
    <div class="hero-body">
      <div class="container">
        <div class="level level-player">
          <div class="level-item">
            <figure class="image is-128x128 is-circle-dark-128x128">
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
        <nav class="tabs is-boxed">
          <div class="container has-text-left">
            <ul>
              <router-link to="/player-example" tag="li"><a>Summary</a></router-link>
              <router-link to="/player-example/vehicles" tag="li"><a>Vehicles</a></router-link>
              <router-link to="/player-example/maps" tag="li"><a>Maps</a></router-link>
            </ul>
          </div>
        </nav>
      </div>
  </section>
</template>

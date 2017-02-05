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
</style>

<template>
  <section class="hero is-dark">
    <div class="hero-body">
      <div class="container">
        <div class="level level-player">
          <div class="level-item">
            <figure class="image is-128x128 is-outline-128x128">
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
                Superstar div 4
              </h2>
            </div>
          </div>
        </div>
      </div>
    </div>
  </section>
</template>

<style scoped lang="scss">
.tabs {
  justify-content: center;
}
</style>

<template>
  <div>
    <section class="hero is-primary">
      <div class="hero-body">
        <div class="container has-text-centered">
          <h1 class="title">
            Statistics
          </h1>
        </div>
      </div>

      <div class="hero-foot">
        <nav class="tabs is-boxed">
          <div class="container">
            <ul>
              <router-link to="/stats" tag="li" exact><a>Summary</a></router-link>
              <router-link to="/stats/battle-cars" tag="li"><a>Battle-Cars</a></router-link>
              <router-link to="/stats/maps" tag="li"><a>Maps</a></router-link>
            </ul>
          </div>
        </nav>
      </div>
    </section>
    <router-view></router-view>
  </div>
</template>

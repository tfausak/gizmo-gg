<template>
  <div id="stats">
    <section class="hero is-info">
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
              <router-link to="/stats/bodies" tag="li"><a>Bodies</a></router-link>
              <router-link to="/stats/wheels" tag="li"><a>Wheels</a></router-link>
              <router-link to="/stats/toppers" tag="li"><a>Toppers</a></router-link>
              <router-link to="/stats/boosts" tag="li"><a>Boosts</a></router-link>
              <router-link to="/stats/maps" tag="li"><a>Maps</a></router-link>
            </ul>
          </div>
        </nav>
      </div>
    </section>
    <router-view></router-view>
  </div>
</template>

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
        <div class="container">
          <nav class="tabs is-boxed is-centered">
            <ul>
              <router-link :to="{ name: 'stats' }" tag="li"><a>Summary</a></router-link>
              <router-link to="/stats/battle-cars" tag="li"><a>Battle-Cars</a></router-link>
              <router-link to="/stats/maps" tag="li"><a>Maps</a></router-link>
            </ul>
          </nav>
        </div>
      </div>
    </section>
    <router-view></router-view>
  </div>
</template>

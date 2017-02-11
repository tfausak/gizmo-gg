<template>
  <div>
    <section class="hero is-dark">
      <div class="hero-body">
        <div class="container">
          <h1 class="title has-text-centered">
            Dev
          </h1>
        </div>
      </div>
      <div class="hero-foot">
        <nav class="tabs is-boxed">
          <div class="container">
            <ul>
              <router-link to="/dev/players" tag="li" exact><a>Players</a></router-link>
            </ul>
          </div>
        </nav>
      </div>
    </section>
    <router-view></router-view>
  </div>
</template>

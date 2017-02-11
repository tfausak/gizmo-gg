<template>
  <div>
    <header-component></header-component>
    <div>
      <router-view></router-view>
    </div>
    <footer-component></footer-component>
  </div>
</template>

<script>
import HeaderComponent from './components/Header'
import FooterComponent from './components/Footer'

export default {
  components: {
    HeaderComponent,
    FooterComponent
  }
}
</script>

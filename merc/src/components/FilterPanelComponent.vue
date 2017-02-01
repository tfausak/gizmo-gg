<style scoped lang="scss">
.panel-heading {
  .heading {
    margin: 0;
    font-size: 13px;
    color: #000;
  }
}
</style>

<template>
  <nav class="panel">
    <p class="panel-heading">
      <span class="heading">{{ title }}</span>
    </p>
    <a class="panel-block" v-for="option in options" @click="filterWith(option)" :class="{ 'is-active': isSelected(option) }">
      <span class="panel-icon">
        <i class="fa" :class="{ 'fa-circle-thin': !isSelected(option), 'fa-check-circle-o': isSelected(option) }"></i>
      </span>
      {{ option }}
    </a>
  </nav>
</template>

<script>
export default {
  props: [ 'title', 'options' ],
  data: function () {
    return {
      selected: this.options[0]
    }
  },
  methods: {
    filterWith: function (option) {
      this.selected = option
      this.$emit('filterWith', String(this.title), Number(this.selected))
    },
    isSelected: function (option) {
      return this.selected === option
    }
  }
}
</script>

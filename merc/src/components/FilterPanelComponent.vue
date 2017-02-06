<style scoped lang="scss">
.panel-heading {
  padding-top: 5px;
  padding-bottom: 5px;

  .heading {
    margin: 0;
    font-size: 13px;
    color: #000;
  }
}
.panel-block {
  padding-top: 3px;
  padding-bottom: 3px;
}
</style>

<template>
  <div class="panel">
    <p class="panel-heading">
      <span class="heading">{{ title }}</span>
    </p>
    <a class="panel-block" v-for="(value, key) in options" @click="updateValue(key)" :class="{ 'is-active': isSelected(key) }">
      <span class="panel-icon">
        <i class="fa" :class="{ 'fa-circle-thin': !isSelected(key), 'fa-check-circle-o': isSelected(key) }"></i>
      </span>
      {{ value }}
    </a>
  </div>
</template>

<script>
export default {
  props: [ 'title', 'options', 'value' ],
  data: function () {
    return {
      selected: this.value
    }
  },
  methods: {
    updateValue: function (key) {
      this.selected = key
      this.$emit('input', String(key))
    },
    isSelected: function (key) {
      return this.selected === key
    }
  }
}
</script>

<template>
  <div class="panel">
    <p class="panel-heading panel-squish">
      <span class="heading">{{ title }}</span>
    </p>
    <a class="panel-block panel-squish" v-for="(value, key) in options" @click="updateValue(key)" :class="{ 'is-active': isSelected(key) }">
      <span class="panel-icon">
        <i class="fa" :class="{ 'fa-circle-thin': !isSelected(key), 'fa-check-circle-o': isSelected(key) }"></i>
      </span>
      {{ value }}
    </a>
  </div>
</template>

<script>
export default {
  props: [ 'title', 'options', 'sync' ],
  data: function () {
    return {
      selected: this.sync
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
  },
  watch: {
    sync: function (val) {
      this.selected = val
    }
  }
}
</script>

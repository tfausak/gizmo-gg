<style scoped lang="scss">
@import "~styles/vars.scss";

.is-sort {
  font-size: 12px;
  width: 12px;
  height: auto;
  margin-top: 1px;
  margin-bottom: -1px;
}
th {
  cursor: pointer;
}
.is-active {
  color: $primary;
}
</style>

<template>
  <th @click="orderByCol" :class="{ 'is-active': isActive }">
    <div class="level">
      <div class="level-left"><slot></slot></div>
      <div class="level-right">
        <span class="icon is-sort">
          <i class="fa fa-sort" v-if="!isActive"></i>
          <i class="fa fa-sort-desc" v-if="isActive && dir"></i>
          <i class="fa fa-sort-asc" v-if="isActive && !dir"></i>
        </span>
      </div>
    </div>
  </th>
</template>

<script>
export default {
  computed: {
    isActive: function () {
      return this.sort === this.col
    }
  },
  data: function () {
    return {
      dir: 0
    }
  },
  methods: {
    orderByCol: function () {
      if (this.sort !== this.col) {
        this.dir = 0
      }
      this.$emit('orderByCol', String(this.col), Number(this.dir))
      this.dir = !this.dir
    }
  },
  props: [ 'sort', 'col' ]
}
</script>

<template>
  <div>
    <section class="hero is-info">
      <div class="hero-body">
        <div class="container has-text-centered">
          <h1 class="title">
            Upload Replay
          </h1>
        </div>
      </div>
    </section>
    <section class="section">
      <div class="container">
        <ul>
          <li v-for="file in files">{{ file.name }}</li>
        </ul>
        <form @submit.prevent="submit" role="form" enctype="multipart/form-data">
          <input type="file" name="files[]" id="files" @change="onFileChange" multiple>
          <button type="submit" class="button is-primary">Submit</button>
        </form>
      </div>
    </section>
  </div>
</template>

<script>
// var _ = require('lodash')

export default {
  data: function () {
    return {
      pending: [],
      uploading_index: null,
      uploading: null,
      uploaded: [],
      files: []
    }
  },
  methods: {
    submit: function () {
      if (this.pending.length || this.uploading !== null) {
        console.log('submit denied')
        return
      }
      this.pending = this.files
      console.log(this.pending)
      console.log('submit init')
      this.uploadFile()
    },
    uploadFile: function () {
      if (!this.pending.length || this.uploading !== null) {
        console.log('upload denied')
        return
      }
      this.uploading = this.pending.item(this.uploading_index)
      console.log('upload file', this.uploading)
      let formData = new FormData()
      // formData.append('Content-Type', this.uploading || 'application/octet-stream')
      formData.append('replay', this.uploading)
      this.$http.post(process.env.API_URL + 'uploads', formData).then(function (response) {
        console.log('success', response)
        this.uploaded.push({
          file: this.uploading,
          error: null
        })
        this.uploading = null
        this.uploadFile()
      }, function (response) {
        console.log('failure', response)
        this.uploaded.push({
          file: this.uploading,
          error: response
        })
        this.uploading = null
      })
    },
    onFileChange: function (event) {
      this.files = event.target.files || event.dataTransfer.files
    }
  }
}
</script>

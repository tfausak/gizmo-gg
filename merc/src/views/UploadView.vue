<style scoped lang="scss">
@import "~styles/vars.scss";

.selectButton {
  margin: 0 auto;
  border: 3px solid $primary;
  color: $primary;
  font-size: 20px;
  width: 200px;
  height: 200px;
  text-align: center;
  border-radius: 3px;
  padding: 10px;
  cursor: pointer;
  flex-direction: column;
  justify-content: center;
  margin-bottom: 0!important;
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 0;

  .numSelected {
    font-size: 14px;
    color: $grey;
  }

  .level-item {
    flex-grow: 0!important;
  }

  .fa {
    font-size: 40px;
  }
}
.submitButton {
  width: 200px;
  border-top-right-radius: 0;
  border-top-left-radius: 0;
}
</style>

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
        <div class="columns">
          <div class="column">
            <form @submit.prevent="submit" role="form" enctype="multipart/form-data">
              <div class="level selectButton" @click="selectFiles">
                <div class="level-item">
                  <i class="fa fa-cloud-upload"></i>
                </div>
                <div class="level-item">
                  Select Replays
                </div>
                <div class="level-item numSelected">
                  <span v-if="anySelected()">{{ this.files.length }} selected</span>
                </div>
              </div>
              <div class="has-text-centered">
                <button type="submit" class="button submitButton" :class="{ 'is-primary': !hasPending() && anySelected() }" :disabled="hasPending() || !anySelected()">Start Upload</button>
              </div>
              <input type="file" name="files[]" id="files" @change="onFileChange" multiple class="is-hidden">
            </form>
          </div>

          <div class="column">
            <nav class="panel">
              <p class="panel-heading">
                Uploads
              </p>
              <a class="panel-block" v-if="hasPending()">
                <span class="panel-icon">
                  <i class="fa fa-clock-o"></i>
                </span>
                {{ pending.length }} pending
              </a>
              <a class="panel-block is-active" v-if="uploading">
                <span class="panel-icon">
                  <i class="fa fa-circle-o-notch fa-spin fa-fw"></i>
                </span>
                Uploading {{ uploading.name }}..
              </a>
              <a class="panel-block" v-for="upload in uploaded">
                <span class="panel-icon">
                  <i class="fa fa-file"></i>
                </span>
                {{ upload.file.name }}
              </a>
            </nav>
          </div>
        </div>
      </div>
    </section>
  </div>
</template>

<script>
// var _ = require('lodash')

export default {
  data: function () {
    return {
      pending: [], // array of File, copied from input when form is submitted
      uploading: null, // the file we are currently uploading
      uploaded: [], // all the files we have uploaded in an array of [{file: File, response: Response}, ..]
      files: null // FileList, from the input on the page
    }
  },
  methods: {
    anySelected: function () {
      return this.files !== null && this.files.length
    },

    hasPending: function () {
      return this.pending.length
    },

    selectFiles: function () {
      let elem = document.getElementById('files')
      elem.click()
    },

    submit: function () {
      // Don't allow a submit if we have pending uploads
      if (this.hasPending() || this.uploading !== null) {
        console.log('submit denied: pending uploads')
        return
      }
      // Copy list of files to upload from the input
      this.pending = []
      for (let i = 0; i < this.files.length; i++) {
        this.pending.push(this.files.item(i))
      }
      // Reset the input
      let elem = document.getElementById('files')
      elem.value = elem.defaultValue
      console.log('submitting..')
      this.uploadFile(0)
    },

    uploadFile: function () {
      // Don't allow a file upload if we have a current upload
      if (this.uploading !== null) {
        console.log('upload denied: still uploading')
        return
      }
      // When there is nothing left to upload
      if (!this.hasPending()) {
        console.log('upload denied: no pending')
        return
      }
      this.uploading = this.pending.shift()
      console.log('uploading file..', this.uploading)
      let formData = new FormData()
      formData.append('replay', this.uploading)
      var vm = this
      let handler = function (response) {
        console.log('handle', response)
        vm.uploaded.push({
          file: vm.uploading,
          response: response
        })
        vm.uploading = null
        vm.uploadFile()
      }
      this.$http.post(process.env.API_URL + 'uploads', formData).then(function (response) {
        setTimeout(function () { handler(response) }, 2000)
      }, function (response) {
        setTimeout(function () { handler(response) }, 2000)
      })
    },

    onFileChange: function (event) {
      // Just copy the files from the input to this component's data
      this.files = event.target.files || event.dataTransfer.files
    }
  }
}
</script>

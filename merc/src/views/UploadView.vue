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
.selectButton.is-disabled {
  color: $grey;
}
.submitButton {
  width: 200px;
  border-top-right-radius: 0;
  border-top-left-radius: 0;
}

.panel-block {
  border-left-width: 3px;
}
.response-fail {
  border-left: 3px solid $danger;
  .fa {
    color: $danger;
  }
}
.response-ok {
  border-left: 3px solid $success;
  .fa {
    color: $success;
  }
}
.level-columns {
  flex-direction: column;
  align-items: flex-start;
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
              <div class="level selectButton" @click="selectFiles" v-if="!isUploading()">
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
              <div class="level selectButton is-disabled" v-if="isUploading()">
                <div class="level-item">
                  <i class="fa fa-circle-o-notch fa-spin"></i>
                </div>
                <div class="level-item">
                  Uploading..
                </div>
              </div>
              <div class="has-text-centered">
                <button type="submit" class="button submitButton" :class="{ 'is-primary': !isUploading() && anySelected() }" :disabled="isUploading() || !anySelected()">Start Upload</button>
              </div>
              <input type="file" name="files[]" id="files" @change="onFileChange" multiple class="is-hidden">
            </form>
          </div>

          <div class="column">
            <nav class="panel">
              <p class="panel-heading">
                Uploads
              </p>
              <div class="panel-block" v-if="!uploaded.length && !isUploading()">
                Select replays to upload
              </div>
              <div class="panel-block" v-if="hasPending()">
                <span class="panel-icon">
                  <i class="fa fa-clock-o"></i>
                </span>
                {{ pending.length }} waiting
              </div>
              <div class="panel-block is-active" v-if="uploading">
                <span class="panel-icon">
                  <i class="fa fa-circle-o-notch fa-spin fa-fw"></i>
                </span>
                <div class="level level-columns">
                  <div class="level-item">{{ uploading.name }}</div>
                  <div class="level-item">Uploading..</div>
                </div>
              </div>
              <div class="panel-block" v-for="upload in uploaded" :class="{ 'response-ok': upload.response.ok, 'response-fail': !upload.response.ok}">
                <span class="panel-icon">
                  <i class="is-success fa fa-check-circle-o" v-if="upload.response.ok"></i>
                  <i class="is-danger fa fa-times-circle-o" v-if="!upload.response.ok"></i>
                </span>
                <div class="level level-columns">
                  <div class="level-item">{{ upload.file.name }}</div>
                  <div class="level-item">{{ upload.response.statusText }}</div>
                </div>
              </div>
            </nav>
          </div>
        </div>
      </div>
    </section>
  </div>
</template>

<script>
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
    isUploading: function () {
      return this.hasPending() || this.uploading
    },

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
      this.uploadFile(0)
    },

    uploadFile: function () {
      // Don't allow a file upload if we have a current upload
      if (this.uploading !== null) {
        return
      }
      // When there is nothing left to upload
      if (!this.hasPending()) {
        return
      }
      this.uploading = this.pending.shift()
      let formData = new FormData()
      formData.append('replay', this.uploading)
      var vm = this
      let handler = function (response) {
        // console.log('handler', response)
        vm.uploaded.unshift({
          file: vm.uploading,
          response: response
        })
        vm.uploading = null
        vm.uploadFile()
      }
      this.$http.post(process.env.API_URL + 'uploads', formData).then(function (response) {
        handler(response)
      }, function (response) {
        handler(response)
      })
    },

    onFileChange: function (event) {
      // Just copy the files from the input to this component's data
      this.files = event.target.files || event.dataTransfer.files
    }
  }
}
</script>

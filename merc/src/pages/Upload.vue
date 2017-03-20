<style scoped lang="scss">
@import "~styles/vars.scss";

$upload_width: 300px;

.uploadBox {
  margin: 0 auto;
  border: 3px solid $primary;
  color: $primary;
  font-size: 20px;
  width: $upload_width;
  height: 150px;
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
  &.is-disabled {
    color: $grey;
    border-color: $grey;
  }
}

#uploadResultsPanel .panel-block {
  border-left-width: 3px;
}
.upload-failed {
  border-left: 3px solid $danger;
  .fa {
    color: $danger;
  }
}
.upload-processing {
  border-left: 3px solid $solarized_yellow;
  .fa {
    color: $solarized_yellow;
  }
}
.upload-success {
  border-left: 3px solid $solarized_green;
  .fa {
    color: $solarized_green;
  }
}
#startUploadButton {
  margin-top: 1em;
  width: $upload_width;
  border-top-right-radius: 0;
  border-top-left-radius: 0;
}
</style>

<template>
  <div id="upload-view">

    <section class="hero is-primary">
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

              <div class="level uploadBox" @click="selectFiles" v-if="!isUploading()">
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

              <div class="level uploadBox is-disabled" v-if="isUploading()">
                <div class="level-item">
                  <i class="fa fa-circle-o-notch fa-spin"></i>
                </div>
                <div class="level-item">
                  Uploading..
                </div>
              </div>

              <div class="has-text-centered">
                <button type="submit" class="button is-medium" id="startUploadButton" :class="{ 'is-success': !isUploading() && anySelected() }" :disabled="isUploading() || !anySelected()">
                  Start Upload
                </button>
              </div>

              <input type="file" name="files[]" id="files" @change="onFileChange" multiple class="is-hidden">
            </form>
          </div>

          <div class="column">
            <div class="panel" id="uploadResultsPanel">
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
                <div class="level level-stacked">
                  <div class="level-item">{{ uploading.name }}</div>
                  <div class="level-item">Uploading..</div>
                </div>
              </div>
              <div class="panel-block" v-for="upload in uploaded" :class="'upload-' + upload.status">
                <span class="panel-icon">
                  <i class="is-success fa fa-cog fa-spin" v-if="upload.status === 'processing'"></i>
                  <i class="is-danger fa fa-times-circle-o" v-if="upload.status === 'failed'"></i>
                  <i class="is-danger fa fa-check-circle-o" v-if="upload.status === 'success'"></i>
                </span>
                <div class="level level-stacked">
                  <div class="level-item" v-if="upload.gameId">
                    <router-link :to="'/game/' + upload.gameId">
                      {{ upload.fileName }}
                    </router-link>
                  </div>
                  <div class="level-item" v-else>
                    {{ upload.fileName }}
                  </div>
                  <div class="level-item">{{ upload.status }}</div>
                </div>
              </div>
            </div>
          </div>

        </div>
      </div>
    </section>

  </div>
</template>

<script>
import { getRaw, getEndpointUrl } from '../store/api'
var _ = require('lodash')

export default {
  created: function () {
    var vm = this
    vm.pollUploaded = setInterval(function () {
      vm.poll()
    }, 2500)
  },
  data: function () {
    let uploaded = []
    let cookie = this.$cookie.get('uploaded')
    if (cookie) {
      uploaded = JSON.parse(cookie)
    }
    return {
      pollUploaded: null,
      pending: [], // array of File, copied from input when form is submitted
      uploading: null, // the file we are currently uploading
      uploaded: uploaded, // all the files we have uploaded in an array of [{file: File, response: Response}, ..]
      files: null // FileList, from the input on the page
    }
  },
  destroyed: function () {
    if (this.pollUploaded) {
      clearInterval(this.pollUploaded)
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

    poll: function () {
      let vm = this
      _.each(vm.uploaded, function (upload) {
        if (upload.url && upload.status === 'processing') {
          getRaw(upload.url)
            .then(function (data) {
              let commit = false
              if (data) {
                if (data.state === 'success') {
                  let m = data.game.match(/games\/(\d+)/)
                  if (m && m.length > 1) {
                    upload.gameId = m[1]
                    upload.status = 'success'
                    commit = true
                  }
                } else if (data.state === 'failure') {
                  upload.status = 'processing-failure'
                  commit = true
                }
              }
              if (commit) {
                vm.$cookie.set('uploaded', JSON.stringify(vm.uploaded))
              }
              return data
            })
        }
      })
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
        vm.uploaded.unshift({
          'gameId': null,
          'url': response.request.responseURL,
          'status': response.ok ? 'processing' : 'failed',
          'fileName': vm.uploading.name
        })
        vm.uploaded = _.slice(vm.uploaded, 0, 100)
        vm.$cookie.set('uploaded', JSON.stringify(vm.uploaded))
        vm.uploading = null
        vm.uploadFile()
      }
      this.$http.post(getEndpointUrl('uploads'), formData)
        .then(function (response) {
          response.ok = true
          handler(response)
        })
        .catch(function (error) {
          error.response.ok = false
          handler(error.response)
        })
    },

    onFileChange: function (event) {
      // Just copy the files from the input to this component's data
      this.files = event.target.files || event.dataTransfer.files
    }
  }
}
</script>

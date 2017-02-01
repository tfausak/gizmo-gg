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
        <files-uploader name="replays" multiple :action="action"></files-uploader>
      </div>
    </section>
  </div>
</template>

<script>
import FilesUploaderComponent from '../components/FilesUploaderComponent.vue'

export default {
  data: function () {
    return {
      // TODO: This doesn't appear to actually work.
      action: `#{process.env.API_URL}uploads`
    }
  },
  components: {
    filesUploader: FilesUploaderComponent
  }
}
</script>

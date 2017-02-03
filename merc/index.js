const express = require('express')
const morgan = require('morgan')
const serveStatic = require('serve-static')

const app = express()

app.use(morgan('combined'))
app.use(serveStatic('dist'))

app.use(/.*/, (_, res) => res.sendFile('index.html', { root: 'dist' }))

app.listen(8081)

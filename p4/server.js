#!/usr/bin/env node

const express = require('express')
const cgi = require('cgi')
const path = require('path')

const EXEC_DIR = path.join(__dirname, '.stack-work', 'dist', 'x86_64-linux', 'Cabal-2.4.0.1', 'build')
const getCGI = (m, x) => cgi(path.join(EXEC_DIR, x, x), { mountPoint: m })

const app = express()
  .use(getCGI('/forum-backend.cgi/', 'forum-backend-cgi'))
  .use(express.static(path.join(__dirname, 'src', 'frontend')))
  .listen(8080)

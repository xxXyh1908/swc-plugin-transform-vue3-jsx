const { spawn } = require('child_process')
const { copyFile } = require('fs/promises')
const path = require('path')

const proc = spawn('cargo', ['build-wasi', '--release'], {
  cwd: path.join(__dirname, '..'),
  stdio: 'inherit'
})

proc.on('exit', (code, signal) => {
  if (!code && !signal) {
    copyFile(
      path.join(__dirname, '../target/wasm32-wasi/release/swc_plugin_transform_vue3_jsx.wasm'),
      path.join(__dirname, '../swc_plugin_transform_vue3_jsx.wasm')
    )
  }
})

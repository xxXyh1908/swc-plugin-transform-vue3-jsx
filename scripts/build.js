const { spawn } = require('child_process')
const { copyFile, mkdir } = require('fs/promises')
const path = require('path')

const proc = spawn('cargo', ['build-wasi', '--release'], {
  cwd: path.join(__dirname, '..'),
  stdio: 'inherit'
})

proc.on('exit', async (code, signal) => {
  if (!code && !signal) {
    await mkdir(path.join(__dirname, '../wasm'), { recursive: true })
    await copyFile(
      path.join(__dirname, '../target/wasm32-wasi/release/swc_plugin_transform_vue3_jsx.wasm'),
      path.join(__dirname, '../wasm/swc_plugin_transform_vue3_jsx.wasm')
    )
  }
})

import vue from '@vitejs/plugin-vue'
import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
  build: { target: 'es2020' },
  plugins: [vue()]
})

<script setup lang="ts">
import { Buffer } from 'buffer'
import { NButton, NForm, NFormItem, NLayout, NLayoutContent, NLayoutHeader, NPopover, NSpace, NSwitch } from 'naive-ui'
import { deflate, inflate } from 'pako'
import { Pane, Splitpanes } from 'splitpanes'
import 'splitpanes/dist/splitpanes.css'
import { onMounted, reactive, ref, watch } from 'vue'
import * as pkg from '../../package.json'
import Editor from './components/Editor.vue'
import { transform as babelTransform } from './transform/babel'
import { transform as swcTransform } from './transform/swc'

const babelTransformContent = ref('')
const swcTransformContent = ref('')
const babelTransformTime = ref(0)
const swcTransformTime = ref(0)

let localStorageObj: any
let hashParamsObj: any
try {
  localStorageObj = JSON.parse(localStorage.getItem('data') ?? 'null')
} catch (error) {}

try {
  let localHash = location.hash
  if (localHash.startsWith('#')) {
    localHash = localHash.slice(1)
  }
  if (localHash) {
    hashParamsObj = JSON.parse(inflate(Buffer.from(localHash, 'hex'), { to: 'string' }))
  }
} catch (error) {}

const { code: hash_code, ...hash_options } = hashParamsObj ?? localStorageObj ?? {}

const options = reactive<Record<string, boolean>>({
  hmr: false,
  mergeProps: true,
  enableObjectSlots: true,
  optimize: false,
  reactStyle: false,
  transformOn: false,
  transformSlot: false,
  transformVSlot: false,
  transformOnUpdateEvent: false,
  vOn: true,
  vModel: true,
  ...hash_options
})

const currentValue = ref<string>(hash_code ?? defaultCode)

let key = 0

const handleCompiler = debounce((value: string) => {
  currentValue.value = value
  if (!value) return

  const currentKey = ++key

  const stringifyData = JSON.stringify({ ...options, code: value })

  location.hash = Buffer.from(deflate(stringifyData)).toString('hex')
  if (!hashParamsObj) {
    localStorage.setItem('data', stringifyData)
  }

  const restOptions = { ...options }

  babelTransform(value, 'example.tsx', restOptions).then(({ code, time }) => {
    if (key === currentKey) {
      babelTransformContent.value = code
      babelTransformTime.value = time
    }
  })

  swcTransform(value, 'example.tsx', restOptions).then(({ code, time }) => {
    if (key === currentKey) {
      swcTransformContent.value = code
      swcTransformTime.value = time
    }
  })
})

watch(options, () => handleCompiler(currentValue.value), { deep: true })

onMounted(() => {
  if (currentValue.value) {
    handleCompiler(currentValue.value)
  }
})

const openGithub = () => void window.open(pkg.repository.url)
</script>

<template>
  <NLayout
    class="layout"
    :content-style="{
      display: 'flex',
      flexDirection: 'column',
      alignItems: 'stretch'
    }"
  >
    <NLayoutHeader>
      <NSpace align="center">
        <h1>swc-plugin-transform-vue3-jsx</h1>
        <NPopover trigger="click" overlap>
          <template #trigger><NButton quaternary>options</NButton></template>
          <NForm label-placement="left" :show-feedback="false" label-width="auto" label-align="left">
            <NFormItem v-for="key in optionsKeys" :label="key">
              <NSwitch v-model:value="options[key]" />
            </NFormItem>
          </NForm>
        </NPopover>
        <NButton quaternary @click="openGithub">
          <template #icon>
            <svg
              height="32"
              aria-hidden="true"
              viewBox="0 0 16 16"
              version="1.1"
              width="32"
              data-view-component="true"
              class="octicon octicon-mark-github v-align-middle"
            >
              <path
                fill-rule="evenodd"
                d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"
              ></path>
            </svg>
          </template>
        </NButton>
      </NSpace>
    </NLayoutHeader>
    <NLayoutContent class="layout-content">
      <Splitpanes class="default-theme">
        <Pane>
          <div class="flex-column">
            <div>Your Code</div>
            <Editor :default-value="currentValue" language="typescript" @change="handleCompiler" />
          </div>
        </Pane>
        <Pane>
          <div class="flex-column">
            <div>
              swc-plugin-transform-vue3-jsx
              <span v-if="swcTransformTime">({{ (swcTransformTime / 1000).toFixed(3) }}ms)</span>
            </div>
            <Editor language="typescript" readonly :value="swcTransformContent" />
          </div>
        </Pane>
        <Pane>
          <div class="flex-column">
            <div>
              @vue/babel-plugin-jsx
              <span v-if="babelTransformTime">({{ (babelTransformTime / 1000).toFixed(3) }}ms)</span>
            </div>
            <Editor language="typescript" readonly :value="babelTransformContent" />
          </div>
        </Pane>
      </Splitpanes>
    </NLayoutContent>
  </NLayout>
</template>

<style scoped>
h1 {
  padding: 0 20px;
  line-height: 1.25em;
  font-size: 20px;
}

.layout {
  height: 100%;
}

.layout-content {
  flex: 1 1;
}

.flex-column {
  text-align: center;
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
  flex-wrap: nowrap;
  align-items: stretch;
}
</style>

<script lang="ts">
function debounce<T extends (...args: any[]) => any>(fn: T, delay = 250): T {
  let prevTimer: any = null
  let currentDelay = delay
  return ((...args: any[]) => {
    currentDelay = Math.min(delay * 3, currentDelay * 1.25)
    if (prevTimer) {
      clearTimeout(prevTimer)
    }
    prevTimer = setTimeout(() => {
      currentDelay = delay
      fn(...args)
      prevTimer = null
    }, currentDelay)
  }) as any
}

const optionsKeys = [
  'hmr',
  'mergeProps',
  'enableObjectSlots',
  'optimize',
  'reactStyle',
  'transformOn',
  'transformSlot',
  'transformVSlot',
  'transformOnUpdateEvent',
  'vOn',
  'vModel'
]

const defaultCode = `/* base */
const vnode1 = <div class="class_1" dyn={dyn}>content</div>

/* v-html */
const vnode2 = <div v-html={html}></div>

/* scope component */
import Comp from './comp.vue'
const vnode3 = <Comp />

/* global component */
const vnode4 = <GlobalComp />

/* scope directive */
import vDirective from './v-directive'
const vnode5 = <div v-directive:args_modifier={dir_exp}></div>

/* global directive */
const vnode6 = <div v-global-directive:args_modifier={dir_exp}></div>

/* v-model */
const vnode7 = <input v-model={model} />
const vnode8 = <Input v-model:value={model} />

/* slots */
const vnode9 = <Comp>{enableObjectSlotsTest}</Comp>
const vnode10 = <Comp v-slots={{...slots}}>{{...slots1}}</Comp>

/* event */
const vnode11 = <input onInput={handleInput} />

/* options mergeProps */
const vnode12 = <div class="class_1" onClick={handleClick1} class="class_2" onClick={handleClick2}></div>

/* options transformOn */
const vnode13 = <div on={{click: handleClick}}></div>

/* options reactStyle */
const vnode14 = <label className="class_4" htmlFor="input" />

/* options transformOnUpdateEvent */
const vnode15 = <NInput onUpdateValue={$event => (value = $event)} />

/* options transformSlot */
const vnode16 = defineComponent({
  render() {
    return <Comp><slot name="slot" slot-data={data}>default</slot></Comp>
  }
})

/* options transformVSlot */
const vnode17 = defineComponent({
  render() {
    return <Comp v-slot:def={render}></Comp>
  }
})


/* options hmr */
export const HMRComponent = defineComponent({/* options */})
`
</script>

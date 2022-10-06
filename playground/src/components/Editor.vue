<template>
  <div class="container" ref="container"></div>
</template>
<style scoped>
.container {
  width: 100%;
  height: 100%;
  flex: 1 1;
  text-align: left;
}
</style>
<script setup lang="ts">
import { useResizeObserver } from '@vueuse/core'
import * as monaco from 'monaco-editor'
import { ref, watch } from 'vue'

const props = defineProps<{
  defaultValue?: string
  value?: string
  readonly?: boolean
  language?: 'javascript' | 'typescript' | 'html' | 'css'
  options?: monaco.editor.IEditorOptions
}>()
const emit = defineEmits<{ (event: 'change', value: string): void }>()
const container = ref<HTMLElement>()

let editor: monaco.editor.IStandaloneCodeEditor | undefined

watch(container, el => {
  if (el) {
    editor = monaco.editor.create(el, {
      ...sharedEditorOptions,
      ...props.options,
      value: props.value ?? props.defaultValue,
      readOnly: props.readonly,
      language: props.language
    })

    editor.onDidChangeModelContent(() => emit('change', editor?.getValue() ?? ''))
  } else {
    editor?.dispose()
  }
})

watch(
  () => [props.options, props.readonly] as const,
  ([options, readonly]) => {
    editor?.updateOptions({
      ...options,
      readOnly: readonly
    })
  },
  { deep: true }
)

watch(
  () => props.value,
  value => {
    editor?.setValue(value ?? '')
  }
)

useResizeObserver(container, () => {
  editor?.layout()
})
</script>
<script lang="ts">
import editorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker'
import cssWorker from 'monaco-editor/esm/vs/language/css/css.worker?worker'
import htmlWorker from 'monaco-editor/esm/vs/language/html/html.worker?worker'
import jsonWorker from 'monaco-editor/esm/vs/language/json/json.worker?worker'
import tsWorker from 'monaco-editor/esm/vs/language/typescript/ts.worker?worker'

const sharedEditorOptions: monaco.editor.IStandaloneEditorConstructionOptions = {
  tabSize: 2,
  fontSize: 14,
  wordWrap: 'on',
  renderWhitespace: 'selection'
}

window.MonacoEnvironment = {
  getWorker(_, label) {
    switch (label) {
      case 'json':
        return new jsonWorker()
      case 'css':
      case 'scss':
      case 'less':
        return new cssWorker()
      case 'html':
      case 'handlebars':
      case 'razor':
      case 'vue':
        return new htmlWorker()
      case 'typescript':
      case 'javascript':
      case 'js':
      case 'jsx':
      case 'ts':
      case 'tsx':
        return new tsWorker()
      default:
        return new editorWorker()
    }
  }
}

monaco.languages.typescript.typescriptDefaults.setCompilerOptions({
  allowJs: true,
  allowNonTsExtensions: true,
  jsx: monaco.languages.typescript.JsxEmit.Preserve,
  target: monaco.languages.typescript.ScriptTarget.Latest
})
</script>

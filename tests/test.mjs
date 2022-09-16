const code = `
import vRipple from 'v-ripple'

export const node = <div></div>

// comment
export const FormTest = defineComponent({
  render() {
    return <>
      <input v-model={value} >{/* comment */}</input>
      <input type="checkbox" v-model={[value]} />
      <input type={type} v-model={[value, "modelValue", ["lazy"]]} />
      <select v-model_number={[value, ["modifier", "lazy"]]} >{{ default: () => <div>A</div>, bar: () => <span>B</span> }}{}</select>
      <Input v-directive:arg_mod={dir} v-model={value1} v-models_modifier={[value1, [value2, "value"]]} ></Input>
      <Input v-html={html} v-text={text}>input</Input>
    </>
  },
})

export const Layout = defineComponent({
  render() {
    return <R v-slots={{render: () => []}} v-slots={{item: () => []}} icon={<i></i>}>
      <FormTest {...props1} class="class" {...props2} on={{click: () => {}, updateEvent: (e) => e}} />
    </R>
  }
})

export default defineComponent({
  render() {
    return <NButton v-slots={{icon: () => <Icon/>}} >{{ default: () => 'Star Kirby' }}</NButton>
  },
})
`

console.clear()

import { transform } from '@swc/core'
import { createRequire } from 'module'

transform(code, {
  isModule: true,
  sourceMaps: false,
  minify: false,
  filename: 'input.tsx',
  module: { type: 'es6' },
  sourceMaps: true,
  jsc: {
    loose: false,
    externalHelpers: true,
    keepClassNames: true,
    target: 'es2022',
    preserveAllComments: true,
    parser: {
      syntax: 'typescript',
      tsx: true,
      dynamicImport: true,
      decorators: true
    },
    experimental: {
      plugins: [
        [
          createRequire(import.meta.url).resolve('..'),
          {
            hmr: true,
            enable_object_slots: true,
            transformOn: true,
            optimize: true,
          }
        ]
      ]
    }
  }
}).then(({ code }) => {
  console.log(code)
})

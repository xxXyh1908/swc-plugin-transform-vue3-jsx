[npm]: https://img.shields.io/npm/v/swc-plugin-transform-vue3-jsx
[npm-url]: https://www.npmjs.com/package/swc-plugin-transform-vue3-jsx
[size]: https://packagephobia.now.sh/badge?p=swc-plugin-transform-vue3-jsx
[size-url]: https://packagephobia.now.sh/result?p=swc-plugin-transform-vue3-jsx

# swc-plugin-transform-vue3-jsx

[![npm][npm]][npm-url]
[![size][size]][size-url]

💡SWC plugin for faster conversion `vue3-jsx`.

## Installation
#### npm
```bash
npm install swc-plugin-transform-vue3-jsx -D
```

#### yarn
```bash
yarn add swc-plugin-transform-vue3-jsx -D
```

## Usage

.swcrc
```json
{
  "jsc": {
    "parser": {
      "syntax": "typescript",
      "tsx": true
    }
  },
  "experimental": {
    "plugins": [["swc-plugin-transform-vue3-jsx", {  }]]
    }
  }
}
```

## VS [@vue/babel-plugin-jsx](https://www.npmjs.com/package/@vue/babel-plugin-jsx)
1. ✅ New option `reactStyle`: Convert `react-jsx` syntax into `vue3-jsx` equivalent conversion product, which is convenient for developers to quickly convert `react-projects` into `vue-projects`.
2. ✅ New Option `transformOnUpdateEvent`: To convert any property that looks like `onUpdateXxx` to an `onUpdate:xxx` property (which is not a legal prop-name due to JSX's own rules), often used for `naive-UI`.
3. ✅ New Option `hmr`: Generate the HMR code.
4. 🤥 Option `isCustomElement` renamed to `customElement`, and only string arrays are supported(`SWC` only supports json options).
5. ⚒️ More radical optimization algorithm.
6. ⚒️ Fixed some bugs for `@vue/babel-plugin-jsx`.

## Usage

### options

#### include

Type: `string[]`

Default: `[/.*/.toString()]`

A minimatch pattern, or array of patterns, which specifies the files in the build the plugin should operate on.

#### exclude

Type: `string[]`

Default: `undefined`

A minimatch pattern, or array of patterns, which specifies the files in the build the plugin should ignore.

#### transformOn

Type: `boolean`

Default: `false`

transform `on: { click: xx }` to `onClick: xxx`

#### optimize

Type: `boolean`

Default: `false`

enable optimization or not. It's not recommended to enable it If you are not familiar with Vue 3.

#### customElement

Type: `string[]`

Default: `undefined`

configuring custom elements.

.e.g
```js
['my-custom-element', /^my-[a-z0-9-]+$/.toString()]
```

#### mergeProps

Type: `boolean`

Default: `true`

merge static and dynamic class / style attributes / onXXX handlers

#### enableObjectSlots

Type: `boolean`

Default: `true`

Whether to enable `object slots` (mentioned below the document) syntax". It might be useful in JSX, but it will add a lot of `toObjectSlots` call expressions which increase your bundle size. And `v-slots` is still available even if `enableObjectSlots` is turned off.

#### pragma

Type: `string`

Default: `createVNode`

Replace the function used when compiling JSX expressions.

#### reactStyle (New)

Type: `boolean`

Default: `false`

Convert `react-jsx` syntax into `vue3-jsx` equivalent conversion product, which is convenient for developers to quickly convert `react-projects` into `vue-projects`.

.e.g `<div className="class-1" dangerouslySetInnerHTML={html} />` => `<div class="class-1" v-html={html} />`

.e.g `<label htmlFor="xxx">...</label>` => `<label for="xxx">...</label>`

#### transformOnUpdateEvent (New)

Type: `boolean`

Default: `false`

To convert any property that looks like `onUpdateXxx` to an `onUpdate:xxx` property (which is not a legal prop-name due to JSX's own rules), often used for `naive-UI`.

.e.g `<NInput onUpdateValue={onUpdate} />` => `<NInput onUpdate:value={onUpdate} />`

#### hmr (New)

Type: `boolean`

Default: `false`

Generate the HMR code.

## Syntax

### Content

functional component

```jsx
const App = () => <div>Vue 3.0</div>;
```

with render

```jsx
const App = {
  render() {
    return <div>Vue 3.0</div>;
  },
};
```

```jsx
import { withModifiers, defineComponent } from "vue";

const App = defineComponent({
  setup() {
    const count = ref(0);

    const inc = () => {
      count.value++;
    };

    return () => (
      <div onClick={withModifiers(inc, ["self"])}>{count.value}</div>
    );
  },
});
```

Fragment

```jsx
const App = () => (
  <>
    <span>I'm</span>
    <span>Fragment</span>
  </>
);
```

### Attributes / Props

```jsx
const App = () => <input type="email" />;
```

with a dynamic binding:

```jsx
const placeholderText = "email";
const App = () => <input type="email" placeholder={placeholderText} />;
```

### Directives

#### v-show

```jsx
const App = {
  data() {
    return { visible: true };
  },
  render() {
    return <input v-show={this.visible} />;
  },
};
```

#### v-model

> Note: You should pass the second param as string for using `arg`.

```jsx
<input v-model={val} />
```

```jsx
<input v-model:argument={val} />
```

```jsx
<input v-model={[val, ["modifier"]]} />
```

```jsx
<A v-model={[val, "argument", ["modifier"]]} />
```

Will compile to:

```js
createVNode(A, {
  argument: val,
  argumentModifiers: {
    modifier: true,
  },
  "onUpdate:argument": ($event) => (val = $event),
});
```

#### v-models (Not recommended since v1.1.0)

> Note: You should pass a Two-dimensional Arrays to v-models.

```jsx
<A v-models={[[foo], [bar, "bar"]]} />
```

```jsx
<A
  v-models={[
    [foo, "foo"],
    [bar, "bar"],
  ]}
/>
```

```jsx
<A
  v-models={[
    [foo, ["modifier"]],
    [bar, "bar", ["modifier"]],
  ]}
/>
```

Will compile to:

```js
createVNode(A, {
  modelValue: foo,
  modelValueModifiers: {
    modifier: true,
  },
  "onUpdate:modelValue": ($event) => (foo = $event),
  bar: bar,
  barModifiers: {
    modifier: true,
  },
  "onUpdate:bar": ($event) => (bar = $event),
});
```

#### custom directive

Recommended when using string arguments

```jsx
const App = {
  directives: { custom: customDirective },
  setup() {
    return () => <a v-custom:arg={val} />;
  },
};
```

```jsx
const App = {
  directives: { custom: customDirective },
  setup() {
    return () => <a v-custom={[val, "arg", ["a", "b"]]} />;
  },
};
```

### Slot

> Note: In `jsx`, _`v-slot`_ should be replace with **`v-slots`**

```jsx
const A = (props, { slots }) => (
  <>
    <h1>{ slots.default ? slots.default() : 'foo' }</h1>
    <h2>{ slots.bar?.() }</h2>
  </>
);

const App = {
  setup() {
    const slots = {
      bar: () => <span>B</span>,
    };
    return () => (
      <A v-slots={slots}>
        <div>A</div>
      </A>
    );
  },
};

// or

const App = {
  setup() {
    const slots = {
      default: () => <div>A</div>,
      bar: () => <span>B</span>,
    };
    return () => <A v-slots={slots} />;
  },
};

// or you can use object slots when `enableObjectSlots` is not false.
const App = {
  setup() {
    return () => (
      <>
        <A>
          {{
            default: () => <div>A</div>,
            bar: () => <span>B</span>,
          }}
        </A>
        <B>{() => "foo"}</B>
      </>
    );
  },
};
```

## Give a ⭐️ if this project helped you!

## License

[MIT](./LICENSE) License © 2022 xxXyh1908

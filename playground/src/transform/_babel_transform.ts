import './_babel_env_polyfill'
import { transform as babelTransform } from '@babel/standalone'
// @ts-ignore
import typescriptPlugin from '@babel/plugin-transform-typescript'
import jsxPlugin from '@vue/babel-plugin-jsx'

export const transform = (input: string, fileName: string, options: any) => {
  return babelTransform(input, {
    filename: fileName,
    babelrc: false,
    configFile: false,
    ast: false,
    code: true,
    comments: true,
    sourceType: 'module',
    parserOpts: {
      plugins: ['typescript', 'jsx', 'bigInt', 'classPrivateMethods', 'classPrivateProperties']
    },
    plugins: [
      [jsxPlugin, options],
      [typescriptPlugin, { isTSX: true, allowNamespaces: true, allowDeclareFields: true, allowExtensions: true }]
    ]
  })
}
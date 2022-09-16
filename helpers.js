'use strict'
Object.defineProperty(exports, '__esModule', { value: true })
const { isVNode, useSSRContext } = require('vue')
const { toString } = Object.prototype
const { isArray } = Array
const castArray = value => {
  if (value === undefined) return []
  else if (isArray(value)) return value
  else return [value]
}
exports.castArray = castArray
exports.ssr = typeof __SSR__ === 'boolean' ? __SSR__ : process.env.SSR
const transformOn = obj => {
  const result = {}
  if (obj) {
    for (const evt of Object.keys(obj)) {
      result[`on${evt[0].toUpperCase()}${evt.slice(1)}`] = obj[evt]
    }
  }
  return result
}
exports.transformOn = transformOn
const toObjectSlots = slots => {
  return slots == null
    ? null
    : toString.call(slots) === '[object Object]' && !isVNode(slots)
    ? slots
    : typeof slots === 'function'
    ? { default: slots }
    : { default: () => [slots] }
}
exports.toObjectSlots = toObjectSlots
/**
 * module during SSR
 */
const ssrRegisterHelper = (comp, filename) => {
  const setup = comp.setup
  comp.setup = (props, ctx) => {
    const ssrContext = useSSRContext()
    ;(ssrContext.modules || (ssrContext.modules = new Set())).add(filename)
    if (setup) {
      return setup(props, ctx)
    }
  }
}
exports.ssrRegisterHelper = ssrRegisterHelper

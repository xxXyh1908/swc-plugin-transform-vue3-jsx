import { isVNode, useSSRContext } from 'vue'

/*@__PURE__*/ const { toString } = Object.prototype
/*@__PURE__*/ const { isArray } = Array

export const castArray = value => {
  if (value === undefined) return []
  else if (isArray(value)) return value
  else return [value]
}

export const ssr = /*@__PURE__*/ import.meta.env
  ? import.meta.env.SSR
  : typeof __SSR__ === 'boolean'
  ? __SSR__
  : process.env.SSR

export const transformOn = obj => {
  const result = {}

  if (obj) {
    for (const evt of Object.keys(obj)) {
      result[`on${evt[0].toUpperCase()}${evt.slice(1)}`] = obj[evt]
    }
  }

  return result
}

export const toObjectSlots = slots => {
  return slots == null
    ? null
    : toString.call(slots) === '[object Object]' && !isVNode(slots)
    ? slots
    : typeof slots === 'function'
    ? { default: slots }
    : { default: () => [slots] }
}

/**
 * module during SSR
 */
export const ssrRegisterHelper = (comp, filename) => {
  const setup = comp.setup
  comp.setup = (props, ctx) => {
    const ssrContext = useSSRContext()
    ;(ssrContext.modules || (ssrContext.modules = new Set())).add(filename)
    if (setup) {
      return setup(props, ctx)
    }
  }
}

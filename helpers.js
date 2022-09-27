'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

const vue = require('vue');

const { toString } = Object.prototype;
const { isArray } = Array;

const castArray = value => {
  if (value === undefined) return []
  else if (isArray(value)) return value
  else return [value]
};

const upperFirst = str => str[0].toUpperCase() + str.slice(1);
const import_env = undefined;
const ssr = import_env ? import_env.SSR : typeof __SSR__ === 'boolean' ? __SSR__ : process.env.SSR;

const transformOn = obj => {
  const result = {};

  if (obj) {
    Object.keys(obj).forEach(evt => {
      result[`on${evt[0].toUpperCase()}${evt.slice(1)}`] = obj[evt];
    });
  }

  return result
};

const onceArrowFunctionWithNoArgs = fn => {
  let result;
  let isCalled = false;

  return () => {
    if (isCalled) {
      return result
    } else {
      result = fn();
      isCalled = true;
      return result
    }
  }
};

const toSlot = slot => (typeof slot === 'function' ? slot : () => [slot]);

const toObjectSlots = slots => {
  return slots == null
    ? null
    : toString.call(slots) === '[object Object]' && !vue.isVNode(slots)
    ? slots
    : typeof slots === 'function'
    ? { default: slots }
    : { default: () => [slots] }
};

/**
 * module during SSR
 */
const ssrRegisterHelper = (comp, filename) => {
  const setup = comp.setup;
  comp.setup = (props, ctx) => {
    const ssrContext = vue.useSSRContext()
    ;(ssrContext.modules || (ssrContext.modules = new Set())).add(filename);
    if (setup) {
      return setup(props, ctx)
    }
  };
};

const hyphenateRE = /\B([A-Z])/g;
const hyphenate = str => str.replace(hyphenateRE, '-$1').toLowerCase();

const isKeyNotMatch = (expect, actual) => {
  if (isArray(expect)) {
    return expect.includes(actual)
  } else {
    return expect !== actual
  }
};

const checkKeyCodes = (eventKeyCode, key, builtInKeyCode, eventKeyName, builtInKeyName) => {
  const mappedKeyCode = builtInKeyCode;
  if (builtInKeyName && eventKeyName) {
    return isKeyNotMatch(builtInKeyName, eventKeyName)
  } else if (mappedKeyCode) {
    return isKeyNotMatch(mappedKeyCode, eventKeyCode)
  } else if (eventKeyName) {
    return hyphenate(eventKeyName) !== key
  }
  return eventKeyCode === undefined
};

exports.castArray = castArray;
exports.checkKeyCodes = checkKeyCodes;
exports.onceArrowFunctionWithNoArgs = onceArrowFunctionWithNoArgs;
exports.ssr = ssr;
exports.ssrRegisterHelper = ssrRegisterHelper;
exports.toObjectSlots = toObjectSlots;
exports.toSlot = toSlot;
exports.transformOn = transformOn;
exports.upperFirst = upperFirst;

import Worker from './babel.worker?worker'
import { asyncState, AsyncState } from './utils'

const { isArray } = Array
const worker = new Worker()
const stateMap = new Map<number, AsyncState<[string, number]>>()
let i = 0

worker.addEventListener('message', ({ data }: MessageEvent<[key: any, value: any]>) => {
  if (!isArray(data)) return
  const [key, value] = data
  const state = stateMap.get(key)
  if (state) {
    stateMap.delete(key)
    if (value && typeof value === 'object' && !isArray(value)) {
      Object.setPrototypeOf(value, Error.prototype)
      state.reject(value)
    } else {
      state.resolve(value)
    }
  }
})

export const transform = async (input: string, fileName: string, options: any) => {
  const state = asyncState<[string, number]>()
  const key = ++i

  worker.postMessage([key, input, fileName, options])
  stateMap.set(key, state)

  const [code, time] = await state
  return { code, time }
}
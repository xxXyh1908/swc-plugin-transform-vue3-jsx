export interface AsyncState<T> extends Promise<T> {
  resolve(value: T | PromiseLike<T>): void
  reject(reason?: any): void
  fulfilled: boolean
  rejected: boolean
}

export const asyncState = <T = void>(): AsyncState<T> => {
  let resolve: (value: T | PromiseLike<T>) => void, reject: (reason?: any) => void
  const state: any = new Promise<T>((_resolve, _reject) => {
    resolve = _resolve
    reject = _reject
  })

  let ready = false

  state.fulfilled = false
  state.rejected = false

  state.resolve = (value: any) => {
    if (ready) return
    state.fulfilled = ready = true
    resolve(value)
  }
  state.reject = (value: any) => {
    if (ready) return
    state.rejected = ready = true
    reject(value)
  }

  state.catch()

  return state
}
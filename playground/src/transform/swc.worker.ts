import init, { transform } from '../../pkg/playground'

let ready = init()
const { isArray } = Array

const handleOnMessage = async function (
  this: {
    postMessage(data: any): void
  },
  { data }: MessageEvent<[key: any, input: string, fileName: string, options: any]>
) {
  if (!isArray(data)) return
  const [key, input, fileName, options] = data
  try {
    await (ready = ready.catch(init))
    const start = performance.now()
    const result = transform(input, fileName, options)
    const end = performance.now()

    this.postMessage([key, [result.code, end - start]])
  } catch (error) {
    this.postMessage([key, error])
  }
}

onmessage = handleOnMessage.bind(globalThis)
// onconnect = ev => {
//   const port = ev.ports[0]

//   port.addEventListener('message', handleOnMessage.bind(port))
//   port.start()
// }

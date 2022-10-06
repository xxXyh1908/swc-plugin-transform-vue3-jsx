/* tslint:disable */
/* eslint-disable */

export interface Options {
    include?: string[]
    exclude?: string[]
    customElement?: string[]
    pragma?: string,
    hmr?: boolean
    mergeProps?: boolean
    enableObjectSlots?: boolean
    optimize?: boolean
    reactStyle?: boolean
    transformOn?: boolean
    transformSlot?: boolean
    transformVSlot?: boolean
    transformOnUpdateEvent?: boolean
    vOn?: boolean
    vModel?: boolean
}

export interface TransformOutput {
    code: string,
    map?: string
}

export function transform(input: string, file_name?: string, options?: Options): TransformOutput;



export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly transform: (a: number, b: number, c: number, d: number, e: number) => void;
  readonly __wbindgen_malloc: (a: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number) => number;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
  readonly __wbindgen_free: (a: number, b: number) => void;
  readonly __wbindgen_exn_store: (a: number) => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;

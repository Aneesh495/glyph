(** Public Glyph virtual machine API.

    Typical use:
    {[
      let chunk = Emit.emit_program mir_program in
      let result = Vm.run_chunk chunk
    ]}
*)

type t = Interp.state

type error =
  | Runtime of string
  | Step_limit of int
  | Load of string

let pp_error fmt = function
  | Runtime msg -> Format.fprintf fmt "runtime error: %s" msg
  | Step_limit n -> Format.fprintf fmt "step limit exceeded (%d)" n
  | Load msg -> Format.fprintf fmt "load error: %s" msg

exception Error of error

(** Load a chunk from an in-memory value (identity — chunks are already
    executable). Kept for API symmetry with file loading. *)
let load (chunk : Chunk.t) : t =
  Interp.create chunk

let load_file path : t =
  try
    let chunk = Chunk.of_file path in
    Interp.create chunk
  with
  | Sys_error msg -> raise (Error (Load msg))
  | Failure msg -> raise (Error (Load msg))

let load_file_result path =
  try Ok (load_file path) with Error e -> Error e

(** Run an already-loaded VM state from its main proto. *)
let run (vm : t) : Value.t =
  try Interp.run_state vm with
  | Interp.Runtime_error msg -> raise (Error (Runtime msg))
  | Interp.Step_limit n -> raise (Error (Step_limit n))

let run_result vm =
  try Ok (run vm) with Error e -> Error e

(** Compile-free entry: execute a chunk end-to-end. *)
let run_chunk ?(heap_size = 512) ?(max_steps = None) (chunk : Chunk.t) :
    Value.t =
  try Interp.run_chunk ~heap_size ~max_steps chunk with
  | Interp.Runtime_error msg -> raise (Error (Runtime msg))
  | Interp.Step_limit n -> raise (Error (Step_limit n))

let run_chunk_result ?heap_size ?max_steps chunk =
  try Ok (run_chunk ?heap_size ?max_steps chunk)
  with Error e -> Error e

(** Emit MIR then run. *)
let run_program ?heap_size ?max_steps (prog : Mir.program) : Value.t =
  let chunk = Emit.emit_program prog in
  run_chunk ?heap_size ?max_steps chunk

let run_program_result ?heap_size ?max_steps prog =
  try Ok (run_program ?heap_size ?max_steps prog) with
  | Emit.Emit_error msg -> Error (Load msg)
  | Error e -> Error e

(** Disassemble the chunk associated with a VM. *)
let disassemble (vm : t) = Disasm.to_string vm.chunk

let chunk (vm : t) = vm.chunk
let heap (vm : t) = vm.heap
let steps (vm : t) = vm.steps
let result (vm : t) = vm.result
let halted (vm : t) = vm.halted

let gc_stats (vm : t) = !(Gc.last_stats)

let force_gc (vm : t) =
  let roots = Gc.create_roots () in
  Interp.fill_roots vm roots;
  Gc.collect vm.heap roots

(** Single-step for debuggers. *)
let step (vm : t) =
  try
    if not vm.halted then ignore (Interp.step_correct vm);
    vm.halted
  with
  | Interp.Runtime_error msg -> raise (Error (Runtime msg))
  | Interp.Step_limit n -> raise (Error (Step_limit n))

let set_max_steps (vm : t) lim = vm.max_steps <- lim

let reset (vm : t) =
  Heap.reset vm.heap;
  vm.frames <- [];
  vm.halted <- false;
  vm.result <- Value.unit;
  vm.steps <- 0

(** Pretty-print VM status. *)
let pp fmt (vm : t) =
  Format.fprintf fmt "VM{halted=%b steps=%d result=%a heap=%s}"
    vm.halted vm.steps Value.pp vm.result (Heap.stats vm.heap)

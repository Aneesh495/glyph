(** Heap object layouts for the Glyph VM.

    Objects live in the semi-space heap ([Heap]). During a Cheney collection,
    evacuated objects leave a [Forward] pointer in fromspace so subsequent
    references can be updated in a single chase.
*)

type t =
  | String of string
  | Tuple of Value.t array
  | Adt of {
      tag : int;
      fields : Value.t array;
    }
  | Closure of {
      proto_id : int;
      env : Value.t array;
    }
  | Array of Value.t array
  | Forward of int
      (** Forwarding pointer — address in tospace after evacuation. *)

(** Standard list tags used by Cons/Car/Cdr opcodes. *)
let tag_nil = 0
let tag_cons = 1

let string s = String s
let tuple fields = Tuple fields
let adt tag fields = Adt { tag; fields }
let closure proto_id env = Closure { proto_id; env }
let array xs = Array xs
let forward addr = Forward addr

let is_forward = function Forward _ -> true | _ -> false

let as_forward = function
  | Forward a -> a
  | _ -> invalid_arg "Object_.as_forward"

let nfields = function
  | String _ -> 0
  | Tuple fs | Array fs -> Array.length fs
  | Adt { fields; _ } -> Array.length fields
  | Closure { env; _ } -> Array.length env
  | Forward _ -> 0

let get_field obj i =
  match obj with
  | Tuple fs | Array fs -> fs.(i)
  | Adt { fields; _ } -> fields.(i)
  | Closure { env; _ } -> env.(i)
  | String _ -> invalid_arg "Object_.get_field: string"
  | Forward _ -> invalid_arg "Object_.get_field: forwarding pointer"

let set_field obj i v =
  match obj with
  | Tuple fs | Array fs -> fs.(i) <- v
  | Adt { fields; _ } -> fields.(i) <- v
  | Closure { env; _ } -> env.(i) <- v
  | String _ -> invalid_arg "Object_.set_field: string"
  | Forward _ -> invalid_arg "Object_.set_field: forwarding pointer"

(** Iterate every Value.t field (for GC scanning). *)
let iter_fields f = function
  | String _ | Forward _ -> ()
  | Tuple fs | Array fs -> Array.iter f fs
  | Adt { fields; _ } -> Array.iter f fields
  | Closure { env; _ } -> Array.iter f env

(** Map fields, producing a new object with the same shape (used when
    reallocating into tospace — we usually copy then update in place). *)
let map_fields f = function
  | String s -> String s
  | Tuple fs -> Tuple (Array.map f fs)
  | Adt { tag; fields } -> Adt { tag; fields = Array.map f fields }
  | Closure { proto_id; env } ->
      Closure { proto_id; env = Array.map f env }
  | Array fs -> Array (Array.map f fs)
  | Forward a -> Forward a

(** Deep copy of the object header/payload into a fresh OCaml value (fields
    still hold old addresses until the GC updates them). *)
let copy = function
  | String s -> String s
  | Tuple fs -> Tuple (Array.copy fs)
  | Adt { tag; fields } -> Adt { tag; fields = Array.copy fields }
  | Closure { proto_id; env } ->
      Closure { proto_id; env = Array.copy env }
  | Array fs -> Array (Array.copy fs)
  | Forward a -> Forward a

let adt_tag = function
  | Adt { tag; _ } -> tag
  | Tuple _ -> 0
  | _ -> invalid_arg "Object_.adt_tag"

let closure_proto = function
  | Closure { proto_id; _ } -> proto_id
  | _ -> invalid_arg "Object_.closure_proto"

let closure_env = function
  | Closure { env; _ } -> env
  | _ -> invalid_arg "Object_.closure_env"

let as_string = function
  | String s -> s
  | _ -> invalid_arg "Object_.as_string"

let is_cons = function
  | Adt { tag; fields } when tag = tag_cons && Array.length fields = 2 ->
      true
  | _ -> false

let cons_hd = function
  | Adt { tag; fields } when tag = tag_cons -> fields.(0)
  | _ -> invalid_arg "Object_.cons_hd"

let cons_tl = function
  | Adt { tag; fields } when tag = tag_cons -> fields.(1)
  | _ -> invalid_arg "Object_.cons_tl"

let make_cons hd tl = Adt { tag = tag_cons; fields = [| hd; tl |] }
let make_nil = Adt { tag = tag_nil; fields = [||] }

let pp fmt = function
  | String s -> Format.fprintf fmt "String(%S)" s
  | Tuple fs ->
      Format.fprintf fmt "Tuple(%d)" (Array.length fs)
  | Adt { tag; fields } ->
      Format.fprintf fmt "Adt(tag=%d, fields=%d)" tag
        (Array.length fields)
  | Closure { proto_id; env } ->
      Format.fprintf fmt "Closure(proto=%d, env=%d)" proto_id
        (Array.length env)
  | Array fs -> Format.fprintf fmt "Array(%d)" (Array.length fs)
  | Forward a -> Format.fprintf fmt "Forward(%d)" a

let to_string o = Format.asprintf "%a" pp o

(** Approximate size in "slots" for heap accounting (1 + nfields). *)
let size_slots o = 1 + nfields o

(** Kind tag for debugging / heap dumps. *)
type kind =
  | KString
  | KTuple
  | KAdt
  | KClosure
  | KArray
  | KForward

let kind = function
  | String _ -> KString
  | Tuple _ -> KTuple
  | Adt _ -> KAdt
  | Closure _ -> KClosure
  | Array _ -> KArray
  | Forward _ -> KForward

let kind_name = function
  | KString -> "string"
  | KTuple -> "tuple"
  | KAdt -> "adt"
  | KClosure -> "closure"
  | KArray -> "array"
  | KForward -> "forward"

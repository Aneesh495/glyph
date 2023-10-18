(** Structured type errors and pretty-printed diagnostics. *)

module Span = Span
module Diagnostic = Diagnostic
module Ident = Ident

type error_kind =
  | Unify_mismatch
  | Occurs_check
  | Unbound_value
  | Unbound_constructor
  | Unbound_type
  | Unbound_field
  | Arity_mismatch
  | Pattern_mismatch
  | Not_a_function
  | Not_a_record
  | Immutable_field
  | Recursive_type
  | Escaping_type_variable
  | Other

type t = {
  span : Span.t;
  message : string;
  expected : Ty.ty option;
  actual : Ty.ty option;
  notes : (Span.t * string) list;
  kind : error_kind;
}

exception Type_error of t

let error_kind_code = function
  | Unify_mismatch -> "E0001"
  | Occurs_check -> "E0002"
  | Unbound_value -> "E0003"
  | Unbound_constructor -> "E0004"
  | Unbound_type -> "E0005"
  | Unbound_field -> "E0006"
  | Arity_mismatch -> "E0007"
  | Pattern_mismatch -> "E0008"
  | Not_a_function -> "E0009"
  | Not_a_record -> "E0010"
  | Immutable_field -> "E0011"
  | Recursive_type -> "E0012"
  | Escaping_type_variable -> "E0013"
  | Other -> "E0000"

let error_kind_to_string = function
  | Unify_mismatch -> "type mismatch"
  | Occurs_check -> "occurs check failed"
  | Unbound_value -> "unbound value"
  | Unbound_constructor -> "unbound constructor"
  | Unbound_type -> "unbound type constructor"
  | Unbound_field -> "unbound record field"
  | Arity_mismatch -> "arity mismatch"
  | Pattern_mismatch -> "pattern type mismatch"
  | Not_a_function -> "not a function"
  | Not_a_record -> "not a record"
  | Immutable_field -> "immutable field"
  | Recursive_type -> "infinite type"
  | Escaping_type_variable -> "escaping type variable"
  | Other -> "type error"

let make ?expected ?actual ?(notes = []) ?(kind = Other) span message =
  { span; message; expected; actual; notes; kind }

let raise_error ?expected ?actual ?notes ?kind span message =
  raise (Type_error (make ?expected ?actual ?notes ?kind span message))

let mismatch ?notes span ~expected ~actual =
  let message =
    Printf.sprintf
      "This expression has type %s but an expression was expected of type %s"
      (Ty.to_string actual) (Ty.to_string expected)
  in
  raise_error ~expected ~actual ?notes ~kind:Unify_mismatch span message

let occurs_error span ~tv ~ty =
  let hint =
    match tv.Ty.namehint with
    | Some n -> n
    | None -> string_of_int tv.Ty.id
  in
  let message =
    Printf.sprintf
      "The type variable '%s occurs inside type %s (infinite type)" hint
      (Ty.to_string ty)
  in
  raise_error ~actual:ty ~kind:Occurs_check span message

let unbound_value span id =
  raise_error ~kind:Unbound_value span
    (Printf.sprintf "Unbound value `%s`" (Ident.to_string id))

let unbound_constructor span id =
  raise_error ~kind:Unbound_constructor span
    (Printf.sprintf "Unbound constructor `%s`" (Ident.to_string id))

let unbound_type span id =
  raise_error ~kind:Unbound_type span
    (Printf.sprintf "Unbound type constructor `%s`" (Ident.to_string id))

let unbound_field span id =
  raise_error ~kind:Unbound_field span
    (Printf.sprintf "Unbound record field `%s`" (Ident.to_string id))

let not_a_function span ty =
  raise_error ~actual:ty ~kind:Not_a_function span
    (Printf.sprintf
       "This expression has type %s which is not a function; it cannot be \
        applied"
       (Ty.to_string ty))

let arity_mismatch span ~expected ~actual =
  raise_error ~kind:Arity_mismatch span
    (Printf.sprintf
       "This constructor expects %d argument(s) but was given %d" expected
       actual)

let to_diagnostic ?source:_ err =
  let labels =
    let base = [ Diagnostic.label ~primary:true err.span err.message ] in
    let with_expected =
      match err.expected with
      | None -> base
      | Some ty ->
          base
          @ [
              Diagnostic.label ~primary:false err.span
                (Printf.sprintf "expected type: %s" (Ty.to_string ty));
            ]
    in
    match err.actual with
    | None -> with_expected
    | Some ty ->
        with_expected
        @ [
            Diagnostic.label ~primary:false err.span
              (Printf.sprintf "actual type: %s" (Ty.to_string ty));
          ]
  in
  let notes =
    List.map
      (fun (sp, msg) -> Printf.sprintf "%s: %s" (Span.to_string sp) msg)
      err.notes
  in
  let help =
    match err.kind with
    | Occurs_check | Recursive_type ->
        Some
          "A type cannot contain itself; wrap the recursion in a datatype."
    | Unify_mismatch ->
        Some "Ensure function arguments and match arms agree on types."
    | Unbound_value ->
        Some "Define the value before use, or check spelling and scope."
    | Arity_mismatch ->
        Some "Pass the exact number of arguments the constructor expects."
    | _ -> None
  in
  Diagnostic.error
    ~code:(error_kind_code err.kind)
    ~labels ~notes ?help err.span
    (Printf.sprintf "%s: %s" (error_kind_to_string err.kind) err.message)

let format ?source err = Diagnostic.render ?source (to_diagnostic ?source err)

let report ?source err =
  output_string stderr (format ?source err);
  flush stderr

let catch f = try Ok (f ()) with Type_error e -> Error e

let pp fmt err = Format.pp_print_string fmt (format err)

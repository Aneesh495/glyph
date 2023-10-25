(** Runtime values. *)

type native = t list -> t

and t =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Unit
  | Tuple of t array
  | Adt of int * t array
  | Closure of int * t array
  | Native of string * native
  | Ptr of int

let rec equal a b =
  match (a, b) with
  | Int x, Int y -> x = y
  | Float x, Float y -> Float.equal x y
  | Bool x, Bool y -> x = y
  | String x, String y -> String.equal x y
  | Unit, Unit -> true
  | Tuple xs, Tuple ys ->
      Array.length xs = Array.length ys
      && Array.for_all2 equal xs ys
  | Adt (t1, xs), Adt (t2, ys) ->
      t1 = t2 && Array.length xs = Array.length ys
      && Array.for_all2 equal xs ys
  | Closure (f1, e1), Closure (f2, e2) ->
      f1 = f2 && Array.length e1 = Array.length e2
      && Array.for_all2 equal e1 e2
  | Native (n1, _), Native (n2, _) -> String.equal n1 n2
  | Ptr x, Ptr y -> x = y
  | _ -> false

let rec to_string = function
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> s
  | Unit -> "()"
  | Tuple xs ->
      "("
      ^ String.concat ", " (Array.to_list (Array.map to_string xs))
      ^ ")"
  | Adt (tag, xs) ->
      Printf.sprintf "#%d(%s)" tag
        (String.concat ", " (Array.to_list (Array.map to_string xs)))
  | Closure (fid, env) ->
      Printf.sprintf "<closure fn%d/%d>" fid (Array.length env)
  | Native (name, _) -> Printf.sprintf "<native %s>" name
  | Ptr i -> Printf.sprintf "<ptr %d>" i

let pp fmt v = Format.pp_print_string fmt (to_string v)

let is_truthy = function
  | Bool b -> b
  | Int 0 -> false
  | Unit -> false
  | _ -> true

let as_int = function
  | Int i -> i
  | v -> failwith ("Value.as_int: " ^ to_string v)

let as_float = function
  | Float f -> f
  | Int i -> float_of_int i
  | v -> failwith ("Value.as_float: " ^ to_string v)

let as_bool = function
  | Bool b -> b
  | v -> failwith ("Value.as_bool: " ^ to_string v)

let as_string = function
  | String s -> s
  | v -> failwith ("Value.as_string: " ^ to_string v)

let tag_of = function
  | Adt (tag, _) -> tag
  | Tuple _ -> 0
  | Bool false -> 0
  | Bool true -> 1
  | Unit -> 0
  | v -> failwith ("Value.tag_of: " ^ to_string v)

let fields_of = function
  | Tuple xs | Adt (_, xs) | Closure (_, xs) -> xs
  | v -> failwith ("Value.fields_of: " ^ to_string v)

let is_heap = function
  | Ptr _ | Tuple _ | Adt _ | Closure _ | String _ -> true
  | _ -> false

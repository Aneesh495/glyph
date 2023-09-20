(** Ordered immutable maps keyed by strings with extras. *)

module Str_map = Map.Make (String)
module Str_set = Set.Make (String)
module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

let str_map_of_list xs =
  List.fold_left (fun m (k, v) -> Str_map.add k v m) Str_map.empty xs

let int_map_of_list xs =
  List.fold_left (fun m (k, v) -> Int_map.add k v m) Int_map.empty xs

(** Scoped environments as a stack of maps. *)
module Scope (M : Map.S) = struct
  type 'a t = 'a M.t list

  let empty : 'a t = [ M.empty ]
  let push env = M.empty :: env

  let pop = function
    | [] | [ _ ] -> failwith "Scope.pop: empty"
    | _ :: rest -> rest

  let add key value = function
    | [] -> failwith "Scope.add: empty"
    | top :: rest -> M.add key value top :: rest

  let rec find key = function
    | [] -> None
    | m :: rest -> (
        match M.find_opt key m with
        | Some _ as v -> v
        | None -> find key rest)

  let find_exn key env =
    match find key env with
    | Some v -> v
    | None -> raise Not_found

  let mem key env = Option.is_some (find key env)

  let flatten env =
    List.fold_right
      (fun m acc -> M.merge (fun _k a b -> match a with Some _ -> a | None -> b) m acc)
      env M.empty
end

module Ident_scope = Scope (Ident.Map)
module Str_scope = Scope (Str_map)

(** Interned identifiers with unique generation stamps. *)

type t = {
  name : string;
  stamp : int;
}

let stamp_counter = ref 0

let fresh name =
  incr stamp_counter;
  { name; stamp = !stamp_counter }

let gensym ?(prefix = "g") () = fresh prefix

let of_string name = { name; stamp = 0 }
let raw = of_string

let refresh t = fresh t.name

let name t = t.name
let stamp t = t.stamp

let equal a b = a.stamp = b.stamp && String.equal a.name b.name

let compare a b =
  match Int.compare a.stamp b.stamp with
  | 0 -> String.compare a.name b.name
  | c -> c

let hash t = Hashtbl.hash (t.name, t.stamp)

let to_string t =
  if t.stamp = 0 then t.name
  else Printf.sprintf "%s#%d" t.name t.stamp

let pp fmt t = Format.pp_print_string fmt (to_string t)

let is_underscore t = String.equal t.name "_"
let is_fresh t = t.stamp > 0

let starts_with t ~prefix =
  let n = String.length prefix in
  String.length t.name >= n && String.sub t.name 0 n = prefix

module Set = Set.Make (struct
  type nonrec t = t
  let compare = compare
end)

module Map = Map.Make (struct
  type nonrec t = t
  let compare = compare
end)

module Tbl = struct
  include Hashtbl.Make (struct
    type nonrec t = t
    let equal = equal
    let hash = hash
  end)

  let of_list pairs =
    let tbl = create (List.length pairs) in
    List.iter (fun (k, v) -> add tbl k v) pairs;
    tbl

  let to_list tbl =
    fold (fun k v acc -> (k, v) :: acc) tbl []
end

module Intern = struct
  let table : (string, t) Hashtbl.t = Hashtbl.create 512

  let intern name =
    match Hashtbl.find_opt table name with
    | Some id -> id
    | None ->
        let id = of_string name in
        Hashtbl.add table name id;
        id

  let mem name = Hashtbl.mem table name
  let reset () = Hashtbl.clear table
  let size () = Hashtbl.length table

  let fold f acc =
    Hashtbl.fold f table acc
end

module Predef = struct
  let underscore = Intern.intern "_"
  let main = Intern.intern "main"
  let unit = Intern.intern "Unit"
  let bool = Intern.intern "Bool"
  let int = Intern.intern "Int"
  let float = Intern.intern "Float"
  let string = Intern.intern "String"
  let list = Intern.intern "List"
  let nil = Intern.intern "Nil"
  let cons = Intern.intern "Cons"
end

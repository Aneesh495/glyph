(** String interning table with stable integer ids. *)

type id = int

type t = {
  mutable by_string : (string, id) Hashtbl.t;
  mutable by_id : string Resizable.t;
}

and 'a resizable = {
  mutable data : 'a array;
  mutable len : int;
}

(* Avoid circular naming — inline growable string array. *)
module Strings = struct
  type t = {
    mutable data : string array;
    mutable len : int;
  }

  let create () = { data = Array.make 32 ""; len = 0 }
  let length a = a.len
  let get a i = a.data.(i)

  let push a s =
    if a.len >= Array.length a.data then (
      let nd = Array.make (Array.length a.data * 2) "" in
      Array.blit a.data 0 nd 0 a.len;
      a.data <- nd);
    let i = a.len in
    a.data.(i) <- s;
    a.len <- i + 1;
    i

  let clear a =
    a.data <- Array.make 32 "";
    a.len <- 0

  let to_array a = Array.sub a.data 0 a.len
end

type nonrec t = {
  by_string : (string, id) Hashtbl.t;
  strings : Strings.t;
}

let create ?(size = 256) () =
  { by_string = Hashtbl.create size; strings = Strings.create () }

let clear t =
  Hashtbl.clear t.by_string;
  Strings.clear t.strings

let length t = Strings.length t.strings

let intern t s =
  match Hashtbl.find_opt t.by_string s with
  | Some id -> id
  | None ->
      let id = Strings.push t.strings s in
      Hashtbl.add t.by_string s id;
      id

let of_id t id =
  if id < 0 || id >= Strings.length t.strings then
    invalid_arg "Intern.of_id";
  Strings.get t.strings id

let mem t s = Hashtbl.mem t.by_string s

let find_opt t s = Hashtbl.find_opt t.by_string s

let iter t f =
  for id = 0 to Strings.length t.strings - 1 do
    f id (Strings.get t.strings id)
  done

let fold t f acc =
  let acc = ref acc in
  iter t (fun id s -> acc := f id s !acc);
  !acc

let to_array t = Strings.to_array t.strings

module Global = struct
  let table = create ~size:1024 ()
  let intern s = intern table s
  let of_id id = of_id table id
  let mem s = mem table s
  let reset () = clear table
  let length () = length table
end

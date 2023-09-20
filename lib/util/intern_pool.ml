(** String interning pool for constant pools and symbols. *)

type t = {
  table : (string, int) Hashtbl.t;
  rev : (int, string) Hashtbl.t;
  mutable next : int;
}

let create ?(size = 64) () =
  {
    table = Hashtbl.create size;
    rev = Hashtbl.create size;
    next = 0;
  }

let intern pool s =
  match Hashtbl.find_opt pool.table s with
  | Some id -> id
  | None ->
      let id = pool.next in
      pool.next <- id + 1;
      Hashtbl.add pool.table s id;
      Hashtbl.add pool.rev id s;
      id

let resolve pool id =
  match Hashtbl.find_opt pool.rev id with
  | Some s -> s
  | None -> invalid_arg (Printf.sprintf "Intern_pool.resolve: %d" id)

let mem pool s = Hashtbl.mem pool.table s
let length pool = pool.next

let to_list pool =
  Hashtbl.fold (fun s id acc -> (id, s) :: acc) pool.table []
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)

let clear pool =
  Hashtbl.clear pool.table;
  Hashtbl.clear pool.rev;
  pool.next <- 0

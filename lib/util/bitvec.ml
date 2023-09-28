(** Growable bit vectors for dataflow analysis. *)

type t = {
  mutable words : int array;
  mutable nbits : int;
}

let word_bits = Sys.word_size - 1 (* use tagged int bits safely: 63/31 *)
(* Actually OCaml ints are 63-bit on 64-bit; use fixed 63 for portability of ops *)
let bits_per_word = 63

let create ?(size = 64) () =
  let words_needed = (size + bits_per_word - 1) / bits_per_word in
  {
    words = Array.make (max 1 words_needed) 0;
    nbits = size;
  }

let copy t = { words = Array.copy t.words; nbits = t.nbits }

let clear t =
  Array.fill t.words 0 (Array.length t.words) 0

let length t = t.nbits

let ensure t bit =
  if bit >= t.nbits then t.nbits <- bit + 1;
  let need = (bit / bits_per_word) + 1 in
  if need > Array.length t.words then (
    let nw = Array.make (max need (Array.length t.words * 2)) 0 in
    Array.blit t.words 0 nw 0 (Array.length t.words);
    t.words <- nw)

let word_index i = i / bits_per_word
let bit_mask i = 1 lsl (i mod bits_per_word)

let get t i =
  if i < 0 then invalid_arg "Bitvec.get";
  if i >= t.nbits then false
  else
    let w = t.words.(word_index i) in
    (w land bit_mask i) <> 0

let set t i v =
  if i < 0 then invalid_arg "Bitvec.set";
  ensure t i;
  let wi = word_index i in
  let m = bit_mask i in
  if v then t.words.(wi) <- t.words.(wi) lor m
  else t.words.(wi) <- t.words.(wi) land lnot m

let set_bit t i = set t i true
let clear_bit t i = set t i false
let toggle t i = set t i (not (get t i))

let is_empty t =
  let empty = ref true in
  let n = Array.length t.words in
  let i = ref 0 in
  while !empty && !i < n do
    if t.words.(!i) <> 0 then empty := false;
    incr i
  done;
  !empty

let popcount_word w =
  (* SWAR popcount for 63-bit positive ints *)
  let rec loop x acc =
    if x = 0 then acc else loop (x lsr 1) (acc + (x land 1))
  in
  loop (w land max_int) 0

let popcount t =
  Array.fold_left (fun acc w -> acc + popcount_word w) 0 t.words

let first_set t =
  let rec word wi =
    if wi >= Array.length t.words then None
    else if t.words.(wi) = 0 then word (wi + 1)
    else
      let w = t.words.(wi) in
      let rec bit b =
        if b >= bits_per_word then None
        else if (w land (1 lsl b)) <> 0 then
          let idx = wi * bits_per_word + b in
          if idx < t.nbits then Some idx else None
        else bit (b + 1)
      in
      bit 0
  in
  word 0

let iter_set t f =
  for i = 0 to t.nbits - 1 do
    if get t i then f i
  done

let fold_set t f acc =
  let acc = ref acc in
  iter_set t (fun i -> acc := f i !acc);
  !acc

let sync_length dst src =
  if src.nbits > dst.nbits then ensure dst (src.nbits - 1);
  let need = Array.length src.words in
  if need > Array.length dst.words then ensure dst ((need * bits_per_word) - 1)

let union_into ~dst ~src =
  sync_length dst src;
  let changed = ref false in
  let n = Array.length src.words in
  for i = 0 to n - 1 do
    let before = dst.words.(i) in
    let after = before lor src.words.(i) in
    if after <> before then (
      dst.words.(i) <- after;
      changed := true)
  done;
  !changed

let inter_into ~dst ~src =
  sync_length dst src;
  let changed = ref false in
  let n = max (Array.length dst.words) (Array.length src.words) in
  for i = 0 to n - 1 do
    let a = if i < Array.length dst.words then dst.words.(i) else 0 in
    let b = if i < Array.length src.words then src.words.(i) else 0 in
    let after = a land b in
    if i < Array.length dst.words && after <> a then (
      dst.words.(i) <- after;
      changed := true)
  done;
  !changed

let diff_into ~dst ~src =
  sync_length dst src;
  let changed = ref false in
  let n = min (Array.length dst.words) (Array.length src.words) in
  for i = 0 to n - 1 do
    let before = dst.words.(i) in
    let after = before land lnot src.words.(i) in
    if after <> before then (
      dst.words.(i) <- after;
      changed := true)
  done;
  !changed

let copy_into ~dst ~src =
  sync_length dst src;
  let changed = ref false in
  let n = Array.length src.words in
  for i = 0 to n - 1 do
    if dst.words.(i) <> src.words.(i) then (
      dst.words.(i) <- src.words.(i);
      changed := true)
  done;
  for i = n to Array.length dst.words - 1 do
    if dst.words.(i) <> 0 then (
      dst.words.(i) <- 0;
      changed := true)
  done;
  dst.nbits <- max dst.nbits src.nbits;
  !changed

let equal a b =
  let nbits = max a.nbits b.nbits in
  let rec loop i =
    if i >= nbits then true
    else if get a i <> get b i then false
    else loop (i + 1)
  in
  loop 0

let subset a b =
  let rec loop i =
    if i >= a.nbits then true
    else if get a i && not (get b i) then false
    else loop (i + 1)
  in
  loop 0

let of_list idxs =
  let max_i = List.fold_left max 0 idxs in
  let t = create ~size:(max_i + 1) () in
  List.iter (set_bit t) idxs;
  t

let to_list t =
  let acc = ref [] in
  iter_set t (fun i -> acc := i :: !acc);
  List.rev !acc

let pp fmt t =
  Format.fprintf fmt "{";
  let first = ref true in
  iter_set t (fun i ->
      if not !first then Format.fprintf fmt ", ";
      first := false;
      Format.fprintf fmt "%d" i);
  Format.fprintf fmt "}"

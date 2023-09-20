(** Source locations and contiguous spans within a file. *)

(** A position in a source file (1-indexed line/column, 0-indexed byte offset). *)
type pos = {
  line : int;
  col : int;
  offset : int;
}

(** A half-open span [start, end_) in a named file. *)
type t = {
  file : string;
  start : pos;
  end_ : pos;
}

val dummy_pos : pos
val dummy : t

val make : file:string -> start:pos -> end_:pos -> t
val of_positions : file:string -> pos -> pos -> t

(** Merge two spans in the same file into their covering range.
    If the files differ, the first span is returned unchanged. *)
val merge : t -> t -> t

(** Merge a non-empty list of spans. *)
val merge_list : t list -> t

val start_line : t -> int
val start_col : t -> int
val end_line : t -> int
val end_col : t -> int
val start_offset : t -> int
val end_offset : t -> int
val file : t -> string
val length : t -> int
val is_dummy : t -> bool
val is_empty : t -> bool
val contains_offset : t -> int -> bool
val contains_pos : t -> pos -> bool
val equal : t -> t -> bool
val compare : t -> t -> int

val to_string : t -> string
val pp : Format.formatter -> t -> unit
val pp_pos : Format.formatter -> pos -> unit

(** Advance a position by a single character. *)
val advance_pos : pos -> ch:char -> pos

(** Advance a position by every character of [s]. *)
val advance_string : pos -> string -> pos

(** Build a span covering [text] starting at [start] in [file]. *)
val spanning : file:string -> start:pos -> text:string -> t

(** Extract the substring of [source] covered by the span. *)
val slice : source:string -> t -> string

(** Return the 1-indexed line numbers touched by the span. *)
val line_range : t -> int * int

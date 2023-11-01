type t
val compute : Mir.func -> t
val idom_of : t -> Mir.label -> Mir.label option
val dominates : t -> Mir.label -> Mir.label -> bool
val dominance_frontier : t -> Mir.label -> Mir.label list
val children_of : t -> Mir.label -> Mir.label list
val dominator_tree_preorder : t -> Mir.label list
val iterated_dominance_frontier : t -> Mir.label list -> Mir.label list
val pp : Format.formatter -> t -> unit

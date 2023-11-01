val predecessors : Mir.func -> Mir.label -> Mir.label list
val successors : Mir.func -> Mir.label -> Mir.label list
val reverse_postorder : Mir.func -> Mir.label list
val reachable : Mir.func -> Mir.label list
val remove_unreachable : Mir.func -> Mir.func

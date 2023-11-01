val convert : Mir.func -> Mir.func
val convert_program : Mir.program -> Mir.program
val eliminate_trivial_phis : Mir.func -> Mir.func
val place_phis : Mir.func -> Dominators.t -> Mir.func
val collect_defs : Mir.func -> (Mir.vreg, Mir.label list) Hashtbl.t

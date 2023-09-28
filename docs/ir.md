# Intermediate representations

Glyph lowers programs through two IRs before bytecode.

## HIR

High-level IR is a desugared lambda calculus:

- explicit binders
- no surface sugar (`if` becomes match or branch primitives)
- patterns still present until `Pattern_compile` rewrites them
- names are `Ident.t` values, already resolved enough for typing

HIR exists so type-directed desugaring and pattern compilation can happen
before we commit to a CFG shape.

## MIR

Mid-level IR is an SSA control-flow graph:

```
func f(params):
  block entry:
    %0 = ...
    %1 = add %0, 1
    branch %1, then, else
  block then:
    ...
    return %r
```

Instructions are ordinary operations plus φ-nodes at join points. Terminators
end blocks (`jump`, `branch`, `switch`, `return`, `tail_call`).

### Why SSA

Constant propagation, dead-code elimination, and value numbering become
local rewrites when each name has a single definition. Dominance frontiers
tell us exactly where φs must appear; the rename pass makes those facts
concrete.

## Bytecode

Bytecode is not SSA. Register allocation (linear scan or a slot map) flattens
values into a finite register file per prototype, and labels become numeric
instruction pointers. From here the VM is a straightforward fetch/decode/execute
loop.

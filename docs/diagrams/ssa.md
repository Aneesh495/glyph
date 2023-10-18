# SSA construction diagram

```mermaid
flowchart TD
  CFG[CFG of a function] --> DOM[Compute dominators]
  DOM --> DF[Dominance frontiers]
  DF --> PHI[Insert φ nodes]
  PHI --> REN[Rename / stack walk]
  REN --> SSA[SSA MIR]
```

```mermaid
flowchart TD
  E[entry] --> C{cond?}
  C -->|true| T[then\nx3 = …]
  C -->|false| F[else\nx4 = …]
  T --> J["join\nx5 = φ(then:x3, else:x4)"]
  F --> J
```

Implementation notes live in `lib/mir/dominators.ml` and `lib/mir/ssa.ml`.
The construction follows the Cytron et al. playbook: frontiers decide where
φs appear; the rename pass threads version stacks per variable.

Prose: [../ir.md](../ir.md).

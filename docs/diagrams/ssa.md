```mermaid
flowchart TD
  CFG[CFG of a function] --> DOM[Compute dominators]
  DOM --> DF[Dominance frontiers]
  DF --> PHI[Insert φ nodes]
  PHI --> REN[Rename / stack walk]
  REN --> SSA[SSA MIR]
```

Implementation notes live in `lib/mir/dominators.ml` and `lib/mir/ssa.ml`.
The construction follows the Cytron et al. playbook: frontiers decide where
φs appear; the rename pass threads version stacks per variable.

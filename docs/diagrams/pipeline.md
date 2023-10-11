# Pipeline diagrams

Embeddable Mermaid diagrams for Glyph docs and READMEs. No prose beyond short
captions.

## End-to-end compiler pipeline

```mermaid
flowchart LR
  SRC[".gl source"] --> LEX[Lexer]
  LEX --> PAR[Parser]
  PAR --> AST[Surface AST]
  AST --> HM[HM inference]
  HM --> TAST[Typed AST]
  TAST --> DES[Desugar]
  DES --> PAT[Pattern compile]
  PAT --> HIR[HIR]
  HIR --> LOW[Lower]
  LOW --> SSA[SSA]
  SSA --> OPT[Opts]
  OPT --> EMIT[Codegen]
  EMIT --> BC["Bytecode .gbc"]
  BC --> VM[VM + GC]
```

## Package dependencies

```mermaid
flowchart TB
  util[glyph_util]
  syntax[glyph_syntax]
  types[glyph_types]
  hir[glyph_hir]
  mir[glyph_mir]
  opt[glyph_opt]
  codegen[glyph_codegen]
  vm[glyph_vm]
  driver[glyph_driver]

  syntax --> util
  types --> util
  types --> syntax
  hir --> util
  hir --> syntax
  hir --> types
  mir --> util
  mir --> hir
  opt --> util
  opt --> mir
  codegen --> util
  codegen --> mir
  vm --> util
  vm --> codegen
  driver --> syntax
  driver --> types
  driver --> hir
  driver --> mir
  driver --> opt
  driver --> codegen
  driver --> vm
```

## Type inference flow

```mermaid
flowchart TD
  A[Infer expression] --> B{Form?}
  B -->|Var| I[Instantiate scheme]
  B -->|App| C[Infer fun + arg]
  C --> U[Unify fun with arg → β]
  B -->|Let| D[Infer RHS]
  D --> G[Generalize free αs]
  G --> E[Infer body with x:σ]
  B -->|Lam| F[Fresh α for binder]
  F --> H[Infer body]
  B -->|Match| M[Infer scrutinee + patterns]
  M --> BR[Unify branch results]
```

## Pattern matrix specialization

```mermaid
flowchart TB
  M["Match matrix"] --> C[Choose column]
  C --> S[Specialize by constructor]
  S --> T1[Subtree Nil]
  S --> T2[Subtree Cons]
  T1 --> L1[Leaf: binds + RHS]
  T2 --> L2[Leaf: binds + RHS]
```

## SSA construction

```mermaid
flowchart LR
  HIR[HIR tree] --> LOW[Lower to CFG]
  LOW --> DOM[Dominators + DF]
  DOM --> PHI[Insert φ]
  PHI --> REN[Rename]
  REN --> SSA[SSA MIR]
```

## Diamond CFG and φ

```mermaid
flowchart TD
  E[entry] --> C{cond?}
  C -->|true| T[then\nx3 = …]
  C -->|false| F[else\nx4 = …]
  T --> J["join\nx5 = φ(then:x3, else:x4)"]
  F --> J
```

## Optimization pipeline

```mermaid
flowchart LR
  MIR[SSA MIR] --> S[Simplify]
  S --> CP[Copy prop]
  CP --> CT[Const prop]
  CT --> SCCP[SCCP]
  SCCP --> CSE[CSE]
  CSE --> IN[Inline]
  IN --> S2[Simplify]
  S2 --> DCE[DCE]
  DCE --> OUT[Optimized MIR]
```

## SCCP executable edges

```mermaid
flowchart TD
  E[entry] -->|executable| T[then]
  E -.->|non-executable| F[else]
  T --> J[join]
  F -.-> J
```

## Call frames and heap

```mermaid
flowchart TB
  subgraph Stack
    F2["frame: callee\nregs + env"]
    F1["frame: main\nregs"]
  end
  F2 --> F1
  HEAP[(Heap)]
  F2 -.-> HEAP
  F1 -.-> HEAP
```

## Semi-space GC flip

```mermaid
flowchart LR
  subgraph Before
    FS["fromspace: live + garbage"]
    TS["tospace: empty"]
  end
  subgraph After
    FS2["fromspace: discarded"]
    TS2["tospace: compacted live"]
  end
  Before --> After
```

## Cheney collection sequence

```mermaid
sequenceDiagram
  participant Mut as Mutator
  participant GC as Collector
  participant From as Fromspace
  participant To as Tospace
  Mut->>GC: allocation threshold
  GC->>GC: flip spaces
  GC->>To: copy roots
  loop scan until scan = free
    GC->>From: chase child
    GC->>To: copy if unforwarded
    GC->>To: fix field pointer
  end
  GC->>Mut: resume
```

## CLI stage selection

```mermaid
flowchart LR
  TC[typecheck] --> HM[stop after HM]
  DM[dump-mir] --> SSA[stop after SSA/opts]
  CP[compile] --> GBC[".gbc"]
  RN[run] --> VM[VM]
  DA[disasm] --> GBC
```

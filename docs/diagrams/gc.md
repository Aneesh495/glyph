# Semi-space GC diagram

```mermaid
stateDiagram-v2
  [*] --> Mutating
  Mutating --> Collecting: allocation fails
  Collecting --> EvacuateRoots
  EvacuateRoots --> ScanTospace
  ScanTospace --> ScanTospace: gray work left
  ScanTospace --> Mutating: flip complete
```

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

Fromspace holds the old heap; tospace is empty at flip. Roots are frames'
registers plus globals. Every evacuated object installs a forwarding pointer
so diamond-shaped references converge.

Prose: [../vm.md](../vm.md).

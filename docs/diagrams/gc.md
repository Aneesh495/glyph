```mermaid
stateDiagram-v2
  [*] --> Mutating
  Mutating --> Collecting: allocation fails
  Collecting --> EvacuateRoots
  EvacuateRoots --> ScanTospace
  ScanTospace --> ScanTospace: gray work left
  ScanTospace --> Mutating: flip complete
```

Fromspace holds the old heap; tospace is empty at flip. Roots are frames'
registers plus globals. Every evacuated object installs a forwarding pointer
so diamond-shaped references converge.

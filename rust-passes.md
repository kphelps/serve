time: 0.007     parsing
time: 0.000     recursion limit
time: 0.000     crate injection
time: 0.000     plugin loading
time: 0.000     plugin registration
time: 0.170     expansion
time: 0.000     maybe building test harness
time: 0.000     maybe creating a macro crate
time: 0.000     checking for inline asm in case the target doesn't support it
time: 0.001     early lint checks
time: 0.000     AST validation
time: 0.023     name resolution
time: 0.001     complete gated feature checking
time: 0.006     lowering ast -> hir
time: 0.001     indexing hir
time: 0.000     attribute checking
time: 0.000     language item collection
time: 0.001     lifetime resolution
time: 0.000     looking for entry point
time: 0.000     looking for plugin registrar
time: 0.002     region resolution
time: 0.000     loop checking
time: 0.000     static item recursion checking
time: 0.019     compute_incremental_hashes_map
time: 0.000     load_dep_graph
time: 0.001     stability index
time: 0.003     stability checking
time: 0.013     type collecting
time: 0.000     variance inference
time: 0.000     impl wf inference
time: 0.043     coherence checking
time: 0.011     wf checking
time: 0.007     item-types checking
time: 0.420     item-bodies checking
time: 0.010     const checking
time: 0.002     privacy checking
time: 0.001     intrinsic checking
time: 0.000     effect checking
time: 0.005     match checking
time: 0.008     liveness checking
time: 0.007     rvalue checking
time: 0.026     MIR dump
  time: 0.004   SimplifyCfg
  time: 0.005   QualifyAndPromoteConstants
  time: 0.012   TypeckMir
  time: 0.000   SimplifyBranches
  time: 0.002   SimplifyCfg
time: 0.023     MIR cleanup and validation
time: 0.092     borrow checking
time: 0.000     reachability checking
time: 0.002     death checking
time: 0.000     unused lib feature checking
time: 0.021     lint checking
time: 0.013     resolving dependency formats
  time: 0.000   NoLandingPads
  time: 0.002   SimplifyCfg
  time: 0.004   EraseRegions
  time: 0.001   AddCallGuards
  time: 0.033   ElaborateDrops
  time: 0.000   NoLandingPads
  time: 0.007   SimplifyCfg
  time: 0.000   Inline
  time: 0.004   InstCombine
  time: 0.001   Deaggregator
  time: 0.000   CopyPropagation
  time: 0.004   SimplifyLocals
  time: 0.001   AddCallGuards
  time: 0.000   PreTrans
time: 0.059     MIR optimisations
  time: 0.000   write metadata
  time: 0.458   translation item collection
  time: 0.090   codegen unit partitioning
  time: 0.040   internalize symbols
time: 1.926     translation
time: 0.000     assert dep graph
time: 0.000     serialize dep graph
  time: 0.140   llvm function passes [0]
  time: 0.102   llvm module passes [0]
  time: 2.713   codegen passes [0]
  time: 0.000   codegen passes [0]
time: 2.957     LLVM passes
time: 0.000     serialize work products
  time: 0.493   running linker
time: 1.260     linking

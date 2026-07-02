# VIP - Vibe Prover

VIP (Vibe Prover) is an experimental automatic theorem prover for first-order logic. It is implemented in OCaml and targets the FOF division of CASC, with support for problems with equality (FEQ) and without equality (FNE). The system was previously known internally as IP; the `ip` executable name is kept as a compatibility alias.

## Architecture

VIP is a saturation-based first-order prover. Problems are read in TPTP syntax, expanded through `include(...)` directives, clausified into clausal normal form, and processed using a given-clause saturation loop. Passive clauses are selected according to the active strategy, simplified, moved to the active set, and used to generate new clauses.

The prover implements resolution, factoring, equality resolution, equality factoring, and superposition-style equality inferences. It also uses standard simplifications such as tautology deletion, demodulation, forward simplification, subsumption, subsumption resolution, condensation, and contextual literal cutting.

VIP contains two main saturation engines. The legacy engine is simple and robust on several non-equality problems. The modern engine provides stronger indexing, simplification, equality handling, and clause selection. The competition portfolios combine both engines because they are complementary on the current benchmarks.

The prover also includes experimental AVATAR-style splitting, SInE-inspired axiom selection, layered clause selection, and limited deduction-modulo-inspired preprocessing for definitional rewrite rules.

## Strategies

VIP uses a sequential portfolio. Each strategy receives a fraction of the global time limit. The main CASC-oriented portfolio is `casc-150`.

The portfolio selects schedules from general syntactic features of the input, such as clause count, equality density, unit-clause ratio, and literal polarity. It does not depend on problem names, paths, or known solutions.

Important strategy components include:

- FEQ-oriented modern saturation with equality simplification;
- SInE axiom-selection probes with different selection widths;
- legacy recovery stages for large FNE problems;
- legacy-guided modern stages;
- layered and age/weight passive-clause selection;
- conservative AVATAR stages;
- clause and time limits to control memory use.

## Implementation

VIP is implemented in OCaml and built with Dune. The installed executable is `vip`; `ip` is also installed as an alias for compatibility.

TPTP include files are resolved relative to the problem file, and otherwise relative to the TPTP root supplied by `--tptp` or the `TPTP` environment variable. For example, `include('Axioms/CSR002+2.ax')` is resolved below the TPTP root directory, not by passing the `Axioms` directory itself.

The prover uses feature-vector indexing for subsumption candidates, discrimination-style indexing for selected rewriting operations, KBO-style term ordering, and derivation tracking for TSTP-style proof output.

A typical competition-style invocation is:

```bash
vip \
  --competition-output \
  --proof-tstp \
  --portfolio casc-150 \
  --time-limit 120 \
  --max-clauses 75000 \
  --tptp /path/to/TPTP-root \
  /path/to/problem.p
```

When a refutation is found, VIP emits SZS status information and a TPTP/TSTP-style CNF refutation delimited by `% SZS output start` and `% SZS output end`.

## Expected Competition Performance

VIP is an experimental FOF prover. It is not expected to match mature systems such as Vampire, E, or Zipperposition, but it is expected to solve a meaningful subset of CASC-style FOF problems.

The strongest current areas are unsatisfiable FOF problems where SInE axiom selection, equality simplification, and staged legacy/modern saturation interact well. The main remaining work before competition submission is proof validation with TPTP4X/GDV, StarExec packaging, and further portfolio tuning.

## References and Influences

VIP is inspired by techniques used in saturation-based provers such as E, Vampire, Zipperposition, and Drodi, including given-clause saturation, superposition, SInE axiom selection, AVATAR-style splitting, demodulation, subsumption, layered clause selection, and age/weight passive-clause scheduling.

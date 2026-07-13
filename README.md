# VIP - Vibe Prover

VIP (Vibe Prover) is an experimental automatic theorem prover for first-order logic. It targets the FOF division of CASC, with support for problems with equality (FEQ) and without equality (FNE).

The system was developed as a short-term experiment in building a complete first-order prover with extensive LLM-assisted implementation. The goal was not to wrap an existing prover, but to implement the main components of a standalone saturation-based ATP system within a limited development window, then make it executable in the standard CASC/StarExec setting. The system was previously known internally as IP; the `ip` executable name is kept as a compatibility alias.

## Architecture

VIP is a saturation-based first-order prover. Input problems are read in TPTP syntax, expanded through `include(...)` directives, clausified into clausal normal form, and processed using a given-clause saturation loop. Passive clauses are selected according to the active strategy, simplified, moved to the active set, and used to generate new clauses.

The prover implements resolution, factoring, equality resolution, equality factoring, and superposition-style equality inferences. It also uses standard simplifications such as tautology deletion, demodulation, forward simplification, subsumption, subsumption resolution, condensation, and contextual literal cutting.

VIP contains two main saturation engines. The legacy engine is simple and robust on several non-equality problems. The modern engine provides stronger indexing, simplification, equality handling, and clause selection. The competition portfolios combine both engines because they are complementary on the development benchmarks.

The prover also includes experimental AVATAR-style splitting, SInE-inspired axiom selection, layered clause selection, and limited deduction-modulo-inspired preprocessing for definitional rewrite rules. The deduction-modulo-inspired component detects some definitional equivalences and uses them as rewrite-oriented support during clausification and polarized search. It is deliberately restricted and is used as a pragmatic search feature rather than as a full deduction modulo implementation.

## Strategies

VIP uses a sequential portfolio. Each strategy receives a fraction of the global time limit. The main CASC-oriented portfolio is `casc-150`.

The portfolio selects schedules from general syntactic features of the input, such as clause count, equality density, unit-clause ratio, and literal polarity. It does not depend on problem names, paths, headers, or known solutions. This is important for CASC, where TPTP-based problems may be obfuscated and where problem-specific strategy selection is not allowed.

Important strategy components include:

- FEQ-oriented modern saturation with equality simplification;
- SInE axiom-selection probes with different selection widths;
- legacy recovery stages for large FNE problems;
- legacy-guided modern stages;
- layered and age/weight passive-clause selection;
- conservative AVATAR stages;
- clause and time limits to control memory use.

The competition run script uses `casc-150` with a default generated-clause limit of 75000. The CASC wall-clock time limit is not hardcoded in the executable package. It can be supplied by the StarExec configuration as a wrapper argument or through a time-limit environment variable; if no internal time limit is supplied, StarExec's external wall-clock limit controls termination.

## Implementation

VIP is implemented in OCaml and built with Dune. The installed executable is `vip`; `ip` is also installed as an alias for compatibility.

TPTP include files are resolved relative to the problem file, and otherwise relative to the TPTP root supplied by `--tptp` or the `TPTP` environment variable. For example, `include('Axioms/CSR002+2.ax')` is resolved below the TPTP root directory, not by passing the `Axioms` directory itself.

The prover uses feature-vector indexing for subsumption candidates, discrimination-style indexing for selected rewriting operations, KBO-style term ordering, and derivation tracking for TSTP-style proof output. Proof output is enabled in the StarExec package. When a refutation is found, VIP emits SZS status information and a TPTP/TSTP-style CNF refutation delimited by `% SZS output start` and `% SZS output end`.

The executable package provides `bin/starexec_run_FOF` and `bin/starexec_run_default`. The run script expects the problem file as its first argument, writes all result information to standard output, and uses the `TPTP` environment variable when it is present.

A typical competition-style invocation is:

```bash
vip \
  --competition-output \
  --proof-tstp \
  --portfolio casc-150 \
  --time-limit <seconds> \
  --max-clauses 75000 \
  --tptp /path/to/TPTP-root \
  /path/to/problem.p
```

## Expected Competition Performance

VIP is an experimental FOF prover developed under a short time constraint. It is not expected to match mature systems such as Vampire, E, or Zipperposition. The expected performance is a meaningful subset of CASC-style FOF problems, with best results on unsatisfiable problems where axiom selection, equality simplification, and staged saturation interact well.

On the development CASC-style FOF benchmark used during the project, the DMT-based release line solved roughly half of the tested problems at 120 seconds. The system is stronger on FEQ than the earliest versions of the project, but equality-heavy problems remain the main bottleneck. The proof output for the required FOF sample problem `SEU140+2` has been checked locally with TPTP4X and GDV.

## References and Influences

VIP is influenced by standard saturation-based theorem-proving techniques and by the design of systems such as E, Vampire, Zipperposition, and Drodi. The main ideas used in VIP include given-clause saturation, resolution and superposition-style equality reasoning, demodulation, subsumption, SInE-style axiom selection, AVATAR-style splitting, layered clause selection, and age/weight passive-clause scheduling.

The current implementation should be understood as an experimental prover rather than a mature ATP system. Its purpose in CASC is to evaluate how far a compact, mostly LLM-assisted implementation can go when combined with conventional first-order proving techniques and empirical portfolio tuning.

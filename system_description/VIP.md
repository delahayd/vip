# VIP 2026

VIP development team  
University of Montpellier / LIRMM, France

## Architecture

VIP 2026 is an automatic theorem prover for first-order logic in TPTP FOF
syntax. It was developed under a short time constraint as a standalone prover,
with extensive LLM-assisted implementation. The submitted system does not call
another ATP system as a backend.

The core is a given-clause saturation loop. Problems are parsed from TPTP
files, include directives are expanded, formulae are clausified, and clauses
are processed by resolution-style and superposition-style rules. The main
inferences are binary resolution, factoring, equality resolution, equality
factoring, and equality-oriented paramodulation/superposition steps.

Standard simplifications include tautology deletion, demodulation, forward
simplification, subsumption, subsumption resolution, condensation, and
contextual literal cutting. VIP contains a simple legacy engine and a more
recent engine with stronger equality handling, indexing, and clause selection;
the competition portfolio combines both.

The system also includes SInE-style axiom selection, conservative AVATAR-style
splitting, layered clause selection, and limited deduction-modulo-inspired
preprocessing for selected definitional equivalences.

## Strategies

VIP uses a sequential portfolio. The main CASC-oriented portfolio is
`casc-150`. Each stage receives a fixed fraction of the available time and runs
with fixed options for the FOF division. The submitted StarExec script uses the
same command line for every FOF problem.

Strategy scheduling is based only on general syntactic characteristics of the
input, such as equality density, clause shape, unit-clause ratio, polarity
patterns, and symbol occurrence information. It is not based on problem names,
file paths, comments, TPTP headers, or stored information about individual
problems or their solutions.

The portfolio includes FEQ-oriented equality stages, FNE recovery stages using
the legacy engine, SInE axiom-selection stages with different widths,
legacy-guided modern stages, layered age/weight passive selection, and
conservative splitting stages. The submitted StarExec script does not hardcode
the CASC time limit; it accepts the announced wall-clock budget as a wrapper
argument or environment value. The default generated-clause limit is 75000.

## Implementation

VIP is implemented in OCaml and is built with Dune. The executable installed in
the StarExec package is `vip`; `ip` is kept as a compatibility alias in the
source tree. The delivered binary is statically linked.

TPTP include files are resolved according to the standard TPTP convention:
first relative to the problem file, and otherwise relative to the TPTP root
supplied by the `TPTP` environment variable. Internal data structures include
feature-vector style indices for subsumption candidates, discrimination-style
indices for rewriting candidates, and KBO-style term ordering for equality
reasoning.

In competition mode, VIP writes all result information to standard output,
emits an SZS status line, and, when a refutation is found, prints a
TPTP/TSTP-style proof delimited by SZS output markers. The required FOF sample
problem `SEU140+2` has been tested locally with TPTP4X and GDV.

The StarExec package provides `bin/starexec_run_FOF` and
`bin/starexec_run_default`. The run script expects the problem file as its
first argument and relies on the StarExec/TPTP environment for include-file
resolution and resource enforcement.

## Expected Competition Performance

VIP is expected to solve a useful subset of FOF theorem problems, with better
performance on unsatisfiable problems where axiom selection, equality
simplification, and staged saturation interact well. It is not expected to
match mature ATP systems such as Vampire, E, or Zipperposition.

On the development CASC-style FOF benchmark used during the project, the
DMT-based release line solved roughly half of the tested problems at 120
seconds. The submitted configuration is intended to use the wall-clock limit
announced by the CASC organizers and the CASC memory environment.

## References

No system paper is available for this first VIP release. The implementation
follows standard saturation-based ATP techniques used in systems such as E,
Vampire, Zipperposition, and Drodi: given-clause saturation, resolution,
superposition-style equality reasoning, demodulation, subsumption, SInE-style
axiom selection, AVATAR-style splitting, layered clause selection, and
age/weight scheduling.

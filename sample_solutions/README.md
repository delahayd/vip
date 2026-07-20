# VIP FOF Sample Solution

The file `VIP_FOF_SEU140+2.out` is VIP's required CASC FOF sample solution for
`SEU140+2`. It was generated through the packaged `starexec_run_FOF` wrapper
using the static executable built from release commit `da00d0a` and the
`casc-150` portfolio.

The output is a TPTP CNF refutation. Formulae copied from the problem retain
their original TPTP names and use `file(...)` source annotations. Variables
named `V0`, `V1`, and so on are implicitly universally quantified in CNF
formulae. Symbols named `sk9`, `sk11`, and so on are fresh Skolem symbols.

`cnf_transformation` records conversion from an input FOF formula to clauses.
The clauses produced from the conjecture have role `negated_conjecture` and
use `status(cth)`. `indexed_superposition` is ordinary superposition whose
candidate terms were retrieved through VIP's term index. The remaining proof
steps use resolution or equality resolution. The final clause is `$false`.

The proof was checked locally with TPTP4X 9.2.1 and with GDV using the CASC
structural flags `-d -u`. TPTP4X accepted the syntax, and GDV reported:

```text
% SZS status VerifiedGood
```

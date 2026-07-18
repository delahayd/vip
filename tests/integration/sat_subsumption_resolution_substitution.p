cnf(source,axiom,(p(X) | q(X))).
cnf(target,axiom,(~p(Y) | q(a) | r(Y))).
cnf(witness_q,axiom,(~q(a))).
cnf(witness_r,axiom,(~r(b))).
cnf(distinct,axiom,(a != b)).

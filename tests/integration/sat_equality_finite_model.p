% Two-element model: a=0, b=1, f swaps 0 and 1, p={0}, q={1}, and r is
% identity. This exercises equality, universal variables, and demodulation.
cnf(a_diff_b, axiom, a != b).
cnf(f_a, axiom, f(a) = b).
cnf(f_b, axiom, f(b) = a).
cnf(p_a, axiom, p(a)).
cnf(not_p_b, axiom, ~p(b)).
cnf(q_b, axiom, q(b)).
cnf(not_q_a, axiom, ~q(a)).
cnf(partition_total, axiom, (p(X) | q(X))).
cnf(partition_disjoint, axiom, (~p(X) | ~q(X))).
cnf(p_maps_to_q, axiom, (~p(X) | q(f(X)))).
cnf(q_maps_to_p, axiom, (~q(X) | p(f(X)))).
cnf(r_reflexive, axiom, r(X,X)).
cnf(r_only_identity, axiom, (~r(X,Y) | X = Y)).
cnf(identity_is_r, axiom, (X != Y | r(X,Y))).

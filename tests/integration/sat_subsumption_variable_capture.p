% A model exists with distinct a and b, p(x,x) true, r(x,x) true, and
% r(a,b) false. Variable capture in subsumption resolution used to derive the
% invalid universal clause r(X,Y), making this set appear inconsistent.
cnf(diagonal_p, axiom, p(X,X)).
cnf(diagonal_implies_r, axiom, (~p(X,Y) | r(X,Y))).
cnf(off_diagonal_not_r, axiom, ~r(a,b)).

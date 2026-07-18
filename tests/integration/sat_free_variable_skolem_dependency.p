% Satisfiable with a two-element domain: each X chooses itself as witness Y.
fof(witness,axiom,?[Y]:p(X,Y)).
fof(characterize,axiom,![U,V]:(p(U,V)<=>U=V)).
fof(distinct,axiom,a!=b).

fof(ax1, axiom, zero = e).
fof(ax2, axiom, ! [X] : plus(e,X) = X).
fof(ax3, axiom, ! [X] : plus(X,e) = X).
fof(ax4, axiom, p(plus(zero,plus(a,e)))).
fof(goal, conjecture, p(a)).

To do
-----

* My procedure for going from non-commuting operators to matrices should be reviewed.  I worry that I am relying on the noncommutative multiply function to behave nicely with matrices.  The density operator contains spin operators multiplied together using the noncommutative times operator, Ix**Sy, while in the step just before substituting matrices I introduce a dot product, Tr[rho . (Ix + I Iy)].  This is an obvious kluge!    

* Get ``Evolver`` to handle the case of a translation, where the commutators after the first one are zero.  I missed this case!  Presently, this fails ::

    H$1 = F (Exp[-i \[Phi]] aL + Exp[i \[Phi]] aL );
    Evolver[H$1, t, aR] 
    
To get this to work, the ``Comm`` function needs to be smart enough to factor *functions* of scalars out front of the commutator too.
To do
-----

Get ``Evolver`` to handle the case of a translation, where the commutators after the first one are zero.  I missed this case!

Presently, this fails ::

    H$1 = F (Exp[-i \[Phi]] aL + Exp[i \[Phi]] aL );
    Evolver[H$1, t, aR] 
    
To get this to work, the ``Comm`` function needs to be smart enough to factor *functions* of scalars out front of the commutator too.
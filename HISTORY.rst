History
-------

2022/04/25 jam99
^^^^^^^^^^^^^^^^

* Rewrite ``UniDyn--Demo-02.nb`` through ``UniDyn--Demo-05.nb``.  See the commit messages for a short summary of major code revisions required for each file.

* Regenerate all the associated ``.pdf`` files.  Clear the outputs of the ``.nb`` files to save space.

2022/03/07 jam99
^^^^^^^^^^^^^^^^

* Upgrade from Mathematica 12.3.1 to 13.0.0.0.  Rewrite ``UniDyn--Demo-01.nb`` and ``UniDyn--Demo-Scratch.nb`` to replace the reserved name ``tr`` with ``test$report``.

2022/03/05 jam99
^^^^^^^^^^^^^^^^

* Rebooting this package, after a long hiatus, now that I am teaching graduate quantum mechanics again.

* Upgraded to *Mathematica* version 12.3.0.1 and NCAlgebra 5.0.6.  These upgrades broke (a) one unit test in OpCreate-tests.m and (b) all the operator rotations (half the tests) in Evolve-tests.m.  The failure in (a) was patched by explicitly declaring a variable to be non-commutative. It seems like lower-case variables are not defaulting, as expected, to be non-commutative in the NCAlgebra package; the patch is a workaround.  The failure is (b) was fixed by replacing NCExpand with NonCommutativeMultiply`NCExpand in Evolve.m.  I note that I had previously figured out that CommutativeQ needs to be called as NonCommutativeMultiply`CommutativeQ inside the private part of my package, so I am puzzled why I didn't wrap the call to NCExpand similarly.

* Rewrote the Demo notebooks Rewrite UniDyn--Demo-01.nb and UniDyn--Demo-Scratch.nb so that you now have to input the filepath for both the NCAlgebra and UniDyn installation locations.  TO DO: Rewrite the other notebooks too.

2016/09/14 jam99
^^^^^^^^^^^^^^^^

* One of the unit tests in Evolve-tests.m broke when I upgraded from *Mathematica* version 10 to 11.  Tweak Evolve-tests.m so the "off-resonance evolution of Iz" test case passes in *Mathematica* version 11.

* Fixed bug the definition of the rules for simplifying harmonic oscillator Q and P operators.  The symptom of the bug was that when P was rewritten in terms of raising and lowering operators and then converted back to position and momentum operators, the result was -P and not P.  Add a unit test to Evolve-tests.m to check that the conversion and back-conversion returns Q and P as expected.

* Added unit tests covering harmonic oscillator "postion kick" and "momentum kick" Hamiltonians. 

2016/05/11 jam99
^^^^^^^^^^^^^^^^

Notebook UniDyn--Demo-05.nb added to illustrate, for two weakly coupled spins, the 1D spectrum, the INEPT experiment, the heteronuclear COSY, and the homonuclear COSY.

2016/05/05 jam99
^^^^^^^^^^^^^^^^

Add a notebook UniDyn--Demo-03.nb, that illustrates how to take the digital Fourier transform of data in *Mathematica*, and a notebook UniDyn--Demo-04.nb that calculates the signal analytically for two weakly coupled spins, calculates a numerical signal, and Fourier transforms it.

2016/01/11 jam99
^^^^^^^^^^^^^^^^

Tweaks to make the package more compatible with *Mathematica* ver 8.  

* Modify how equations are passed to ``DSolve[]`` so that ``Evolver[]`` works in *Mathematica* version 8.

* Unit-testing functions are new in *Mathematica* version 10; consequently, the unit tests in the ``*-tests.nb`` files and in ``UniDyn--Demo-01.nb`` will not run correctly in a *Mathematica* version below 10.  In each testing file, modify the unit-testing function so that the function created  depends on the *Mathematica* version.  When the unit-testing files are executed in a *Mathematica* version below 10, the unit tests will now print a ``Pass`` or ``Fail`` message.  For now, the unit tests in ``UniDyn--Demo-01.nb`` still only work in *Mathematica* version 10; in versions below 10, the only way to do unit testing is to open up each of the ``*-tests.nb`` files and run them one at a time.


2016/01/10 jam99
^^^^^^^^^^^^^^^^

Minimal working version of ``Evolver`` with unit tests and two example notebooks.
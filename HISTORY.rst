History
-------

2016/05/05 jam99
^^^^^^^^^^^^^^^^

Add a notebook UniDyn--Demo-03.nb that illustrates how to take the digital Fourier transform of data in *Mathematica*.

2016/01/11 jam99
^^^^^^^^^^^^^^^^

Tweaks to make the package more compatible with *Mathematica* ver 8.  

* Modify how equations are passed to ``DSolve[]`` so that ``Evolver[]`` works in *Mathematica* version 8.

* Unit-testing functions are new in *Mathematica* version 10; consequently, the unit tests in the ``*-tests.nb`` files and in ``UniDyn--Demo-01.nb`` will not run correctly in a *Mathematica* version below 10.  In each testing file, modify the unit-testing function so that the function created  depends on the *Mathematica* version.  When the unit-testing files are executed in a *Mathematica* version below 10, the unit tests will now print a ``Pass`` or ``Fail`` message.  For now, the unit tests in ``UniDyn--Demo-01.nb`` still only work in *Mathematica* version 10; in versions below 10, the only way to do unit testing is to open up each of the ``*-tests.nb`` files and run them one at a time.


2016/01/10 jam99
^^^^^^^^^^^^^^^^

Minimal working version of ``Evolver`` with unit tests and two example notebooks.
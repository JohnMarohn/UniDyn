UniDyn
======

Unitary evolution of quantum mechanical operators

Requirements
------------

This package requires the *Mathematica* NCAlgebra package, version NCAlgebra 4.0.6, available for download from the University of San Diego non-commutative algebra group at http://math.ucsd.edu/~ncalg/download10.shtml.   To install the package, download  NC2015.tar and unpack the file by clicking on it.   The package should unpack into the ``NC/`` directory.  In this directory you should see the *Mathematica* notebook ``InstallNCAlgebra.nb``.  Open up this notebook and run it to install the package.

Package files
-------------

upper directory
^^^^^^^^^^^^^^^

Example notebooks ::

    UniDyn--Demo-01.nb    loads ``NCAlgebra``, ``UniDyn``, and runs the unit tests


unidyn directory
^^^^^^^^^^^^^^^^

The package files are stored in the ``unidyn/`` directory.  These consist of *Mathematica* files ::

    UniDyn.m              master file; loads all the other package files
    OpCreate.m            CreateOperator[] and CreateScalar[] convenience functions

plus unit-testing files ::

    OpCreate-tests.m 
    

    
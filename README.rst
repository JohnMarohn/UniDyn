UniDyn
======

Summary
-------

We have developed *Mathematica* algorithms for symbolically calculating unitary transformations of quantum-mechanical operators. The algorithms obtain closed-form analytical results, do not rely on a matrix representation of operators, and are applicable to both bounded systems like coupled spins and unbounded systems like harmonic oscillators.  The transformations are *self derived* from the operators' underlying commutation relations and can therefore be carried out in a *basis free* way.

Here are some representative unitary-evolution calculations:

- Evolution of transverse, :math:`x`-axis spin magnetization in a magnetic field along the :math:`z` axis: :math:`e^{-i \, \omega t \, I_z} \: I_x \: e^{+i \, \omega t \, I_z} = I_x \, \sin{(\omega t)} + I_y \, \cos{(\omega t)}`.

- Evolution of transverse, :math:`x`-axis spin magnetization under a scalar coupling: :math:`e^{-i \, J t \, I_z S_z} \: I_x \: e^{+i \, J \, I_z S_z} = I_x \, \cos{(J t /2 )} + 2 I_y S_z \, \sin{(J t/2)}`.

- Evolution of the position operator under the harmonic oscillator Hamiltonian: :math:`e^{- i \omega t \frac{1}{2} (a^{\dagger} \, a + a \, a^{\dagger})} X e^{+i \omega t \frac{1}{2} (a^{\dagger} \, a + a \, a^{\dagger})} =  X \cos{(\omega t)} - P \sin{(\omega t)}`.

- Evolution of a spin raising operator under a spin-dependent translation: :math:`e^{g I_z (a^{\dagger} - a)} I_{+} e^{-g I_z (a^{\dagger} - a)} = e^{-g (a^{\dagger} - a)} I_{+}`.

The ``UniDyn`` package contains two versions of the algorithm and numerous associated functions needed to execute the algorithms.  These associated functions implement non-commutative multiplication, operator ordering, commutators, and symbolic operator inverses.

Requirements
------------

The package has been tested in the following environments:

* *Mathematica* versions 12.3 and 13.0.0.0

* macOS versions 14.7.1 (Sonoma) and 15.7.3 (Sequoia)

Installation
------------

Download this repository as a ``zip`` file.

Open up ``UniDyn--Demo-01.nb`` and follow the directions.  You will be asked to enter in the *Mathematica* notebook a string indicating the location where you downloaded the ``UniDyn`` package files to.  After entering this location string, evaluate the notebook to import ``UniDyn`` and run the 150+ unit tests.  If that all goes well, you are ready to run the other notebooks, starting with ``UniDyn--Demo-02.nb``.  At the top of each notebook you will have to re-enter the location strings.

Package files
-------------

upper directory
^^^^^^^^^^^^^^^

Example notebooks ::

    UniDyn--Demo-01.nb    Loads UniDyn and runs the unit tests.
    
    UniDyn--Demo-02.nb    Spin magnetization evolving under off-resonance, 
                          variable-phase rf irradiation.  Use Evolver to 
                          calculate the magnetization analytically, then make a 
                          vector plot of the magnetization under various 
                          resonance offsets and rf phases.

    UniDyn--Demo-03.nb    Demonstrates how to take the digital Fourier 
                          transform of one-dimensional and two-dimensional 
                          data. Examples illustrate the FT data-ordering 
                          problem, aliasing, apodization, and zero filling 
                          with one-dimensional data and the phase-twist 
                          lineshape observed when Fourier transforming 
                          two-dimensional data sets.
                           
    UniDyn--Demo-04.nb    Calculates magnetic resonance signal analytically for 
                          two weakly coupled spins, calculates a numerical 
                          signal, and Fourier transforms the numerical signal 
                          to reveal the *spectrum* of the two coupled spins.  
                           
    UniDyn--Demo-05.nb    Calculates the magnetic resonance signal analytically 
                          and numerically for two weakly coupled spins in the 
                          following experiments: INEPT polarization 
                          transfer, heteronuclear COSY, and homonuclear COSY.
                           
    UniDyn--Demo-06.nb    Compares the execution time for Evolver1 and Evolver2
                          and shows examples of Evolver2 involving one spin, two
                          spins, the harmonic oscillator, quantum optics, and
                          electron transfer.

Documentation ::

    UniDyn-doc.pdf        Documentation.  Most of the documentation is 
                          contained *inline*, in comments in the source code.
                          The source code is read directly into the tex 
                          document and typeset.
    

unidyn directory
^^^^^^^^^^^^^^^^

The package files are stored in the ``unidyn/`` directory.  

The package files consist of *Mathematica* files ::

    UniDyn.m     master file; loads all the other package files
    OpQ.m        CreateOperator[] and CreateScalar[] convenience functions
    Mult.m       NCSort[], SortedMult[], and MultSort[] functions to sort operators
    Comm.m       Comm[,] to implement the commutator function
    Inv.m        Inv[] to implement a symbolic operator inverse
    Spins.m      Angular momentum operators for a single spin (L = 1/2 or not)
    Osc.m        Raising and lowering operators for a single harmonic oscillator
    Evolve.m     Unitary evolution
    Evolver1.m   The Evolver1 algorithm
    Evolver2.m   The Evolver2 algorithm
    SpinBoson.m  Spin and harmonic-oscillator operators for quantum optics
  
and ::

    Matrices--two-spin-half.m    Matrices for the six angular momentum
                                 operators for a spin system consisting 
                                 of two I=1/2 spins

plus unit-testing files ::

    OpQ-tests.m 
    Mult-tests.m
    Comm-tests.m
    Inv-tests.m
    Spins-tests.m
    Osc-tests.m
    Evolve-tests.m
    Evolver1-tests.m
    Evolver2-tests.m
    SpinBoson-tests.m

study directory
^^^^^^^^^^^^^^^

The ``study/`` directory contains files used during the development of improvements and new features.

Other files
-----------

* History [`link <https://github.com/JohnMarohn/UniDyn/blob/master/HISTORY.rst>`__]

* To-do list [`link <https://github.com/JohnMarohn/UniDyn/blob/master/TODO.rst>`__]

* License [`link <https://github.com/JohnMarohn/UniDyn/blob/master/LICENSE>`__]


Packaging notes
---------------

Background reading
^^^^^^^^^^^^^^^^^^

I rely a lot on the ``UpSetDelayed[]`` function, ``:^=`` in shorthand.  The idea up an *upvalue* and a *downvalue* is explained pretty well in the article "Associating Definitions with Different Symbols" in the Wolfram Language Tutorial [#mma-updelayed]_.  

Creating a *Mathematica* package is not as well documented as I would expect.  While a list of functions used to create a *Mathematica* package can be found in the "Package Development" section of the Wolfram Language Guide [#MMA-packaging]_, a good example illustrating how to create a package is lacking in the *Mathematica* documentation.  The discussions at the *Mathematica* Stack Exchange are helpful.  The "Creating Mathematica packages" article [#MSE29324]_ is a quick and easy introduction to packaging.  The question "How can I return private members of a Mathematica package as the output of package functions without the ``PackageName`Private``` prefix?" is answered in a longer article [#MMA-packaging-1]_.

Privacy issues
^^^^^^^^^^^^^^

Creating a *Mathematica* package out of the ``UniDyn`` code was tricky.  The main reason for this was that lot of the functions in my package's ``.m`` files create *upvalues* for variables that are passed to the functions.  

The usual way to package a function is to do something like

.. code:: 

    BeginPackage["MyPackage`"]
    my$function::usage="f(a,b) returns a^2 + b"
    Begin["Private`"]
    my$function[a_,b_] := (c = a^2; Return[b+c])
    End[]
    EndPackage[]

    s = my$function[2,3];
    s (* <== returns 7 *)
    c (* <== returns c *)

In this example, the inner details of ``my$function`` are hidden in the ``Private``` context, in *Mathematica* speak. When you run an ``nb`` or ``.m`` file, you are working in the ``Global``` context.  The name ``my$function`` is exposed to the ``Global``` context because the ``my$function::usage`` declaration appears before ``Begin["Private`"]``.  The function ``my$function`` returns its result ``7`` to the ``Global``` context but if code in an ``nb`` or ``.m`` file asks for the value of the intermediate variable ``c``, then nothing is returned; the function ``my$function`` and any variable declared between ``Begin["Private`"]`` and ``End[]`` will not be reported to the ``Global``` context.

In the ``UniDyn`` package we will define some symbols as commutative and others as non-commutative.  To decide whether a symbol is commutative or not we look to the ``ScalarQ`` and ``OperatorQ`` functions; a symbol is commutative if it returns ``True`` when passed to the function ``ScalarQ``.  To define the ``a$sym`` variable, for example, as commutative we would declare

.. code::

    ScalarQ[a$sym] ^:= True

In words, the *upvalue* of ``a$sym`` when passed to the function ``ScalarQ`` is the value ``True``.  By implementing the assignment using the ``^:=`` operator, this assignment is stored with the variable ``a$sym`` and not with this function ``ScalarQ``.  This way of doing things makes it a variable's job to know whether it is commutative or not and keeps the function ``ScalarQ`` lightweight and fast.

This assignment works fine if implemented in a notebook.  If we implement the above code in a function defined between the ``Begin["Private`"]`` and ``End[]`` declarations in an ``.m`` file, however, then the assignment is not communicated back to the ``Global``` context where it's needed.  I tried a couple of work-arounds: passing the ``a$sym`` variable back up to the ``Global``` context using a ``Return[]`` statement doesn't seem to work, nor does writing the variable ``Global`a$sym`` in the private function.  In the end, I decided to simply keep the functions defining upvalues public.  This is achieved by omitting the ``Begin["Private`"]`` and ``End[]`` statements in the package ``.m`` file.

Going public
^^^^^^^^^^^^

The code snippet below, taken from ``OpQ.m``, shows how this works. 

.. code:: 

    BeginPackage["OpQ`",{"Global`"}]

    ScalarQ::usage="ScalarQ[a] returns True if ..."
    OperatorQ::usage="OperatorQ[a] returns True if ..."

    (* Begin["Private`"] <== Not needed.  We do not want the following functions private! *)
        
    Clear[OperatorQ, ScalarQ]
    OperatorQ[x$var_] := 
        Apply[Or, 
            Map[SimpleOperatorQ,
                Level[x$var,{-1}]]];
    ScalarQ[x$var_]:= !OperatorQ[x$var];
    
    Clear[CreateScalar];
    CreateScalar[a$sym_Symbol] := (Clear[a$sym]; CommQ[a$sym] ^:= True;)

    (* End[] <== Not needed. *)
    
    EndPackage[]

Code placed between the ``(*`` and ``*)`` characters is a *comment*.  I have left comments in the above code to indicate where the ``Begin["Private`"]`` and ``End[]`` would normally go.

Keeping private
^^^^^^^^^^^^^^^

The packages ``OpQ.m``, ``Mult.m``, and ``Comm.m`` are set up this way, with no ``"Private`"`` context.  In contrast, the package ``Spins.m`` *does* have a ``"Private`"`` context.  This can be seen the following code snippet.

.. code:: 

    BeginPackage["Spins`",{"Global`","OpQ`","Mult`","Comm`"}]
    
    SpinSingle$CreateOperators::usage="Descriptive messsage" s
    
    Begin["Private`"] (* <<==== IMPORTANT *)
    
    SpinSingle$CreateOperators[Ix$sym_,Iy$sym_,Iz$sym_,L_:Null] := 

        Module[{nonexistent},
        
            nonexistent = 
                Not[OperatorQ[Ix$sym]] || 
                Not[OperatorQ[Iy$sym]] ||  
                Not[OperatorQ[Iz$sym]];
    
            Ix$sym /: Comm[Ix$sym,Iy$sym] =  I Iz$sym; 
    
            <more code here>
    
        ];
        
        Return[{Ix$sym,Iy$sym,Iz$sym}] (* <<==== IMPORTANT *)
    ]
    
    End[]
    EndPackage[]

Without the ``"Private`"`` context, *Mathematica* would get confused by the appearance of the ``OperatorQ`` and ``Comm`` functions because they are defined elsewhere first.  Without the ``"Private`"`` context in ``Spins.m``, you get the following problems.  First, when you load the ``UniDyn``` package in a notebook 

.. code::

    $VerboseLoad = True;
    Needs["UniDyn`"]

you get the error

.. code::

    CommutativeQ::shdw: Symbol OperatorQ appears in multiple contexts
    ... definitions in context Spins` may shadow or be shadowed by other
    definitions.
    
Moreover, when you run the unit-testing files, most of the tests fail.  Wrapping the function ``SpinSingle$CreateOperators`` in ``Begin["Private`"]`` and ``End[]`` solves the *shadowing* problem.  Because the function is now hidden in a private context, the declaration ``SpinSingle$CreateOperators::usage`` is needed to expose the function's existence to the ``Global``` context.  The function ``SpinSingle$CreateOperators`` defines *upvalues* for the spin operators.  The ``Return[]`` statement is needed to pass these definitions back up to the ``Global``` context.

References
----------

.. [#mma-updelayed] https://reference.wolfram.com/language/tutorial/AssociatingDefinitionsWithDifferentSymbols.html

.. [#MSE29324] http://mathematica.stackexchange.com/questions/29324/creating-mathematica-packages

.. [#MMA-packaging] https://reference.wolfram.com/language/guide/PackageDevelopment.html

.. [#MMA-packaging-1] http://mathematica.stackexchange.com/questions/7502/how-can-i-return-private-members-of-a-mathematica-package-as-the-output-of-packa

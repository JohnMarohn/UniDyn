UniDyn
======

Implements the unitary evolution of quantum mechanical operators in *Mathematica*.

Requirements
------------

This package requires the *Mathematica* NCAlgebra package, version NCAlgebra 4.0.6, available for download from the University of San Diego non-commutative algebra group at http://math.ucsd.edu/~ncalg/download10.shtml.   To install the package, download  NC2015.tar and unpack the file by clicking on it.   The package should unpack into the ``NC/`` directory.  In this directory you should see the *Mathematica* notebook ``InstallNCAlgebra.nb``.  Open up this notebook and run it to install the package.

Package files
-------------

upper directory
^^^^^^^^^^^^^^^

Example notebooks ::

    UniDyn--Demo-01.nb    loads NCAlgebra and UniDyn; runs the unit tests


unidyn directory
^^^^^^^^^^^^^^^^

The package files are stored in the ``unidyn/`` directory.  The package files consist of *Mathematica* files ::

    UniDyn.m    master file; loads all the other package files
    OpCreate.m  CreateOperator[] and CreateScalar[] convenience functions
    Mult.m      NCSort[], SortedMult[], and MultSort[] functions to sort operators
    Comm.m      Comm[,] to implement the commutator function
    Spins.m     Angular momentum operators for a single spin; can specify L = 1/2 or not.
    Osc.m       Raising and lowering operators for a single harmonic oscillator
    Evolve.m    Unitary evolution

plus unit-testing files ::

    OpCreate-tests.m 
    Mult-tests.m
    Comm-tests.m
    Spins-tests.m
    Osc-tests.m
    Evolve-tests.m
    
Background reading
------------------

I rely a lot on the ``UpSetDelayed[]`` function, ``:^=`` in shorthand.  The idea up an *upvalue* and a *downvalue* is explained pretty well in the article "Associating Definitions with Different Symbols" in the Wolfram Language Tutorial [#mma-updelayed]_.  

Creating a *Mathematica* package is not as well documented as I would expect.  While a list of functions used to create a *Mathematica* package can be found in the "Package Development" section of the Wolfram Language Guide [#MMA-packaging]_, a good example illustrating how to create a package is lacking in the Mathematica documentation.  The discussions at the Mathematica Stack Exchange are helpful.  The "Creating Mathematica packages" article [#MSE29324]_ is a quick and easy introduction to packaging.  The question "How can I return private members of a Mathematica package as the output of package functions without the ``PackageName`Private``` prefix?" is answered in a longer article [#MMA-packaging-1]_.

Packaging notes
---------------

Creating a *Mathematica* package out of the ``UniDyn`` code was tricky.  There were two reasons for this: (1) I am using functions from another custom package in my package and (2) a lot of the functions in my package's ``m`` files create *upvalues* for variables that are passed to the functions.  

The usual way to package a function is to do something like

.. code:: Mathematica

    BeginPackage["MyPackage`"]
    my$function::usage="f(a,b) returns a^2 + b"
    Begin["Private`"]
    my$function[a_,b_] := (c = a^2; Return[b+c])
    End[]
    EndPackage[]

    s = my$function[2,3];
    s (* <== returns 7 *)
    c (* <== returns c *)

In this example, the inner details of ``my$function`` are hidden in the ``Private``` context, in *Mathematica* speak. When you run an ``nb`` or ``m`` file, you are working in the ``Global``` context.  The name ``my$function`` is exposed to the ``Global``` context because the ``my$function::usage`` declaration appears before ``Begin["Private`"]``.  The function ``my$function`` returns its result ``7`` to the ``Global``` context but if code in an ``nb`` or ``m`` file asks for the value of the intermediate variable ``c``, then nothing is returned; the function ``my$function`` and any variable declared between ``Begin["Private`"]`` and ``End[]`` will not be reported to the ``Global``` context.

In the ``UniDyn`` package we will define some symbols as commutative and others as non-commutative.  We will be using the version of the ``NonCommutativeMultiply`` function defined in the ``NCAlgebra`` package.  To decide whether a symbol is commutative or not, the functions in the ``NCAlgebra`` package look to the ``CommutativeQ`` function; a symbol is commutative if it returns ``True`` when passed to the function ``CommutativeQ``.  To define the ``a$sym`` variable, for example, as commutative we would declare 

.. code:: Mathematica

    CommutativeQ[a$sym] ^:= True

In words, the *upvalue* of ``a$sym`` when passed to the function ``CommutativeQ`` is the value ``True``.  By implementing the assignment using the ``^:=`` operator, this assignment is stored with the variable ``a$sym`` and not with this function ``CommutativeQ``.  This way of doing things makes it a variable's job to know whether it is commutative or not and keeps the function ``CommutativeQ`` lightweight and fast.

This assignment works fine if implemented in a notebook.  If we implement the above code in a function defined between the ``Begin["Private`"]`` and ``End[]`` declarations in an ``m`` file, however, then the assignment is not communicated back to the ``Global``` context where it's needed.  I tried a couple of work-arounds: passing the ``a$sym`` variable back up to the ``Global``` context using a ``Return[]`` statement doesn't seem to work, nor does writing the variable ``Global`a$sym`` in the private function.  In the end, I decided to simply keep the functions defining upvalues public.  This is achieved by omitting the ``Begin["Private`"]`` and ``End[]`` statements in the package ``m`` file.

Going public
^^^^^^^^^^^^

The code below, taken from ``OpCreate.m``, shows how this works. 

.. code:: Mathematica

    BeginPackage["OpCreate`",{"Global`","NC`","NCAlgebra`"}]

    CreateOperator::usage="CreateOperator[] is used ..."
    CreateScalar::usage="CreateScalar[list] is used ..."

    (* Begin["Private`"] <== Not needed.  We do not want the following functions private! *)
    
    CommQ = NonCommutativeMultiply`CommutativeQ
    
    Clear[CreateScalar];
    CreateScalar[a$sym_Symbol] := (Clear[a$sym]; CommQ[a$sym] ^:= True;)
    
    <more code here>
    
    (* End[] <== Not needed. *)
    
    EndPackage[]

Code placed between the ``(*`` and ``*)`` characters is a *comment*.  I have left comments in the above code to indicate where the ``Begin["Private`"]`` and ``End[]`` would normally go.

In the above code it was important to *not* use the function ``CommutativeQ``; if we do, then *Mathematica* will think we are talking about a new, conflicting function, will throw a warning, and the code will not do what we want.  Instead, we need to specify the function we want by its full name, ``NonCommutativeMultiply`CommutativeQ``.  Since this function name is really long, in the code above we define ``CommQ`` as a short name for the function.

Keeping private
^^^^^^^^^^^^^^^

The packages ``OpCreate.m``, ``Mult.m``, and ``Comm.m`` are set up this way, with no ``"Private`"`` context.  In contrast, the package ``Spins.m`` *does* have a ``"Private`"`` context:

.. code:: Mathematica

    BeginPackage["Spins`",{"Global`","NC`","NCAlgebra`","OpCreate`","Mult`","Comm`"}]
    
    SpinSingle$CreateOperators::usage="Descriptive messsage" 
    
    Begin["Private`"] (* <<==== IMPORTANT *)
    
    SpinSingle$CreateOperators[Ix$sym_,Iy$sym_,Iz$sym_,L_:Null] := 

        Module[{nonexistent},
        
            nonexistent = Or @@ (CommutativeQ /@ {Ix$sym,Iy$sym,Iz$sym});
    
            <more code here>
    
            Ix$sym /: Comm[Ix$sym,Iy$sym] =  I Iz$sym; 
    
            <more code here>
    
        ];
        
        Return[{Ix$sym,Iy$sym,Iz$sym}] (* <<==== IMPORTANT *)
    ]
    
    End[]
    EndPackage[]

Without the ``"Private`"`` context, *Mathematica* would get confused by the appearance of the ``CommutativeQ`` and ``Comm`` functions because they are defined elsewhere first.  Without the ``"Private`"`` context in ``Spins.m``, you get the following problems.  First, when you load the ``UniDyn``` package in a notebook 

.. code:: Mathematica

    $VerboseLoad = True;
    Needs["UniDyn`"]

you get the error

.. code:: Mathematica

    CommutativeQ::shdw: Symbol CommutativeQ appears in multiple contexts {Spins`,NonCommutativeMultiply`}; definitions in context Spins` may shadow or be shadowed by other definitions. >>
    
Moreover, when you run the unit-testing files, most of the tests fail.  Wrapping the function ``SpinSingle$CreateOperators`` in ``Begin["Private`"]`` and ``End[]`` solves the *shadowing* problem.  Because the function is now hidden in a private context, the declaration ``SpinSingle$CreateOperators::usage`` is needed to expose the function's existence to the ``Global``` context.  The function ``SpinSingle$CreateOperators`` defines *upvalues* for the spin operators.  The ``Return[]`` statement is needed to pass these definitions back up to the ``Global``` context.

References
----------

.. [#mma-updelayed] https://reference.wolfram.com/language/tutorial/AssociatingDefinitionsWithDifferentSymbols.html

.. [#MSE29324] http://mathematica.stackexchange.com/questions/29324/creating-mathematica-packages

.. [#MMA-packaging] https://reference.wolfram.com/language/guide/PackageDevelopment.html

.. [#MMA-packaging-1] http://mathematica.stackexchange.com/questions/7502/how-can-i-return-private-members-of-a-mathematica-package-as-the-output-of-packa

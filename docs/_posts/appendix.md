Usage Notes\[pyclips-unotes\]
=============================

Environments\[pyclips-unotes-env\]
----------------------------------

As seen in the detailed documentation, also provides access to the
*environment* features of CLIPS, through the class. objects provide
almost all functions normally available at the top level of , that is
importing into a Python script. object methods having the same name of
functions found at the top level of , have the same effect of the
corresponding function – but restricted to the logical environment
represented by the object itself[^1]. Normally in a true CLIPS session
users would not use environments, so the concept of environment may, in
many cases, not be useful.

There are also functions (namely: and ) that allow the user to switch
environment and to use top level functions and classes in any of the
created environments. This is useful in cases where environments are
used, because due to the double nature of CLIPS API (see about
*companion functions* for standard API), objects defined in environments
have slightly different types than corresponding top level objects –
since these types are *environment-aware*. However environment aware
classes define exactly the same methods as the top level counterparts,
so their logical use is the same.

Please note that the function returns a fully functional object. However
the system prevents[^2] invasive access via *environment-aware*
functions to current *environment*: user code should *always* use
functions defined at module level to access current *environment*. The
methods become safe as soon as another has become current – and in this
case its methods and properties will raise an error.

s are a limited resource: this is because it is impossible in to destroy
a created . In order to reuse s it may be useful to keep a reference to
each of them for the whole session. If there is an attempt to create
more s than allowed, an exception is raised.

Multiple Ways\[pyclips-unotes-multiw\]
--------------------------------------

There is more than one way to use the module, since it exposes almost
all the API functions of CLIPS, seen in fact as a library, to the Python
environment.

The module provides some functions, that is and , that let the user
directly issue commands in the CLIPS subsystem from a Python program. In
fact, the use of allows definition of constructs in CLIPS[^3], and lets
the user evaluate values or call code directly in the subsystem. So for
instance rules can be built in using

    >>> import clips
    >>> clips.Build("""
    (defrule duck-rule "the Duck Rule"
       (duck)
       =>
       (assert (quack)))
    """)
    >>> clips.PrintRules()
    MAIN:
    duck-rule

and evaluate a sum using

    >>> n = clips.Eval("(+ 5 2)")
    >>> print n
    7

Also, the user is allowed to call functions that do not return a value
using [^4], as in the following example:

    >>> clips.Eval('(printout t "Hello, World!" crlf)')
    >>> print clips.StdoutStream.Read()
    Hello, World!

There is another function, namely , that sends an entire CLIPS command
(it has to be a full, correct command, otherwise will issue an
exception): as it does not return any value or object[^5]. However this
can be particularly useful when the user needs to implement an
interactive CLIPS shell within an application built on . Unless the
application is mostly CLIPS oriented (if for instance Python is used
just as a “glue” script language) probably the use of this function has
to be discouraged, in favour of the code readability that – at least for
Python programmers – is provided by the Python oriented interface.

Using it becomes possible to write:

    >>> import clips
    >>> clips.SendCommand("""
    (defrule duck-rule "the Duck Rule"
       (duck)
       =>
       (assert (quack)))
    """)
    >>> clips.SendCommand("(assert (duck))")
    >>> clips.Run()
    >>> clips.PrintFacts()
    f-0     (duck)
    f-1     (quack)
    For a total of 2 facts.
    >>> clips.PrintRules()
    MAIN:
    duck-rule

The most important caveat about is that CLIPS accepts some kinds of
input which normally have to be considered incorrect, and does neither
return an error value, nor raise an exception: for instance, it is
possible to pass a symbol to CLIPS to the command line as in

    CLIPS> thing
    thing

and in this case CLIPS “evaluates” the symbol, printing it to the
console as a result of the evaluation. does not automatically capture
evaluation output, and just accepts a symbol (or other commands that can
be evaluated) as input without any production:

    >>> import clips
    >>> clips.SendCommand("thing")

but, turning on the verbosity flag:

    >>> clips.SendCommand("thing", True)
    >>> print clips.StdoutStream.Read()
    thing

Of course, complains if something incorrect is passed to the function
and raises an exception as previously stated. However the exception is
accompanied by a rather non-explanatory text. The object should be
queried in this case in order to obtain some more information about the
error:

    >>> clips.SendCommand("(assert (duck)") # no right bracket

    Traceback (most recent call last):
      File "<pyshell#5>", line 1, in -toplevel-
        clips.SendCommand("(assert (duck)") # no right bracket
      File ".../_clips_wrap.py", line 2575, in SendCommand
        _c.sendCommand(s)
    ClipsError: C09: unable to understand argument
    >>> print clips.ErrorStream.Read()

    [PRNTUTIL2] Syntax Error:  Check appropriate syntax for RHS patterns.

Obviously can lead to serious errors if not used with some kind of
interaction.

The point of this paragraph is that, for entity definition (evaluation
can only be performed using the or functions), the module provides a
full set of specific functions which also return appropriate objects
corresponding to specific entities. So, the task of building a *rule* in
CLIPS (in fact, a object in Python) could preferably be performed
directly using the function, that is:

    >>> clips.Clear()
    >>> clips.Reset()
    >>> r0 = clips.BuildRule("duck-rule", "(duck)", "(assert (quack))",
                             "the Duck Rule")
    >>> print r0.PPForm()
    (defrule MAIN::duck-rule "the Duck Rule"
       (duck)
       =>
       (assert (quack)))

    >>> clips.PrintRules()
    MAIN:
    duck-rule

thus with the same effect as with the function, but obtaining
immediately a reference to the rule entity in CLIPS as a Python object.
Similar examples could be provided for the function, using the
appropriate constructs or commands that can be used to achieve the same
goals.

This allows the user to choose between at least two programming styles
in : the former, more CLIPS oriented, relies heavily on the use of the ,
and functions, and is probably more readable to CLIPS developers. The
latter is somewhat closer to Python programming style, based on the
creation of objects of a certain nature by calling specific Python
functions. The advice is to avoid mixing the two styles unless
necessary, since it can make the code quite difficult to understand.

Python Functions in CLIPS\[pyclips-unotes-pyfuncs\]
---------------------------------------------------

In it is possible to execute Python functions from within CLIPS embedded
constructs and statements. This allows the extension of the underlying
inference engine with imperative functionalities, as well as the
possibility to retrieve information from the Python layer asynchronously
with respect to Python execution. Of course this possibility enables
some enhancements of the CLIPS environment, but – as a drawback – it
also opens the way to errors and misunderstandings.

Usage of Python external functions is fairly simple: the user should
register functions that will be called from within the CLIPS subsystem
in using the toplevel function. If no alternate name for the function is
specified, then the Python name will be used[^6]. If necessary, Python
function names can be deregistered using and utilities. Once a function
is registered it can be called from within the CLIPS engine using the
following syntax:

        (python-call <funcname> [arg1 [arg2 [ ... [argN]]]])

and will return a value (this allows its use in assignments) to the
CLIPS calling statement. In the call, is a *symbol* (using a string will
result in an error) and the number and type of arguments depends on the
actual Python function. When arguments are of wrong type or number, the
called function fails. Using the previously illustrated example, we
have:

    >>> clips.RegisterPythonFunction(py_square)
    >>> clips.SetExternalTraceback(True)    # print traceback on error
    >>> print clips.Eval("(python-call py_square 7)")
    49
    >>> print clips.Eval('(python-call py_square "a")')
    Traceback (most recent call last):
      File ".../_clips_wrap.py", line 2702, in <lambda>
        f = lambda *args: _extcall_retval(func(*tuple(map(_cl2py, list(args)))))
      File "<pyshell#85>", line 2, in py_square
    TypeError: can't multiply sequence to non-int
    FALSE
    >>> print clips.Eval("(python-call py_square 7 7)")
    Traceback (most recent call last):
      File ".../_clips_wrap.py", line 2702, in <lambda>
        f = lambda *args: _extcall_retval(func(*tuple(map(_cl2py, list(args)))))
    TypeError: py_square() takes exactly 1 argument (2 given)
    FALSE

It is important to know, in order to avoid errors, that the Python
interpreter that executes functions from within CLIPS is exactly the
same that calls the function used to invoke the engine: this means, for
example, that a Python function called in CLIPS is subject to change the
state of the Python interpreter itself. Moreover, due to the nature of
CLIPS external function call interface, Python functions called in CLIPS
will never raise exceptions[^7] in the Python calling layer.

Here are some other issues and features about the nature of the external
function call interface provided by :

*Functions should be CLIPS-aware:* when CLIPS calls a Python external
function with arguments, these are converted to values that Python can
understand using the previously described *wrapper classes*. Thus, for
instance, if the Python function is given an integer argument, then an
argument of type (not ) will be passed as actual parameter. This means
that in most cases has to be imported by modules that define external
Python functions.

*Actual parameters cannot be modified:* there is no way to pass values
back to CLIPS by modifying actual parameters. The possibility to use
parameters as lists should not deceive the user, as every modification
performed on s that Python receives as parameters will be lost after
function completion. A way to handle this is to treat parameters as
*immutable* values.

*External functions should always return a value:* functions always
return a value in CLIPS, even in case of an error. This can be clearly
seen in the following chunk of CLIPS code:

    CLIPS> (div 1 0)
    [PRNTUTIL7] Attempt to divide by zero in div function.
    1

where, although an error message is printed to the console, the value is
returned by the system. In the same way, CLIPS expects Python external
functions to return a value. solves this issue by converting a return
value of (which is the real return value for Python functions that
simply ) into the symbol , that has a meaning similar to the one of for
Python. Also, functions that raise uncaught exceptions will in fact
return a value to the underlying CLIPS engine: in this case the returned
value is the symbol , and an error message is routed to the error stream
– thus, it can be retrieved using . The following example imitates the
CLIPS example above:

    >>> import clips
    >>> exceptor = lambda : 1 / 0
    >>> clips.RegisterPythonFunction(exceptor, 'exceptor')
    >>> clips.SetExternalTraceback(True)    # print traceback on error
    >>> clips.Eval('(python-call exceptor)')
    Traceback (most recent call last):
      File ".../_clips_wrap.py", line 2702, in <lambda>
        f = lambda *args: _extcall_retval(func(*tuple(map(_cl2py, list(args)))))
      File "<pyshell#79>", line 1, in <lambda>
    ZeroDivisionError: integer division or modulo by zero
    <Symbol 'FALSE'>

*Return values must be understood by CLIPS:* only values that can be
converted to CLIPS base types can be returned to the inference engine.
This includes all values that can be converted to *wrapper classes*. In
fact it can be considered a good practice to cast return values to
*wrapper classes* when the main purpose of a function is to be called
from within CLIPS.

*Python functions act as generic functions:* due to the nature of
Python, functions are generally polymorphic:

    >>> def addf(a, b):
            return a + b
    >>> print addf("egg", "spam")
    eggspam
    >>> print addf(2, 4)
    6

The intrinsic polymorphism of Python functions is kept within the CLIPS
subsystem:

    >>> import clips
    >>> clips.RegisterPythonFunction(addf)
    >>> print clips.Eval('(python-call addf "egg" "spam")')
    eggspam
    >>> print clips.Eval('(python-call addf 2 4)')
    6

Thus Python functions act in a way that is similar to s.

The Submodule\[pyclips-llclips\]
================================

It has been said throughout the whole document that there are two
different “natures” of , called the *high level module* and the *low
level module*. The former has been described in detail here, and is
supposed to be the main interface to CLIPS. However, since all
communication between Python and the CLIPS subsystem is implemented in
the *low level* part, some users might find it useful to access this
interface instead of the *higher level* one. It is not the intention of
this manual to provide documentation for the *low level* interface, but
only to give some indications to users who already have experience of
the CLIPS C interface.

Submodule provides low-level classes that have the same names as their
counterparts in CLIPS, such as , and so on. Also, defines an class to
refer to *environment* addresses.

Almost all functions described in are ported to Python, except for
missing features described below: the name is the same as in the
reference guide, except for the first letter that is not
capitalized[^8]. CLIPS “top level” functions have been ported to as well
as *companion functions* that accept an instance as the first
argument[^9], whose name begins with followed by the same name as the
corresponding top level function.

*Low level* functions are documented by themselves through
*documentation strings*, that describe purpose, arguments and return
values. For instance, let’s show the documentation of a function:

    >>> import clips
    >>> cl = clips._clips
    >>> print cl.listDeftemplates.__doc__
    listDeftemplates(logicalname [, module])
    list deftemplates to output identified by logicalname
    arguments:
      logicalname (str) - the logical name of output
      module (module) - the module to inspect, all modules if omitted

Most low level function documentation strings, i.e. the ones given for
functions that are not trivially identifiable with the CLIPS API
counterparts[^10], have this form and describe in detail arguments and
return values. Users who want to benefit of these functions, can display
*documentation strings* as a reference.

The underlying engine is the same, there is no separation of environment
between the two interfaces. Operations performed at *lower level* are
obviously reflected in the *higher level* layer, as in the following
example:

    >>> clips.Clear()
    >>> cl.facts("stdout")
    >>> s = clips.StdoutStream.Read()
    >>> print s
    None
    >>> print cl.assertString.__doc__
    assertString(expr) -> fact
    assert a fact into the system fact list
    returns: a pointer to the asserted fact
    arguments:
      expr (str) - string containing a list of primitive datatypes
    >>> f = cl.assertString("(duck)")
    >>> clips.PrintFacts()
    f-0     (duck)
    For a total of 1 fact.

so the two interfaces can be used interchangeably.

Error Codes\[pyclips-errors\]
=============================

It has been discussed above, that some of the functions can raise a
CLIPS specific exception, namely . Some of the exceptions of this type
(in fact, the ones raised by the underlying CLIPS engine and caught at
the lower level), come with an error code in the accompanying text. A
brief description of exceptions that arise at low level follows:

<span>l|l</span><span>code</span><span>Code</span><span>Description</span>

These codes can be extracted from the exception description and used to
determine errors – for instance, in an control statement. Some of these
errors are caught by the high-level layer and interpreted in different
ways (e.g. the error is used to generate lists or to return after last
element in a list).

There are also some CLIPS specific exceptions that can be thrown at the
higher level: they are identified by a code beginning with the letter .
A list of these exceptions follows, along with their description:

<span>l|l</span><span>code</span><span>Code</span><span>Description</span>

Finally, there is a particular error that occurs in case of *fatal*
memory allocation failures, which is identified by a particular
exception, namely . This excepion is raised with the following code and
has the following meaning:

<span>l|l</span><span>code</span><span>Code</span><span>Description</span>

In this case the calling program *must* exit, as the underlying engine
has reached an unstable state. An exception different from the standard
has been provided in order to allow quick and effective
countermeasures[^11].

Multithreading\[pyclips-threading\]
===================================

The CLIPS engine is a separate subsystem, as stated many times before.
In other words, it maintains a state independently from what happens in
the Python interpreter. This also means that, since CLIPS was never
conceived to be used as a multithreaded library, multiple threads should
not try to access the engine concurrently. The Python interpreter, due
to its nature, does not actually allow concurrent calls to the low-level
module, so it is safe to create concurrent threads that interact with .

However this has the side effect that, during a time consuming task
(such as calling on a complex set of rules and facts) the calling thread
may block the other ones.

A partial solution to this, to allow multiple threads to switch more
reactively, is to call with the parameter, which specifies the number of
rules to be fired at once. Of course this allows subsequent calls to the
CLIPS engine to modify its state, and consequently enables execution
paths that could be different from the ones that a full CLIPS would
normally cause. Obviously this only applies to the function.

The consideration also implies that multithreaded Python applications
should take care of examining the engine state before interacting with
it, especially when splitting (which normally modifies the state) in
multiple calls.

Missing Features\[pyclips-missing\]
===================================

Most of the CLIPS API is implemented in . The *lower level* interface
(which directly maps CLIPS exposed functions as described in ) can be
accessed using the submodule. Almost all the functions defined here have
a counterpart in the CLIPS API, and a combined use of documentation
strings and itself can allow you to directly manipulate the CLIPS
subsystem as you would have done by embedding it in a C program.
However, there are some functions that appear in but, when called, issue
an error:

    >>> clips._clips.addClearFunction()
    Traceback (most recent call last):
      File "<pyshell#46>", line 1, in ?
        clips._clips.addClearFunction()
    ClipsError: C98: unimplemented feature/function

and in fact, even their documentation string reports

    >>> print clips._clips.addClearFunction.__doc__
    unimplemented feature/function

even if the name of the function is defined and is a *callable*.

A list of such functions follows:

<span>l|l</span><span>code</span><span>Function</span><span>Type</span>

The description of these functions is outside the scope of this guide,
and can be found in of CLIPS. It is not likely that these functions will
be implemented even in future versions of since Python programmers are
usually not interested in dealing with low level memory handling (which
is the primary use of the memory oriented functions), and tasks like
reference count handling are performed directly by Python itself (for
Python objects which shadow CLIPS entities) and by the low level
submodule. Also, the functions referred to above as *execution hooks*
often have to deal with CLIPS internal structures at a very low level,
so they would be of no use in a Python program.

Other API functions, which are used internally by (for instance the C
function), are not implemented in the module.

Some of the features (either in current and possibly in further versions
of ) may depend on the version of CLIPS that is used to compile the
module. Using the most recent stable version of CLIPS is recommended in
order to enable all features. Features that are excluded from the module
because of this reason will issue an exception, in which the exception
text reports the following: . Moreover, the CLIPS engine version may
affect the behaviour of some functions. Please consider reading the
documentation related to the used CLIPS version when a function does not
behave as expected.

Installing \[pyclips-setup\]
============================

Installation\[pyclips-setup-installation\]
------------------------------------------

To install you should also download the full CLIPS source distribution.
You will find a file called at the CLIPS download location: you should
choose to download this instead of the compressed source, since the
setup program itself performs the task of extracting the files to an
appropriate directory with the correct line endings. The ZIP file format
has been chosen in order to avoid using different extraction methods
depending on the host operating system.

uses or for its installation. So in all supported systems the module can
be easily set up once the whole source has been extracted to a directory
and the CLIPS source code has been put in the same place, by means of
the following command:

    # python setup.py install

In fact recent versions of will attempt to download the latest supported
CLIPS source directly from the web site if no CLIPS source package is
found. Otherwise no attempt to connect to the Internet will be made. The
file provides more up-to-date and detailed information on the setup
process.

On , if you have a system-wide Python distribution, your privileges for
installation should be the same as the Python owner.

The CLIPS library itself is compiled for a specific platform, since
modifies the file in the CLIPS distribution.

is known to build and pass the tests on *Linux (x86 and x86\_64)*[^12],
*Win32* (many flavours of it have been tested), *Sun Solaris* with
32-bit gcc, *FreeBSD*, *Mac OS X* with *Fink* and, using a customized
build process, has been ported to the *Sharp Zaurus (SA-1110)* platform.

Requirements\[pyclips-setup-requirements\]
------------------------------------------

requires Python 2.4 or above to function: it uses decorators to check
and enforce types where needed, and in some places it also uses modern
aspects of the Python API.

At least version 6.23 of CLIPS is required: it allows the definition and
use of *environments*, and the function and macro definitions are more
conformant to the ones described in . Of course features present in
CLIPS 6.24 are not available when using the previous CLIPS version, so
if there is no particular reason to use it, please compile PyCLIPS with
CLIPS 6.24, which also fixes some bugs.

[^1]: In fact, the Python submodule that implements the class is
    generated automatically: the process can be examined by looking at
    the code in and .

[^2]: Raising an appropriate exception.

[^3]: Note that the function does not return any value or object, so you
    will have to call to find entities created using the function.

[^4]: There is a discussion about functions that only have *side
    effects* in CLIPS, such as , in , that is, the CLIPS tutorial.

[^5]: Some information about the command result can be retrieved reading
    the appropriate output streams.

[^6]: An example of function registration has been provided in the
    introduction.

[^7]: Exceptions can arise *during* the Python function execution, and
    can be caught inside the function code. However, for debugging
    purposes, there is the possibility to force print a standard
    traceback whenever an error occurs in a Python function called by
    CLIPS.

[^8]: There can be some exceptions: the most important one is the
    assertion function, since Python already has an keyword. This
    function is called instead.

[^9]: When trying to show the *documentation string* of these functions,
    the first argument is not described because their code has been
    generated automatically.

[^10]: Some functions have a documentation string that actually refers
    to the CLIPS API itself, explicitly containing the words
    “*equivalent of C API*” and the C function name: is especially
    useful in this case.

[^11]: Please note that in some cases, and depending on how the
    operating system treats memory allocation failures, the Python
    interpreter too could loose stability in case of memory shortage.

[^12]: The x86\_64 platforms requires some optional patches to be
    applied.

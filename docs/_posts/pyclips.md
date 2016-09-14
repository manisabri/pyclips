---
abstract: |
    This manual documents the high-level interface to the CLIPS system
    provided by the module. This module allows the creation of a
    fully-functional CLIPS environment in a Python session, thus providing
    access to a well-known expert systems shell programmatically.

    incorporates most of the API bindings documented in the <span>*Clips
    Reference Guide Vol. II: Advanced Programming Guide*</span> (freely
    downloadable from the CLIPS web site, see below) and embeds these
    bindings in an Object Oriented layer, incorporating most CLIPS
    constructs into Python classes. Instances of these classes allow access
    to the status and functionality of the corresponding CLIPS objects.
author:
- Francesco Garosi
date: 'Feb 22, 2008'
title:  Manual
---

Front Matter\[front\] {#front-matterfront .unnumbered}
=====================

Copyright © 2002-2008 Francesco Garosi/JKS. All rights reserved.

See the end of this document for complete license and permissions
information.

Introduction\[introduction\]
============================

Overview\[pyclips-overview\]
----------------------------

This module aims to embed a fully functional CLIPS engine in Python, and
to give to the developer a more Python-compliant interface to CLIPS
without cutting down on functionalities. In fact CLIPS is compiled into
the module in its entirety, and most API functions are bound to Python
methods. However the direct bindings to the CLIPS library (implemented
as the submodule) are not described here: each function is described by
an appropriate documentation string, and accessible by means of the
function or through the tool. Each direct binding maps to an API
provided function. For a detailed reference[^1] for these functions see
<span>*Clips Reference Guide Vol. II: Advanced Programming
Guide*</span>, available for download at the CLIPS website.

is also capable of generating CLIPS text and binary files: this allows
the user to interact with sessions of the CLIPS system itself.

An important thing to know, is that implements CLIPS as a separated[^2]
engine: in the CLIPS module implementation, CLIPS “lives” in its own
memory space, allocates its own objects. The module only provides a way
to send information and commands to this engine and to retrieve results
from it.

### Structure\[pyclips-ov-structure\]

is organized in a package providing several classes and top-level
functions. Also, the module provides some objects that are already
instanced and give access to some of the CLIPS internal functions and
structures, including debug status and engine I/O.

CLIPS is accessible through these classes and functions, that send
appropriate commands to the underlying engine and retrieve the available
information. Many of the CLIPS classes, constructs and objects are
shadowed by Python classes and objects. However, whereas classes provide
a comfortable way to create objects that reference the actual engine
objects, there is no one-to-one mapping between the two memory spaces:
for instance, when a Python object is deleted (via the command), the
corresponding CLIPS object will still remain alive in the CLIPS memory
space. An appropriate command is necessary to remove the object from the
underlying engine, and this is provided by the module interface.

### Interactive Usage\[pyclips-ov-interactive\]

The package can also be used interactively, since it can inspect an
underlying CLIPS session and give some of the output that CLIPS usually
provides when used as an interactive shell.

A simple interactive session with follows:

    >>> import clips
    >>> clips.Reset()
    >>> clips.Assert("(duck)")
    <Fact 'f-1': fact object at 0x00DE4AE0>
    >>> clips.BuildRule("duck-rule", "(duck)", "(assert (quack))", "the Duck Rule")
    <Rule 'duck-rule': defrule object at 0x00DA7E00>
    >>> clips.PrintRules()
    MAIN:
    duck-rule
    >>> clips.PrintAgenda()
    MAIN:
       0      duck-rule: f-1
    For a total of 1 activation.
    >>> clips.PrintFacts()
    f-0     (initial-fact)
    f-1     (duck)
    For a total of 2 facts.
    >>> clips.Run()
    >>> clips.PrintFacts()
    f-0     (initial-fact)
    f-1     (duck)
    f-2     (quack)
    For a total of 3 facts.

Users of the CLIPS interactive shell will find the output quite
familiar. In fact the functions are provided for interactive use, and
retrieve their output directly from the underlying CLIPS engine I/O
subsystem, in order to resemble an interactive CLIPS session. Other
functions are present to retrieve object names, values and the so called
*pretty-print forms* for programmatic use.

Implementation Structure\[pyclips-implstructure\]
-------------------------------------------------

This section describes the guidelines and considerations that lead to
this CLIPS interface implementation. For the developers which normally
use CLIPS as a development environment or expert systems shell, the
architecture of the module will look a little bit different, and in some
ways could also seem confusing.

The main topics covered by these sections are the *Implementation of
Constructs as Classes*, the *Implementation of CLIPS I/O Subsystem*, the
*Configuration and Debug Objects*, the *Coexistence of Global and
Environment-Aware Engines* and the *Conventions used for Naming* which
explains the rules that helped choose the current naming scheme for
classes, functions and objects implemented in .

### Implementation of Constructs as Classes\[pyclips-ov-casc\]

CLIPS users know that this shell offers several constructs to populate
the system memory. These constructs are not described here, since a
detailed explaination of the CLIPS language can be found in the official
CLIPS documentation. These constructs, many of which have their
particular syntax, create “objects” (not necessarily in the sense of
OOP[^3], although some of these can be s of es) in the subsystem memory.

The choice of implementing most of these constructs as classes gives to
a more organic structure. Most of the construct classes share
similarities which make the interface structure simpler and the access
to CLIPS objects more systematic.

Most constructs are implemented as *factory functions* which return
instances of Python classes. These Python instances (that shadow the
corresponding CLIPS objects), on their turn, have methods and properties
which operate directly on the objects they map in the CLIPS subsystem.
Methods and properties provide both access and *send messages* to these
objects.

An example of this follows:

    >>> import clips
    >>> f0 = clips.Assert("(duck)")
    >>> print f0
    f-0
    >>> print f0.Exists()
    True
    >>> f0.Retract()
    >>> print f0.Exists()
    False

In the above example, a fact () is asserted and then retracted. The
assertion is done by means of a module-level function () and the fact is
retracted using a method of the shadow object (). A verification on the
CLIPS object, using the Python instance[^4] ’s method , shows that after
invoking the method of the object does no longer exists in the CLIPS
subsystem, thus it has been actually retracted.

As stated previously, this does not remove the Python object (a
instance) from the namespace pertinent to Python itself: as it can be
seen from the code snip shown above, is still a functional instance, and
can be queried about the existence of the corresponding object in CLIPS.

Objects in the CLIPS operating space can be referenced by more than a
Python object (or even not be referenced at all, if CLIPS creation does
not correspond to a Python assignment), as demonstrated by the following
code:

    >>> clips.Reset()
    >>> f1 = clips.Assert("(duck)")
    >>> clips.Assert("(quack)")
    <Fact 'f-2': fact object at 0x00DE8420>
    >>> f1
    <Fact 'f-1': fact object at 0x00DE3020>
    >>> fl = clips.FactList()
    >>> f1b = fl[1]
    >>> f1b
    <Fact 'f-1': fact object at 0x00E08C40>

Both and refer to the same object in CLIPS namespace, but their address
is different: in fact they are two different Python objects (equality
test fails) but correspond to the same in CLIPS.

### Implementation of CLIPS I/O Subsystem\[pyclips-ov-io\]

The CLIPS shell interacts with the user answering to typed in commands
with some informational output. An interactive CLIPS session will show
this:

    CLIPS> (reset)
    CLIPS> (watch activations)
    CLIPS> (defrule duck-rule "the Duck Rule"
       (duck)
    =>
       (assert (quack)))
    CLIPS> (assert (duck))
    ==> Activation 0      duck-rule: f-1
    <Fact-1>
    CLIPS> (run)
    CLIPS> (facts)
    f-0     (initial-fact)
    f-1     (duck)
    f-2     (quack)
    For a total of 3 facts.

Each time a is asserted, CLIPS outputs a string containing its index,
and since we decided to show some debug output about activations, CLIPS
produces a line as soon as is asserted, since would be activated by
this. Although in an interactive session all of the output would go to
the terminal, CLIPS logically considers the “streams” for different
output types as separated: in fact, debug output (the one generated by
the command) goes to a special stream called . In this special case, for
instance, the debug output can be captured by through a special
stream-like Python object, which provides a function[^5]. Comparing the
behaviour of two interactive sessions, the former in the CLIPS subsystem
and the latter in Python, will help to understand the close relationship
between CLIPS I/O and stream objects. CLIPS will interact with the user
as follows:

    CLIPS> (defrule sayhello
       (hello)
    =>
       (printout t "hello, world!" crlf))
    CLIPS> (assert (hello))
    ==> Activation 0      sayhello: f-1
    <Fact-1>
    CLIPS> (run)
    hello, world!

And the Python counterpart follows:

    >>> import clips
    >>> clips.DebugConfig.ActivationsWatched = True
    >>> r0 = clips.BuildRule("sayhello", "(hello)",
                             '(printout stdout "hello, world!" crlf)')
    >>> print r0.PPForm()
    (defrule MAIN::sayhello
       (hello)
       =>
       (printout stdout "hello, world!" crlf))

    >>> clips.Assert("(hello)")
    <Fact 'f-0': fact object at 0x00DE81C0>
    >>> t = clips.TraceStream.Read()
    >>> print t
    ==> Activation 0      sayhello: f-0
    >>> clips.Run()
    >>> t = clips.StdoutStream.Read()
    >>> print t
    hello, world!

The I/O access objects can be used both in interactive and unattended
sessions. In this case, they can be useful to retrieve periodical
information about CLIPS internal status, since most of the output
provided can be easily interpreted in a programmatic way. Also, there is
one more stream called (which has a method) that might be useful to send
input to the CLIPS engine when some “user interaction” is required[^6].

There is no way to create other instances of these streams: the
high-level module hides the class used to build these objects. This is
because the I/O streams have to be considered like “physical devices”
whose use is reserved to the engine to report trace and debug
information as well as user requested output.

These I/O streams will be described later in the detail, since each one
can be used to report about a specific task.

### Configuration and Debug Objects\[pyclips-ov-cado\]

As well as I/O streams, there are two other objects directly provided by
. These objects provide access to the CLIPS engine global configuration.
Many aspects of the CLIPS engine, that in the command line environment
would be configured using particular commands, are accessible via the
(global engine configuration) object and the (global debug and trace
configuration) object. For example, we can take the code snip shown
above:

    >>> clips.DebugConfig.ActivationsWatched = True

(...)

    >>> t = clips.TraceStream.Read()
    >>> print t
    ==> Activation 0      sayhello: f-0

The line tells to the underlying subsystem that debug information about
*rule activations* has to be written to the proper stream (the stream
dedicated to debug output in CLIPS is called and is accessible in
through the object).

As it has been said for the I/O streams, these objects cannot be
instanced by the user: access to these objects affects global (or at
least *environmental*, we will see the difference later) configuration,
so it would be of no meaning for the user to create more, possibly
confusing, instances of such objects.

### Coexistence of Global and Environment-Aware Engines\[pyclips-ov-env\]

As of version 6.20, CLIPS API offers the possibility to have several
*environments* in which to operate. We can consider environments as
separate engines that only share the operating mode, in other words “the
code”. also implements environments by means of a special class. This
class implements all the features provided by the top level methods and
classes. The class reimplements all classes provided by , but – although
their behaviour is quite similar – methods of classes provided by only
affect the CLIPS environment represented by the instance itself.

There is normally no need to use environments. However, access to them
is provided for CLIPS “gurus” who want to have more than one engine
working at the same time. The end user of will see no real difference
between a call to a function and its environmental counterpart (defined
as *companion function* in the official CLIPS documentation), apart from
being called as a member function of an object.

A simple example will be explanatory:

    >>> clips.Clear()
    >>> clips.Reset()
    >>> e0 = clips.Environment()
    >>> e1 = clips.Environment()
    >>> e0.Assert("(duck)")
    <Fact 'f-0': fact object at 0x00E7D960>
    >>> e1.Assert("(quack)")
    <Fact 'f-0': fact object at 0x00E82220>
    >>> e0.PrintFacts()
    f-0     (duck)
    For a total of 1 fact.
    >>> e1.PrintFacts()
    f-0     (quack)
    For a total of 1 fact.

### Using External Functions in CLIPS\[pyclips-ov-extfuncs\]

gives the ability to users to call Python code from within the CLIPS
subsystem. Virtually every function defined in Python can be called from
CLIPS code using the special CLIPS function . However, since CLIPS has
different basic types than Python, in most cases it would be useful for
modules that implement function to be called in the CLIPS engine to
import the module themselves, in order to be aware of the structures
that CLIPS uses.

Functions have to be registered in in order to be available to the
underlying engine, and the registration process can dynamically occur at
any moment.

A simple example follows:

    >>> import clips
    >>> def py_square(x):
            return x * x
    >>> clips.RegisterPythonFunction(py_square)
    >>> print clips.Eval("(python-call py_square 7)")
    49
    >>> print clips.Eval("(python-call py_square 0.7)")
    0.49

A more detailed description of the features provided by can be found in
the appendices.

### Conventions Used for Naming\[pyclips-ov-names\]

In , the simple convention that is used is that all valuable content
exposed has a name beginning with a capital letter. Names beginning with
a single underscore have normally no meaning for the user. Functions,
class names and objects use mixed capitals (as in Java), and *manifest
constants* (names used in *lieu* of explicit values to pass instructions
to CLIPS functions or properties) are all capitalized, as is usual for
the C language.

CLIPS users will perhaps be confused because often the constructs in
CLIPS are expressed by keywords containing a prefix. The choice was made
in to drop this prefix in many cases: the use of this prefix has a
strong logic in the CLIPS language, because in this way the developer
knows that a *construct* is used, that is, a *definition* is made. The
keyword used to instance this definition, both encapsulates the meaning
of inition itself, and also the type of construct that is being defined
(e.g. ine a is ), thus avoiding making constructs more difficult by
means of two separate keywords. In , since the definition happens at
class declaration and the instantiation of classes shadows a construct
definition when it has already been performed, it seemed unnecessary to
keep the prefix: in fact, to follow the above example, it does not seem
correct to refer to a rule within the CLIPS subsystem as a “” object,
hence it is simply referred to as a .

### Pickling Errors\[pyclips-ov-pickle\]

Python objects cannot be pickled or unpickled. This is because, since
pickling an object would save a reference to a CLIPS entity – which is
useless across different sessions – the unpickling process would feed
the underlying engine in an unpredictable way, or at least would
reference memory locations corresponding to previous CLIPS entities
without the engine having them allocated.

One better way to achieve a similar goal is to use the or (and related
or ) to save the engine[^7] status in its entirety.

If a single entity is needed, its *pretty-print form* can be used in
most cases to recreate it using the functions.

Other Usage Modes\[pyclips-ov-otheruses\]
-----------------------------------------

It is also interesting that, by using some particular functions and the
provided I/O subsystem, even “pure” CLIPS programs can be executed by ,
and while the simple output from CLIPS can be read to obtain feedback,
the possibility of inspecting the internal CLIPS subsystem state
remains.

The following example, taken from the CLIPS website[^8], illustrates
this: first we take a full CLIPS program, saved as , and reported below:

then we execute all commands (using the function) in the current of an
interactive session:

    >>> clips.BatchStar("zebra.clp")
    >>> clips.Reset()
    >>> clips.Run()
    >>> s = clips.StdoutStream.Read()
    >>> print s
    There are five houses, each of a different color, inhabited by men of
    different nationalities, with different pets, drinks, and cigarettes.

    The Englishman lives in the red house.  The Spaniard owns the dog.
    The ivory house is immediately to the left of the green house, where
    the coffee drinker lives.  The milk drinker lives in the middle house.
    The man who smokes Old Golds also keeps snails.  The Ukrainian drinks
    tea.  The Norwegian resides in the first house on the left.  The
    Chesterfields smoker lives next door to the fox owner.  The Lucky
    Strike smoker drinks orange juice.  The Japanese smokes Parliaments.
    The horse owner lives next to the Kools smoker, whose house is yellow.
    The Norwegian lives next to the blue house.

    Now, who drinks water?  And who owns the zebra?

    HOUSE | Nationality | Color  | Pet    | Drink        | Smokes
    --------------------------------------------------------------------
      1   | norwegian   | yellow | fox    | water        | kools
      2   | ukrainian   | blue   | horse  | tea          | chesterfields
      3   | englishman  | red    | snails | milk         | old-golds
      4   | spaniard    | ivory  | dog    | orange-juice | lucky-strikes
      5   | japanese    | green  | zebra  | coffee       | parliaments

    >>> clips.PrintFacts()
    f-0     (initial-fact)
    f-26    (avh (a smokes) (v parliaments) (h 1))
    f-27    (avh (a smokes) (v parliaments) (h 2))
    f-28    (avh (a smokes) (v parliaments) (h 3))

\[... a long list of facts ...\]

    f-150   (avh (a color) (v red) (h 5))
    For a total of 126 facts.
    >>> li = clips.FactList()
    >>> for x in li:
    ...	if str(x) == 'f-52':
    ...		f52 = x
    >>> f52
    <Fact 'f-52': fact object at 0x00E6AA10>
    >>> print f52.PPForm()
    f-52    (avh (a drink) (v tea) (h 2))

You can just copy the program above to a file, say as in the example,
and follow the same steps to experiment with objects and with the CLIPS
subsystem.

Module Contents\[pyclips-modulecontents\]
=========================================

This chapter gives a detailed description of top-level functions and
constants in the module. It’s not intended to be a CLIPS reference: the
official CLIPS Reference Guide still remains the main source of
documentation for this. This Guide will often be referred to for
information about the engine itself, and the user is expected to know
the CLIPS language and structure sufficiently with respect to his goals.

Although the user is not supposed to know CLIPS API, it is advisable to
have its reference at least at hand to sometimes understand *how*
interacts with CLIPS itself. Besides the programmatic approach offered
by is vastly different to the C API – with the occasional exception of
the intrinsic logic.

We will first describe the top level functions.

Top Level Functions and Constants\[pyclips-toplevel\]
-----------------------------------------------------

### Constants\[pyclips-tl-constants\]

Several constants are provided in order to configure CLIPS environment
or to instruct some functions to behave in particular ways.

#### Scope Save Constants

Constants to decide what to save in CLIPS dump files (see the , and
functions).

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

#### Salience Evaluation Constants

Constants to tell the underlying engine when salience has to be
evaluated (see the property).

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

#### Conflict Resolution Strategy Constants

Constants to specify the way the underlying engine should resolve
conflicts among rules having the same salience (see the property).

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

In the table above, the term *specificity* refers to the number of
comparisons in the LHS of a rule.

#### Class Definition Default Mode Constants

Constants to specify default mode used for classes definition. Please
refer to <span>*Clips Reference Guide Vol. I: Basic Programming
Guide*</span> for details about the usage of the two modes, as the
meaning is quite complex and outside the scope of this manual (see the
property).

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

#### Message Handler Type Constants

Constants to define the execution time, the purpose and the behaviour of
*message handlers*: see function and the following members of the class:
and . The following table summarizes the analogous one found in
<span>*Clips Reference Guide Vol. I: Basic Programming Guide*</span>.

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

#### Template Slot Default Type Constants

It’s possible to inspect whether or not a slot has been defined to have
a default value, and in case it is, if its default value is *static*
(that is, constant) or *dynamically generated* (for instance, using a
function like ). See the documentation of for more details.

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

Notice that evaluates to , so it’s legal to use the function just to
test the presence of a default value. Please also note that is only
returned when the default value for a slot is set to as stated in the
<span>*Clips Reference Guide Vol. II: Advanced Programming
Guide*</span>.

### Functions\[pyclips-tl-functions\]

<span>AgendaChanged</span> test whether or not has changed since last
call.

<span>Assert</span><span>o</span> Assert a (already created or from a
string). This perhaps needs some explanation: CLIPS allows the creation
of s based on s, and in this is done by instancing a with a argument.
The resulting slots can then be modified and the object can be used to
make an assertion, either by using the function or this version od .

<span>BLoad</span><span>filename</span> Load the constructs from a
binary file named . Binary files are not human-readable and contain all
the construct information.

<span>BLoadInstances</span><span>filename</span> Load s from binary file
named . Binary files are not human-readable and contain all the
construct information.

<span>BSave</span><span>filename</span> Save constructs to a binary file
named .

<span>BSaveInstances</span><span>filename </span> Save s to binary file
named . The parameter can be one of (for all s whose s are defined in
current ) or (for all s visible to current ).

<span>BatchStar</span><span>filename</span> Execute commands stored in
text file named as specified in .

<span>BrowseClasses</span><span>name</span> Print the list of es that
inherit from specified one.

<span>Build</span><span>construct</span> Build construct given in
argument as a string. The string must enclose a full construct in the
CLIPS language.

<span>BuildClass</span><span>name, text </span> Build a with specified
name and body. is the optional comment to give to the object. This
function is the only one that can be used to create es with multiple
inheritance.

<span>BuildDeffacts</span><span>name, text </span> Build a object with
specified name and body. is the optional comment to give to the object.

<span>BuildDefinstances</span><span>name, text </span> Build a having
specified name and body. is the optional comment to give to the object.

<span>BuildFunction</span><span>name, args, text </span> Build a with
specified name, arguments and body. is the optional comment to give to
the object. can be either a blank-separated string containing argument
names, or a sequence of strings corresponding to argument names. Such
argument names should be coherent to the ones used in the function body
(that is, ). The argument list, if expressed as a string, should *not*
be surrounded by brackets. can also be used as the argument list if the
function has no arguments.

<span>BuildGeneric</span><span>name, text </span> Build a with specified
name and body. is the optional comment to give to the object.

<span>BuildGlobal</span><span>name, </span> Build a variable with
specified and . The parameter can be of any of the types supported by
CLIPS: it can be expressed as a Python value (with type defined in
Python: the module will try to pass to CLIPS a value of an according
type), but for types that normally do not exist in Python (such as s) an
explicit conversion is necessary. If the is omitted, then the module
assigns to the variable.

<span>BuildInstance</span><span>name, defclass </span> Build an of given
overriding specified . If no is specified to be overridden, then the
will assume default values.

<span>BuildMessageHandler</span><span>name, class, args, text </span>
Add a new *message handler* to the supplied class, with specified name,
body (the argument) and argument list: this can be specified either as a
sequence of variable names or as a single string of whitespace separated
variable names. Variable names (expressed as strings) can also be
*wildcard parameters*, as specified in the <span>*Clips Reference Guide
Vol. I: Basic Programming Guide*</span>. The parameter should be one of
, , , defined at the module level: if omitted it will be considered as .
The body must be enclosed in brackets, as it is in CLIPS syntax. The
function returns the *index* of the *message handler* within the
specified .

<span>BuildModule</span><span>name </span> Build a with specified name
and body. is the optional comment to give to the object. The current is
set to the new one.

<span>BuildRule</span><span>name, lhs, rhs </span> Build a object with
specified name and body. is the optional comment to give to the object.
The and parameters correspond to the *left-hand side* and *right-hand
side* of a .

<span>BuildTemplate</span><span>name, text </span> Build a object with
specified name and body. is the optional comment to give to the object.

<span>Call</span><span>func, args</span> Call a CLIPS internal with the
given argument string. The parameter, in its easiest form, can be a list
of arguments separated by blank characters using CLIPS syntax. There are
other forms that can be used, depending on how many arguments the called
function requires: if it accepts a single argument, the caller can just
specify the argument[^9] possibly cast using one of the *wrapper
classes* described below. When the function accepts multiple arguments
it is possible to specify them as a sequence of values (either a list or
a tuple) of basic[^10] values. It is always preferable to convert these
values using the *wrapper classes* in order to avoid ambiguity,
especially in case of string arguments.

<span>ClassList</span> Return the list of names.

<span>Clear</span> Clear current .

<span>ClearPythonFunctions</span> Unregister all user defined Python
functions from .

<span>ClearFocusStack</span> Clear focus stack.

<span>CurrentEnvironment</span> Return an object representing current
CLIPS *environment*. This is useful for switching between *environment*s
in a session. Please note that almost all *environment* operations are
disallowed on the returned object until another is selected as current:
all operations on current should be performed using the *top level*
module functions.

<span>CurrentModule</span> Return current module as a object.

<span>DeffactsList</span> Return a list of names in current .

<span>DefinstancesList</span> Retrieve list of all names.

<span>Eval</span><span>expr</span> Evaluate expression passed as
argument. Expressions that only have *side effects* (e.g. expressions)
return .

<span>ExternalTracebackEnabled</span> Return if functions called from
within CLIPS using the engine function will print a standard traceback
to on exceptions, otherwise. This function is retained for backwards
compatibility only: please use the flag of the object to enable or
disable this feature instead.

<span>FactList</span> Return list of s in current .

<span>FactListChanged</span> Test whether list is changed since last
call.

<span>FindClass</span><span>name</span> Find a by name.

<span>FindDeffacts</span><span>name</span> Find a by name.

<span>FindDefinstances</span><span>name</span> Find a by name.

<span>FindFunction</span><span>name</span> Find a by name.

<span>FindGeneric</span><span>name</span> Find a function by name.

<span>FindGlobal</span><span>name</span> Find a variable by name.

<span>FindInstance</span><span>name</span> Find an in all s (including
imported).

<span>FindInstanceLocal</span><span>name</span> Find an in non imported
s.

<span>FindModule</span><span>name</span> Find a in list by name.

<span>FindRule</span><span>name</span> Find a by name.

<span>FindTemplate</span><span>name</span> Find a by name.

<span>FocusStack</span> Get list of names in focus stack.

<span>FunctionList</span> Return the list of names.

<span>GenericList</span> Return the list of names.

<span>GlobalList</span> Return the list of variable names.

<span>GlobalsChanged</span> Test whether or not variables have changed
since last call.

<span>InitialActivation</span> Return first object in current CLIPS .

<span>InitialClass</span> Return first in current CLIPS .

<span>InitialDeffacts</span> Return first in current CLIPS .

<span>InitialDefinstances</span> Return first in current CLIPS .

<span>InitialFact</span> Return first in current CLIPS .

<span>InitialFunction</span> Return first in current CLIPS .

<span>InitialGeneric</span> Return first in current CLIPS .

<span>InitialGlobal</span> Return first variable in current CLIPS .

<span>InitialInstance</span> Return first in current CLIPS .

<span>InitialModule</span> Return first in current CLIPS .

<span>InitialRule</span> Return first in current CLIPS .

<span>InitialTemplate</span> Return first in current CLIPS .

<span>InstancesChanged</span> Test if s have changed since last call.

<span>Load</span><span>filename</span> Load constructs from the
specified file named .

<span>LoadFacts</span><span>filename</span> Load s from the specified
file named .

<span>LoadFactsFromString</span><span>s</span> Load s from the specified
string.

<span>LoadInstances</span><span>filename</span> Load s from file named .

<span>LoadInstancesFromString</span><span>s</span> Load s from the
specified string.

<span>MessageHandlerList</span> Return list of constructs.

<span>MethodList</span> Return the list of all methods.

<span>ModuleList</span> Return the list of names.

<span>PopFocus</span> Pop focus.

<span>PrintAgenda</span> Print s in to standard output.

<span>PrintBreakpoints</span> Print a list of all breakpoints to
standard output.

<span>PrintClasses</span> Print a list of all es to standard output.

<span>PrintDeffacts</span> Print a list of all to standard output.

<span>PrintDefinstances</span> Print a list of all to standard output.

<span>PrintFacts</span> Print s to standard output.

<span>PrintFocusStack</span> Print focus stack to standard output.

<span>PrintFunctions</span> Print a list of all s to standard output.

<span>PrintGenerics</span> Print list of functions to standard output.

<span>PrintGlobals</span> print a list of variables to standard output

<span>PrintInstances</span> Print a list of s to standard output. If the
argument is omitted, all s in the subsystem will be shown. The parameter
can be a object or a string containing a name.

<span>PrintMessageHandlers</span> Print a list of all s.

<span>PrintModules</span> Print a list of s to standard output.

<span>PrintRules</span> Print a list of s to standard output.

<span>PrintSubclassInstances</span> Print subclass s to standard output
for the specified. If the argument is omitted, all instances in the
subsystem will be shown. The parameter can be a string containing a name
or a object.

<span>PrintTemplates</span> Print names to standard output.

<span>RefreshAgenda</span> Refresh s for current .

<span>RegisterPythonFunction</span><span>callable </span> Register the
function for use within CLIPS via the engine function . If the parameter
of type is not given, then the attribute of the first argument will be
used. is the name that will be used in CLIPS to refer to the function.
See appendix for a more detailed explanation.

<span>ReorderAgenda</span> Reorder s for current .

<span>Reset</span> Reset current .

<span>RestoreInstancesFromString</span><span>s</span> Restore s from the
specified string.

<span>RuleList</span> Return a list of names in current .

<span>Run</span> Execute s up to (which is an if given). If is omitted,
then no limitation is assumed and the program runs countinuously until
all rules are executed. The function returns the number of rules that
have been fired[^11].

<span>Save</span><span>filename</span> Save constructs to the file
specified by . The constructs are saved in text (human readable) form.

<span>SaveFacts</span><span>filename </span> Save current s to file
specified by . The parameter can be one of (for all s whose s are
defined in current ) or (for all s visible to current ).

<span>SaveInstances</span><span>filename </span> Save s to file
specified by . The parameter can be one of (for all s whose s are
defined in current ) or (for all s visible to current ).

<span>SendCommand</span><span>cmd </span> Send a command to the
underlying CLIPS engine, as if it was typed at the console in an
interactive CLIPS session. This command could actually be useful when
embedding a CLIPS shell in a Python program. Please note that other
input than commands, in such a case, should be entered using the input
stream. If is set to the possible[^12] command output is sent to the
appropriate output stream.

<span>SetExternalTraceback</span> Allow or disallow functions called
from within the CLIPS engine using to print a standard traceback to in
case an exception occurs. Please note that this does not mean that a
real exception arises, as there is no possibility to catch Python
exceptions in CLIPS code. In such case all failing Python functions will
return the symbol to CLIPS[^13]. This behaviour is initially set to , as
it is useful only for debugging purposes. This function is retained for
backwards compatibility only: please use the flag of the object to
enable or disable this feature instead.

<span>ShowGlobals</span> Print list of variables and their values to
standard output (the functions only prints out variable names).

<span>TemplateList</span> Return a list of names.

<span>UnregisterPythonFunction</span><span>name</span> Remove the
function referred as within the CLIPS engine from the set of functions
that can be called via calls.

Among other exception types, arising in cases that can also occur in
Python, the module can raise exceptions specific to CLIPS identified by
the following:

<span>ClipsError</span> Exception raised when an operation fails in the
CLIPS subsystem: normally it occurs when CLIPS finds an error, when an
iteration is over or when an invalid value is passed to CLIPS. This
exception is accompanied by explanatory text preceded by an alphanumeric
code that can be used to programmatically identify the error.

<span>ClipsMemoryError</span> Severe memory error raised when the CLIPS
subsystem is unable to allocate the needed memory. In normal
circumstances, when an error of this type occurs, the CLIPS system has
become inconsistent and the only way to recover is exiting. This
exception is raised only in order to allow a developer to notify the
user of the impossibility to continue.

Classes and Objects\[pyclips-objects\]
======================================

As previously stated in the introduction, provides classes and objects
to access CLIPS “*entities*”. It could be preferable to refer to
counterparts “living” in the CLIPS subsystem as *entities* than as
*objects*, because it is common practice in OOP to give the name of
“*objects*” to class instances: since CLIPS has its own object oriented
structure (in fact there are *classes* in CLIPS, and therefore also
*instances* of these), calling these structures simply *objects* may
generate confusion.

Entities in CLIPS are generated by *constructs*, one for each type of
entity. In Python, the common way to create something is to instance an
for a certain . So it seemed straightforward to make a class for each of
these entities, and to substitute the constructs used in CLIPS to create
entities with *factory functions*. These functions are defined at module
level, and have names of the type where is the type of entity that has
to be created. The only exception for this are objects, which are
created in several ways from s or ions.

There is another way to create entities in the CLIPS subsystem, that is
directly using the function with a full CLIPS construct as string
argument. However, this function does not return anything to the caller,
so the created entity has to be sought after creation to obtain a
reference.

The functions and the function return objects of proper types (whose
detailed list is given below) which shadow the corresponding entities in
the CLIPS space.

Wrapper Classes\[pyclips-cl-wrapper\]
-------------------------------------

There are some simple classes that deserve a special mention in the
module, used to represent in Python namespace the basic types in CLIPS.
These *wrappers* are used to differentiate values that CLIPS returns
from other values that live in the Python space. However these classes
are equivalent to their Python counterparts, and there is no need to
pass objects converted to these classes to the module functions. Here is
a list containing the class names and their equivalents in Python:

<span>l|l|l</span><span>class</span><span>Class</span><span>Type</span><span>Python
Equivalent</span>

A special object named is defined, and is equivalent to in comparisons
and slot assignments. It is provided to make code more readable in such
situations. It has to be noticed that also evaluates to in boolean
tests: this also yields for the explicit and definitions[^14].

Template\[pyclips-cl-Template\]
-------------------------------

s are used to build objects, that is, they provide a systematic way to
construct s sharing a common pattern, and the only way to define s that
have named (the equivalent of record *fields* or *structure members* in
other programming languages).

<span>Template</span>

This represents a copy of a construct in the CLIPS subsystem, and not a
true entity. More than one object in Python can refer to the same entity
in the CLIPS subsystem.

<span>BuildFact</span> Build a object using this without asserting it.
The created can be modified and the asserted using its method.

\[property\]<span>Deletable</span> Read-only property to verify if this
can be deleted from the CLIPS subsystem.

<span>InitialFact</span> Return initial in list created using this .

\[property\]<span>Module</span> Read-only property to retrieve the CLIPS
name of the where the is defined.

\[property\]<span>Name</span> Read-only property returning the name in
CLIPS of this . The name identifies this entity in the CLIPS subsystem,
and has nothing to do with the name given to the corresponding object in
Python.

<span>Next</span> Return next[^15] in the list of all s. is returned at
the end of the list.

<span>NextFact</span><span>fact</span> Return next in list created using
this , using the supplied as offset.

<span>PPForm</span> Return the *pretty-print form* of this .
*Pretty-print forms* are often the code necessary to build a construct
in CLIPS, formatted in a way that makes it quite readable. The result of
the method can be used as the argument for the top level function to
rebuild the construct once the has been cleared[^16].

<span>Remove</span> Remove the entity corresponding to this from the
CLIPS subsystem. This does not remove the corresponding Python object
that has instead to be deleted via the statement or garbage collected.

\[property\]<span>Slots</span> information. This is itself an object,
having many methods, and deserves a special explaination.

<span>AllowedValues</span><span>name</span> Return a list of allowed
values for specified by .

<span>Cardinality</span><span>name</span> Return *cardinality* for
specified by .

<span>DefaultValue</span><span>name</span> Return *cardinality* for
specified by .

<span>Exists</span><span>name</span> Return if specified by exists,
otherwise.

<span>HasDefault</span><span>name</span> Return one of the following
values: if the default value is set to , when the default value is
static and when it is dynamically generated (eg. ).

<span>IsMultifileld</span><span>name</span> Return if specified by is a
value, otherwise.

<span>IsSinglefield</span><span>name</span> Return if specified by is a
single field value, otherwise.

<span>Names</span> Return the list of names.

<span>Range</span><span>name</span> Return *numerical range information*
for specified by .

<span>Types</span><span>name</span> Return names of *primitive types*
for specified by .

\[property\]<span>Watch</span> Read-only property to verify if this is
being watched.

The name of this entity in CLIPS is also returned by the string coercion
function. The *factory function* for s is , which has been discussed
above.

Fact\[pyclips-cl-Fact\]
-----------------------

s are one of the main entities in CLIPS, since it is whether a exists or
not of that drives the subsystem in the decision to fire or not certain
s. s, as seen above, can be created in several ways, that is either by
directly asserting sentences in string form, or by building them first
from s and then asserting them.

<span>Fact</span>

This represents a copy of a fact definition in the CLIPS subsystem, and
not a true fact entity. More than one Fact objects in Python can refer
to the same fact entity in the CLIPS subsystem. Many CLIPS functions
return a object, but most objects obtained from CLIPS are
*read-only*[^17]. Read-only s cannot be reasserted or modified in , and
are provided for “informational” purposes only.

The argument can be a string with the same format of the function seen
in the previous chapter: in this case the fact is created and asserted.
Otherwise the argument can be a object, and in this case the resulting
can be modified and then asserted via the member function.

<span>Assert</span> Assert this . Only s that have been constructed from
s can be ed using this method: read-only s can only be inspected with
the other methods/properties.

<span>AssignSlotDefaults</span> Assign default values to of this .

<span>CleanPPForm</span> Return only the second part of this s
*pretty-print form* – which can be used to build the itself as described
above.

<span>Exists</span> Is if this has been asserted (and never retracted),
otherwise.

\[property\]<span>ImpliedSlots</span> The list of all *implied* for this
.

\[property\]<span>Index</span> Read-only property returning the index in
CLIPS of this . As for other entities the is a unique identifier, as is
the for s.

<span>Next</span> Return next in the list of all s. This list is not
based on s, but global to the CLIPS subsystem.

<span>PPForm</span> Return the *pretty-print form* of this . In this
case, only the second part of the returned string (the one between
parentheses) can be used to build the via the function[^18].

<span>PPrint</span> Print the to the standard output. When is set to
(the default), slots containing the default values are omitted.

\[property\]<span>Relation</span> Return only the name of the *relation*
that identifies this [^19] as a .

<span>Retract</span> Retract this : in other words, remove the
corresponding entity from the CLIPS subsystem. As in seen above, this
does not delete the corresponding Python object. is returned at the end
of the list.

\[property\]<span>Slots</span> Dictionary of . This member *behaves*
like a , but is not related to such objects. In fact, the values of are
accessible using a -like syntax (square brackets), but not all the
members of are implemented.

Please note that s have slightly different methods than classes
representing other entities in CLIPS: an instance of is created using
the module-level function, and removed using the member function: this
syntax, closer to the original CLIPS form, was seen as the preferred
method instead of using a name such as for creation and a member because
of the particular nature of related to other types of entity.

Here is an example of usage of and objects:

    >>> import clips
    >>> clips.Reset()
    >>> t0 = clips.BuildTemplate("person", """
        (slot name (type STRING))
        (slot age (type INTEGER))
    """, "template for a person")
    >>> print t0.PPForm()
    (deftemplate MAIN::person "template for a person"
       (slot name (type STRING))
       (slot age (type INTEGER)))

    >>> f1 = clips.Fact(t0)
    >>> f1_slotkeys = f1.Slots.keys()
    >>> print f1_slotkeys
    <Multifield [<Symbol 'name'>, <Symbol 'age'>]>
    >>> f1.Slots['name'] = "Grace"
    >>> f1.Slots['age'] = 24
    >>> print f1.PPForm()
    f-0     (person (name "Grace") (age 24))
    >>> clips.PrintFacts()
    f-0     (initial-fact)
    >>> f1.Assert()
    <Fact 'f-1': fact object at 0x00E0CB10>
    >>> print f1.Exists()
    True
    >>> clips.PrintFacts()
    f-0     (initial-fact)
    f-1     (person (name "Grace") (age 24))
    For a total of 2 facts.
    >>> f1.Retract()
    >>> print f1.Exists()
    False
    >>> clips.PrintFacts()
    f-0     (initial-fact)
    For a total of 1 fact.

Please note that slot names are implemented as s, and the list of is
returned as a . Also note that the , that has been constructed from a
(and not yet ed) object and then modified using the property, can be ed
while other s built from construct strings cannot.

Deffacts\[pyclips-cl-Deffacts\]
-------------------------------

A is used to modify the “initial structure” of a CLIPS environment, by
allowing some s to be ed by default each time the function is called.

<span>Deffacts</span>

This represents a copy of a construct in the CLIPS subsystem, and not a
true entity. More than one object in Python can refer to the same entity
in the CLIPS subsystem.

\[property\]<span>Deletable</span> Read-only property to verify if this
can be deleted.

\[property\]<span>Module</span> Read-only property to retrieve the CLIPS
name of the where the is defined.

\[property\]<span>Name</span> Read-only property returning the name in
CLIPS of this .

<span>Next</span> Return next in the list of all . is returned at the
end of the list.

<span>PPForm</span> Return the *pretty-print form* of this .

<span>Remove</span> Remove the entity corresponding to this from the
CLIPS subsystem.

The name of this entity in CLIPS is also returned by the string coercion
function. The *factory function* for is , which has been discussed
above.

Rule\[pyclips-cl-Rule\]
-----------------------

The construct defines rules to be activated and then *fired* whenever
particular conditions are met. This construct is in fact the counterpart
of the construct in CLIPS. Normally conditions that fire s are s ed
during a session.

<span>Rule</span>

This represents a copy of a construct in the CLIPS subsystem, and not a
true entity. More than one object in Python can refer to the same entity
in the CLIPS subsystem.

\[property\]<span>Breakpoint</span> Set or remove a breakpoint from this
.

\[property\]<span>Deletable</span> Read-only property to verify if this
can be deleted.

\[property\]<span>Module</span> Read-only property to retrieve the CLIPS
name of the where the is defined.

\[property\]<span>Name</span> Read-only property returning the name in
CLIPS of this .

<span>Next</span> Return next in the list of all s. is returned at the
end of the list.

<span>PPForm</span> Return the *pretty-print form* of this .

<span>PrintMatches</span> Print partial matches of this to standard
output.

<span>Refresh</span> Refresh this .

<span>Remove</span> Remove the entity corresponding to this from the
CLIPS subsystem.

\[property\]<span>WatchActivations</span> Set or reset debugging of
*activations* for this .

\[property\]<span>WatchFirings</span> Set or reset debugging of
*firings* for this .

The name of this entity in CLIPS is also returned by the string coercion
function. The *factory function* for s is , which has been discussed
above.

An example – derived from the ones present in the standard CLIPS
documentation – may be useful here:

    >>> clips.Reset()
    >>> r1 = clips.BuildRule("duck-rule", "(duck)",
                             "(assert (quack))", "The Duck rule")
    >>> print r1.PPForm()
    (defrule MAIN::duck-rule "The Duck rule"
       (duck)
       =>
       (assert (quack)))

    >>> clips.PrintFacts()
    f-0     (initial-fact)
    For a total of 1 fact.
    >>> clips.PrintRules()
    MAIN:
    duck-rule
    >>> f1 = clips.Assert("(duck)")
    >>> clips.PrintAgenda()
    MAIN:
       0      duck-rule: f-1
    For a total of 1 activation.
    >>> clips.PrintFacts()
    f-0     (initial-fact)
    f-1     (duck)
    For a total of 2 facts.
    >>> clips.Run()
    >>> clips.PrintFacts()
    f-0     (initial-fact)
    f-1     (duck)
    f-2     (quack)
    For a total of 3 facts.

Activation\[pyclips-cl-Activation\]
-----------------------------------

s are only returned by the CLIPS subsystem, and cannot be created – thus
there is no *factory function* for these objects. CLIPS provides objects
to keep the program flow under control.

<span>Activation</span>

This represents a copy of an object in the CLIPS subsystem, and not a
true entity. More than one object in Python can refer to the same entity
in the CLIPS subsystem.

\[property\]<span>Name</span> Retrieve name.

<span>Next</span> Return next in the list of all s. is returned at the
end of the list.

<span>PPForm</span> Return the *pretty-print form* of .

\[property\]<span>Salience</span> Retrieve *salience*[^20].

<span>Remove</span> Remove this from CLIPS.

The name of this entity in CLIPS is also returned by the string coercion
function.

Global\[pyclips-cl-Global\]
---------------------------

objects represent *global variables* in CLIPS, which are normally built
using the construct. To define a new variable the function must be used,
which returns a new object.

<span>Global</span>

A object represents a copy of a construct in the CLIPS subsystem, and
not a true entity. More than one object in Python can refer to the same
entity in the CLIPS subsystem.

\[property\]<span>Deletable</span> Verify if this can be deleted.

\[property\]<span>Module</span> Read-only property to retrieve the CLIPS
name of the where the is defined.

\[property\]<span>Name</span> Retrieve name. The returned value is a
containing the name of the global variable in the CLIPS subsystem.

<span>Next</span> Return next in the list of all global variables. is
returned at the end of the list.

<span>PPForm</span> Return the *pretty-print* form of .

<span>Remove</span> Remove this from CLIPS subsystem.

\[property\]<span>Value</span> Set or retrieve value. The returned value
can be of many types, depending on the type of value contained in the
corresponding CLIPS global variable.

<span>ValueForm</span> Return a “*printed*” form of value. The *printed*
form is the one that would be used in CLIPS to represent the variable
itself.

\[property\]<span>Watch</span> Set or retrieve debug status.

Some examples follow to show the use of objects:

    >>> g_x = clips.BuildGlobal("x", 15)

This is equivalent to the CLIPS declaration:

    CLIPS> (defglobal ?*x* = 15)

Some of the methods are illustrated here:

    >>> g_x
    <Global 'x': defglobal object at 0x00E09960>
    >>> print g_x
    x
    >>> g_x.Value
    <Integer 15>
    >>> print g_x.Value
    15
    >>> print g_x.ValueForm()
    ?*x* = 15

The name of this entity in CLIPS is also returned by the string coercion
function.

Function\[pyclips-cl-Function\]
-------------------------------

Objects of this type represent newly defined *functions* (usually via
the CLIPS construct) in the CLIPS subsystem. In fact the function
described above, which returns a object, corresponds to the construct.

<span>Function</span>

This represents a copy of a construct in the CLIPS subsystem, and not a
true entity. More than one object in Python can refer to the same entity
in the CLIPS subsystem.

<span>Call</span> Call this with the given arguments, if any. If one
only argument is passed and it is a , then it is considered a “list of
whitespace separated arguments[^21]” and follows the CLIPS syntax: in
order to pass a single string it has to be explicitly cast to the
*wrapper class*. Conversion to *wrapper classes* is however recommended
for all passed arguments.

\[property\]<span>Deletable</span> Verify if this can be deleted.

\[property\]<span>Module</span> Read-only property to retrieve the CLIPS
name of the where the is defined.

\[property\]<span>Name</span> Retrieve name.

<span>Next</span> Return next in the list of all CLIPS functions. is
returned at the end of the list.

<span>PPForm</span> Return the *pretty-print form* of .

<span>Remove</span> Remove this .

\[property\]<span>Watch</span> Set or retrieve debug status.

The name of this entity in CLIPS is also returned by the string coercion
function.

Generic\[pyclips-cl-Generic\]
-----------------------------

s (in CLIPS called *generic functions*) are similar to s, but they add
*generic programming* capabilities to the CLIPS system. Python
programmers will find them similar to Python functions, since
*overloading* is possible within the corresponding construct.

Each different implementation (for different argument sets) of a
*generic function* is called a , and the class provides several ways to
inspect the various s. s are identified by an *index*.

<span>Generic</span>

This represents a copy of a construct in the CLIPS subsystem, and not a
true entity. More than one objects in Python can refer to the same
entity in the CLIPS subsystem.

<span>AddMethod</span><span>restrictions, actions</span> Add a to this .
The structure of this function resembles the one of the functions: in
fact this method of actually implements the construct which is present
in CLIPS. For proper documentation of this construct, see the CLIPS
reference: the parameter (which represents the *parameter restrictions*)
must be expressed *without* parentheses; the parameter must be expressed
as in the construct, that is with all the necessary parentheses pairs.
is the index when it has to be forced (optionally). The example below
should be explanatory. can also be expressed as a sequence of tuples, in
each of which the first element is the argument name (with its proper
prefix) as a string and the following ones are the actual restrictions,
either in string form or as CLIPS primitive types – which can be
specified using *wrapper classes* types, see above.

<span>Call</span> Call this with the given arguments, if any. If one
only argument is passed and it is a , then it is considered a “list of
whitespace separated arguments” and follows the CLIPS syntax: in order
to pass a single string it has to be explicitly cast to the *wrapper
class*. Conversion to *wrapper classes* is however recommended for all
passed arguments.

\[property\]<span>Deletable</span> Verify if this can be deleted.

<span>InitialMethod</span> Return the index of first in this .

<span>MethodDeletable</span><span>midx</span> Test whether or not
specified can be deleted from this .

<span>MethodDescription</span><span>midx</span> Return the synopsis of
specified *restrictions*.

<span>MethodList</span> Return the list of indices for this .

<span>MethodPPForm</span><span>midx</span> Return the *pretty-print
form* of specified .

<span>MethodRestrictions</span><span>midx</span> Return the
*restrictions* of specified in this object: the parameter must be an or
indicating the index.

<span>MethodWatched</span><span>midx</span> Test whether or not
specified is being watched.

\[property\]<span>Module</span> Read-only property to retrieve the CLIPS
name of the where the is defined.

\[property\]<span>Name</span> Retrieve name.

<span>NextMethod</span><span>midx</span> Return the index of next in
this given the start index as an or .

<span>PPForm</span> Return the *pretty-print form* of .

<span>PrintMethods</span> Print out list for this .

<span>Remove</span> Remove this .

<span>RemoveMethod</span><span>midx</span> Remove specified from this .

<span>UnwatchMethod</span><span>midx</span> Deactivate watch on
specified .

\[property\]<span>Watch</span> Set or retrieve debug status.

<span>WatchMethod</span><span>midx</span> Activate watch on specified .

The name of this entity in CLIPS is also returned by the string coercion
function. The *factory function* for s is , which has been discussed
above.

An example for this class follows.

    >>> import clips
    >>> addf = clips.BuildGeneric("my-addf", "my generic add function")
    >>> addf.AddMethod("(?a STRING)(?b STRING)", "(str-cat ?a ?b)")
    >>> addf.AddMethod("(?a INTEGER)(?b INTEGER)", "(+ ?a ?b)")
    >>> addf.PrintMethods()
    my-addf #1  (STRING) (STRING)
    my-addf #2  (INTEGER) (INTEGER)
    For a total of 2 methods.
    >>> print addf.MethodPPForm(1)
    (defmethod MAIN::my-addf
       ((?a STRING)
        (?b STRING))
       (str-cat ?a ?b))

    >>> print addf.PPForm()
    (defgeneric MAIN::my-addf "my generic add function")

    >>> print clips.Eval('(my-addf 5 13)')
    18
    >>> print clips.Eval('(my-addf "hello,"(my-addf " " "world!"))')
    hello, world!
    >>> print clips.Eval('(my-addf "hello" 13)')
    Traceback (most recent call last):
      File "<pyshell#14>", line 1, in ?
        print clips.Eval('(my-addf "hello" 13)')
      File ".../_clips_wrap.py", line 2472, in Eval
        return _cl2py(_c.eval(expr))
    ClipsError: C10: unable to evaluate expression
    >>> s = clips.ErrorStream.Read()
    >>> print s
    [GENRCEXE1] No applicable methods for my-addf.

Please note how the *error stream* () can be used to retrieve a more
explanatory text for the error. The *error stream* can be very useful
during interactive debugging sessions to fix errors.

Class\[pyclips-cl-Class\]
-------------------------

objects are definition constructs, the most important feature of the
*COOL*[^22] sublanguage of CLIPS. As in other OOP environments, es
represent in CLIPS new data types (often resulting from aggregation of
simpler data types) which have particular ways of being handled.
Normally, as in Python, these particular ways are called *methods*[^23],
while in CLIPS they are called *message handlers*, since to apply a
method to a CLIPS object (in fact, the of a in ) a *message* has to be
sent to that object.

<span>Class</span>

This represents a copy of a construct in the CLIPS subsystem, and not a
true entity. More than one object in Python can refer to the same entity
in the CLIPS subsystem.

\[property\]<span>Abstract</span> Verify if this is *abstract* or not.

<span>AddMessageHandler</span><span>name, args, text </span> Add a new
*message handler* to this class, with specified name, body (the
argument) and argument list: this can be specified either as a sequence
of variable names or as a single string of whitespace separated variable
names. Variable names (expressed as strings) can also be *wildcard
parameters*, as specified in the <span>*Clips Reference Guide Vol. I:
Basic Programming Guide*</span>. The parameter should be one of , , ,
defined at the module level: if omitted it will be considered as . The
body must be enclosed in brackets, as it is in CLIPS syntax. The
function returns the *index* of the *message handler* within this .

<span>AllMessageHandlerList</span> Return the list of constructs of this
including the ones that have been inherited from the superclass.

<span>BuildInstance</span><span>name, </span> Build an of this with the
supplied name and overriding specified . If no is specified to be
overridden, then the will assume default values.

<span>BuildSubclass</span><span>name, text </span> Build a subclass of
this with specified name and body. is the optional comment to give to
the object.

\[property\]<span>Deletable</span> Verify if this can be deleted.

<span>Description</span> Return a summary of description.

<span>InitialInstance</span> Return initial of this . It raises an error
if the has no subclass s.

<span>InitialSubclassInstance</span> Return initial instance of this
including its subclasses. It raises an error if the has no subclass s.

<span>IsSubclassOf</span><span>o</span> Test whether this is a subclass
of specified .

<span>IsSuperclassOf</span><span>o</span> Test whether this is a
superclass of specified .

<span>MessageHandlerDeletable</span><span>index</span> Return true if
specified can be deleted.

<span>MessageHandlerIndex</span><span>name </span> Find the specified ,
given its and type (as the parameter ). If type is omitted, it is
considered to be .

<span>MessageHandlerName</span><span>index</span> Return the name of
specified .

<span>MessageHandlerList</span> Return the list of constructs for this .

<span>MessageHandlerPPForm</span><span>index</span> Return the
*pretty-print form* of .

<span>MessageHandlerType</span><span>index</span> Return the type of the
specified by the provided .

<span>MessageHandlerWatched</span><span>index</span> Return watch status
of specified .

\[property\]<span>Module</span> Read-only property to retrieve the CLIPS
name of the where the is defined.

\[property\]<span>Name</span> Retrieve name.

<span>Next</span> Return next in the list of all CLIPS . is returned at
the end of the list.

<span>NextInstance</span><span>instance</span> Return next of this .
Returns if there are no s left.

<span>NextMessageHandlerIndex</span><span>index</span> Return index of
next with respect to the specified one.

<span>NextSubclassInstance</span><span>instance</span> Return next
instance of this , including subclasses. Returns if there are no s left.

<span>PPForm</span> Return the *pretty-print form* of .

<span>PreviewSend</span><span>msgname</span> Print list of s suitable
for specified message.

<span>PrintAllMessageHandlers</span> Print the list of all s for this
including the ones that have been inherited from the superclass.

<span>PrintMessageHandlers</span> Print the list of s for this .

<span>RawInstance</span><span>name</span> Create an empty of this with
specified name.

\[property\]<span>Reactive</span> Verify if this is *reactive* or not.

<span>Remove</span> Remove this .

<span>RemoveMessageHandler</span><span>index</span> Remove specified by
the provided .

\[property\]<span>Slots</span> information. This is itself an object,
having many methods, and deserves a special explaination.

<span>AllowedClasses</span><span>name</span> Return a list of allowed
class names for specified by .

<span>AllowedValues</span><span>name</span> Return a list of allowed
values for specified by .

<span>Cardinality</span><span>name</span> Return *cardinality* for
specified by .

<span>DefaultValue</span><span>name</span> Return the default value for
specified by .

<span>Exists</span><span>name</span> Return if specified by exists,
otherwise.

<span>ExistsDefined</span><span>name</span> Return if specified by is
defined in this , otherwise.

<span>Facets</span><span>name</span> Return *facet names* for specified
by .

<span>HasDirectAccess</span><span>name</span> Return if specified by is
directly accessible, otherwise.

<span>IsInitable</span><span>name</span> Return if specified by is
*initializable*, otherwise.

<span>IsPublic</span><span>name</span> Return if specified by is
*public*, otherwise.

<span>IsWritable</span><span>name</span> Return if specified by is
*writable*, otherwise.

<span>Names</span> Return the list of names.

<span>NamesDefined</span> Return the list of names explicitly defined in
this .

<span>Range</span><span>name</span> Return *numerical range information*
for specified by .

<span>Sources</span><span>name</span> Return *source class names* for
specified by .

<span>Types</span><span>name</span> Return names of *primitive types*
for specified by .

<span>Subclasses</span> Return the names of subclasses of this .

<span>Superclasses</span> Return the names of superclasses of this .

<span>UnwatchMessageHandler</span><span>index</span> Turn off debug for
specified .

\[property\]<span>WatchInstances</span> Set or retrieve debug status for
this s.

<span>WatchMessageHandler</span><span>index</span> Turn on debug for
specified .

\[property\]<span>WatchSlots</span> Set or retrieve debug status.

The name of this entity in CLIPS is also returned by the string coercion
function. The *factory function* for es is , which has been discussed
above.

Instance\[pyclips-cl-Instance\]
-------------------------------

objects represent *class instances* (that is, *objects* in the OOP
paradigm) that live in the CLIPS subsystem. Messages can be sent to
those objects and values can be set and retrieved for the *slots*
defined in the related *class*, where the meaning of has been described
in the section above.

<span>Instance</span>

This represents a copy of an object in the CLIPS subsystem, and not a
true entity. More than one object in Python can refer to the same entity
in the CLIPS subsystem.

\[property\]<span>Class</span> Retrieve the of this : this property
actually refers to a object, so all of its methods are available.

<span>DirectRemove</span> Directly remove this , without sending a
message.

<span>GetSlot</span><span>slotname</span> Retrieve the value of
specified as argument. The synonym is retained for readability and
compatibility. Please notice that these functions are provided in order
to be more coherent with the behaviour of CLIPS API, as CLIPS C
interface users know that a function like usually bypasses message
passing, thus accessing directly. The possibilities offered by are also
accessible using the property described below.

<span>IsValid</span> Determine if this is still valid.

\[property\]<span>Name</span> Retrieve the name.

<span>Next</span> Return next in the list of all CLIPS . It returns if
there are no s left.

<span>PPForm</span> Return the *pretty-print form* of .

<span>PutSlot</span><span>slotname, value</span> Set the value of
specified . The parameter should contain a value of the correct type, if
necessary cast to one of the *wrapper classes* described above if the
type could be ambiguous. The synonym is provided for readability and
compatibility. What has been said about also yields for the hereby
described function, as the possibilities offered by are also accessible
using the property described below.

<span>Remove</span> Remove this (passing a message).

<span>Send</span><span>msg </span> Send the provided *message* with the
given arguments to . The parameter (that is, *message arguments*),
should be a string containing a list of arguments separated by
whitespace, a tuple containing the desired arguments or a value of a
basic type. Also in the second case the tuple elements have to be of
basic types. The function returns a value depending on the passed
message.

\[property\]<span>Slots</span> Dictionary of . This member *behaves*
like a , but is not related to such objects. In fact, the values of are
accessible using a -like syntax (square brackets), but not all the
members of are implemented. The functionality of and is superseded by
this property.

The name of this entity in CLIPS is also returned by the string coercion
function. The *factory function* for s is , which has been discussed
above.

Here is an example of usage of and objects:

    >>> import clips
    >>> clips.Build("""(defclass TEST1
        (is-a USER)
        (slot ts1 (type INSTANCE-NAME))
        (multislot ts2))""")
    >>> c = clips.FindClass("TEST1")
    >>> print c.PPForm()
    (defclass MAIN::TEST1
       (is-a USER)
       (slot ts1
          (type INSTANCE-NAME))
       (multislot ts2))

    >>> clips.Reset()
    >>> i = clips.BuildInstance("test1", c)
    >>> i.Slots['ts2'] = clips.Multifield(['hi', 'there'])
    >>> i.Slots['ts1'] = i.Name
    >>> print i.PPForm()
    [test1] of TEST1 (ts1 [test1]) (ts2 "hi" "there")

Definstances\[pyclips-cl-Definstances\]
---------------------------------------

As there are for objects, are supported in CLIPS by the construct: it
allows certain default s to be created each time a is issued. In this
construct is provided via the class.

<span>Definstances</span>

This represents a copy of the construct in the CLIPS subsystem, and not
a true entity. More than one object in Python can refer to the same
entity in the CLIPS subsystem.

\[property\]<span>Deletable</span> Verify if this can be deleted.

\[property\]<span>Module</span> Read-only property to retrieve the CLIPS
name of the where the is defined.

\[property\]<span>Name</span> Retrieve name.

<span>Next</span> Return next in the list of all CLIPS . is returned at
the end of the list.

<span>PPForm</span> Return the *pretty-print form* of this object.

<span>Remove</span> Delete this object from CLIPS subsystem.

The name of this entity in CLIPS is also returned by the string coercion
function. The *factory function* for is , which has been discussed
above.

Module\[pyclips-cl-Module\]
---------------------------

s are a way, in CLIPS, to organize constructs, facts and objects. There
is a big difference between *modules* and *environments*[^24]: one
should think of a as a *group* of definitions and objects, which can
interoperate with entities that are defined in other s. The class
provides methods, similar to the ones defined at top level, to directly
create entities as part of the itself, as well as methods to examine
contents. Also, objects have methods that instruct the related CLIPS to
become *current*, so that certain operations can be performed without
specifying the to which they have to be applied.

<span>Module</span>

This represents a copy of a construct in the CLIPS subsystem, and not a
true entity. More than one object in Python can refer to the same entity
in the CLIPS subsystem.

\[property\]<span>Name</span> Return the name of this .

<span>Next</span> Return next in the list of all CLIPS . is returned at
the end of the list.

<span>PPForm</span> Return the *pretty-print form* of this .

<span>SetCurrent</span> Make the that this object refers the current .

<span>SetFocus</span> Set focus to this .

For the following methods:

    TemplateList(), FactList(), DeffactsList(), ClassList(), DefinstancesList(),
    GenericList(), FunctionList(), GlobalList(), BuildTemplate(),
    BuildDeffacts(), BuildClass(), BuildDefinstances(), BuildGeneric(),
    BuildFunction(), BuildGlobal(), BuildRule(), BuildInstance(),
    PrintTemplates(), PrintDeffacts(), PrintRules(), PrintClasses(),
    PrintInstances(), PrintSubclassInstances(), PrintDefinstances(),
    PrintGenerics(), PrintFunctions(), PrintGlobals(), ShowGlobals(),
    PrintAgenda(), PrintBreakpoints(), ReorderAgenda(), RefreshAgenda()

please refer to the corresponding function defined at module level,
keeping in mind that these methods perform the same task but within the
where they are executed.

The name of this entity in CLIPS is also returned by the string coercion
function. The *factory function* for s is , which has been discussed
above.

Environment\[pyclips-cl-Environment\]
-------------------------------------

This class represents an *environment*, and implements almost all the
module level functions and classes. The only objects appearing at level
and *not* at level are the CLIPS I/O subsystem *streams*, which are
shared with the rest of the CLIPS engine.

objects are not a feature of CLIPS sessions (as stated above), thus
there is no way to identify them in CLIPS using a *symbol*. So objects
do not have a property. Instead, CLIPS provides a way to identify an
*environment* through an integer called *index*.

<span>Environment</span>

Please refer to top level functions, variables and classes for
information on contents of objects. The extra methods and properties
follow below.

\[property\]<span>Index</span> Retrieve the *index* identifying this
internally in CLIPS.

<span>SetCurrent</span> Make the *environment* that this object refers
the current .

Further explanations about objects can be found in the appendices.

Status and Configuration Objects\[pyclips-cl-statusconf\]
---------------------------------------------------------

As seen in the introduction, there are a couple of objects that can be
accessed to configure the underlying CLIPS engine and to retrieve its
status. These are the and objects. The reason why configuration and
status functions have been grouped in these objects is only cosmetic: in
fact there is no counterpart of and in CLIPS. It was seen as convenient
to group configuration and debug functions in two main objects and to
make them accessible mainly as *properties* in Python, instead of
populating the module namespace with too many *get/set* functions.

There is also an object, called , which gives information about memory
utilization and allow the user to attempt to free memory used by the
CLIPS engine and no longer needed.

A description of what the above objects (which can not be instanced by
the user of [^25]) actually expose follows.

### Engine Configuration\[pyclips-cl-statusconf-engine\]

The object allows the configuration of some features of the underlying
CLIPS engine. Here are the properies provided by :

\[property\]<span>AutoFloatDividend</span> Reflects the behaviour of
CLIPS . When the dividend is always considered to be a floating point
number within divisions.

\[property\]<span>ClassDefaultsMode</span> Reflects the behaviour of
CLIPS . Possible values of this flag are and . See <span>*Clips
Reference Guide Vol. II: Advanced Programming Guide*</span> for details.

\[property\]<span>DynamicConstraintChecking</span> Reflects the
behaviour of CLIPS . When , *function calls* and *constructs* are
checked against constraint violations.

\[property\]<span>FactDuplication</span> Reflects the behaviour of CLIPS
. When , can be reasserted when they have already been asserted[^26].

\[property\]<span>IncrementalReset</span> Reflects the behaviour of
CLIPS . When newly defined are updated according to current , otherwise
new will only be updated by defined after their construction.

\[property\]<span>ResetGlobals</span> Reflects the behaviour of CLIPS .
When the variables are reset to their initial value after a call to .

\[property\]<span>SalienceEvaluation</span> Reflects the behaviour of
CLIPS . Can be one of , , . See the previous chapter and <span>*Clips
Reference Guide Vol. II: Advanced Programming Guide*</span> for more
information.

\[property\]<span>SequenceOperatorRecognition</span> Reflects the
behaviour of CLIPS . When , values in function calls are treated as a
single argument.

\[property\]<span>StaticConstraintChecking</span> Reflects the behaviour
of CLIPS . When , *slot values* are checked against constraint
violations.

\[property\]<span>Strategy</span> Reflects behaviour. Can be any of the
following values: , , , , , or . See the previous chapter and
<span>*Clips Reference Guide Vol. II: Advanced Programming Guide*</span>
for more information.

### Debug Settings\[pyclips-cl-statusconf-debug\]

The object provides access to the debugging and trace features of CLIPS.
During a CLIPS interactive session debug and trace messages are printed
on the system console (which maps the I/O *router*). Users of the trace
systems will have to poll the to read the generated messages.

In CLIPS, the process of enabling trace features on some class of
entities is called *to watch* such a class; this naming convention is
reflected in . Note that specific objects can be *watched*: many classes
have their own property to enable or disable debugging on a particular
object.

Also, CLIPS provides a facility to log all debug information to physical
files: this is called *to dribble* on a file. *Dribbling* is possible
from via the appropriate methods.

The names of methods and properties provided by this object are quite
similar to the corresponding commands in CLIPS, so more information
about debugging features can be found in <span>*Clips Reference Guide
Vol. I: Basic Programming Guide*</span>.

\[property\]<span>ActivationsWatched</span> Flag to enable or disable
trace of activations and deactivations.

\[property\]<span>CompilationsWatched</span> Flag to enable or disable
trace of construct definition progress.

<span>DribbleActive</span> Tell whether or not *dribble* is active.

<span>DribbleOff</span> Turn off *dribble* and close the *dribble* file.

<span>DribbleOn</span><span>fn</span> Enable *dribble* on the file
identified by provided filename .

\[property\]<span>ExternalTraceback</span> Flag to enable or disable
printing traceback messages to Python if an error occurs when the CLIPS
engine calls a Python function. Please note that the error is not
propagated to the Python interpreter. See the appendices for a more
detailed explaination.

\[property\]<span>FactsWatched</span> Flag to enable or disable trace of
assertions and retractions.

\[property\]<span>FunctionsWatched</span> Flag to enable or disable
trace of start and finish of .

\[property\]<span>GenericFunctionsWatched</span> Flag to enable or
disable trace of start and finish of functions.

\[property\]<span>GlobalsWatched</span> Flag to enable or disable trace
of assignments to variables.

\[property\]<span>MethodsWatched</span> Flag to enable or disable trace
of start and finish of within functions.

\[property\]<span>MessageHandlersWatched</span> Flag to enable or
disable trace of start and finish of .

\[property\]<span>MessagesWatched</span> Flag to enable or disable trace
of start and finish of *messages*.

\[property\]<span>RulesWatched</span> Flag to enable or disable trace of
firings.

\[property\]<span>SlotsWatched</span> Flag to enable or disable trace of
changes to .

\[property\]<span>StatisticsWatched</span> Flag to enable or disable
reports about timings, number of and , and other information after has
been performed.

<span>UnwatchAll</span> Turn off *watch* for all items above.

<span>WatchAll</span> *Watch* all items above.

### Memory Operations\[pyclips-cl-statusconf-memory\]

This object provides access to the memory management utilities of the
underlying CLIPS engine. As said above, it allows the reporting of
memory usage and the attempt to free memory that is used not for
computational purposes. Also, a property of this object affects the
engine behaviour about whether or not to cache some information. Here is
what the object exposes:

\[property\]<span>Conserve</span> When set to , the engine does not
cache *pretty-print forms* to memory, thus being more conservative.

\[property\]<span>EnvironmentErrorsEnabled</span> When set to , the
engine is enabled to directly write fatal environment errors to the
console (). This kind of messages is in most of the cases printed when
the program exits, so it can be annoying. The behaviour is disabled by
default.

<span>Free</span> Attempt to free as much memory as possible of the one
used by the underlying CLIPS engine for previous computations.

\[property\]<span>PPBufferSize</span> Report the size (in bytes) of the
buffers used by to return *pretty-print forms* or similar values. By
default this is set to 8192, but the user can modify it using values
greater than or equal to 256. Greater values than the default can be
useful when such forms are used to reconstruct CLIPS entities and
definitions are so complex that the default buffer size is insufficient.

\[property\]<span>Requests</span> Read-only property reporting the
number of memory request made by the engine to the operating system
since has been initialized.

\[property\]<span>Used</span> Read-only property reporting the amount,
in kilobytes, of memory used by the underlying CLIPS engine.

\[property\]<span>NumberOfEnvironments</span> Read-only property
reporting the number of currently allocated s.

I/O Streams\[pyclips-cl-iostreams\]
-----------------------------------

In order to be more embeddable, CLIPS defines a clear way to redirect
its messages to locations where they can be easily retrieved. CLIPS
users can access these locations for reading or writing by specifying
them as *logical names* (namely , , , , , , , )[^27]. creates some
special unique objects[^28], called *I/O streams* throughout this
document, to allow the user to read messages provided by the underlying
engine. Most of these objects have only one method, called , that
consumes CLIPS output and returns it as a string: this string contains
all output since a previous call or module initialization. The only
exception is whose single method is and it accepts a string[^29] as
parameter. As CLIPS writes line-by-line, the string resulting from a
call to can contain newline characters, often indicating subsequent
messages.

Here is a list of the *I/O streams* provided by , along with a brief
description of each.

<span>l|l</span><span>var</span><span>Stream</span><span>Description</span>

Some of the provided *I/O streams* are actually not so relevant for
programmers: for instance, it is of little use to read the contents of
and . In the latter case, in fact, there are other inspection functions
that provide the same information in a more structured way than text.
However they are available to provide a closer representation of the
programming interface and allow CLIPS programmers to verify if the
output of *CLIPS-oriented* calls (see the paragraph about and in the
appendices) really do what they are expected to.

Predefined es\[pyclips-cl-stockclasses\]
----------------------------------------

defines[^30], some objects, that is the ones that are present in CLIPS
itself by default. They are defined in order to provide a compact access
to CLIPS “stock” classes: most of these objects are of little or no use
generally (although they can be handy when testing for class
specification or generalization), but at least one () can be used to
make code more readable.

Namely, these es are:

<span>l|l</span><span>var</span><span>Python Name</span><span>CLIPS
defclass</span>

The following code, shows how to use the “traditional” factory function
and how to directly subclass one of the predefined object. In the latter
case, probably, the action of subclassing is expressed in a more clear
way:

    >>> import clips
    >>> C = clips.BuildClass("C", "(is-a USER)(slot s)")
    >>> print C.PPForm()
    (defclass MAIN::C
       (is-a USER)
       (slot s))

    >>> D = clips.USER_CLASS.BuildSubclass("D", "(slot s)")
    >>> print D.PPForm()
    (defclass MAIN::D
       (is-a USER)
       (slot s))

Although it actually does not save typing (the statement is slightly
longer), the second form can be used to produce more readable Python
code.

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
represented by the object itself[^31]. Normally in a true CLIPS session
users would not use environments, so the concept of environment may, in
many cases, not be useful.

There are also functions (namely: and ) that allow the user to switch
environment and to use top level functions and classes in any of the
created environments. This is useful in cases where environments are
used, because due to the double nature of CLIPS API (see <span>*Clips
Reference Guide Vol. II: Advanced Programming Guide*</span> about
*companion functions* for standard API), objects defined in environments
have slightly different types than corresponding top level objects –
since these types are *environment-aware*. However environment aware
classes define exactly the same methods as the top level counterparts,
so their logical use is the same.

Please note that the function returns a fully functional object. However
the system prevents[^32] invasive access via *environment-aware*
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
fact, the use of allows definition of constructs in CLIPS[^33], and lets
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
using [^34], as in the following example:

    >>> clips.Eval('(printout t "Hello, World!" crlf)')
    >>> print clips.StdoutStream.Read()
    Hello, World!

There is another function, namely , that sends an entire CLIPS command
(it has to be a full, correct command, otherwise will issue an
exception): as it does not return any value or object[^35]. However this
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

    >>> clips.SendCommand("(assert (duck)")	# no right bracket

    Traceback (most recent call last):
      File "<pyshell#5>", line 1, in -toplevel-
        clips.SendCommand("(assert (duck)")	# no right bracket
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
specified, then the Python name will be used[^36]. If necessary, Python
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
    >>> clips.SetExternalTraceback(True)	# print traceback on error
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
will never raise exceptions[^37] in the Python calling layer.

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
    >>> clips.SetExternalTraceback(True)	# print traceback on error
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

Almost all functions described in <span>*Clips Reference Guide Vol. II:
Advanced Programming Guide*</span> are ported to Python, except for
missing features described below: the name is the same as in the
reference guide, except for the first letter that is not
capitalized[^38]. CLIPS “top level” functions have been ported to as
well as *companion functions* that accept an instance as the first
argument[^39], whose name begins with followed by the same name as the
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
counterparts[^40], have this form and describe in detail arguments and
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
countermeasures[^41].

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
(which directly maps CLIPS exposed functions as described in
<span>*Clips Reference Guide Vol. II: Advanced Programming
Guide*</span>) can be accessed using the submodule. Almost all the
functions defined here have a counterpart in the CLIPS API, and a
combined use of documentation strings and <span>*Clips Reference Guide
Vol. II: Advanced Programming Guide*</span> itself can allow you to
directly manipulate the CLIPS subsystem as you would have done by
embedding it in a C program. However, there are some functions that
appear in but, when called, issue an error:

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
and can be found in <span>*Clips Reference Guide Vol. II: Advanced
Programming Guide*</span> of CLIPS. It is not likely that these
functions will be implemented even in future versions of since Python
programmers are usually not interested in dealing with low level memory
handling (which is the primary use of the memory oriented functions),
and tasks like reference count handling are performed directly by Python
itself (for Python objects which shadow CLIPS entities) and by the low
level submodule. Also, the functions referred to above as *execution
hooks* often have to deal with CLIPS internal structures at a very low
level, so they would be of no use in a Python program.

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

is known to build and pass the tests on *Linux (x86 and x86\_64)*[^42],
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
conformant to the ones described in <span>*Clips Reference Guide Vol.
II: Advanced Programming Guide*</span>. Of course features present in
CLIPS 6.24 are not available when using the previous CLIPS version, so
if there is no particular reason to use it, please compile PyCLIPS with
CLIPS 6.24, which also fixes some bugs.

License Information\[pyclips-license\]
======================================

The following is the license text, which you can obtain by issuing a

    >>> import clips
    >>> print clips.license

at the Python prompt once the module has been installed.

[^1]: The order of parameters is changed sometimes, in order to allow a
    more intuitive use of default parameters in the Python interface:
    however the meaning of each parameter is described in the function
    documentation string, and it should not be difficult for the
    programmer to correctly understand the relationship between a module
    function and the corresponding CLIPS API.

[^2]: This has an impact on the way the module can be used, as the
    engine is only set up once when the module is ed the first time.

[^3]: In fact, the word *object* is used here to indicate an item that
    takes part in a program. For instance, one such object can be a : it
    is not a proper OO object, but something that in imperative
    languages would act as a control structure.

[^4]: It should be clear that terminology differs semantically from
    Python system to the CLIPS system: while OOP, to which terms used in
    Python are coherent, uses the words *method*, *instance* and so on
    with a particular meaning (to which Python developers are familiar),
    CLIPS terminology often differs from OOP, sometimes only slightly
    but at other times more substantially. The reader should note that
    throughout this manual each term is used – as far as possible – with
    the meaning that it assumes in its specific environment. In this
    case, the word *instance* represents the instance of a Python , and
    is not referred to an entity in CLIPS.

[^5]: Note that is capitalized: this is because the stream-like objects
    do not really act as “files” as many objects which can be read in
    Python do. So it becomes impossible to use these objects where a
    file-like object is to be used.

[^6]: This only happens actually when CLIPS invokes the or functions.

[^7]: The mentioned functions are also *members* of the class, in which
    case the status is saved.

[^8]: In fact the file has been slightly reformatted for typesetting
    reasons.

[^9]: It must be of a type compatible with CLIPS. If a string is
    supplied, however, it will be considered as a list of arguments
    separated by whitespace: in order to explicitly pass a string it has
    either to be converted or to be specified surrounded by double
    quotes.

[^10]: Complex values, as *multifield*, are not supported: CLIPS does
    not allow external calls with non-constant arguments and there is no
    possibility to build a *multifield* in place without an explicit
    function call.

[^11]: This means, for instance, that continuously using this function
    and checking whether or not the result is less than the specified
    limit can give more control over the running CLIPS subprogram,
    eventually giving the ability to actively check for the end of the
    program.

[^12]: Except for the CLIPS printing functions, as for instance , that
    issue an output even when the flag is not set.

[^13]: This is not always an error condition because a function can
    intentionally return boolean values to CLIPS. However the CLIPS
    engine will report an error message which can be read from the error
    stream.

[^14]: In this is different from as only the empty string evaluates to
    false in Python. However, it seemed closer to the assumption that
    symbols in CLIPS are not to be considered as “*literals*” (they are
    more similar to implicitly defined variables) to implement such
    behaviour, that can be reverted with an explicit conversion to .

[^15]: CLIPS stores its objects (or entities) in ordered lists, so it
    makes sense to “iterate” over these lists. However this
    implementation of does not implement *iterators* (as known in
    Python) on these classes: a way to do this is currently under
    examination.

[^16]: Actually *pretty-print forms* use fixed size buffers to build the
    representing string: when such a form is too complex, the default
    buffer size of 8192 bytes can be insufficient. In this case the
    *PPBufferSize* property of the *Memory* object can be used to allow
    the creation of properly sized buffers.

[^17]: Actually, the main rule is that if a has been ed then it is
    read-only. Note that all *shadow representations* of CLIPS asserted
    entities are read-only.

[^18]: This is also not always true: as said before, there is no way to
    s that have named slots using a string if there is not a for this
    kind of . However, once a with the specified slots has been created,
    this becomes possible.

[^19]: The authors of CLIPS call a *relation* the first field of the
    itself, although it is not needed to actually represent a real
    relationship.

[^20]: *Salience* is a value that represents the *priority* of a in
    CLIPS.

[^21]: See the syntax for the toplevel function with the same name.

[^22]: Acronym for CLIPS Object-Oriented Language.

[^23]: Note that the term has been used for function overloading in the
    definition of functions.

[^24]: Besides the discussion above, also notice that in a “pure” CLIPS
    session there is no concept of *environment* at all: the use of
    environment is reserved to those who embed CLIPS in another program,
    such as users.

[^25]: Besides removal of class definitions, a *singleton*-styled
    implementation mechanism prevents the user from creating further
    instances of the objects.

[^26]: This does not change the behaviour of the class, which prohibits
    reassertion anyway. However, that would be asserted through firing
    of rules and would generate duplications will not raise an error
    when this behaviour is set.

[^27]: CLIPS also defines as a *logical name*: as stated in <span>*Clips
    Reference Guide Vol. II: Advanced Programming Guide*</span> this
    indicates in functions that read text and in function that print
    out. In , for all functions that print out to the user must read
    from *StdoutStream*.

[^28]: in fact defines one more I/O stream, called , which is used
    internally to retrieve output from CLIPS that shouldn’t go anywhere
    else. users however are not supposed to interact with this object.

[^29]: The current implementation converts the argument to a string, so
    other types can be accepted.

[^30]: At the module level only: defining these objects at the
    *environment* level could cause aliasing current CLIPS enviroment.
    On the other hand, if these objects were implemented in a way that
    checked for aliasing, access to the actual entities would be surely
    slower only favouring compactness of user code.

[^31]: In fact, the Python submodule that implements the class is
    generated automatically: the process can be examined by looking at
    the code in and .

[^32]: Raising an appropriate exception.

[^33]: Note that the function does not return any value or object, so
    you will have to call to find entities created using the function.

[^34]: There is a discussion about functions that only have *side
    effects* in CLIPS, such as , in <span>*Clips User’s Guide*</span>,
    that is, the CLIPS tutorial.

[^35]: Some information about the command result can be retrieved
    reading the appropriate output streams.

[^36]: An example of function registration has been provided in the
    introduction.

[^37]: Exceptions can arise *during* the Python function execution, and
    can be caught inside the function code. However, for debugging
    purposes, there is the possibility to force print a standard
    traceback whenever an error occurs in a Python function called by
    CLIPS.

[^38]: There can be some exceptions: the most important one is the
    assertion function, since Python already has an keyword. This
    function is called instead.

[^39]: When trying to show the *documentation string* of these
    functions, the first argument is not described because their code
    has been generated automatically.

[^40]: Some functions have a documentation string that actually refers
    to the CLIPS API itself, explicitly containing the words
    “*equivalent of C API*” and the C function name: <span>*Clips
    Reference Guide Vol. II: Advanced Programming Guide*</span> is
    especially useful in this case.

[^41]: Please note that in some cases, and depending on how the
    operating system treats memory allocation failures, the Python
    interpreter too could loose stability in case of memory shortage.

[^42]: The x86\_64 platforms requires some optional patches to be
    applied.

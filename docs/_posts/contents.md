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
refer to for details about the usage of the two modes, as the meaning is
quite complex and outside the scope of this manual (see the property).

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

#### Message Handler Type Constants

Constants to define the execution time, the purpose and the behaviour of
*message handlers*: see function and the following members of the class:
and . The following table summarizes the analogous one found in .

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

#### Template Slot Default Type Constants

It’s possible to inspect whether or not a slot has been defined to have
a default value, and in case it is, if its default value is *static*
(that is, constant) or *dynamically generated* (for instance, using a
function like ). See the documentation of for more details.

<span>l|l</span><span>constant</span><span>Constant</span><span>Description</span>

Notice that evaluates to , so it’s legal to use the function just to
test the presence of a default value. Please also note that is only
returned when the default value for a slot is set to as stated in the .

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
*wildcard parameters*, as specified in the . The parameter should be one
of , , , defined at the module level: if omitted it will be considered
as . The body must be enclosed in brackets, as it is in CLIPS syntax.
The function returns the *index* of the *message handler* within the
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
specify the argument[^1] possibly cast using one of the *wrapper
classes* described below. When the function accepts multiple arguments
it is possible to specify them as a sequence of values (either a list or
a tuple) of basic[^2] values. It is always preferable to convert these
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
have been fired[^3].

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
stream. If is set to the possible[^4] command output is sent to the
appropriate output stream.

<span>SetExternalTraceback</span> Allow or disallow functions called
from within the CLIPS engine using to print a standard traceback to in
case an exception occurs. Please note that this does not mean that a
real exception arises, as there is no possibility to catch Python
exceptions in CLIPS code. In such case all failing Python functions will
return the symbol to CLIPS[^5]. This behaviour is initially set to , as
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

[^1]: It must be of a type compatible with CLIPS. If a string is
    supplied, however, it will be considered as a list of arguments
    separated by whitespace: in order to explicitly pass a string it has
    either to be converted or to be specified surrounded by double
    quotes.

[^2]: Complex values, as *multifield*, are not supported: CLIPS does not
    allow external calls with non-constant arguments and there is no
    possibility to build a *multifield* in place without an explicit
    function call.

[^3]: This means, for instance, that continuously using this function
    and checking whether or not the result is less than the specified
    limit can give more control over the running CLIPS subprogram,
    eventually giving the ability to actively check for the end of the
    program.

[^4]: Except for the CLIPS printing functions, as for instance , that
    issue an output even when the flag is not set.

[^5]: This is not always an error condition because a function can
    intentionally return boolean values to CLIPS. However the CLIPS
    engine will report an error message which can be read from the error
    stream.

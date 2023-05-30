# Basic Syntax

## Program list
1. [hello.erl](hello.erl) - it is a simple hello world program.
2. [import.erl](import.erl) - it is a simple program to show how to import modules.
3. [number.erl](number.erl) - it is a simple program to show how to use numbers.
4. [atom.erl](atom.erl) - it is a simple program to show how to use atoms.
5. [boolean.erl](boolean.erl) - it is a simple program to show how to use boolean.
6. [bit_string.erl](bit_string.erl) - it is a simple program to show how to use bit string.
7. [tuple.erl](tuple.erl) - it is a simple program to show how to use tuple.
8. [map.erl](map.erl) - it is a simple program to show how to use map.
9. [list.erl](list.erl) - it is a simple program to show how to use list.
10. [bit_op.erl](bit_op.erl) - it is a simple program to show how to use bitwise operators.
11. [while.erl](while.erl) - it is a simple program to show how to use while loop.
12. [for.erl](for.erl) - it is a simple program to show how to use for loop.
13. [if.erl](if.erl) - it is a simple program to show how to use if/case statement.
14. [function.erl](function.erl) - it is a simple program to show how to use function.
15. [recursion.erl](recursion.erl) - it is a simple program to show how to use recursion.
16. [myfile.erl](myfile.erl) - it is a simple program to show how to use file I/O. see [api](https://www.erlang.org/doc/search)
17. [myRecord.erl](myRecord.erl) - it is a simple program to show how to use record.
18. [exception.erl](exception.erl) - it is a simple program to show how to use exception.
19. [macro.erl](macro.erl) - it is a simple program to show how to use macro.
20. [funs.erl](funs.erl) - it is a simple program to show how to use `fun`

## Shell commands

1. `b()` - Prints the current variable bindings.
2. `f()` - Removes all variable bindings.
3. `f(x)` - Removes the binding for a particular variable.
4. `h()` - Prints the history of all the commands executed in the shell.
5. `history(N)` - Prints the last N commands executed in the shell. A default
value of 20 is used for N.
6. `e(N)` - Executes the command numbered N in the history list. If the N is a negative number, the Nth previous command is repeated.

## Data Types

1. Number - In Erlang, there are 2 types of numeric literals: integers and floats.
2. Atom - An atom is a literal, a constant with name. An atom is to be enclosed in single quotes (') if it
does not begin with a lower-case letter or if it contains other characters than alphanumeric characters,
underscore or @.
3. Boolean - Boolean data types in Erlang are the two reserved atoms true and false.
4. Bit String - A bit string is used to store an area of un-typed memory.
5. Tuple - A tuple is a compound data type with a fixed number of terms. Each term in the tuple is
called as an element. The number of elements is said to be the size of the tuple.
6. Map - A map is compound data type with a variable number of key-value associations. Each key-value
association in the map is called an association pair. The key and value parts of the pair are called elements.
7. List - A list is a compound data type with a variable number of terms. Each term in the list is called an element.

## Variables

In Erlang, all the variables are bound with the `=` operator. All variables in Erlang need to start with
an uppercase character. In Erlang, the `=` is not an assignment operator, but it is used  to define variables.

In Erlang, variables are immutable, which means that once a variable is bound to a value, it can never
be changed, unless you use `f()` to remove the binding.

## Operators

1. Arithmetic Operators
    
    | Operator | Description                |
    |----------|----------------------------|
    | +        | Addition                   |
    | -        | Subtraction                |
    | *        | Multiplication             |
    | /        | Division                   |
    | div      | Integer division(int(a/b)) |
    | rem      | Integer remainder(%)       |

2. Relational Operators
    
    | Operator | Description              |
    |----------|--------------------------|
    | ==       | Equal to                 |
    | /=       | Not equal to             |
    | =<       | Less than or equal to    |
    | <        | Less than                |
    | >=       | Greater than or equal to |
    | >        | Greater than             |

3. Logical Operators

    | Operator | Description |
    |----------|-------------|
    | and      | Logical and |
    | or       | Logical or  |
    | not      | Logical not |
    | xor      | Logical xor |

4. [Bitwise Operators](bit_op.erl)

    | Operator | Description         |
    |----------|---------------------|
    | band     | Bitwise and         |
    | bor      | Bitwise or          |
    | bnot     | Bitwise not         |
    | bxor     | Bitwise xor         |
    | bsl      | Bitwise shift left  |
    | bsr      | Bitwise shift right |

5. Operator Precedence
    
    | Operator | Description          |
    |----------|----------------------|
    | 1        | :                    |
    | 2        | #                    |
    | 3        | not bnot             |
    | 4        | * / div rem band and |
    | 5        | + - bor bxor  or xor |
    | 6        | == /= =< < => >      |

## Functions

Syntax:

```
FunctionName(Param1, Param2, ..., ParamN) -> FunctionBody.
```

1. FunctionName - It is the name of the function.
2. Param1, Param2, ..., ParamN - These are the parameters of the function.
3. N - It is the arity of the function.
4. FunctionBody - It is the body of the function.

### Anonymous FUnctions

An anonymous function is a function without a name.

### Functions with Guard Sequences

Syntax:

```
FunctionName(Param1, Param2, ..., ParamN) when GuardSequence1, GuardSequence2, ..., GuardSequenceN -> FunctionBody.
```

see[function.erl](function.erl)

### Functions API

see [Erlang doc](https://www.erlang.org/doc/search)

## Records

A record is created using the Record Identifier. A record is 
a user-defined data type that is used to store a fixed number of
elements.

Syntax:

```
-record(RecordName, {Field1, Field2, ..., FieldN}).
```

1. RecordName - It is the name of the record.
2. Field1, Field2, ..., FieldN - These are the list of various fields which constitute the record.

## Exceptions

Exception handling is required in any programming language to handle the 
runtime errors, so that normal flow of the application can be maintained.
Exception normally disrupts the normal flow of the application, which
is the reason why we need to use Exception handling in our application.

In Erlang, there are 3 types of exceptions:

1. Error - Calling erlang:error(Reason) will end the exception in the current process and include
a task trace of the last functions called with their arguments when you catch it. These are the kind of exceptions
that provoke the runtime error message.
2. Exists - There are two kinds of exits: internal exits and external exits. The internal exits
are triggered by calling `exit/1` and make the current process stop its execution. The external exits
are called with `exit/2` and have to do with multiple processes in the concurrent aspect of Erlang.

    > exit/2, however, is used to send an exit signal from one process to another, it's an external exit. This is where the "concurrent aspect" comes into play. In Erlang, you can have many processes running at the same time (concurrently). These processes can communicate and interact with each other in various ways, including sending exit signals.
    > Below is an example of an external exit:
    > ```erlang
    > Pid = spawn(fun() -> receive after 10000 -> ok end end),
    > exit(Pid, kill).
    > ```

3. Throw - A throw is a class of exception used for cases that the programmer can be expected to handle.
In comparison with exits and errors, they don't really carry any `crash that process` semantics, but rather
they control the flow. As you use throws while expecting the programmer to handle them, it's usually
a good idea to document their use within a module using them.

A `try...catch` is a way to evaluate an expression while letting you handle the successful case as well as
the errors encountered.

Syntax:

```
try Exression of
SuccessPattern1 [when GuardSeq1] -> Expression1;
SuccessPattern2 [when GuardSeq2] -> Expression2;
...
SuccessPatternN [when GuardSeqN] -> ExpressionN
catch
TypeOfError:ErrorPattern1 [when GuardSeq1] -> Expression1;
TypeOfError:ErrorPattern2 [when GuardSeq2] -> Expression2;
...
TypeOfError:ErrorPatternN [when GuardSeqN] -> ExpressionN
end
```
The Expression in between try and of is said to be protected. This means that any kind of exception
happening within that call will be caught by the catch part of the `try...catch` expression.

We can replace the `TypeOfError` by either `error`, `exit` or `throw`, for each respective type we've
seen above. If no type is specified, a throw is assumed.

## Macros

Macros are generally used for inline code replacements. In Erlang, macros
are defined via the following syntax:

```
-define(ConstantName, ConstantValue).
-efine(FunctionName(Param1, Param2, ..., ParamN), FunctionBody).
```

The following additional statements are available for macros -
1. `undef(Macro)` - Undefines the macro; after this you cannot call the macro.
2. `ifdef(Macro)` - Evaluates the following lines only if the Macro has been defined.
3. `ifndef(Macro)` - Evaluates the following lines only if Macro is undefined.
4. `else` - Allowed after an ifdef or ifndef statement. If the condition was false, the statements following else are evaluated.
5. `endif` - Marks the end of ifdef or ifndef statemetn.

When using the above statements, it should be used in the proper way as shown
in the following program:

```e
-ifdef(<FLAGnAME>).

-define(...).

-else.

-define(...).
-endif.
```

## Header Files

Header files are like include files in any other programming language. It is useful for splitting modules into different
files and then accessing these header files into separate programs. 

1. Let us create a header file called `user.hrl` with the following content:

```erlang
-record(user, {name, age, address}).
```

## Preprocessors

Before an Erlang module is compiled, it is automatically processed by the Erlang Preprocessor.
The preprocessor expands any macros that might be in the source file and inserts any necessary
include files.

Ordinarily, you won't need to look at the output of the preprocessor, but in exceptional circumstances,
you might want to save the output of the preprocessor. To see the result of preprocessing the module
`some_module.erl` give the OS shell command.

```bash
erlc -P some_module.erl
```

Then a file called `some_module.P` would be generated.

## Pattern matching

Patterns look the same as terms - they can be simple literals like atoms and numbers, compound like 
tuples and lists, or a mixture of both. They can also contain variables, which are alphanumeric strings
that begin with a capital letter or underscore. A special `anonymous variable`, `_`, is used when you don't
care about the value to be matched, and won't be using it.

A pattern matches if it has the same `shape` as the term being matched, and atoms encountered are the same.
For example, the following matches succeed:
```erlang
B = 1.
2 = 2.
{ok, C} = {ok, 40}.
[H|T] = {1, 2, 3, 4}.
```

Note that in the fourth example, the pipe(|) signifying the head and tail of the list as described in Terms.
Also note that the left hand side should match the right hand side which is the normal case patterns.

The following examples of pattern matching will fail.

```erlang
1 = 2.
{ok, A} = {failure, "Don't know the question"}.
{H|T} = []
```

In the case of the pattern-matching operator, a failure generates an error and the process exits. How this
can be trapped and handled is covered in Errors. Patterns are used to select which clause of a function
will be executed.

## BIFs

BIFs are functions that are built into Erlang. They usually do tasks that impossible to program in Erlang.
For example, it's impossible to turn a list into a tuple or to find the current time and date. To perform
such an operation, we call a BIF.

## Binaries

Use a data structure called a binary to store large quantities of raw data. Binaries store data in much more spece
efficient manner than in lists or tuples, and the runtime system is optimized for the efficient input and output
binaries.

binaries are written and printed as sequences of integers or strings, enclosed in double less than
and greater than brackets.

Following is an example of binaries in Erlang -

```erlang
-module(helloworld).
-export([start/0]).

start() ->
  io:fwrite("~p~n",[<<5,10,20>>]),
  io:fwrite("~p~n",[<<"hello">>]).
```

## Funs

Funs are used to define anonymous function in Erlang. The general syntax of a anonymous function is given
below -

```erlang
F = fun(Arg1, Arg2, ..., ArgN) ->
...
end.
```
```erlang
start() ->
  A = fun() -> io:fwrite("Hello World!") end,
  A().
```

## Processes

The granularity of concurrency in Erlang is a process. A process is an activity/task that runs concurrently
with and is independent of the other processes. These processes in Erlang are different than the processes and threads
most people are familiar with. Erlang processes are lightweight, operate in isolation from other processes, and
are scheduled by Erlang's Virtual Machine. The creation time of process is very low, the memory footprint of a just spawned
process is very small, and a single Erlang VM can have millions of processes running.

A process is created with the help of the spawn method. The general syntax of the method is given below:

```bash
spawn(Module, Name, Args).
```

1. Module - This is a predefined atom value which must be ?MODULE.
2. Name - This is the name of the function to be called when the process is defined.
3. Args - These are the arguments which need to be sent to the function.

### Return Value

Returns the process id of the new process created.

## Email

To send an email using Erlang, you need to use a package available from github for the same. 
see [link](https://github.com/Vagabond/gen_smtp)

## Databases

Erlang has the ability to connect to the traditional databases such as SQL Server and Oracle. Erlang
has an inbuilt odbc library that can be used to work with databases.

## Ports

In Erlang, ports are used for communication between different programs. A socket is a communication endpoint that allows
machines to communicate over the Internet by using the Internet Protocol(IP).

There are 2 types of protocols available for communication. One is UDP and the other is TCP. UDP lets
applications send short messages to each other.

## Distributed Programming

Distributed programs are those programs that are designed to run on networks of computers and that can
coordinate their activities only by message passing.

There are a number of reasons why we might want to write distributed applications. Here are some of them.

1. Performance - We can make our programs go faster by arranging that different parts of the program are run parallel
on different machines.
2. Reliability - We can make fault-tolerant systems by structuring the system to run on several machines. If one machine
fails, we can continue on another machine.
3. Scalability - As we scale up an application, sooner or later we will exhaust the capabilities of even 
the most powerful machine. At this stage we have to add more machines to add capacity. Adding a new machine
should be a simple operation that does not require large changes to the application architecture.


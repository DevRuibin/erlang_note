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
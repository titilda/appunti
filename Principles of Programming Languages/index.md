---
title: "Principles of Programming Languages"
author:
  - "Andrea Lunghi"
---

## Scheme

**Scheme** is a minimalist dialect of the Lisp programming language, designed with a focus on simplicity and flexibility.

Scheme is a **functional programming language**, every computation is an **expression** that evaluates to a value, there are no statements or commands.

> Statements are instructions that perform actions but do not return values, while expressions are constructs that evaluate to produce values.

The syntax of Scheme is characterized by its use of parentheses to denote function application and its uniform treatment of code and data. This type of syntax is known as **S-expressions** (Symbolic Expressions) and resemble _polish notation_.

This uniform syntax allows for powerful metaprogramming capabilities, as code can be manipulated as data structures.

> Metaprogramming is the practice of writing programs that can generate, manipulate, or analyze other programs or themselves as data.

```c
x == 5 + 3 * 2 + z;
```

is written in Scheme as:

```scheme
(= x (+ 5 (* 3 2) z))
```

Each expression in Scheme is enclosed in parentheses (`(e1 e2 ... en)`), where `e1` is typically a symbol representing a function or operator, and `e2` to `en` are its arguments.

The evaluation order of each expression is _unspecified_, meaning that the language does not guarantee a specific order in which the arguments are evaluated, but it ensures that all arguments are evaluated before the function is applied.

### Variables and Bindings

Variables in Scheme are created using the `let` keyword, which allows for the binding of values to names.

```scheme
(let ((x 10)
      (y 20))
  (+ x y)) ; This will evaluate to 30
```

The `let` construct creates a new scope where `x` is bound to `10` and `y` is bound to `20`.

The scope of variables in Scheme is **Static** (or lexical), meaning that the visibility of a variable is determined by the structure of the code and the location where it is defined.

> Scoping can be either static (lexical) or dynamic.
>
> In static scoping, the scope of a variable is determined by the program's structure, while in dynamic scoping, the scope is determined by the program's execution context.
>
> Static scoping is more common in modern programming languages as allows for better predictability as the bindings of variables can be determined at compile time.

#### Types

Scheme is a **dynamically typed language**, meaning that types are associated with values rather than variables. This allows for greater flexibility in programming, as functions can accept arguments of any type without explicit type declarations.

Scheme supports a variety of data types, including:

- **Numbers**:
  - Integers (e.g., `42`, `-7`)
  - Floating-point numbers (e.g., `3.14`, `-0.001`)
  - Rational numbers (e.g., `1/2`, `-3/4`)
  - Complex numbers (e.g., `2+3i`, `-1-4i`)
- **Booleans**: `#t` (true) and `#f` (false)
- **Characters**: e.g., `#\a`, `#\space`, `#\newline`
- **Strings**: e.g., `"Hello, World!"`, `"Scheme is fun!"`
- Vectors: fixed-size collections of elements, e.g., `#(1 2 3)`, `#("a" "b" "c")`
- Lists: ordered collections of elements it can contain heterogeneous types, e.g., `(1 2 3)`, `("a" "b" 2)`
- Pairs: create a concatenation of two elements, similar to a linked list, e.g., `(1 . 2)`
- **Symbols**: symbols used as identifiers or for symbolic computation, e.g., `'foo`, `'bar`, `'my-symbol`
- **Procedures**: first-class functions that can be passed as arguments, returned from other functions, and stored in data structures.

### Procedures

In Scheme, procedures (or functions) are first-class citizens, meaning they can be treated like any other data type.

#### Lambda Expressions

Lambda expressions are used to define _anonymous functions_ in Scheme. Anonymous functions are functions that are defined without a name and can be used as arguments to other functions or assigned to variables. They are a fundamental concept in functional programming.

The syntax for a lambda expression is defined by the `lambda` keyword and is as follows:

```scheme
(lambda (arg1 arg2 ... argn) body)
```

Where `arg1` to `argn` are the parameters of the function, and `body` is the expression that defines the function's behavior.

For example, the following lambda expression defines a function that takes two arguments and returns their sum:

```scheme
(lambda (x y) (+ x y))
```

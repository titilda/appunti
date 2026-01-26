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

Each expression in Scheme is enclosed in parentheses (`(e1 e2 ... en)`), where `e1` is typically a symbol representing a function or operator, and `e2` to `en` are its arguments.

The evaluation order of each expression is _unspecified_, meaning that the language does not guarantee a specific order in which the arguments are evaluated, but it ensures that all arguments are evaluated before the function is applied.

```c
x == 5 + 3 * 2 + z;
```

is written in Scheme as:

```scheme
(= x (+ 5 (* 3 2) z))
```

Scheme is **homoiconic**, meaning that the code and data share the same representation.

> Homoiconicity is natural for machine as code is just memory data, it all depends on how that data is interpreted.
>
> In general programming languages are not homoiconic to separate code from data, making it easier to reason about programs.

This uniform syntax allows for powerful metaprogramming capabilities, as code can be manipulated as data structures.

> Metaprogramming is the practice of writing programs that can generate, manipulate, or analyze other programs or themselves as data.

### Variables and Bindings

The scope of variables in Scheme is **Static** (or lexical), meaning that the visibility of a variable is determined by the structure of the code and the location where it is defined.

> Scoping can be either static (lexical) or dynamic.
>
> In static scoping, the scope of a variable is determined by the program's structure, while in dynamic scoping, the scope is determined by the program's execution context.
>
> Static scoping is more common in modern programming languages as allows for better predictability as the bindings of variables can be determined at compile time.

#### Local Variables

Local variables in Scheme are created using the `let` keyword, which allows for the binding of values to names.

```scheme
(let ((x 10)
      (y 20))
  (+ x y)) ; This will evaluate to 30
```

The `let` construct creates a new scope where `x` is bound to `10` and `y` is bound to `20`.

The binding of multiple variable happens in parallel, meaning that the values of the variables cannot depend on each other, inside of the same `let` expression.

```scheme
(let ((x 10)
      (y (+ x 5))) ; This will result in an error because x is not yet defined
  (+ x y))
```

##### Sequential Bindings

To create sequential bindings, where the value of a variable can depend on previously defined variables, the `let*` construct is used.

```scheme
(let* ((x 10)
       (y (+ x 5))) ; This is valid because x is already defined
  (+ x y)) ; This will evaluate to 25
```

##### Recursive Bindings

For defining recursive functions or variables that refer to themselves, the `letrec` construct is used.

```scheme
(letrec ((factorial
           (lambda (n)
             (if (= n 0)
                 1
                 (* n (factorial (- n 1)))))))
  (factorial 5)) ; This will evaluate to 120
```

#### Global Variables

Global variables in Scheme can be defined using the `define` keyword. These variables are accessible from any part of the program after their definition.

```scheme
(define pi 3.14159)
(define radius 5)
(* pi (* radius radius)) ; This will evaluate to 78.53975
```

#### Variable Assignment

In Scheme, variables are immutable by default, meaning that once a variable is bound to a value, it cannot be changed.

However, mutable variables can be created using the `set!` keyword.

```scheme
(define counter 0)
(set! counter (+ counter 1)) ; This will update the value of counter to 1
```

The `!` operator is called a **bang** and is used to indicate that a function or operation has side effects, such as modifying a variable or changing the state of the program.

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
- **Vectors**: fixed-size collections of elements, e.g., `#(1 2 3)`, `#("a" "b" "c")`
- **Pairs**: create a concatenation of two elements, the `car` (Content of the Address Register - first element) and the `cdr` (Contents of the Data Register - second element), e.g., `(x . y)` or `(cons x y)`
- **Symbols**: symbols used as identifiers or for symbolic computation, e.g., `'foo`, `'bar`, `'my-symbol`
- **Procedures**: first-class functions that can be passed as arguments, returned from other functions, and stored in data structures.

### Lists

**Lists** are a fundamental data structure in Scheme, used to represent ordered collections of elements. Lists can contain elements of heterogeneous types, including numbers, strings, symbols, and even other lists.

Lists in Scheme are implemented using **pairs**.

```scheme
(cons 1 (cons 2 (cons 3 ())))   ; This creates the list (1 2 3)
(1 . (2 . (3 . ())))            ; This creates the list (1 2 3)
(list 1 2 3)                    ; This creates the list (1 2 3)
(1 2 3)                         ; This creates the list (1 2 3)
```

The `()` notation represents the empty list, also known as `nil`.

#### List Operations

Scheme provides several built-in functions for manipulating lists:

- `car`: Returns the first element of a list.
  
  ```scheme
  (car '(1 2 3)) ; This will return 1
  ```

- `cdr`: Returns the rest of the list after removing the first element.
  
  ```scheme
    (cdr '(1 2 3)) ; This will return (2 3)
  ```

- `cons`: Constructs a new pair (or list) by adding an element to the front of an existing list.
  
  ```scheme
  (cons 0 '(1 2 3)) ; This will return (0 1 2 3)
  ```

- `member`: Checks if an element is present in a list and returns the sublist starting from that element if found, or `#f` if not found.
  
  ```scheme
  (member 2 '(1 2 3)) ; This will return (2 3)
  (member 4 '(1 2 3)) ; This will return #f
  ```

- `apply`: Applies a procedure to a list of arguments.
  
  ```scheme
  (apply + '(1 2 3 4)) ; This will return 10
  ```

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

#### Defining Named Procedures

Named procedures can be defined using the `define` keyword. This allows us to create reusable functions that can be called by their name.

```scheme
(define (add x y)
  (+ x y))
```

To have an undefined number of arguments, we can use the dot (`.`) notation:

```scheme
(define (sum . numbers)
  (if (null? numbers)
      0
      (+ (car numbers) (apply sum (cdr numbers)))))
```

### Syntactic Form

Scheme has some special syntactic forms that are not evaluated in the same way as regular expressions.

#### Conditional Branching

Conditional branching is used to evaluate different expressions based on certain conditions.

##### If Expression

The `if` expression is used for conditional branching in Scheme. In this expression, a condition is evaluated, and based on its truth value, one of two branches is executed, not both.

It has the following syntax:

```scheme
(if condition then-branch else-branch)
```

Where `condition` is an expression that evaluates to a boolean value, `then-branch` is the expression executed if the condition is true, and `else-branch` is the expression executed if the condition is false.

##### When Expression

The `when` expression is a simplified form of conditional branching that only includes a `then-branch`. It is used when there is no need for an `else-branch`.

```scheme
(when condition then-branch)
```

#### Quote

The `quote` syntactic form is used to prevent the evaluation of an expression. When an expression is quoted, it is treated as a literal value rather than being evaluated.

The syntax for quoting an expression is as follows:

```scheme
(quote expression)
```

Alternatively, a shorthand notation using a single apostrophe (`'`) can be used:

```scheme
'expression
```

For example, the expression `'(1 2 3)` represents the list containing the elements `1`, `2`, and `3` without evaluating it.

##### Quasiquote

Quasiquote is a syntactic form that allows for partial evaluation of an expression. It is denoted by the backquote character (`` ` ``). Within a quasiquoted expression, parts of the expression can be evaluated using the comma (`,`) operator.

For example:

```scheme
`(1 2 ,(+ 3 4)) ; This will evaluate to the list (1 2 7)
```

##### Eval

To evaluate the quoted expression, the `eval` function can be used:

```scheme
(eval '(1 2 3)) ; This will evaluate to the list (1 2 3)
```

#### Procedural Code Execution

To write procedural code in Scheme, we can use the `begin` syntactic form, which allows for the sequential execution of multiple expressions.

The syntax for the `begin` form is as follows:

```scheme
(begin expression1 expression2 ... expressionN)
```

Where `expression1` to `expressionN` are the expressions to be executed in sequence. The value of the `begin` expression is the value of the last expression executed.

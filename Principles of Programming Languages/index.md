---
title: "Principles of Programming Languages"
author:
  - "Andrea Lunghi"
---

## Scheme

**Scheme** is a minimalist dialect of the Lisp programming language, designed with a focus on simplicity and flexibility.

Scheme is (mostly) a **functional programming language**: every computation is an **expression** that evaluates to a value, and there are no statements or commands.

> **Statements** are instructions that perform actions but do not return values, while **expressions** are constructs that evaluate to produce values.

The syntax of Scheme is characterized by its use of parentheses to denote function application and its uniform treatment of code and data. This type of syntax is known as **S-expressions** (Symbolic Expressions) and uses **prefix** (or _Polish_) notation.

Each expression in Scheme is enclosed in parentheses (`(operator arg1 ... argn)`), where `operator` is typically a symbol representing a function or special form, and `arg1` to `argn` are its arguments.

The evaluation order of arguments in an expression is **unspecified**, meaning that the language does not guarantee a specific order in which the arguments are evaluated, but it ensures that all arguments are evaluated before the procedure is applied.

For example, the mathematical expression $5 + 3 \cdot 2 + z$ is written in Scheme as:

```scheme
(+ 5 (* 3 2) z)
```

It is also possible to use the square brackets `[` and `]` as parentheses for better readability, especially in nested expressions:

```scheme
(+ 5 [* 3 2] z)
```

Scheme is **homoiconic**, meaning that the code and data share the same representation.

> **Homoiconicity**: The property where the program structure is similar to its data structure. In Lisp dialects, both code and data are represented as S-expressions (nested lists).
>
> In general, most programming languages are not homoiconic to separate code from data, making it easier to reason about programs.

This uniform syntax allows for powerful **metaprogramming** capabilities, as code can be manipulated as data structures.

> **Metaprogramming** is the practice of writing programs that can generate, manipulate, or analyze other programs or themselves as data.

### Variables and Bindings

The scope of variables in Scheme is **static** (or lexical), meaning that the visibility of a variable is determined by the structure of the code and the location where it is defined.

> Scoping can be either **static** (lexical) or **dynamic**.
>
> In **static scoping**, the scope of a variable is determined by the program's structure (at compile time), while in **dynamic scoping**, the scope is determined by the program's execution context (call stack at runtime).

Variable are stored in the **heap** memory area, which is used for dynamic memory allocation. The heap allows for the creation of variables whose lifetimes are not tied to the function call stack, enabling features like closures.

There is also a **Garbage Collector** (GC) in Scheme that automatically reclaims memory occupied by objects that are no longer reachable or needed by the program.

When a variable is defined as a **constant** the value is immutable and the memory area is marked as read-only to prevent accidental modification. This prevents banging operations.

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

Scheme is a **dynamically typed language**, meaning that types are associated with values rather than variables. However, it is also **strongly typed**, as it enforces type safety during runtime operations (e.g., trying to add a number to a string will result in an error).

Scheme supports a variety of data types, including:

- **Numbers**:
  - Integers (e.g., `42`, `-7`)
  - Floating-point numbers (e.g., `3.14`, `-0.001`)
  - Rational numbers (e.g., `1/2`, `-3/4`)
  - Complex numbers (e.g., `2+3i`)
- **Booleans**: `#t` (true) and `#f` (false)
- **Characters**: e.g., `#\a`, `#\space`, `#\newline`
- **Strings**: e.g., `"Hello, World!"`
- **Vectors**: Fixed-size collections of elements accessed by index, e.g., `(vector 1 2 3)` (mutable), `#("a" "b" "c")` (immutable)
- **Pairs**: The building block of lists, created with `cons`. A pair consists of a `car` (first element) and a `cdr` (second element), e.g., `(x . y)` or `(cons x y)`
- **Symbols**: Unique identifiers used for symbolic computation or as keys, e.g., `'foo`, `'bar`, `'my-symbol`. Symbols are immutable and efficient to compare.
- **Procedures**: First-class functions that can be passed as arguments, returned from other functions, and stored in data structures.

##### Lists

**Lists** are a fundamental data structure in Scheme, used to represent ordered collections of elements. Lists can contain elements of heterogeneous types, including numbers, strings, symbols, and even other lists.

Lists in Scheme are implemented as linked chains of **pairs**. Each pair's `car` contains an element, and its `cdr` points to the next pair (or the empty list).

```scheme
(cons 1 (cons 2 (cons 3 '())))  ; This creates the list (1 2 3)
'(1 . (2 . (3 . ())))           ; Pair notation for the same list
(list 1 2 3)                    ; Procedure calling for the same list
'(1 2 3)                        ; Quoted literal for the same list
```

The `()` notation represents the empty list, also known as `nil`.

By default list are _immutable_.

##### Structs

Scheme allows the definition of custom data structures using the `struct` construct. They are also called **records**. This enables the creation of new types with named fields. By default, fields are immutable, but is possible to create mutable fields by specifying the `#:mutable` keyword.

```scheme
(struct person (
  (name) 
  (age #:mutable)
))
```

A struct is instantiated by calling it as a procedure:

```scheme
(define alice (person "Alice" 30))
```

To access the fields of a struct, accessor procedures are automatically generated:

```scheme
(person-name alice) ; Returns "Alice"
(person-age alice)  ; Returns 30
```

To modify a mutable field, a setter procedure is also generated:

```scheme
(person-age-set! alice 31) ; Updates Alice's age to 31
```

Structs can also support **inheritance** by specifying a parent struct:

```scheme
(struct employee person (
  (employee-id)
))
```

Records are not classes and do not support methods or encapsulation like in object-oriented programming.

### Procedures

In Scheme, procedures (or functions) are first-class citizens, meaning they can be treated like any other data type.

The parameters are passed by **value** (technically _object sharing_ as objects are not copied in the stack but only the reference to the object is passed), meaning that the actual values of the arguments are passed to the procedure, rather than references to the variables.

#### Lambda Expressions

Lambda expressions are used to define _anonymous functions_ in Scheme. Anonymous functions are functions that are defined without a name and can be used as arguments to other functions or assigned to variables. They are a fundamental concept in functional programming.

The syntax for a lambda expression is defined by the `lambda` or `λ` keyword and is as follows:

```scheme
(lambda (arg1 arg2 ... argn) body)
```

Where `arg1` to `argn` are the parameters of the function, and `body` is the expression that defines the function's behavior.

For example, the following lambda expression defines a function that takes two arguments and returns their sum:

```scheme
(lambda (x y) (+ x y))
```

#### Defining Named Procedures

Named procedures can be defined using the `define` keyword.

```scheme
(define add
  (lambda (x y)
    (+ x y)))
```

Or using the syntactic sugar for procedure definition:

```scheme
(define (add x y)
  (+ x y))
```

To define a procedure with a variable number of arguments (variadic), we can use the dot (`.`) notation:

```scheme
(define (sum . numbers)
  (if (null? numbers)
      0
      (+ (car numbers) (apply sum (cdr numbers)))))
```

#### String Operations

Scheme provides several built-in procedures for manipulating strings:

- `string-append`: Concatenates multiple strings into a single string.
  
  ```scheme
  (string-append "Hello, " "world!") ; Returns "Hello, world!"
  ```

- `make-string`: Creates a new string of a specified length, optionally filled with a specified character.
  
  ```scheme
  (make-string 5 #\*) ; Returns "*****"
  ```

#### Vector Operations

Scheme provides several built-in procedures for manipulating vectors:

- `vector-ref`: Returns the element at a specified index in a vector.
  
  ```scheme
  (vector-ref #(10 20 30) 1) ; Returns 20
  ```

- `vector-set!`: Sets the element at a specified index in a vector to a new value.
  
  ```scheme
    (vector-set! my-vector 1 42) ; Sets the second element to 42
  ```
  
- `vector-for-each`: Applies a procedure to each element of a vector.
  
  ```scheme
  (vector-for-each (lambda (x) (displayln x)) #(1 2 3)) ; Prints 1, 2, and 3 on separate lines
  ```

#### List Operations

Scheme provides several built-in procedures for manipulating lists:

- `car`: Returns the first element of a list.
  
  ```scheme
  (car '(1 2 3)) ; Returns 1
  ```

- `cdr`: Returns the "rest" of the list (everything after the first element).
  
  ```scheme
  (cdr '(1 2 3)) ; Returns (2 3)
  ```

- `cons`: Constructs a new pair by prepending an element to an existing list.
  
  ```scheme
  (cons 0 '(1 2 3)) ; Returns (0 1 2 3)
  ```

- `member`: Checks if an element is present in a list and returns the sublist starting from that element if found, or `#f` if not found.
  
  ```scheme
  (member 2 '(1 2 3)) ; Returns (2 3)
  (member 4 '(1 2 3)) ; Returns #f
  ```

- `apply`: Applies a procedure to a list of arguments.
  
  ```scheme
  (apply + '(1 2 3 4)) ; Returns 10
  ```

Then there are higher-order procedures that operate on lists like:

- `for-each`: Applies a procedure to each element of a list, return void.
  
  ```scheme
  (for-each (lambda (x) (displayln x)) '(1 2 3)) ; Prints 1, 2, and 3 on separate lines
  ```

- `map`: Applies a procedure to each element of a list and returns a new list of the results.
  
  ```scheme
  (map (lambda (x) (* x 2)) '(1 2 3)) ; Returns (2 4 6)
  ```

- `filter`: Returns a new list containing only the elements that satisfy a given predicate.
  
  ```scheme
  (filter (lambda (x) (> x 2)) '(1 2 3 4)) ; Returns (3 4)
  ```

- `foldl` and `foldr`: Reduce a list to a single value by applying a binary procedure from the left ($fold_{left}(f, i, (e_1, e_2, \ldots, e_n)) = f(e_n, f(e_{n-1}, \ldots f(e_1, i)))$) or right ($fold_{right}(f, i, (e_1, e_2, \ldots, e_n)) = f(e_1, f(e_2, \ldots f(e_n, i)))$), respectively. `foldl` is tail-recursive and more efficient.
  
  ```scheme
  (foldl cons '() '(1 2 3)) ; Returns (3 2 1)
  (foldr cons '() '(1 2 3)) ; Returns (1 2 3)
  ```

  `foldr` can be implemented using tail recursion, removing the need for the stack but using the heap instead, making it less efficient:

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

##### Cond Expression

The `cond` expression is used for multi-way branching, similar to `switch` or `if-else if-else` in other languages.

```scheme
(cond
  (condition1 result1)
  (condition2 result2)
  (else result_else))
```

##### Case Expression

The `case` expression is used for branching based on the value of a single expression. It compares the value against multiple cases and executes the corresponding result for the first matching case.

```scheme
(case expression
  ((value1 value2 ...) result1)
  ((value3 value4 ...) result2)
  (else result_else))
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

To evaluate a quoted expression at runtime, the `eval` function can be used:

```scheme
(eval '(+ 1 2)) ; Returns 3
```

#### Procedural Code Execution

To write procedural code in Scheme, we can use the `begin` syntactic form, which allows for the sequential execution of multiple expressions.

The syntax for the `begin` form is as follows:

```scheme
(begin expression1 expression2 ... expressionN)
```

Where `expression1` to `expressionN` are the expressions to be executed in sequence. The value of the `begin` expression is the value of the last expression executed.

### Predicates

Predicates are procedures that return a boolean value (`#t` for true and `#f` for false).

Some common predicates in Scheme include checking the type of a value:

- `null?`: Checks if a list is empty.
  
  ```scheme
  (null? '()) ; Returns #t
  (null? '(1 2 3)) ; Returns #f
  ```

- `number?`: Checks if a value is a number.
  
  ```scheme
    (number? 42) ; Returns #t
    (number? "hello") ; Returns #f
  ```

- `string?`: Checks if a value is a string.
  
  ```scheme
    (string? "hello") ; Returns #t
    (string? 42) ; Returns #f
  ```

- `pair?`: Checks if a value is a pair.
  
  ```scheme
    (pair? '(1 . 2)) ; Returns #t
    (pair? '(1 2 3)) ; Returns #f
  ```

- `list?`: Checks if a value is a list.
  
  ```scheme
    (list? '(1 2 3)) ; Returns #t
    (list? 42) ; Returns #f
  ```

- `vector?`: Checks if a value is a vector.
  
  ```scheme
    (vector? #(1 2 3)) ; Returns #t
    (vector? '(1 2 3)) ; Returns #f
  ```

- `symbol?`: Checks if a value is a symbol.
  
  ```scheme
    (symbol? 'foo) ; Returns #t
    (symbol? "foo") ; Returns #f
  ```

- `struct-name?`: Checks if a value is an instance of a specific struct type.
  
  ```scheme
    (person? alice) ; Returns #t if alice is a person struct
    (person? 42) ; Returns #f
  ```

To compare two values for equality, Scheme provides several predicates:

- `=`: Checks if two numeric values are equal.
  
  ```scheme
    (= 42 42) ; Returns #t
    (= 42 43) ; Returns #f

- `eq?`: Checks if two values are the same object (reference equality).
  
  ```scheme
  (eq? 'a 'a) ; Returns #t
  (eq? '(1 2) '(1 2)) ; Returns #f
  ```

- `eqv?`: Checks if two values are equivalent, considering numeric and character types (value equality for certain types).
  
  ```scheme
    (eqv? 42 42) ; Returns #t
    (eqv? #\a #\a) ; Returns #t
  ```

- `equal?`: Checks if two values are structurally equal (recursive equality).
  
  ```scheme
    (equal? '(1 2) '(1 2)) ; Returns #t
    (equal? "hello" "hello") ; Returns #t
  ```

The logical operators `and`, `or`, and `not` can be used to combine predicates:

```scheme
(and (number? x) (> x 0)) ; Checks if x is a positive number
(or (null? lst) (pair? lst)) ; Checks if lst is empty or a pair
(not (string? y)) ; Checks if y is not a string
```

### Iteration

Scheme does not have traditional looping constructs like `for` or `while` found in imperative programming languages. Instead, iteration is typically achieved through recursion or named let expressions.

#### Named Let

Named let is a syntactic form that allows for defining recursive functions in a concise manner. The syntax for named let is as follows:

```scheme
(let label ((param1 init1)
            (param2 init2)
            ...
            (paramN initN))
  body)
```

Where `label` is the name of block, `param1` to `paramN` are the parameters with their initial values, and `body` is the expression that defines the function's behavior.

To perform iteration, the label needs to be called to perform a goto-like jump to the beginning of the block with updated parameters.

```scheme
(let label ((n 5))
  (when (>= n 0)
    (displayln n)
    (label (- n 1))))
```

#### Tail Recursion

Recursion is a fundamental concept in Scheme and functional programming in general. It involves defining a function that calls itself to solve a problem by breaking it down into smaller subproblems.

A special case of recursion is **tail recursion**, where the recursive call is the last operation in the function. Tail-recursive functions are optimized by the Scheme interpreter to execute in constant stack space, effectively turning them into iterations.

```scheme
(define (factorial n acc)
  (if (= n 0)
      acc
      (factorial (- n 1) (* n acc)))) ; Tail call

(factorial 5 1) ; Evaluates to 120
```

To evaluate memory usage of tail-recursive functions, we can use the `trace` procedure to monitor function calls:

```scheme
(require racket/trace) ; Import the trace module
(trace factorial)
(factorial 5 1)
```

#### Closure

A **closure** is a function that captures the lexical scope in which it was defined, allowing it to access variables from that scope even when invoked outside of it.

```scheme
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter (make-counter))

(counter) ; Returns 1
(counter) ; Returns 2
```

This allows to create iterators and generators that maintain state across invocations.

### Macros

Macros in Scheme are a powerful feature that allows programmers to define new syntactic constructs and transformations at compile time. They enable the creation of custom language features by manipulating the code structure before it is evaluated.

This system is turing complete and allows for advanced metaprogramming techniques that are evaluated at compile time.

Macro definitions use the `define-syntax` and `syntax-rules` constructs.

```scheme
(define-syntax while
  (syntax-rules ()
    ((_ condition body ...)
      (let loop ()
        (when condition
          (begin
            body ...
            (loop))
        ))
    )
  ))
```

where:

- `while` is the name of the macro.
- `syntax-rules` defines the pattern matching rules for the macro.
- The pattern `(_ condition body ...)` matches the usage of the macro, where `_` is a wildcard that matches the macro name, `condition` is the loop condition, and `body ...` represents the body of the loop.
- `...` indicates that there can be zero or more expressions in the body.

```scheme
(define-syntax let
  (syntax-rules ()
    ((_ ((var expr) ...) body ...)
      ((lambda (var ...) body ...) expr ...))
  ))
```

This macro transforms a `let` expression into an equivalent lambda application, `(var expr) ...` represent multiple variable bindings.

Macros in sceme are **hygienic** meaning that they prevents unintended variable capture and name collisions during macro expansion. Unlike traditional macros that operate on raw text substitution, hygienic macros maintain the lexical scope of identifiers, ensuring that macro parameters and locally-bound variables don't inadvertently conflict with variables in the scope where the macro is invoked. This is achieved through automatic renaming of identifiers during expansion, typically by attaching unique tags or timestamps to variable names.

### Error

In scheme errors are raised using the `error` procedure:

```scheme
(error "An error occurred")
```

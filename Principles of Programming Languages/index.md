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

Sometimes global variables are defined surrounded by `*` symbols to indicate that they are special or global variables, for example `(define *global-var* '())`.

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

##### Unless Expression

The `unless` expression is the opposite of the `when` expression. It executes the `then-branch` only if the `condition` evaluates to false.

```scheme
(unless condition then-branch)
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

#### Type Predicates

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

#### Equality Predicates

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

#### Logical Operators

The logical operators `and`, `or`, and `not` can be used to combine predicates:

```scheme
(and (number? x) (> x 0)) ; Checks if x is a positive number
(or (null? lst) (pair? lst)) ; Checks if lst is empty or a pair
(not (string? y)) ; Checks if y is not a string
```

#### Custom Predicates

Custom predicates can be defined using lambda expressions or named procedures that should have names ending with a question mark (`?`):

```scheme
(define (even? n)
  (= (modulo n 2) 0))
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

### Closure

A **closure** is a function that captures the lexical scope in which it was defined, allowing it to access variables from that scope even when invoked outside of it. It is made of two components:

- The environment where the function is created.
- The function code itself.

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

> The react's useState hook is an example of closures in action, where the state variable and its updater function are captured within the closure created by the hook.

### Macros

Macros in Scheme are a powerful feature that allows programmers to define new syntactic constructs and transformations at compile time. They enable the creation of custom language features by manipulating the code structure before its evaluation.
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

`syntax-rules` can also define **literals**, which are identifiers that must match exactly during macro expansion and are not treated as pattern variables.

```scheme
(define-syntax my-if
  (syntax-rules (else) ; 'else' is a literal keyword
    ((_ condition then-branch else else-branch)
      (if condition then-branch else-branch))))
```

In this example, `else` is a literal—it must appear verbatim in the macro call. If you write `(my-if (> x 0) 1 else 2)`, the `else` keyword is recognized. Using a different identifier would not match the pattern.

#### Hygienic Macros

Macros in sceme are **hygienic** meaning that they prevents unintended variable capture and name collisions during macro expansion. Unlike traditional macros that operate on raw text substitution, hygienic macros maintain the lexical scope of identifiers, ensuring that macro parameters and locally-bound variables don't inadvertently conflict with variables in the scope where the macro is invoked. This is achieved through automatic renaming of identifiers during expansion, typically by attaching unique tags or timestamps to variable names.

To use global variable, defined at the top level, the name must be surrounded by `*` symbols, for example `(define *global-var* '())`.

To break hygiene and allow variable capture, the user should pass the variable as a parameter to the macro:

```scheme
(define-syntax with-temp
  (syntax-rules ()
    ((_ var body ...)
      (let ((var (make-temp)))
        body ...))))
```

### Continuations

**Continuations** represent "the rest of the computation" at any given point in a program. They capture what the program will do next, allowing you to pause execution, save that state, and resume it later—possibly multiple times or from different contexts.

In Scheme, continuations are accessed using `call/cc` (call-with-current-continuation), a procedure that captures the current continuation and passes it to a function as an argument.

#### Saving Continuations for Later

Continuations become powerful when you save them and invoke them later:

```scheme
(define saved-k #f)

(+ 1 
  (call/cc
    (lambda (k)
      (set! saved-k k)
      10))) ; Returns 11

(saved-k 20) ; Returns 21
```

When calling `(saved-k 20)`, you're saying: "resume the computation from where I captured this continuation, but use `20` instead of `10`." This effectively re-executes `(+ 1 20)`, returning `21`.

#### Early Exit from Loops

A practical use case is breaking out of loops without checking all remaining elements:

```scheme
(define (search lst target)
  (call/cc
    (lambda (exit)
      (for-each
        (lambda (x)
          (if (= x target)
              (exit x)))
        lst)
      #f)))

(search '(1 2 3 4 5) 3) ; Returns 3
```

When the target is found, calling `exit` abandons the rest of the loop and returns the result immediately.

#### Goto-like jumps

Continuations can also be used to implement goto-like jumps in the code, allowing for non-linear control flow.

```scheme
(let ([cc (call/cc (lambda (k) (k k)))])
  (set! x (add1 x))
  (if (< x 3)
    (cc cc)   ; Jump back to the beginning
    x
  )
)
```

#### Implementation

Continuations can be implemented using two main techniques:

- **Garbage Collection (GC)**: This strategy doesn't require the stack, it allocate continuations on the heap and relies on garbage collection to reclaim memory when continuations are no longer needed.
- **Stack Copying**: When the continuation is issued, the stack is copied in the heap as a _continuation object_. When the continuation is invoked, the stack is restored from the continuation object, discarding the current stack.

### Non-Deterministic Programming

Scheme supports non-deterministic programming through the use of continuations, allowing for the exploration of multiple computation paths.

Using the `choose` construct, we can define non-deterministic choices in our programs. This construct allows us to specify a set of possible values, and the program can explore different paths based on these choices. This construct stores the current continuation before making a choice, enabling backtracking if needed.

If at any point the we understand that the current computation path is not leading to a valid solution, we can use the `fail` procedure to backtrack and explore alternative paths. This construct will restore the last saved continuation, removing the current computation state and allowing the program to try a different choice.

```scheme
(define (is-the-sum-of sum)
  (unless (and (>= sum 0)(<= sum 10))
    (error "out of range" sum))
  (let ((x (choose ’(0 1 2 3 4 5)))
        (y (choose ’(0 1 2 3 4 5))))
    (if (= (+ x y) sum)
      (list x y)
      (fail)
    )
  )
)
```

This function create two non-deterministic variables `x` and `y`. At the beginning they are both 0. When the `fail` procedure is called, the last continuation is restored, and `y` becomes 1. If `fail` is called again, `y` becomes 2, and so on. When all values for `y` are exhausted the `y` choice will fail and `x` will be incremented to 1, restarting the cycle for `y`.

### Proto-OO

Scheme does not have built-in support for Object Oriented Programming (OOP) like class-based languages (e.g., Java, C++). However, it is possible to implement OOP concepts using various patterns, such as **struct-based implementation**, **message passing**, and **prototype-based implementation**.

> Object Oriented Programming (OOP) is a programming paradigm based on the concept of "objects", which can contain data and code: data in the form of fields (often known as attributes or properties), and code in the form of procedures (often known as methods).

#### Struct-based Implementation (Records)

**Closures** allow for data encapsulation as the data can be hidden within the closure's environment.

By storing lambdas inside a `struct` (or record), we can create objects where data is hidden within the closure's environment.

```scheme
(struct counter (inc dec val))

(define (make-counter)
  (let ((count 0))                         ; internal state
    (counter
      (lambda () (set! count (+ count 1))) ; inc
      (lambda () (set! count (- count 1))) ; dec
      (lambda () count))))                 ; val

(define c (make-counter))

((counter-inc c))   ; Increments count
((counter-val c))   ; Returns 1
```

In this pattern, the "object" is a record of functions, and "methods" are invoked by selecting a field and calling the returned lambda.

#### Message Passing Implementation

In the **message passing** pattern, an object is represented as a single procedure (the **dispatcher**).

This procedure accepts a "message" (usually a symbol) and arguments, then decides which internal logic to execute.

> This follows Alan Kay's original vision of OOP, where communication between objects happens through messages rather than direct method calls.

```scheme
(define (make-counter)
  (let ((count 0))
    ;; Local methods
    (define (inc)         (set! count (+ count 1)))
    (define (set-val val) (set! count val))
    (define (get-val)     count)
  
    ;; The Dispatcher
    (lambda (message . args)
      (case message
        ((increase) (apply inc args))
        ((set)      (apply set-val args))
        ((value)    (apply get-val args))
        (else       (error "Unknown message" message))))))

(define c (make-counter))
(c 'increase) ; Increments
(c 'value)    ; Returns 1
```

##### Delegation and Inheritance

Inheritance is achieved by **delegation**: if the child object doesn't recognize a message, it passes it to a "parent" object.

```scheme
(define (make-dec-counter)
  (let ((parent (make-counter)))
    (define (decrease) (parent 'set (sub1 (parent 'value))))
    (define (increase) (parent 'set (add1 (parent 'value))))
  
    (lambda (message . args)
      (case message
        ((decrease) (apply decrease args))        ; new methods
        ((increase) (apply increase args))        ; override parent methods
        (else (apply parent (cons message args))) ; delegate to parent
      )
    )
  )
)
```

#### Prototype-based Implementation

In prototype-based OO (like JavaScript or Lua), there are no classes. Objects are cloned from existing **prototypes**, and behaviors are added or modified dynamically.

**Hash tables** are typically used to store fields and methods.

```scheme
(define new-object make-hash)
(define clone      hash-copy)

;; Macros for cleaner syntax
(define-syntax !!
  (syntax-rules ()
    ((_ obj key val)
      (hash-set! obj 'key val))))
(define-syntax ??
  (syntax-rules ()
    ((_ obj key)
      (hash-ref obj 'key))))
(define-syntax ->
  (syntax-rules ()
    ((_ obj msg arg ...)
      ((?? obj msg) obj arg ...))))

(define person (new-object))
(!! person name "Alice")
(!! person greet
    (lambda (self) (string-append "Hi, I'm " (?? self name))))

(-> person greet) ; "Hi, I'm Alice"
```

The `self` argument must be passed explicitly to methods to allow access to the object's local state, mimicking the `this` pointer.

##### Inheritance via Prototype Chain

Inheritance is implemented via a **dispatch chain**: if a property is not found in the current object, the search continues in its `parent` (prototype).

```scheme
(define (deep-clone obj)
  (if (not (hash-ref obj '<<parent>> #f))
    (clone obj)
    (let* ((cl (clone obj))
          (par (?? cl <<parent>>)))
      (!! cl <<parent>> (deep-clone par)))))
      
(define (son-of parent)
  (let ((o (new-object)))
    (!! o <<parent>> (deep-clone parent))
    o))

(define (dispatch obj msg)
  (if (eq? obj 'unknown)
    (error "Unknown message" msg)
    (let ((slot (hash-ref obj msg 'unknown)))
      (if (eq? slot 'unknown)
        (dispatch (hash-ref obj '<<parent>> 'unknown) msg)
        slot))))

(define-syntax ??       ;; reader
  (syntax-rules ()
    ((_ obj key)
      (dispatch obj 'key))))
(define-syntax ->        ;; send message
  (syntax-rules ()
    ((_ obj msg arg ...)
      ((dispatch obj 'msg) obj arg ...))))
```

### Error Handling

In scheme errors are raised using the `error` procedure:

```scheme
(error "An error occurred")
```

To define a custom error handling mechanism we must:

- Define a global variable to hold the stack of error handlers
- Define a procedure to pop and push error handlers in the stack

  ```scheme
  (define *error-handlers* '())

  (define (push-error-handler handler)
    (set! *error-handlers* (cons handler *error-handlers*)))
  (define (pop-error-handler)
    (let ((handler (car *error-handlers*)))
      (set! *error-handlers* (cdr *error-handlers*))
      handler))
  ```

- Define a `throw` procedure that invokes the top-most error handler from the stack, or raises an uncaught error if no handler is available.

  ```scheme
  (define (throw error)
    (let ((handler (pop-error-handler)))
      (if handler
          (handler error)
          (error "Uncaught error:" error))))
  ```

- Define a `try-catch` construct that pushes a new error handler onto the stack, executes the `try` block, and pops the handler off the stack afterward.

  ```scheme
  (define-syntax try
    (syntax-rules ()
      ((_ try-block ... (catch error-var catch-block ...))
        (call/cc (lambda (exit)
            (push-error-handler (lambda (error)     ; add new error handler
              (if (equal? error-var error)
                (exit begin catch-block ...)
                (throw error))))
            (let ((result (begin try-block ...)))   ; execute try block
              (pop-error-handler)                   ; remove error handler
              result))
        ))
    )
  )
  ```

## Haskell

**Haskell** is a _purely functional programming language_ based on immutability and side-effect-free functions. It is statically typed with strong type inference and polymorphism.

### Evaluation Strategy

Being purely functional allows the order of evaluation to be irrelevant, enabling **lazy evaluation** of **redexes** (reducible expressions). This means that expressions are not evaluated until their values are actually needed, which:

- Enables the creation of infinite data structures
- Improves performance by avoiding unnecessary computations
- Allows more flexible control flow

> A **redex** is an expression that can be reduced or simplified according to the rules of the language. In functional programming, a redex typically refers to a function application that can be evaluated to produce a result.

#### Call by Need

When calling a function, Haskell uses **call by need** evaluation strategy, which is an optimization of **call by name**. In call by need:

1. Arguments to a function are not evaluated until they are actually used
2. Once evaluated, the result is cached ("memoized")
3. Subsequent uses of the same argument reuse the cached value without re-evaluation

This is implemented using **thunks**, deferred computations. A thunk is a parameterless function that encapsulates an expression to be evaluated later.

**Evaluation Strategies Comparison:**

| Strategy | Order | Description |
| ---------- | ------- | ------------- |
| **Call by Value** | Innermost-first | Evaluates arguments before function application (leftmost redexes first) |
| **Call by Name** | Outermost-first | Evaluates function application before arguments (outermost redexes first) |
| **Call by Need** | Outermost-first + memoization | Like call by name, but caches results |

**Why Call by Name is More Robust:**

Call by name is more robust than call by value (Church-Rosser confluence) because it guarantees finding a normal form (fully reduced expression) when one exists. This is because:

- Call by name starts from the root of the expression tree
- Call by value starts from the leaves and may get stuck in non-terminating sub-computations

#### Scheme Implementation of Call by Name

In Scheme, we can simulate call by name using lambda expressions to create promises (thunks):

```scheme
(struct promise (thunk value?) #:mutable)

(define-syntax delay
  (syntax-rules ()
    ((_ expr ...)
      (promise (lambda () expr ...) #f))))

(define (force p)
  (cond
    ((not (promise? p)) p)                    ; If not a promise, return as is
    ((promise-value? p) (promise-value? p))   ; If already evaluated, return cached value
    (else                                     ; Otherwise, evaluate the thunk
      (let ((val ((promise-thunk p))))
        (set-promise-value?! p val)           ; Cache the evaluated value
        val))))
```

#### Forcing Strict Evaluation

Haskell provides mechanisms to override lazy evaluation when needed:

1. **BangPatterns (`!` symbol)**: Indicates that a value should be evaluated immediately (eagerly) rather than lazily.

    ```haskell
    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n * factorial (n - 1)  -- Normal lazy evaluation

    factorial' :: !Int -> Int
    factorial' 0 = 1
    factorial' !n = n * factorial' (n - 1)  -- Eager evaluation
    ```

2. **The `seq` function**: Forces evaluation of an expression before proceeding with the rest of the computation.

    ```haskell
    factorial'' :: Int -> Int
    factorial'' 0 = 1
    factorial'' n = n `seq` (n * factorial'' (n - 1))  -- Force evaluation of n
    ```

> **Use Case:** Strict evaluation is useful for preventing space leaks in accumulating parameters and improving performance in specific scenarios.

### Variables

Variables in Haskell are **immutable** by default, once a variable is bound to a value, it cannot be changed. This immutability is a core principle of functional programming and ensures **referential transparency** (an expression can be replaced by its value without changing program behavior).

#### Defining Variables

Haskell provides two keywords for defining local variables: `let` and `where`.

The `let` keyword allows us to define local variables within an expression:

```haskell
square :: Int -> Int
square x = 
  let y = x * x 
  in y
```

The `where` keyword allows us to define local variables at the end of a function definition that will be resolved as substitutions in the function body:

```haskell
square :: Int -> Int
square x = y
  where y = x * x
```

> **Difference:** `let` is an expression (can be used anywhere), while `where` is a declaration (only at function level).

### Type System

Haskell is a **statically typed** language — the type of every expression is known at compile time.

#### Type Inference

Haskell uses **type inference** via the Hindley-Milner type system to automatically deduce types without explicit annotations. The system infers the most general type for each expression.

**Type Annotations:**

The type can be explicitly specified using the `::` operator:

```haskell
x :: Int
x = 42
```

#### Built-in Types

Haskell provides several primitive types:

- `Integer`: Represents arbitrary-precision integers.
- `Int`: Represents fixed-precision integers.
- `Float`: Represents floating-point numbers.
- `Rational`: Represents rational numbers as fractions (`%`).
- `Char`: Represents a single Unicode character.
- `[a]`: Represents a list of elements of type `a`.
- `(a, b)`: Represents a tuple containing two elements of types `a` and `b`.

#### Type Aliases

Create descriptive names for existing types using the `type` keyword:

```haskell
type String = [Char]
```

### User-Defined Types

Haskell allows creating custom data types using the `data` keyword.

- **Type Constructor**: The name of the new type (used in type signatures)
- **Data Constructor(s)**: Functions to create values (also used in pattern matching)
- **Sum Types** (`|`): Multiple constructors representing alternatives (like C unions)
- **Product Types**: Single constructor with multiple fields (like C structs)

```haskell
data Shape = Circle Float | Rectangle Float Float
```

To create instances of user-defined types, we use the _data constructors_:

```haskell
circle :: Shape
circle = Circle 5.0

rectangle :: Shape
rectangle = Rectangle 4.0 6.0
```

#### Parametric Types (Generics)

Type constructors can take type parameters:

```haskell
data Maybe a = Nothing | Just a  -- 'a' is a type variable
```

#### Accessing Fields

##### Positional Access (Pattern Matching)

By default, fields are accessed via pattern matching:

```haskell
data Point = Point Float Float

point :: Point
point = Point 3.0 4.0

pointX :: Point -> Float
pointX (Point x _) = x  -- Extracts the first field
```

To simplify access, custom accessor functions can be defined manually:

```haskell
getX :: Point -> Float
getX (Point x _) = x

xVal :: Float
xVal = getX point  -- Accessing the x field
```

##### Record Syntax (Named Fields)

Record syntax provides named fields and automatically generates accessor functions:

```haskell
data Point = Point { x :: Float, y :: Float }

point :: Point
point = Point { x = 3.0, y = 4.0 }

valX :: Float
valX = x point  -- 'x' is now a function: Point -> Float
```

### Type Classes

Type classes define a set of functions that can operate on different types, providing **polymorphism** (similar to interfaces in OOP languages). Any type can become an instance of a type class by implementing its required methods.

#### Defining Type Classes

Use the `class` keyword to define a type class:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

The type signature `(==) :: (Eq a) => a -> a -> Bool` means:

- `(Eq a) =>` is a **type constraint** ("for any type `a` that is an instance of `Eq`")
- The function works for any type implementing the `Eq` type class

#### Creating Instances

Use the `instance` keyword to make a type an instance of a type class:

```haskell
instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False
```

#### Using Type Constraints

Enforce type class constraints in a signatures:

```haskell
-- Single constraint
areEqual :: (Eq a) => a -> a -> Bool
areEqual x y = x == y

-- Multiple constraints (comma-separated)
showAndEq :: (Show a, Eq a) => a -> a -> String
showAndEq x y = "Are they equal? " ++ show (x == y)
```

#### Common Type Classes

##### Eq

Supports equality testing with two methods:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

##### Ord

Supports ordering and comparison operations:

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<)     :: a -> a -> Bool
  (<=)    :: a -> a -> Bool
  (>)     :: a -> a -> Bool
  (>=)    :: a -> a -> Bool
```

> **Note:** `Eq a => Ord a` indicates that `Ord` requires `Eq`, any ordered type must also support equality.

##### Foldable

For data structures that can be reduced (folded) to a summary value:

```haskell
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b

  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f a bs = foldr (\b g x -> g (f x b)) id bs a

-- example

instance Foldable Maybe where
  foldr _ z Nothing  = z
  foldr f z (Just x) = f x z

instance Foldable List where
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)
```

##### Functor

For types that can be mapped over (containers that support applying a function to their contents):

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- example

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Functor List where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs
```

**Functor Laws:**

All Functor instances must satisfy:

1. **Identity**: `fmap id = id` (mapping the identity function does nothing)
2. **Composition**: `fmap (f . g) = fmap f . fmap g` (mapping a composition equals composing the maps)

###### Applicative

Extends Functor to support applying functions wrapped in a context to values wrapped in a context:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- example

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  (Just f) <*> (Just x) = Just (f x)

instance Applicative List where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
```

Where:

- `pure`: Takes a value into the applicative context
- `<*>`: Applies a wrapped function to a wrapped value (implementation varies: cartesian product or zip-like)

##### Monad

Extends Applicative to represent computations (called _actions_) that can be sequenced, where each computation can depend on the result of the previous one.

Monads enable **imperative-style** programming in a functional language while maintaining purity.

**Methods**:

- `>>=` (bind): Sequences two actions, passing the result of the first to the second
- `>>`: Sequences two actions, discarding the result of the first  
- `return`: Wraps a value in the monadic context (alias for `pure`)

```haskell
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b

  (>>)   :: m a -> m b -> m b
  m >> k = m >>= \_ -> k

  return :: a -> m a
  return = pure

-- example

instance Monad Maybe where
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x

instance Monad List where
  [] >>= _ = []
  (x:xs) >>= f = f x ++ (xs >>= f)
```

**Monad Laws:**

All Monad instances must satisfy:

1. **Left Identity**: `return a >>= f  ==  f a`
2. **Right Identity**: `m >>= return  ==  m`
3. **Associativity**: `(m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)`

###### Do Notation

Provides syntactic sugar for chaining monadic operations, making monadic code more readable and imperative-looking.

```haskell
example :: Maybe Int
example = do
  x <- Just 3
  y <- Just 4
  return (x + y)
```

Desugared version:

```haskell
example :: Maybe Int
example = Just 3 >>= \x ->
            Just 4 >>= \y ->
              return (x + y)
```

List comprehensions are syntactic sugar for the list monad:

```haskell
-- These three are equivalent:
do x <- [1, 2]
   y <- [3, 4]
   return (x, y)

[1, 2] >>= \x -> [3, 4] >>= \y -> return (x, y)

[(x, y) | x <- [1, 2], y <- [3, 4]]

-- All produce: [(1,3), (1,4), (2,3), (2,4)]
```

###### State Monad

Encapsulates stateful computations, allowing functions to read and modify shared state without explicit parameter passing.

**Type:** `State s a` represents a computation that:

- Takes an initial state of type `s`
- Produces a result of type `a` plus a new state of type `s`

```haskell
data State s a = State ( s -> (s, a) )

instance Functor (State s) where
  fmap f (State g) = State $ \s ->
    let (s', x) = g s
    in (s', f x)

instance Applicative (State s) where
  pure x = State $ \s -> (s, x)

  (State f) <*> (State g) = State $ \s ->
    let (s', h) = f s
        (s'', x) = g s'
    in (s'', h x)

instance Monad (State s) where
  (State g) >>= f = State $ \s ->
    let (s', x) = g s
        (State h) = f x
    in h s'

-- runState is used to execute a State computation with an initial state
runState :: State s a -> s -> (s, a)
runState (State f) s = f s

-- Example usage of State monad
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put newState = State $ \_ -> (newState, ())

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

main :: IO ()
main = do
    let (finalState, result) = runState 
          (do x <- get          -- Get the state (initially 100)
              modify (+ 10)     -- Modify the state by adding 10 (state becomes 110)
              return (x + 1))   -- Return the result of adding 1 to the original state (100)
          100
    
    print result                -- Print the result (101)
    print finalState            -- Print the final state (110)
```

**Key Functions:**

- `get`: Retrieves the current state
- `put`: Replaces the state  
- `modify`: Applies a function to the state
- `runState`: Executes a State computation with an initial state

### Data Structures

Haskell provides several built-in **immutable** data structures:

#### Arrays

Fixed-size collections (`Data.Array` module):

- **Create**: `listArray (lower, upper) [elements]`
- **Update**: `array // [(index, newValue)]` (creates new array)
- **Access**: `array ! index`

  ```haskell
  import Data.Array
  arr = let m = listArray (0, 4) [10, 20, 30, 40, 50]
            n = arr // [(2, 99)]  -- Update index 2 to 99
        in (m ! 2, n ! 2)
  ```

#### Maps

Key-value pairs (`Data.Map` module):

- **Create**: `fromList [(key1, val1), (key2, val2)]`
- **Insert**: `insert key value map`
- **Access**: `map ! key`

  ```haskell
  import qualified Data.Map as Map

  myMap = let m = fromList [("one", 1), ("two", 2), ("three", 3)]
              n = insert "four" 4 m
          in (m ! "two", n ! "four")
  ```

### Functions

Haskell functions are **first-class citizens**, they can be:

- Passed as arguments to other functions
- Returned from functions
- Assigned to variables

#### Basic Function Definition

Use the `->` operator in type signatures:

```haskell
add1 :: Int -> Int
add1 x = x + 1
```

A function is called by simply providing its arguments:

```haskell
result :: Int
result = add1 5  -- result will be 6
```

#### Point-Free Style

Functions can be defined without explicitly mentioning arguments:

```haskell
add1 :: Int -> Int
add1 = (+ 1)  -- Partial application of (+)
```

#### Infix Operators

Haskell allows functions with two arguments to be used as **infix operators** by enclosing their names in backticks (`` ` ``).

```haskell
5 `add` 3  -- Equivalent to (add 5 3)
```

Conversely, an infix operator (non-alphabetical like `+` or `*`) can be used as a prefix function by enclosing it in parentheses `()`.

```haskell
(+) 5 3  -- Equivalent to (5 + 3)
```

#### Lambda Functions

Lambda functions are defined using the `\` symbol followed by the parameters, an arrow `->`, and the function body.

```haskell
\x y -> x + y + 1
```

#### Composition

Function composition is achieved using the `(.)` operator, which allows combining two functions into a new function.

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

-- Example usage:
increment :: Int -> Int
increment = (+1)
double :: Int -> Int
double = (*2)

incrementThenDouble :: Int -> Int
incrementThenDouble = double . increment

result :: Int
result = incrementThenDouble 3  -- result will be 8
```

#### Function Application Operator

The `$` operator is used for function application and can be used to avoid parentheses.

```haskell
result :: Int
result = double $ increment 3  -- Equivalent to double (increment 3)
```

#### Currying

All functions in Haskell are **curried** by default, meaning that a multi-argument function is actually a chain of single-argument functions.

**Mathematical representation**:

A function $f: \mathbb{N} \times \mathbb{Z} \rightarrow \mathbb{Q}$ is represented as:

$$f: \mathbb{N} \rightarrow (\mathbb{Z} \rightarrow \mathbb{Q})$$

Or simply: $f: \mathbb{N} \rightarrow \mathbb{Z} \rightarrow \mathbb{Q}$ (since $\rightarrow$ is right-associative)

```haskell
add :: Integer -> Integer -> Integer
add x y = x + y
```

The function `add` takes an integer `x` and returns a new function that takes an integer `y` and returns the sum of `x` and `y`.

##### Partial Application

Currying enables **partial application** of functions, meaning that providing only some arguments returns a new function expecting the remaining ones:

```haskell
increment :: Int -> Int
increment = add 1
```

The `increment` function is created by partially applying the `add` function with the first argument set to `1`. It takes a single integer argument and adds `1` to it.

#### Polymorphism

Haskell supports **parametric polymorphism**, allowing functions to operate on values of any type without being tied to a specific one. This is achieved using _type variables_.

```haskell
identity :: a -> a
identity x = x
```

The `identity` function takes a value of any type `a` and returns a value of the same type.

> This is similar to generics in languages like Java or C#.

#### Pattern Matching

In Haskell, **pattern matching** is a powerful feature that allows to define functions by specifying patterns for their arguments. When a function is called, Haskell tries to match the provided arguments against the defined patterns in order from top to bottom.

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

Haskell also support **boolean guards** to define conditions for different cases in function definitions.

```haskell
absolute :: Int -> Int
absolute x
  | x < 0     = -x
  | otherwise = x
```

It is possible to define a function using the `case` expression, which allows for pattern matching within the function body.

```haskell
describeList :: [a] -> String
describeList xs = case xs of
  []      -> "The list is empty."
  [x]     -> "The list has one element."
  (x:y:_) -> "The list has multiple elements."
```

#### Common Higher-Order Functions

Haskell's standard library provides powerful functions for list manipulation:

- `map`: Applies a function to each element of a list and returns a new list of the results.

  ```haskell
  map :: (a -> b) -> [a] -> [b]
  map f [] = []
  map f (x:xs) = f x : map f xs
  ```

- `concatMap`: Maps a function over a list and concatenates the results.

  ```haskell
  concatMap :: (a -> [b]) -> [a] -> [b]
  concatMap f xs = concat (map f xs)
  ```

- `zip`: Combines two lists into a list of pairs.

  ```haskell
  zip :: [a] -> [b] -> [(a, b)]
  zip [] _ = []
  zip _ [] = []
  zip (x:xs) (y:ys) = (x, y) : zip xs ys

  -- Example usage:
  paired :: [(Int, Char)]
  paired = zip [1, 2, 3] ['a', 'b', 'c']  -- Returns [(1,'a'), (2,'b'), (3,'c')]
  ```

- **List Comprehensions**: A concise way to create lists based on existing lists (This is a monad).

  ```haskell
  squares :: [Int]
  squares = [x^2 | x <- [1..10]]  -- Generates a list of squares from 1 to 10
  ```

- `any`: Checks if any element in a list satisfies a given predicate.

  ```haskell
  any :: (a -> Bool) -> [a] -> Bool
  any _ [] = False
  any p (x:xs) = p x || any p xs
  ```

- `all`: Checks if all elements in a list satisfy a given predicate.

  ```haskell
  all :: (a -> Bool) -> [a] -> Bool
  all _ [] = True
  all p (x:xs) = p x && all p xs
  ```

- `elem`: Checks if an element is present in a list.

  ```haskell
  elem :: Eq a => a -> [a] -> Bool
  elem _ [] = False
  elem y (x:xs)
    | y == x    = True
    | otherwise = elem y xs
  ```

- `takeWhile`: Takes elements from a list while a predicate holds true.

  ```haskell
  takeWhile :: (a -> Bool) -> [a] -> [a]
  takeWhile _ [] = []
  takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []
  ```

- `filter`: Filters a list based on a predicate function.

  ```haskell
  filter :: (a -> Bool) -> [a] -> [a]
  filter _ [] = []
  filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
  ```

- `foldr` and `foldl`: Reduces a list to a single value by recursively applying a binary function.
  
  > **Performance Note:** Due to lazy evaluation, `foldr` is generally preferred:
  > - `foldr` works with infinite lists
  > - `foldl` can lead to stack overflows unless used with `foldl'` (strict version). It doesn't use stack space, but the lazy nature can cause excessive heap usage.

  ```haskell
  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)
  foldl _ z []     = z
  foldl f z (x:xs) = foldl f (f z x) xs
  ```

- `concat`: Concatenates a list of lists into a single list.

  ```haskell
  concat :: [[a]] -> [a]
  concat [] = []
  concat (xs:xss) = xs ++ concat xss

  -- Example usage:
  combined :: [Int]
  combined = concat [[1, 2], [3, 4], [5]]  -- Returns [1, 2, 3, 4, 5]
  ```

- `length`: Returns the number of elements in a list.

  ```haskell
  length :: [a] -> Int
  length [] = 0
  length (_:xs) = 1 + length xs
  ```

- `reverse`: Reverses a list.

  ```haskell
  reverse :: [a] -> [a]
  reverse [] = []
  reverse (x:xs) = reverse xs ++ [x]
  ```

- `take`: Takes the first `n` elements from a list.

  ```haskell
  take :: Int -> [a] -> [a]
  take _ [] = []
  take n (x:xs)
    | n <= 0    = []
    | otherwise = x : take (n - 1) xs
  ```

- `drop`: Drops the first `n` elements from a list.

  ```haskell
  drop :: Int -> [a] -> [a]
  drop _ [] = []
  drop n xs@(x:xs')
    | n <= 0    = xs
    | otherwise = drop (n - 1) xs'
  ```

### Infinite Data Structures

Haskell's lazy evaluation enables working with **infinite data structures**. Values are computed only when needed, allowing theoretically infinite lists.

```haskell
-- Infinite list of ones
ones :: [Int]
ones = 1 : ones

-- Infinite list from n onward  
ones' = [1, 1..]  -- Alternative notation

numFrom n = n : numFrom (n+1) -- An infinite list of natural numbers starting from n
squares = map (^2) (numFrom 1) -- An infinite list of squares of natural numbers
```

We can work with infinite lists using functions like `take`, which retrieves a finite number of elements from the beginning of the list.

```haskell
firstFiveOnes :: [Int]
firstFiveOnes = take 5 ones  -- Returns [1, 1, 1, 1, 1]
```

### Control Flow

#### Conditional Expressions

Haskell provides `if-then-else` expressions:

```haskell
if condition then trueExpression else falseExpression
```

The `else` clause is **mandatory** (since expressions must always return a value).

**Implementation as a function**:

```haskell
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
```

#### Haskell Iteration

Haskell uses **recursion with pattern matching** instead of traditional loops:

### Input/Output

Haskell handles I/O using the **`IO` monad**, which encapsulates side-effecting operations while maintaining functional purity.

```haskell
main :: IO ()
main = do
  putStrLn "Hello, World!"
```

> **Note:** The `IO` monad internally uses an implicit "world state" token to sequence operations.

#### Basic I/O Operations

- `putStrLn`: Outputs a string followed by a newline to the standard output.

  ```haskell
  putStrLn :: String -> IO ()

  putStrLn "Hello, World!"
  ```

- `getLine`: Reads a line of input from the standard input.

  ```haskell
  getLine :: IO String

  name <- getLine
  ```

- `getChar`: Reads a single character from the standard input.

  ```haskell
  getChar :: IO Char

  char <- getChar
  ```

- `print`: Outputs a value to the standard output, converting it to a string using the `Show` type class.

  ```haskell
  print :: Show a => a -> IO ()

  print 42
  ```

#### Command Line Arguments

Haskell provides the `System.Environment` module to access command line arguments.

```haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Command line arguments: " ++ show args)

  progName <- getProgName
  putStrLn ("Program name: " ++ progName)
```

### Haskell Error Handling

#### Bottom Type (⊥)

Represents non-terminating computations and errors. Defined as `⊥ = ⊥` or `bot = bot`.

#### Raising errors

```haskell
error :: String -> a
error "An error occurred"  -- Terminates program with error message
```

#### Safe Error Handling with Maybe

The `Maybe` type represents computations that may fail:

```haskell
data Maybe a = Nothing | Just a
```

- `Just a`: Successful computation with value of type `a`
- `Nothing`: Failure or absence of value

#### Exception Handling in IO

The `Control.Exception` module provides exception handling for `IO` operations. This is done with the `handle` function, which takes an exception handler and an `IO` action.

```haskell
import Control.Exception
main :: IO ()
main = handle handler (do
    putStrLn "Enter a number:"
    input <- getLine
    let number = read input :: Int
    print (10 `div` number))
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Caught an exception: " ++ show ex
```

## Erlang

**Erlang** is a functional programming language designed for building concurrent, distributed, and fault-tolerant systems.

- Runs on the **BEAM** virtual machine
- Provides lightweight processes (thousands can run concurrently)
- Processes are isolated and communicate via **message passing**
- Hot code swapping (update code without stopping the system)

> Erlang's actor model inspired the **Akka** framework (Scala/Java) and influenced the "Reactive Manifesto" for building responsive systems.

### Erlang Variables

In Erlang, variables are immutable. Once a variable is bound to a value, it cannot be changed. Attempting to rebind a variable results in a runtime error.

**Naming Convention:**

- Variables start with an **uppercase** letter or underscore (`_`)
- `_` alone is the "don't care" variable (matches anything, value is ignored)

```erlang
X = 10         % Bind X to 10
Y = X + 5      % Bind Y to 15
_ = foo()      % Call foo() but ignore the result
```

### Data Types

Erlang provides several built-in data types:

- **Numbers**: Integers and floating-point numbers
- **Atoms**: Constants whose name is their value
- **Tuples**: Fixed-size collections
- **Lists**: Variable-length ordered collections
- **Maps**: Key-value pairs
- **Pids**: Process identifiers

#### Atoms

Atoms are constants whose value is their own name (similar to symbols in other languages).

**Syntax:**

- Start with a **lowercase** letter, can include letters, digits, underscores
- Can be enclosed in single quotes (`'...'`) to include spaces or special characters

```erlang
ok                        % Atom
error                     % Atom  
'Error occurred'          % Atom with spaces
'Hello@World!'            % Atom with special characters
```

Atoms are stored in a global atom table (not garbage collected) and are extremely fast to compare (just comparing references)

#### Tuples

Fixed-size collections of elements, defined using curly braces `{}`.

```erlang
Point = {3, 4}                    % A tuple representing a point (x, y)
Person = {person, "Alice", 30}    % Common pattern: {tag, ...data}
```

**Common Functions:**

- `element(N, Tuple)`: Retrieves the N-th element (1-based indexing)
- `size(Tuple)`: Returns the number of elements

> **Common Pattern:** Use tuples with an atom tag as the first element: `{ok, Result}`, `{error, Reason}`

#### Erlang Lists

Ordered, variable-length collections, defined using square brackets `[]`.

```erlang
Numbers = [1, 2, 3, 4, 5]         % A list of numbers
Mixed = [atom, 42, "string"]      % Lists can contain mixed types
Empty = []                         % Empty list
```

**List Operations:**

```erlang
List1 = [1, 2, 3].
List2 = [4, 5, 6].

% Concatenation
List1 ++ List2.      % Results in [1, 2, 3, 4, 5, 6]

% Cons operator |
[0 | List1].         % Results in [0, 1, 2, 3]
[H | T] = [1, 2, 3]. % Pattern matching: H=1, T=[2,3]

% Warning: This creates a nested list!
[List1 | List2].     % Results in [[1, 2, 3], 4, 5, 6]
```

**Common Functions:**

- `hd(List)`: Returns the head (first element)
- `tl(List)`: Returns the tail (all elements except the head)
- `length(List)`: Returns the number of elements
- `lists:reverse(List)`: Reverses the list
- `lists:nth(N, List)`: Returns the N-th element (1-based)
- `lists:map(Fun, List)`: Applies function to each element
- `lists:filter(Pred, List)`: Filters elements matching predicate
- `lists:foldl(Fun, Acc, List)`: Left fold (tail recursive)
- `lists:foldr(Fun, Acc, List)`: Right fold

##### List Comprehensions

Concise syntax for generating lists:

```erlang
Squares = [X * X || X <- [1, 2, 3, 4, 5]].              % [1, 4, 9, 16, 25]
Evens = [X || X <- [1, 2, 3, 4, 5], X rem 2 == 0].      % [2, 4]
Pairs = [{X, Y} || X <- [1, 2], Y <- [a, b]].           % [{1,a}, {1,b}, {2,a}, {2,b}]
```

#### Erlang Maps

Key-value pairs (hash maps), defined using `#{}` syntax.

**Syntax:**

```erlang
% Creating maps
Person = #{name => "Alice", age => 30}.
Empty = #{}.

% Accessing values
Name = maps:get(name, Person).     % Returns "Alice" or throws exception
Age = maps:get(age, Person, 0).    % Returns 30, or 0 if not found

% Updating (creates new map)
Updated = Person#{age => 31}.      % Update existing key
Updated2 = Person#{city => "NYC"}. % Add new key
Updated3 = Person#{age := 31}.     % := requires key to exist
```

**Common Functions:**

- `maps:get(Key, Map)` / `maps:get(Key, Map, Default)`: Get value
- `maps:put(Key, Value, Map)`: Insert or update
- `maps:remove(Key, Map)`: Remove a key
- `maps:keys(Map)`: List of all keys
- `maps:values(Map)`: List of all values
- `maps:size(Map)`: Number of key-value pairs
- `maps:merge(Map1, Map2)`: Merge two maps
- `maps:filter(Pred, Map)`: Filter map by predicate

### Erlang Pattern Matching

Erlang uses **pattern matching** as a fundamental feature for:

- Variable binding
- Function clause selection
- Data extraction
- Control flow

**Basic Examples:**

```erlang
% Tuple matching
{A, B} = {10, 20}.                % A=10, B=20
{ok, Value} = {ok, 42}.           % Value=42
{A, A, B} = {1, 1, 2}.            % A=1, B=2 (A must match both positions)
{A, A, B} = {1, 2, 3}.            % ERROR: A can't be both 1 and 2

% List matching
[A, B | Rest] = [1, 2, 3, 4, 5].  % A=1, B=2, Rest=[3,4,5]

% Map matching
#{name := Name} = #{name => "Alice", age => 30}. % Name="Alice"
```

### Erlang Functions

Functions are defined with multiple **clauses**, selected by pattern matching on arguments.

**Syntax Rules:**

- Clauses are separated by `;`
- Last clause ends with `.`
- Clause head and body separated by `->`
- Statements within a body separated by `,`

```erlang
factorial(0) -> 1;                    % Base case
factorial(N) -> N * factorial(N - 1). % Recursive case
```

#### Function Arity

Functions are identified by **name/arity** (name and number of arguments).

The same name can be used for different arities:

```erlang
add(X, Y) -> X + Y.           % add/2
add(X, Y, Z) -> X + Y + Z.    % add/3 (different function)
```

**Referencing Functions:**

Use the `/` notation to specify the specific function:

```erlang
Result = add(2, 3).           % Calls add/2
Result2 = add(1, 2, 3).       % Calls add/3
Fun = fun add/2.              % Reference to add/2 function
```

#### Anonymous Functions (Lambdas)

Defined using the `fun` keyword:

```erlang
% Simple lambda
Add = fun(X, Y) -> X + Y end.
Result = Add(2, 3).  % Result is 5

% Lambda with multiple clauses
Abs = fun(X) when X < 0 -> -X;
         (X) -> X
      end.
```

> **Note:** Named functions must be referenced with the `fun Name/Arity` syntax when passed as arguments.

#### Guards

Additional conditions that refine pattern matching, specified with the `when` keyword.

```erlang
absolute(X) when X < 0 -> -X;
absolute(X) -> X.

max(X, Y) when X > Y -> X;
max(_, Y) -> Y.

% Multiple guards with semicolon (OR)
is_valid(X) when X > 0; X < -10 -> true;
is_valid(_) -> false.

% Multiple conditions with comma (AND)
in_range(X) when X > 0, X < 100 -> true;
in_range(_) -> false.
```

Guards uses a specific sub-language to ensure validation in constant time. It allows:

- **Comparison**: `<`, `>`, `=<`, `>=`, `==`, `/=`, `=:=`, `=/=`
  - `==`: Value equality (1 == 1.0 is true)
  - `=:=`: Exact equality (1 =:= 1.0 is false)
- **Logical**: `and`, `or`, `xor`, `not`
- **Type tests**: `is_atom/1`, `is_integer/1`, `is_list/1`, `is_tuple/1`, `is_map/1`, etc.
- **Arithmetic**: `+`, `-`, `*`, `/`, `div`, `rem`
- **Built-ins**: `length/1`, `tuple_size/1`, `map_size/1`, `hd/1`, `tl/1`, `element/2`

> **Restriction:** User-defined functions cannot be called in guards. Only built-in guard functions are allowed.

### Modules

Erlang code is organized into **modules**, units of compilation and namespace.

**Module Structure:**

```erlang
% Module declaration (must match filename: math_utils.erl)
-module(math_utils).

% Export public functions (name/arity)
-export([factorial/1, absolute/1]).

% Optional: Import functions from other modules
-import(lists, [map/2, filter/2]).

% Function definitions
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

absolute(X) when X < 0 -> -X;
absolute(X) -> X.

% Private function (not exported)
helper(X) -> X * 2.
```

**Using Modules:**

```erlang
% Call exported function with Module:Function syntax
X = math_utils:factorial(5).  % 120

% Compile module
c(math_utils).  % In Erlang shell
```

### Erlang Control Flow

#### If Expressions

The `if` expression uses guard expressions for conditions. Evaluates guards in order and executes the first true branch.

```erlang
classify(X) ->
  if
    X < 0 -> negative;
    X > 0 -> positive;
    true -> zero    % Catch-all (like 'else')
  end.
```

#### Case Expressions

The `case` expression is based on pattern matching (more powerful than `if` as it allows calling custom functions).

```erlang
% Basic case
case X of
  0 -> zero;
  1 -> one;
  _ -> other  % _ matches anything (catch-all)
end.

% Case with function calls
case lists:member(a, X) of
  true -> contains_a;
  false -> does_not_contain_a
end.
```

#### Iteration (Recursion)

Erlang uses **recursion** instead of loops. Use tail recursion for efficiency (constant stack space).

```erlang
countdown(0) ->
  ok;
countdown(N) when N > 0 ->
  io:format("~p~n", [N]),
  countdown(N - 1).
```

### Concurrency

Erlang is built on the **Actor Model**, everything runs in isolated processes that communicate via message passing.

- Processes are lightweight (can run millions concurrently)
- Each process has its own heap and mailbox
- **No shared memory** between processes
- Message passing is **asynchronous** (send and continue)
- Message order between two processes is preserved, but not globally

> **Concurrency Models Comparison:**
>
> - **Actor Model** (Erlang): Isolated processes, message passing, no shared state
> - **Shared Memory** (Java threads): Shared data, locks/mutexes for synchronization
> - **CSP** (Go): Processes communicate via synchronous channels

#### Core Primitives

1. `spawn` - Create a process:

    ```erlang
    Pid = spawn(Module, Function, Args).  % Returns process ID
    Pid = spawn(fun() -> loop() end).     % Spawn with lambda
    ```

2. `!` - Send a message (asynchronous):

    ```erlang
    Pid ! {self(), hello}.  % Send message to Pid
    Pid ! stop.
    ```

3. `receive` - Receive messages: Takes the first message from the mailbox that matches a pattern. Blocks if no match.

    ```erlang
    receive
      {From, Msg} -> 
        io:format("Received ~p from ~p~n", [Msg, From]);
      stop -> 
        ok;
      _ -> 
        io:format("Unknown message~n")
    end.
    ```

**Complete Example:**

```erlang
% Echo server: receives messages and sends responses
echo() ->
  receive
    {From, Message} ->
      io:format("Received: ~p~n", [Message]),
      From ! {self(), "Message received!"},  % Reply to sender
      echo();  % Tail recursive call to keep process alive
    stop ->
      io:format("Stopping echo server~n"),
      ok  % Terminate (process exits)
  end.

% Start the echo server
start() ->
  Pid = spawn(fun echo/0),              % Create new process
  Pid ! {self(), "Hello, Erlang!"},     % Send message
  receive
    {Pid, Reply} -> io:format("Got reply: ~p~n", [Reply])
  end,
  Pid ! stop.  % Stop the server
```

**Useful Built-ins:**

- `self()`: Returns PID of current process
- `is_process_alive(Pid)`: Check if process is running

#### Registered Processes

Processes can be registered with **global atom names** for easier access (no need to track PIDs).

```erlang
% Register a process
start() ->
  Pid = spawn(fun echo/0),
  register(echo_server, Pid),  % Register with atom name
  ok.

% Send message using registered name
echo_server ! {self(), "Hello"}.
```

Some useful functions are:

- `unregister(Name)`: Unregister a name
- `whereis(Name)`: Get PID of registered name

#### Encapsulation Pattern

A good practice when implementing modules is to export only public API functions keeping process implementation private.

```erlang
-module(echo).
% Public API
-export([start/0, send_message/1, stop/0]).

% Public functions
start() ->
  Pid = spawn(fun loop/0),      % spawn internal function
  register(echo_server, Pid),
  {ok, Pid}.

send_message(Message) ->
  echo_server ! {self(), Message},
  ok.

stop() ->
  echo_server ! stop,
  ok.

% Private functions (not exported)
loop() ->
  receive
    {From, Msg} ->
      From ! {echo, Msg},
      loop();
    stop ->
      ok
  end.
```

#### Timeouts and Timers

**Receive Timeout:**

Handle cases where no matching message arrives within a time limit.

```erlang
receive
  {data, Value} ->
    io:format("Received: ~p~n", [Value])
after 5000 ->  % Timeout after 5000 ms (5 seconds)
    io:format("No message received~n")
end.

% Timeout of 0: check mailbox without blocking
receive
  Msg -> handle(Msg)
after 0 ->
    no_message
end.
```

**Delays:**

```erlang
timer:sleep(1000).  % Sleep current process for 1 second
```

**Scheduled Messages:**

```erlang
% Timer with reference (can be cancelled)
Ref = erlang:start_timer(3000, self(), tick).
erlang:cancel_timer(Ref).  % Cancel if needed
```

#### Flush Mailbox

The `flush()` function clears all messages in the current process's mailbox:

```erlang
flush().
```

### Error Handling Philosophy: "Let It Crash"

Erlang embraces a unique philosophy: **"Let It Crash"**.

**Core Principles:**

1. Don't defend against every possible error, as failures in distributed system are inevitable
2. Isolate failures, one process crash doesn't bring down the system
3. Use supervisors, monitoring processes that restart failed workers
4. Design for recovery, make restart cheap and state restorable

#### Supervisors

Supervisors are special processes that monitor workers and can restart them on failure.

A supervisor needs to be **linked** to a worker process. This allows to receive exit signals when the worker crashes, and stop the worker when the supervisor crashes.

To be able to handle exit signals, the supervisor process must set `trap_exit` flag to `true`. This converts exit signals into messages (`{'EXIT', Pid, Reason}`) that the supervisor can receive and handle.

```erlang
% Start supervisor with N workers
start_supervisor(Count) ->
  process_flag(trap_exit, true),  % Convert exit signals to messages
  spawn_workers(Count),
  supervisor_loop(Count).

% Spawn N linked workers
spawn_workers(0) -> ok;
spawn_workers(N) -> 
  spawn_link(fun worker/0),  % Create linked worker
  spawn_workers(N - 1).

% Supervisor main loop
supervisor_loop(Count) ->
  receive
    {'EXIT', Pid, normal} ->
      % Worker exited normally
      io:format("Worker ~p exited normally~n", [Pid]),
      NewCount = Count - 1,
      if
        NewCount > 0 -> supervisor_loop(NewCount);
        true -> io:format("All workers done. Shutting down.~n")
      end;
    
    {'EXIT', Pid, Reason} ->
      % Worker crashed - restart it
      io:format("Worker ~p crashed (~p). Restarting...~n", [Pid, Reason]),
      spawn_workers(1),  % Restart one worker
      supervisor_loop(Count)  % Keep same count
  end.

% Worker process (may crash randomly)
worker() ->
  timer:sleep(1000),
  case rand:uniform(10) of
    N when N > 7 -> 
      exit(random_crash);  % 30% chance to crash
    _ -> 
      io:format("Worker ~p working...~n", [self()]),
      worker()  % Continue working
  end.
```

### Erlang Patterns

- **Spawning Multiple Processes**: To spawn multiple processes for parallel computation, use list comprehensions combined with `spawn`.

    ```erlang
    Pids = lists:reverse([spawn(fun() -> deep_map_par(self(), X) end) || X <- List])
    ```

- **Collecting Results**: To collect in order the results from multiple processes, use a list comprehension with `receive`.

    ```erlang
    Results = [receive {Pid, Result} -> Result end || Pid <- Pids],
    ```

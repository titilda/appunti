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

**Haskell** is a _purely functional programming language_ based on immutability and non-side-effecting functions. It is statically typed with strong type inference and polymorphism.

### Evaluation Strategy

Being purely functional allow the order of evaluation to be irrelevant, allowing **lazy evaluation** of the **redex** (reducible expression). This means that expressions are not evaluated until their values are actually needed, enabling the creation of infinite data structures and improving performance by avoiding unnecessary computations.

> A **redex** is an expression that can be reduced or simplified according to the rules of the language. In functional programming, a redex typically refers to a function application that can be evaluated to produce a result.

When calling a function haskell use **call by need** evaluation strategy, which is an optimization of **call by name**. In call by need, the arguments to a function are not evaluated until they are actually used within the function body, and once evaluated, the result is cached (or "memoized") so that subsequent uses of the same argument do not require re-evaluation.

This is implemented using **thunks**, which are essentially deferred computations. A thunk is a parameterless function that encapsulates an expression to be evaluated later.

> The order of evaluation of call by value is a innermost-first strategy, meaning that the leftmost redexes are evaluated first.
>
> In contrast, call by name uses an outermost-first strategy, where the outermost expressions are evaluated first.

Call by name is more robust than call by value (Church-Rosser confluence) because it guarantees that if there is a normal form (a fully reduced expression), as it starts from the root of the expression tree. In contrast, call by value may fail to find a normal form if the evaluation order leads to non-terminating computations, as it starts from the leaves of the expression tree.

#### Scheme implementation of Call by Name

In Scheme, we can simulate call by name using lambda expressions to create promises.

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

#### Enforce Evaluation

To enforce evaluation of an expression in Haskell, we can use the `BangPatterns`, the `!` symbol, to indicate that a value should be evaluated immediately (eagerly) rather than lazily.

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)  -- Normal lazy evaluation
factorial' :: Int -> Int
factorial' 0 = 1
factorial' !n = n * factorial' (n - 1)  -- Eager evaluation
```

It is also possible to use the `seq` function to force the evaluation of an expression before proceeding with the rest of the computation.

```haskell
factorial'' :: Int -> Int
factorial'' 0 = 1
factorial'' n = n `seq` (n * factorial'' (n - 1))  -- Force evaluation of n
```

### Variables

Variables in Haskell are **immutable** by default, meaning that once a variable is assigned a value, it cannot be changed. This immutability is a core principle of functional programming and helps to ensure referential transparency.

#### Haskell Types

Haskell is a statically typed language, meaning that the type of every expression is known at compile time.

Haskell uses **type inference** to automatically deduce the types of expressions without requiring explicit type annotations. This is done using the Hindley-Milner type system, that will infer the least general type for each expression.

A variable is defined using the `let` keyword:

```haskell
let x = 5
```

It is possible to use the `where` keyword to define local variables within a function:

```haskell
f y = x + y
  where x = 10
```

To define a variable with an explicit type annotation, we use the `::` operator:

```haskell
x :: Int
```

Haskell has several built-in types, including:

- `Integer`: Represents arbitrary-precision integers.
- `Int`: Represents fixed-precision integers.
- `Float`: Represents floating-point numbers.
- `Rational`: Represents rational numbers as fractions (`%`).
- `Char`: Represents a single Unicode character.
- `[a]`: Represents a list of elements of type `a`.
- `(a, b)`: Represents a tuple containing two elements of types `a` and `b`.

#### Type Class

Type classes in Haskell are a way to define a set of functions that can operate on different types. They provide a form of polymorphism, allowing functions to work with any type that implements the required interface.

We can define a type class using the `class` keyword:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

The type of `==` is `(==) :: (Eq a) => a -> a -> Bool`, meaning that it works for any type `a` that is an instance of the `Eq` type class.

To create an instance of a type class for a specific type, we use the `instance` keyword:

```haskell
instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False
```

#### User-Defined Types

Haskell allows the creation of custom data types using the `data` keyword. This enables the definition of complex data structures.

Key components include:

- Type Constructor: The name of the new type, used in type signatures.
- Data Constructor(s): Functions used to create values of the type; these are also used in pattern matching.
- Sum Types: The `|` operator allows for multiple constructors, representing a choice between different data shapes (c-like unions).
- Product Types: A single constructor can take multiple arguments (types), grouping different values together (c-like structs).

```haskell
data Shape = Circle Float | Rectangle Float Float
```

The type constructor can take parameters to create **parametric types** (generics):

```haskell
data Maybe a = Null | Just a
```

To create instances of user-defined types, we use the _data constructors_:

```haskell
circle :: Shape
circle = Circle 5.0

rectangle :: Shape
rectangle = Rectangle 4.0 6.0
```

##### Accessing Fields

By default, Haskell uses positional notation to define and access the fields of a data constructor. Accessing these values typically requires **pattern matching**.

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

However, Haskell provides **record syntax** to define data types with named fields. This automatically generates accessor functions for each field.

```haskell
data Point = Point { x :: Float, y :: Float }

point :: Point
point = Point { x = 3.0, y = 4.0 }

valX :: Float
valX = x point  -- 'x' is now a function: Point -> Float
```

#### Type Aliases

Type aliases can be created using the `type` keyword, allowing for more readable code by giving descriptive names to existing types.

```haskell
type String = [Char]
```

### Function

Haskell functions are **first-class citizens**, meaning they can be passed as arguments, returned from other functions, and assigned to variables.

A function is defined using the `->` operator to denote the type of its parameters and return value.

```haskell
add1 :: Int -> Int
add1 x = x + 1
```

A function is called by simply providing its arguments:

```haskell
result :: Int
result = add1 5  -- result will be 6
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

#### Currying

In Haskell, all functions are **curried** by default. This means that a function that takes multiple arguments is actually a series of functions that each take a single argument and return another function until all arguments have been provided.

So a function the is $f: \mathbb{N} \times \mathbb{Z} \rightarrow \mathbb{Q}$ is actually represented as $f: \mathbb{N} \rightarrow (\mathbb{Z} \rightarrow \mathbb{Q})$, or simple $f: \mathbb{N} \rightarrow \mathbb{Z} \rightarrow \mathbb{Q}$ (the $\rightarrow$ is right associative).

```haskell
add :: Integer -> Integer -> Integer
add x y = x + y
```

The function `add` takes an integer `x` and returns a new function that takes an integer `y` and returns the sum of `x` and `y`.

##### Partial Application

This allows for **partial application** of functions, where you can provide some of the arguments and get back a new function that takes the remaining arguments.

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

#### Common Functions

Haskell provides many built-in higher-order functions for working with lists, such as:

- `map`: Applies a function to each element of a list and returns a new list of the results.

  ```haskell
  map :: (a -> b) -> [a] -> [b]
  map f [] = []
  map f (x:xs) = f x : map f xs

  -- Example usage:
  doubled :: [Int]
  doubled = map (*2) [1, 2, 3]  -- Returns [2, 4, 6]
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

- **List Comprehensions**: A concise way to create lists based on existing lists.

  ```haskell
  squares :: [Int]
  squares = [x^2 | x <- [1..10]]  -- Generates a list of squares from 1 to 10
  ```

- `any`: Checks if any element in a list satisfies a given predicate.

  ```haskell
  any :: (a -> Bool) -> [a] -> Bool
  any _ [] = False
  any p (x:xs) = p x || any p xs

  -- Example usage:
  hasEven :: Bool
  hasEven = any even [1, 3, 5, 6]  -- Returns True
  ```

- `takeWhile`: Takes elements from a list while a predicate holds true.

  ```haskell
  takeWhile :: (a -> Bool) -> [a] -> [a]
  takeWhile _ [] = []
  takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []

  -- Example usage:
  leadingEvens :: [Int]
  leadingEvens = takeWhile even [2, 4, 6, 1, 8]  -- Returns [2, 4, 6]
  ```

- `foldr` and `foldl`: Reduces a list to a single value by recursively applying a binary function. Due to Haskell's lazy evaluation, `foldr` is more efficient as on `foldl` increase the heap usage.

  ```haskell
  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)

  -- Example usage:
  sumList :: Int
  sumList = foldr (+) 0 [1, 2, 3, 4]  -- Returns 10
  ```

### Infinite Computations

Haskell's call-by-need evaluation allows for the creation and manipulation of **infinite data structures**. Since values are only computed when needed, we can define lists that are theoretically infinite.

```haskell
ones :: [Int]
ones = 1 : ones  -- An infinite list of 1

-- alternative notation
ones = [1..]

numFrom n = n : numFrom (n+1) -- An infinite list of natural numbers starting from n
squares = map (^2) (numFrom 1) -- An infinite list of squares of natural numbers
```

We can work with infinite lists using functions like `take`, which retrieves a finite number of elements from the beginning of the list.

```haskell
firstFiveOnes :: [Int]
firstFiveOnes = take 5 ones  -- Returns [1, 1, 1, 1, 1]
```

### Conditional Expressions

Haskell uses the `if <c> then <t> else <e>` construct for conditional expressions. The syntax is as follows:

```haskell
if condition then trueExpression else falseExpression
```

An example of implementation using functions would be:

```haskell

if :: Bool -> a -> a -> a
if True x _ = x
if False _ y = y
```

### Error

Haskell represent errors using **bottom type** (denoted as `⊥`), which is defined as `bot = bot`. The bottom type represents non-terminating computations.

Errors can be raised using the `error` function:

```haskell
error :: String -> a
error "An error occurred"
```

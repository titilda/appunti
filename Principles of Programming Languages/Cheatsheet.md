# Functional Programming Languages Cheatsheet

## SCHEME

### Variables & Binding

```scheme
(let ((x 10) (y 20)) ...)           ; parallel
(let* ((x 10) (y (+ x 5))) ...)     ; sequential
(letrec ((f (lambda (n) ...))) ...) ; recursive
(define x 10)                       ; global
(set! x 20)                         ; mutation (!)
```

### Data Types

```scheme
42 3.14 1/2                ; numbers
#t #f                      ; booleans
#\a                        ; characters
"string"                   ; strings
'symbol                    ; symbols
'(1 2 3)                   ; lists
(cons 1 2)                 ; pairs (1 . 2)
#(1 2 3)                   ; vectors
(struct person (name age)) ; records
```

#### Struct

```scheme
(define alice (person "Alice" 30))  ; create instance
(person-name alice)                 ; access field
(set-person-name! alice "Bob")       ; update field (returns new struct)
```

#### Vectors

```scheme
(vector-length v)       ; length
(vector-ref v i)       ; access element
(vector-set! v i val)  ; update element
```

#### Lists

```scheme
(car '(1 2 3))     ; → 1
(cdr '(1 2 3))     ; → (2 3)
(cons 1 '(2 3))    ; → (1 2 3)
(cadr lst)         ; car-cdr shortcuts
(append l1 l2)     ; concatenate
(map f lst)        ; apply function
(member val lst)   ; check if element is in list
(filter pred lst)  ; filter
(apply f lst)      ; apply with list of args
(foldl f init lst) ; left fold
(foldr f init lst) ; right fold
```

### Functions

```scheme
(lambda (x y) (+ x y))               ; anonymous
(define (square x) (* x x))          ; named
(define square (lambda (x) (* x x))) ; equivalent

; Closures
(define (make-adder n)
  (let ((count 0))
    (lambda () (set! count (+ count 1))
      count)))
```

### Quote

```scheme
'expr           ; prevents evaluation (literal)
`(a ,b c)       ; quasiquote with unquote
(eval (cons 'a '(b c))) ; → (a b c)
```

### Control Flow

```scheme
(if condition then else)

(cond
  [(test1) result1]
  [(test2) result2]
  [else default])

(when test body ...)

(case x [(v1 v2) r1] [else r2])

(unless condition then-branch)

(begin expression1 expression2 ... expressionN)
```

#### Iteration

```scheme
(let label ((n 5))
  (when (>= n 0)
    (displayln n)
    (label (- n 1))))
```

### Condition

```scheme
(= x 0)          ; equality
(eq? x y)        ; same object
(eqv? x y)       ; same object or value (for numbers, chars)
(equal? x y)     ; deep equality (for lists, vectors)
```

### Pattern Matching

```scheme
(match value
  ['() "empty"]
  [(cons h t) (+ 1 (length t))]
  [(list a b c) (+ a b c)]
  [_ "default"])
```

### Continuations

```scheme
(call/cc (lambda (exit)
  (+ 1 (exit 5) 100)))  ; → 5 (exit aborts)
```

### Macros

```scheme
(define-syntax when
  (syntax-rules ()
    [(when test body ...)
     (if test (begin body ...) (void))]))
```

### Choose

```scheme
(let ((x (choose 1 2 3)))
  (displayln x)
  (if (= x 2)
      (fail)))
```

## HASKELL

### Variables & Functions

```haskell
x = 42                          -- immutable
let x = 10 in x * 2             -- local (expression)
square x = x * x where y = x    -- where (declaration)

-- Pattern matching
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Guards
abs x | x < 0     = -x
      | otherwise = x

-- Lambda
(\x y -> x + y)
```

### Types

```haskell
x :: Int                        -- type annotation
data Maybe a = Nothing | Just a -- sum type
data Point = Point Float Float  -- product type
data Tree a = Leaf | Node a (Tree a) (Tree a)

-- Record syntax
data Person = Person { name :: String, age :: Int }
name alice  -- access field
alice { age = 31 }  -- update field (returns new record)

-- Type aliases
type String = [Char]
```

### Type Classes

```haskell
-- Definition
class Eq a where
  (==) :: a -> a -> Bool

-- Instance
instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False

-- Constraints
areEqual :: (Eq a) => a -> a -> Bool
```

### Common Type Classes

```haskell
Eq          -- (==), (/=)
Ord         -- compare, (<), (>), (<=), (>=)
Show        -- show :: a -> String
Read        -- read :: String -> a
Num         -- (+), (-), (*), abs
Enum        -- succ, pred, [1..]
Foldable    -- foldr :: (a -> b -> b) -> b -> t a -> b
            -- foldr f z (x:xs) = f x (foldr f z xs)
Functor     -- fmap :: (a -> b) -> f a -> f b
            -- fmap f (x:xs) = f x : fmap f xs
Applicative -- pure :: a -> f a
            -- zip or cartesian product
            -- (<*>) :: f (a -> b) -> f a -> f b 
            -- fs <*> xs = [f x | f <- fs, x <- xs]
Monad       -- (>>=) :: m a -> (a -> m b) -> m b
            -- (x:xs) >>= f = f x ++ (xs >>= f)
```

### Lists

```haskell
[1, 2, 3]           -- literal
1 : 2 : 3 : []      -- cons
[1..10]             -- range
[x^2 | x <- [1..5]] -- comprehension

head [1,2,3]        -- → 1
tail [1,2,3]        -- → [2,3]
[1,2] ++ [3,4]      -- → [1,2,3,4]
take 3 [1..]        -- → [1,2,3]
filter even [1..5]  -- → [2,4]
map (*2) [1,2,3]    -- → [2,4,6]
length [1,2,3]      -- → 3
reverse [1,2,3]     -- → [3,2,1]
foldr (+) 0 [1..5]  -- → 15
elem 3 [1..5]       -- → True
take 5 (repeat 1)     -- → [1,1,1,1,1]
zip [1,2] ['a','b'] -- → [(1,'a'), (2,'b')]
[x | x <- [1..10], even x] -- → [2,4,6,8,10]
```

#### Infinite Lists

```haskell
naturals = [0..]  -- infinite list of natural numbers
take 5 naturals   -- → [0,1,2,3,4]

numFrom n = n : numFrom (n+1)
```

### Maps

```haskell
fromList :: [(k, v)] -> Map k v
insert key value map
map ! key
```

### Function Composition

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x  -- right-associative, low precedence

-- Currying & partial application
add x y = x + y
add5 = add 5
```

### IO Monad

```haskell
main :: IO ()
main = do
  putStrLn "Enter name:"
  name <- getLine
  putStrLn ("Hello, " ++ name)
```

### Control Flow

```haskell
if condition then expr1 else expr2
case x of
  0 -> "zero"
  1 -> "one"
  _ -> "other"
```

## ERLANG

### Variables & Atoms

```erlang
X = 10              % bind (uppercase)
_ = foo()           % ignore result
ok, error, true     % atoms (lowercase)
'Atom With Space'   % quoted atoms
```

### Data Types

```erlang
42, 3.14            % numbers
{ok, 42}            % tuple (fixed size)
[1, 2, 3]           % list (variable length)
#{key => val}       % map (hash)

% Tuple access
element(1, {a, b})  % → a (1-based)
size({a, b, c})     % → 3

% List operations
[H | T] = [1, 2, 3] % H=1, T=[2,3]
[1, 2] ++ [3, 4]    % → [1,2,3,4]
hd([1, 2, 3])       % → 1
tl([1, 2, 3])       % → [2,3]
length([1, 2, 3])   % → 3
lists:reverse([1, 2, 3]) % → [3,2,1]
lists:map(fun(X) -> X*2 end, [1, 2, 3]) % → [2,4,6]

% List comprehension
[X*X || X <- [1,2,3]].              % → [1,4,9]
[X || X <- [1..5], X rem 2 == 0].   % → [2,4]

% Map operations
#{key := Val} = Map % pattern match
Map#{key => val}    % update/add (=>)
Map#{key := val}    % update only (:=)
maps:get(key, Map)  % access
maps:remove(key, Map) % remove
maps:keys(Map)      % get keys
```

### Pattern Matching

```erlang
{A, B} = {10, 20}               % A=10, B=20
{ok, Value} = {ok, 42}          % Value=42
[A, B | Rest] = [1, 2, 3, 4]    % A=1, B=2, Rest=[3,4]
#{name := N} = #{name => "Alice", age => 30}
```

### Functions

```erlang
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

% Guards
abs(X) when X < 0 -> -X;
abs(X) -> X.

% Anonymous functions
Add = fun(X, Y) -> X + Y end.

% Function reference
Fun = fun factorial/1.
```

### Control Flow

```erlang
% If (at least one must be true)
if
  X > 0 -> positive;
  X < 0 -> negative;
  true -> zero
end.

% Case (pattern matching)
case X of
  0 -> zero;
  1 -> one;
  _ -> other
end.
```

### Concurrency

```erlang
% Spawn process
Pid = spawn(fun() -> loop() end).

% Send message (async)
Pid ! {self(), hello}.

% Receive messages
receive
  {From, Msg} ->
    io:format("Got: ~p~n", [Msg]),
    From ! {self(), reply};
  stop ->
    ok
after 5000 ->
    timeout
end.

% Useful functions
self()                  % current PID
register(name, Pid)     % register name
name ! Msg              % send to registered
```

### Supervisor Pattern

```erlang
start_supervisor(Count) ->
  process_flag(trap_exit, true),  % trap exits
  spawn_workers(Count),
  supervisor_loop(Count).

spawn_workers(N) -> 
  spawn_link(fun worker/0).  % linked process

supervisor_loop(Count) ->
  receive
    {'EXIT', Pid, normal} ->
      % normal exit
      supervisor_loop(Count - 1);
    {'EXIT', Pid, Reason} ->
      % crash - restart
      spawn_workers(1),
      supervisor_loop(Count)
  end.
```

### Common Patterns

```erlang
% Parallel spawn
Pids = [spawn(fun() -> work(X) end) || X <- List].

% Collect results in order
Results = [receive {Pid, R} -> R end || Pid <- Pids].
```

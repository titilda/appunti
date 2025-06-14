# JML (Java Modeling Language) Full Guide


---

## 1. Basic Annotations

```java
//@ annotation
```

A single-line JML annotation.

```java
/*@ multi-line annotation @*/
```

Allows multiple lines of annotations.

```java
/*@ strict specification @*/
```

Enables strict mode, which enforces stricter checks during verification.

---

## 2. Method Contracts

JML supports **Design by Contract**. Contracts specify the behavior a method must follow:

| Clause               | Syntax             | Description                                                                |
| -------------------- | ------------------ | -------------------------------------------------------------------------- |
| Precondition         | `requires P;`      | Requires condition `P` to be true before the method is called.             |
| Postcondition        | `ensures Q;`       | Guarantees that condition `Q` will hold after the method returns normally. |
| Exceptional Behavior | `signals (E e) R;` | If exception `E` is thrown, condition `R` must hold.                       |
| Frame Condition      | `assignable L;`    | Limits modifications to locations in `L`.                                  |
| Pure Method          | `//@ pure`         | Marks method as side-effect-free.                                          |
| Divergence           | `diverges P;`      | Indicates method may not terminate if `P` holds.                           |

### Special Keywords

| Keyword       | Meaning                                        |
| ------------- | ---------------------------------------------- |
| `\result`     | Represents the return value of a method.       |
| `\old(expr)`  | Captures the value of `expr` at method entry.  |
| `\nothing`    | Specifies that no memory location is modified. |
| `\everything` | All memory locations may be modified.          |

**Example:**

```java
//@ requires a > 0 && b > 0;
//@ ensures \result == a * b;
//@ assignable \nothing;
public int multiply(int a, int b) {
    return a * b;
}
```

---

## 3. Class Specifications

### Invariants

An invariant is a condition that must always be true in visible states of an object.

```java
//@ invariant balance >= 0;
private double balance;
```

To make it public:

```java
//@ public invariant balance >= 0;
```

For static fields:

```java
//@ static invariant MAX_LIMIT <= 1000;
```

### History Constraints

Ensure that values evolve according to constraints:

```java
//@ constraint balance >= \old(balance);
```

### Initialization Requirement

Invariants checked during constructor:

```java
//@ invariant initial_capacity > 0;
```

---

## 4. State Abstraction

### Abstract Fields

```java
//@ model int size;
```

An abstract field for specifications only.

### Concrete Binding

```java
//@ represents size = data.length;
```

Binds abstract model to actual implementation.

### Ghost Fields

Used for spec/debug purposes, not compiled:

```java
//@ ghost int counter;
//@ set counter = 0;
```

### Model Methods

Pure, spec-only helper:

```java
//@ model boolean isEmpty() { return size == 0; }
```

**Example:**

```java
//@ model String[] items;
//@ represents items = \nonnullelements(data);
```

---

## 5. Special Operators

| Operator                | Description                                 |
| ----------------------- | ------------------------------------------- |
| `\typeof(obj)`          | Gets runtime type of object.                |
| `\type(Class)`          | Refers to class type.                       |
| `\nonnullelements(arr)` | Ensures all elements in array are non-null. |
| `\not_assigned(x)`      | Checks if `x` was not modified.             |
| `\reach(field)`         | All objects reachable from a field.         |
| `\fresh(obj)`           | Indicates object was newly created.         |

---

## 6. Quantifiers

| Quantifier  | Syntax              | Meaning                                            |
| ----------- | ------------------- | -------------------------------------------------- |
| Universal   | `\forall T x; R; P` | For all `x` in range `R`, predicate `P` must hold. |
| Existential | `\exists T x; R; P` | There exists an `x` in `R` such that `P` holds.    |
| Minimum     | `\min T x; R; expr` | Finds the minimum `expr` over range `R`.           |
| Count       | `\num_of T x; R; P` | Counts how many `x` in `R` satisfy `P`.            |

**Examples:**

```java
//@ requires (\forall int i; 0 <= i < arr.length; arr[i] > 0);
//@ ensures (\exists int i; 0 <= i < arr.length; arr[i] == key);
//@ ensures \result == (\sum int i; 0 <= i < n; i);
```

---

## 7. Loop Specifications

Used to prove correctness and termination of loops:

```java
//@ loop_invariant i >= 0 && sum == (\sum int j; 0 <= j < i; arr[j]);
//@ decreases n - i;
//@ assignable sum;
for (int i = 0; i < n; i++) {
    sum += arr[i];
}
```

* `loop_invariant`: Must hold before and after each iteration.
* `decreases`: Ensures termination by showing a decreasing measure.
* `assignable`: Lists writable locations in the loop.

---

## 8. Advanced Constructs

| Feature       | Syntax             | Purpose                              |
| ------------- | ------------------ | ------------------------------------ |
| Assertion     | `//@ assert P;`    | Runtime assertion.                   |
| Assumption    | `//@ assume P;`    | Assume `P` for verification.         |
| Unreachable   | `//@ unreachable;` | Marks code as unreachable.           |
| Spec Public   | `//@ spec_public`  | Exposes private field to spec tools. |
| Helper Method | `//@ helper`       | Skips invariant checks.              |
| Debug         | `//@ debug expr;`  | Prints expression in debug output.   |

---

## 9. Null Safety

```java
//@ nullable String name;
//@ non_null Object ref;

//@ requires input != null;
//@ ensures \result != null;
```

JML lets you enforce or permit null references for safer code.

---

## 10. Common Patterns

### Data Structure Invariant

```java
//@ invariant size >= 0 &&
//@   (\forall int i; 0 <= i < size; elements[i] != null);
```

### Exception Contracts

```java
//@ signals (IOException e) !file.exists();
//@ signals_only IOException, FileNotFoundException;
```

### Frame Conditions

```java
//@ assignable \nothing;
//@ assignable this.value;
//@ assignable array[*];
//@ assignable obj.field, ClassName.*;
```

---

## 11. Tool Directives

Directives used for tool configuration:

```java
//@ refine "MySpec.spec";
//@ henceforth P;
```


---

## 12. Method Refinement

Use `also` to extend specifications in subclasses:

```java
//@ also
//@ requires a >= 0;
//@ ensures \result >= 0;
```

---

## 13. Visibility and Abstraction

Make abstract fields/specs public and bind to concrete fields:

```java
//@ public model int size;
//@ represents size = data.length;
//@ spec_public data;
```

---

## 14. Behavioral Subtyping

To follow behavioral subtyping:

* Subclasses must **weaken preconditions** (require less).
* Subclasses must **strengthen postconditions** (guarantee more).

This ensures subtype objects can substitute supertype objects safely.

---

## 15. Full Class Example

```java
public class BankAccount {
    //@ public invariant balance >= 0;
    //@ spec_public
    private double balance;

    //@ ensures \result == balance;
    //@ pure
    public double getBalance() {
        return balance;
    }

    //@ requires amount > 0;
    //@ assignable balance;
    //@ ensures balance == \old(balance) + amount;
    public void deposit(double amount) {
        balance += amount;
    }

    //@ requires amount > 0 && amount <= balance;
    //@ assignable balance;
    //@ ensures balance == \old(balance) - amount;
    public void withdraw(double amount) {
        balance -= amount;
    }
}
```

---

## Quick Reference Summary

| Concept            | Syntax                             |
| ------------------ | ---------------------------------- |
| Pre-state value    | `\old(expr)`                       |
| Return value       | `\result`                          |
| Object creation    | `\fresh(obj)`                      |
| Loop termination   | `decreases expr;`                  |
| Model field        | `//@ model int x;`                 |
| Ghost field        | `//@ ghost int x;`                 |
| Pure method        | `//@ pure assignable \nothing;`    |
| Non-null array     | `\nonnullelements(arr)`            |
| Visible invariants | `//@ public invariant condition;`  |
| Exception handling | `signals (Exception e) condition;` |

---

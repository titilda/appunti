---
title: "Database 2"
author:
  - "Andrea lunghi"
---

## Transaction

A transaction is an atomic unit of work made by an application.

A transaction starts with a `begin-transaction` and ends with an _end-transaction_ statement like:

- `commit-work`: commit the changes made into the bd, making them permanent;
- `rollback-work`: abort all the changes made by the transaction, discarding them.

### ACID

Transactions supports these properties:

- **Atomicity** (all-or-nothing): a transaction is treated like a _single indivisible operation_. If there is a rollback or a fail all the changes are discarded;
- **Consistency**: A transaction ensures that the database moves from one valid state to another. It maintains the database's integrity by following all defined constraints and rules;
- **Isolation**: concurrent transaction should not interfere with each other;
- **Durability**: once a transaction is committed its changes are permanent and survive any system failure.

## Concurrency

While executing transactions **serially** (one after the other) guarantees correctness, it severely impacts performance and system efficiency. To maximize resource utilization, a DBMS uses **concurrency control** to allow the operations of multiple transactions to be **interleaved** (run in parallel).

An interleaved execution sequence is called a **schedule**.

### Concurrency Issues

If operations are interleaved poorly, it can lead to anomalies (or consistency violations). The primary anomalies are:

#### Lost Update

This anomaly occurs when two transactions read the same value, and a transaction's update is overwritten by another concurrent transaction, effectively losing the first update.

> 1. $r_1(x)$: $T_1$ reads x
> 2. $r_2(x)$: $T_2$ reads x
> 3. $w_1(x)$: $T_1$ writes x += 10
> 4. $w_2(x)$: $T_2$ writes x += 5

#### Dirty Read

This anomaly occurs when a transaction $T_2$ reads a value updated by another transaction $T_1$ that has not yet been committed. If $T_1$ later aborts, $T_2$ will have read a value that was never actually committed to the database.

> 1. $w_1(x)$: $T_1$ writes x (uncommitted)
> 2. $r_2(x)$: $T_2$ reads the uncommitted x
> 3. $\text{abort}_1$: $T_1$ aborts
> 4. $w_2(x)$: $T_2$ writes x (based on dirty read)

#### Non Repeatable Read

This anomaly occurs when a transaction $T_1$ reads the same value multiple times and get a different result because another transaction $T_2$ has modified and committed the value between the reads.

> 1. $r_1(x)$: $T_1$ reads x
> 2. $w_2(x)$: $T_2$ update x and commits
> 3. $r_1(x)$: $T_1$ reads x again and gets a different value

#### Phantom Update

This anomaly occurs when a transaction $T_1$ reads a set of records that satisfy a certain constraint, then another transaction $T_2$ modifies the database in such a way that still satisfies the constraint. If $T_1$ reads the records again, it will violate the constraint.

> Constraint: x + y = 10
>
> 1. $r_1(x)$: $T_1$ reads x
> 2. $w_2(y+5)$ & $w_2(x-5)$: $T_2$ updates the x and y values without violating the constraint
> 3. $r_1(y)$: $T_1$ reads y again and the constraint is violated (the x value is before the update)

#### Phantom Insert

This anomaly occurs when a transaction $T_1$ reads a set of records that satisfy a certain condition, then another transaction $T_2$ inserts new records that satisfy the condition. If $T_1$ reads the records again, it will get a different result.

> 1. $r_1(\sigma_{x>5}(R))$: $T_1$ reads the set of records where x > 5
> 2. $w_2(\text{insert }(x=6))$: $T_2$ inserts a new record with x = 6
> 3. $r_1(\sigma_{x>5}(R))$: $T_1$ reads the set of records again and gets a different result

### Scheduling

The DBMS should be able to to **schedule** the operations of the transactions in a way that preserves the order of operations inside each transaction.

The operations performed by a transaction $T_n$ on a data item $x$ are:

- Read: $r_n(x)$
- Write: $w_n(x)$

The transactions can be scheduled in three ways:

- **Serial**: all the operations of a transaction are executed before the operations of another transaction begin. It guarantees correctness but is inefficient as it doesn't exploit parallelism.
- **Interleaved**: the operations of multiple transactions are mixed together, but the order of operations within each transaction is preserved. This can lead to anomalies if not managed properly.

Assuming we have n transactions $T_1, T_2, \ldots, T_n$ where each transaction $T_i$ has $k_i$ operations, the number of distinct schedules (that respect the sequence of the operations) is:

$$N_D = \dfrac{(\sum_{i=1}^n{k_i})!}{\prod_{i=1}^n{k_i!}}$$

Within this only a fraction is serial:

$$N_S = n!$$

#### Serializable Schedule

From all the possible schedule there are schedules that might encounter an issue due to the concurrency.

We need to identify the **serializable schedules** that are the one that leave the database in the same state as _some_ serial schedule transaction.

> Some assumptions are:
>
> - the transaction doesn't abort.
> - the observation is a-posteriori

#### View-Serializable Schedules (VSR)

Two transactions are **view-equivalents** if they have the same:

1. operations:
2. reads operations (read the same data);
3. final writes operations from the same transactions and the same data.

A schedule is **view-serializable** if it's view-equivalents to a serial schedule and by being equivalent to a serial schedule there are no concurrency issue (within the assumptions).

One way to find a serial schedule would be to enumerate all the possible serial schedule (factorial), making it computational intensive and not functional.

To find a solution in a polynomial time we need to add some restriction to find a computable solution.

#### Conflict Serializable Schedules (CSR)

A conflict occurs if two different transactions perform operations on the same data and at least one of the operations is a write.

- Read-Write conflict (R-W, W-R): This leads to _dirty read_ and _non-repeatable read_ anomalies.
- Write-Write conflict (W-W): This leads to _lost update_ anomalies.

Two schedules are **conflict-equivalent** (CSR) if all the conflicting pairs occurs in the same order. A schedule is **conflict-serializable** if it is conflict-equivalent to a serial schedule.

CSR is a subset of VSR ($CSR \subseteq VSR$).

Testing if a schedule is conflict-serializable is done by checking if a _conflict-graph_ is acyclic.

1. Given a schedule group the operation by the resource used.
2. Than create an arch between the transactions if there is a conflict

Topologically sorting the graph allows to find the equivalent serial schedule of the graph.

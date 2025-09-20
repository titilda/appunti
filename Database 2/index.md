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

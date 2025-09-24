---
title: "Numerical linear algebra Summary"
author: 
- "Andrea Oggioni"
---

# Introduction

This course is the continuation of [Foncamenti di Calcolo Numerico](/Fondamenti%20di%20Calcolo%20Numerico/index.html). The needed concept from that subject will be revised when needed.

In this file there will be first an overview of the notation and the basics prerequisites on matrices and then the main topics.

The main objective on the **Numerical Linear Algebra** is to solve linear systems in the form of $Ax = b$ in the fastest and most precise way possible. This is because the calculation of $x = A^{-1}b$ is really slow and may introduce a lot of error, expecially during the calculation of $A^{-1}$.

To better understand this summary, it may be helpsul to read [Foncamenti di Calcolo Numerico](/Fondamenti%20di%20Calcolo%20Numerico/index.html), [Logica e Algebra](/Logica%20e%20Algebra/index.html) and [Geometria e Algebra Lineare](/Geometria%20e%20Algebra%20Lineare/index.html).

## Notation and matrix overview

In this course, everything belongs to the real space ($\mathbb{R}$, $\mathbb{R}^n$) unless otherwise specified.

Apart from the conventional vector names (generic vector $x$, null vector $0$) in this course the notation $e_i$ will be used to denote canonical vectors.

The inner product between two vectors $x, y \in \mathbb{R}^n$ is defined as $x^Ty = \sum\limits_{i=1, \dots, n} x_i y_i$ and it's commutative.

Two vectors $x, y \in \mathbb{R}^n$ are considered orthogonal if $x^Ty = y^Tx = 0$.

A matrix $A \in A \in \mathbb{R}^{n \times n}$ is idempotent if $A^2 = A$.

A matrix $A \in A \in \mathbb{R}^{n \times n}$ is nilpotent if $\exists k \in \mathbb{N} : A^k = 0$.

If $\exists x \in \mathbb{R}^n : x \ne 0, Ax = 0$ then $A$ is not invertible.

A matrix $A \in A \in \mathbb{R}^{n \times n}$ is orthogonal if $A^T = A^-1$.

A matrix $U$ is an **unitary** upper triangular matrix if it is upper triangular and has only ones on the diagonal. The definition is analog for the unitary lower triangular matrix.

## Basic matrix decompositions

There are three main ways to decompose a matrix into two other matrices that, when multiplied, give back the original matrix:

- **LU factorization**: $PA = LU$ where
  - $P$ is a permutation matrix;
  - $L$ is a unit lower triangular matrix;
  - $U$ is an upper triangulat matrix.
- **Cholesky decomposition**: $A = L^TL$ where
  - $L$ is a lower triangular matrix;
  - works only for SPD matrices.
- **QR decomposition**: $A = QR$ where
  - $Q$ is an orthogonal matrix;
  - $R$ is an upper triangular matrix;
  - works only for nonsingular matrices.

All these methotds are computationally intensive as the factorisation of a matrix is an $O(n^3)$ operation.

## Sparse matrices

Sparse matrices are matrices that contains a big number of null entries. Sparse matrices are very common so they can be exploited to save on memory when storing big matrices (the null elements aren't stored).

There are multiple encoding types one can choose from:

| Encoding | Fast insertion | Easy $Ax$ |
| -------- | -------------- | --------- |
| COO      | Y              | N         |
| CSR      | N              | Y         |
| CSC      | N              | Y         |
| ELLPACK  | N              | Y         |

Each encoding will be explained in the future.

Using sparse matrices instead of "normal" ones is also useful because only the useful calculations are performed (i.e. all the products by zero are ignored) saving on time.

# Iterative methods

Solving $Ax = B$ inverting the $A$ matrix is not possible due to the big time complexity of the matrix inversion. There are two main ways to solve the system without inverting $A$: **direct methods** and **iterative methods**.

Direct methods are precise but are also $O(n^3)$ so they need a lot of time. Moreover they suffer from the fill-in phenomenon so that the eventual sparsity of $A$ is destroyed.

Direct methods consists in using the decomposed matrices obtained using one of the matrix decompositions seen before to solve multiple "simpler" systems to get to the solution.

Iterative methids consists in the creation of a vector series $\{x^{(k)}\}_{k \ge 1}$ that gradually converges to the solution.

Given $x^{(k)}$, $x^{(k+1)}$ is computed as $x^{(k+1)} = Bx^{(k)} + f$. $B$ and $f$ are derived from the $A$ matrix.

::: {.callout .callout-property title="Iterative methods"}
An **Iterative method** must satisfy two properties: **Consistency** and **Convergence**.

An iterative method is consistent if $x = Bx + f$. This means that once the solution is found, each iteration will always return the same solution.

From this property it is possible to deduce a relation between $B$ and $f$: $f = (I - B)x = (I - B)A^{-1}b$.

An iterative method is convegent if the error (the difference between the caomputed solution at a given step and the real solution) goes asimptotically at zero. 

Let $e^{(k+1)} = x - x^{(k+1)}$ be the error at the $(k+1)^{th}$ step then $\|e^{(k+1)}\| \le \|B\| \|e^{(k)}\|$.

From the formula above, the following can be derived $\|e^{(k+1)}\| \le \|B\|^{(k+1)}\|e^{(0)}\|$. This means that a sufficient condition for convergence is that $\|B\| \lt 1$.
:::

The spectral radius $\rho(B)$ is defined as $\max\limits_j |\lambda_j(B)|$ (or if $B$ is SPD then $\rho(B) = \|B\|_2$).

::: {.callout .callout-theorem title="Necessary and succifient condition for convergence"}
A consistent iterative method converges if and only if $\rho(B) \lt 1$.
:::

We know that $\rho(B) \ge \|B\|$ so $\exists \|\cdot\| : \|B\| \lt 1 \implies \rho(B) \lt 1$ and the theorem is applicable.

An iterative method can always be implemented using the followinfg pseudocode struture

```
while (stopping criteria is not met)
  x(k+1) = B * x(k) + f
end
```



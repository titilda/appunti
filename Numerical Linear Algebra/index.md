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

Consider the following decomposition of the matrix $A$:

- $D$ is the matrix containing only the elements in the diagonal of A;
- $-E$ is the lower triangular part of $A$ with all zeros on the diagonal;
- $-F$ is the upper triangular part of $A$ with all zeros on the diagonal.

The two most used iterative methods are the Jacobi methond and the Gauss-Seidel method.

The iteration matrix of the Jacobi method can be expressed as $B_J = I - D^{-1}A$.

The iteration matrix of the Gauss-Seidel method can be expressed as $B_{GS} = (D - E)^{-1} F$.

Both methods are consistent.

::: {.callout .callout-theorem title="Sufficient condition for convergence"}
If $A$ is strictly diagonally dominant by rows then both methods converge.

$A$ is strictly diagonally dominant by rows if

$$
|a_{ii}| \lt \sum_{j \ne i} |a_{ij}| \qquad i = 1, \dots, n
$$
:::

The following theorem resumes three contitions that are sifficient to prove convergence in various cases.

::: {.callout .callout-theorem title="Sufficient conditions for convergence"}
- If $A$ is strictly diagonally dominant by columns, then both methods are convergent;
- If $A$ is SPD then Gauss-Seidel is convergent;
- If $A$ is tridiagonal then $\rho(B_J)^2 = \rho(B_{GS})$ (i.e. Gauss-Seidel converges two times faster than Jacobi).
:::

## Stopping criteria

An iterative method never ends. It only converges to the solution so there needs to be a way to determine when to stop the computation.

There are various possible stopping criteria: the molst used two are the "residual based" and the "increment based".

The **residual based** method consists in stopping the computation when the residual is small enough:

$$
\begin{align}
  \frac{\|x - x^{(k)}\|}{\|x\|} \le K(A) \frac{\|r^{(k)}\|}{\|b\|} &\implies \frac{\|r^{(k)}\|}{\|b\|} & \\
  \frac{\|x - x^{(k)}\|}{\|x\|} \le K(P^{-1}A) \frac{\|z^{(k)}\|}{\|b\|} &\implies \frac{\|z^{(k)}\|}{\|b\|} &\qquad z^{(k)} = P^{-1}r^{(k)} \\
\end{align}
$$

This method is suitable for small condition numbers.

The **increment based** method consists in stopping the computation when the difference between two consecutive steps is small enough:

$$
\|x^{(k+1)} - x^{(k)}\| \le \varepsilon
$$

This method is suitable for small $\rho(B)$.

## Stationary Richardson Method

The **Stationary Richardson Method** is based on the idea that, given the current solution, you can use the residual to understand in which direction to move next.

The update rule is as follows:

$$
x^{(k+1)} = x^{(k)} + \alpha(b - Ax^{(k)})
$$

The iteration matrix for this method is $B_\alpha = I - \alpha A$.

The parameter $\alpha$ specifies how long is each step. This parameter does not change during the execution of the algorithm (hence the **stationary** part of the name).

::: {.callout .callout-theorem title="Convergence and optimal $\alpha$ for the Stationary Richardson method"}
Let $A$ be SPD. The stationary Richardson method converges if and only if

$$
0 \lt \alpha \lt \frac{2}{\lambda_{max}(A)}
$$

The value for $\alpha$ that guarantees the fastest convergence is

$$
\alpha_{opt} = \frac{2}{\lambda_{min}(A) + \lambda_{max}(A)}
$$

In such case, the specral radius of the iteration matrix is

$$
\rho_{opt}(B) = \frac{K(A) - 1}{K(A) + 1}
$$
:::

## Preconditioned Richardson Method

It is known that badly conditioned matrices will make the specral radius $\rho$ close to $1$, slowing down the convergence. Starting from the base $Ax = b$ it is possible to compute the solution of an equivalent problem with a much lower condition number.

Let $P^{-1}$ be a SPD matrix such that $K(P^{-1}A) \lt\lt K(A)$. Solving $Ax = b$ is equivalent to solving $P^{-\frac{1}{2}}AP^{-\frac{1}{2}}z = P^{-\frac{1}{2}}$ where $z = P^{\frac{1}{2}}z$.

The update rule for the preconditioned version of the method is $x^{(k+1)} = x^{(k)} + \alpha P^{-1}r^{(k)}$. This means that the iteration matrix can be computed as $B = I - \alpha P^{-1}A$.

::: {.callout .callout-note title="Convergence and Optimal values for the Preconditioned Richardson Methods"}
The condition for convergence and the optimal values are computed in the same way for the non-preconditioned version of the method but replacing $A$ with $P^{-1}A$.

The method converges if 
$$
0 \lt \alpha \lt \frac{2}{\lambda_{max}(P^{-1}A)}
$$

The optimal values are

$$
\begin{align*}
  \alpha_{opt} &= \frac{2}{\lambda_{min}(P^{-1}A) + \lambda_{max}(P^{-1}A)} \\
  \rho_{opt} &= \frac{K(P^{-1}A) - 1}{K(P^{-1}A) + 1}
\end{align*}
$$
:::

## The Gradient Method

Let $\varPhi(y) = \frac{1}{2}y^TAy - y^Tb$. Mathematially, the minimization of $\varPhi(y)$ is equivalent to computing the solution of $Ax = b$ (this is because $\nabla\varPhi(y) = Ay = b$).

As there is only one solution to $Ax = b$, then there will be only one minimum (and no maximum) and that point will coincide to the point where $\nabla\varPhi(x^{(k)}) = 0$ so the residual can just be expressed as $r^{(k)} = -\nabla\varPhi(x^{(k)})$ (the error is still $e^{(k)} = x - x^{(k)}$ and it is still unknown).

In terms of implementation, the **Gradient Method** works exactly as the Richardson method (using the gradient in place of the residual).

The new formula for the optimal $\alpha$ is

$$
\alpha_{opt} = \frac{\left(r^{(k)}\right)^Tr^{(k)}}{\left(r^{(k)}\right)^TAr^{(k)}}
$$

The error is bounded:

$$
\|e^{(k)}\|_A \le \left( \frac{K(A) - 1}{K(A) + 1} \right)^k \|e^{(0)}\|_A
$$

## The Conjugate Gradient method

It can be shown that, with the gradient method, two consecutive update directions are orthogonal. The conjugate gradient method works by choosing an update direction that is orthogonal not only to the previus one bu to all the previous ones.

::: {.callout .callout-theorem title="Conjugate Gradient convergence"}
In exact arithmetic the method converges in exactly $n$ iterations.

The error is bounded:

$$
\|e^{(k)}\|_A \le \frac{2c^k}{1+c^{2k}} \|e^{(0)}\|_A \qquad c = \frac{\sqrt{K(A)} - 1}{\sqrt{K(A)} + 1}
$$
:::

## Preconditioned gradient methods

In the same way as the othe preconditioned methods, convergence speed can be improved also with the gradient method.

Error is bounded as in the other preconditioned methods.

## Krylov-space methods

::: {.callout .callout-definition title="Krylov space"}
Given a nonsingular $A \in \mathbb{R}^{n \times n}$ and $y \in \mathbb{R}^n, y \ne 0$, the $k$-th Krylov space generated by $A$ from $y$ is

$$
\mathscr{K}_k(A, y) = \operatorname{span}(y, Ay, \dots, A^{k-1}y)
$$

By definition, it is true that

$$
\mathscr{K}_1(A, y) \sube \mathscr{K}_2(A, y) \sube \dots
$$
:::

Since $r^{(k+1)} = r^{(k)} - Ar^{(k)}$ then $r^{(k)} = p_{k-1}(A)r^{(0)}$ where $p_r(z) = (1 - z)^r$. This means that $x^{(k)} = x^{(0)} + p_{k-1}(A)r^{(0)}$.

::: {.callout .callout-definition title="Grade of $y$ with respect to $A$"}
The **grade of $y$ with respect to $A$** is defined as a positive integer $\nu = \nu(y, A)$ such that

$$
\dim(\mathscr{K}_s(A, y)) = \min(s, \nu)
$$

$\nu$ always exists.

$\mathscr{K}_\nu(A, y)$ is the smallest $A$-invariant subspace that contains $y$.

$$
\nu(y, A) = \min\{s | A^{-1}y \in \mathscr{K}_s(A, y)\}
$$
:::

::: {.callout .callout-theorem title="Lemma"}
Let $x$ be the solution of $Ax = b$ and let $x^{(0)}$ be any initial approximation of it. $r^{(0)} = b - Ax^{(0)}$ is the initial residual. Let $\nu = \nu(r^{(0)})$, then

$$
x \in x^{(0)} + \mathscr{K}_\nu(A, r^{(0)})
$$
:::

The rationale behind a Krylov-space method is that for each solution $x^{(x)}$ in the sequence of the approximate solutions, it is true that $x^{(k)} \in x^{(0)} + \mathscr{K}_k(A, r^{(0)})$ (from which follows that $r^{(k)} \in \mathscr{K}_{k+1}(A, r^{(0)})$).

If the method makes sure that all the residuals are linearly independent, then, in exact arithmetic, the method converges.

A Krylov-space method used to find the solution of a linear system is called a **Krylov-space solver**.

::: {.callout .callout-definition title="Krylov-space solver"}
A **Krylov-space solver** is an iterative method that, starting from an initial guess $x^{(0)}$ and the corresponding residual $r^{(0)}$, computes a sequence of $x^{(0)}$ such that

$$
x^{(k)} = x^{(0)} + p_{k-1}(A)r^{(0)}
$$
:::

Krylov-space solvers are usually slow (or do not converge at all) so preconditioning is almost always a must.

The conjugate gradient method qualifies as a Krylov-space solver.

Krylov-space solvers can be used when $A$ is not symmetrical. The two main methods used in this case are the **BiCG** and the **GMRES**.

### BiConjugate Gradient method

The BiConjucate Gradient method exploits the **shadow residuals** and the **shadow directions** obtained by solving the sistem obtained by transposing both sides:

$$
Ax = b \mapsto [Ax]^T = b^T \iff x^Ta^T = b^T \iff \hat x \hat A = \hat b
$$

This method is prone to explosive divergence so a variation (the _stabilized_ one) is usually used instead.

### GMRES

If $A_S = \frac{A + A^T}{2}$ is SPD, then

$$
\|r^{(k)}\|_2 \le \left[ 1 - \frac{\lambda^2_{min}(A_S)}{\lambda_{max}(A^TA)} \right]^{\frac{h}{2}} \|r^{(0)}\|
$$

If $A$ is SPD, then

$$
\|r^{(0)}\|_2 \le \left[ \frac{K_2(A)^2 - 1}{K_2(A)^2} \right]\|r^{(0)}\|_2
$$

## Preconditioning techniques

_Coming soon_

<!-- TODO: 06/10/25 -->

# Eigenvalue problems

An **eignevalue problem** is usually a problem where the objective is to find the eigenvalues (or a subset of them) of a matrix. Problems like these have numerous practical applications, of which none will be explained in this document.

The key point to keep in mind to understand everything in this chapter is that each eigenvalue measures "how much a vector is streched" in the direction pointed by the corresponding eigenvector when applying the matrix to the vector.

Other important notions are constituted by the formula for the **Rayleigh quotient** by the notion of **similar matrices**.

::: {.callout .callout-note title="Rayleight quotient"}
$$
\lambda_i = \frac{v_i^HAv_i}{v_i^Hv_i}
$$
:::

::: {.callout .callout-definition title="Similar matrices"}
Two matrices are said to be **similar** if they have the exact same eigenvalues with the exact same multiplicities.

An equivalent definition is that two matrices $A$ and $B$ are similar if $\exists T : B ? T^{-1} A T$.

A matrix is diagonalisable if it's similar to a diagonal matrix.
:::

## Power method

The **power method** is an iterative method used to extract the bigger (in modulo) eigenvalue (with the corresponding eigenvector) from a matrix. The method converges if the eigenvalue (in modulo) is **isolated**. The more the eigenvalue is isolated, the more this method converges faster.

Each iteration of the power method can be expressed as

$$
y^{(k+1)} \gets Ax^{(k)} \\
x^{(k+1)} \gets \frac{y^{(k+1)}}{\|y^{(k+1)}\|} \\
\nu^{(k+1)} \gets [x^{(k+1)}]^Hax^{(k+1)}
$$

The initial guess $x^{(0)}$ can be any vector with unitary norm.

The idea behind this method comes from the "stretching" interpretation above: with a great number of iterations, all but the bigger eigenvalues become negligible in comparison with the bigger. The normalization applied every iteration prevents the numbers from exploding.

If $A$ is diagonalisable with $v_1, v_2, \dots, v_n$ as eigenvectors, then the eigenvectors form a base for $\mathbb{C}^n$. Let $x^{(0)} : \|x^{(0)}\| = 1$ be the initial guess, then $x^{(0)} = \sum_i a_i v_i$.

Assuming that $|\lambda_1| \gt |\lambda_2| \ge \dots \ge |\lambda_n|$ (note that the first $\gt$ is not a $\ge$) If we multiply the initial guess by $A$ then

$$
y = Ax^{(0)} = \sum_i \alpha_i A v_i = \sum_i \alpha_i \lambda_i v_i = \alpha_1 \lambda_1 \left( v_1 + \sum_{i=2}^n \frac{\alpha_i}{\alpha_1} \frac{\lambda_i}{\lambda_1} v_i \right)
$$

Performing multiple left multiplications by $A$, the result become

$$
Ax^{(k)} = \alpha_1 \lambda_1^k \left( v_1 + \sum_{i=2}^n \frac{\alpha_i}{\alpha_1} \left( \frac{\lambda_i}{\lambda_1} \right)^k v_i \right) \underset{k \to \infty}{\longrightarrow} \alpha_1 \lambda_1 v_1
$$

hence, the dominant eigenvalue will dominate over all the other.

If $\lambda_1$ is not isolated, all sorts of things may happen and nothig is guaranteed.

## Deflation methods

Deflation methods are used to find eigenvalues that are not the dominant one. Those methods consists in finding (with the power method or any equivalent one) the dominant eigevalue until the desired one is found.

Let $(\lambda_1, v_1)$ be an eigenpair for $A$ and $Sv_1 = \alpha e_1$, then it is true that

$$
SAS^{-1} = \begin{pmatrix}
  \lambda_1 & b^T \\
  0 & B
\end{pmatrix}
$$

It holds that $B$ is a matrix with the same eigenvalues of $A$ except for $\lambda_1$. The power method is then applied to $B$ to find $(\lambda_2, z_2)$

The eigenvector $z_2$ needs to be converted back to an eigenvector of $A$:

$$
v_2 = S^{-1}\begin{pmatrix}\alpha \\ z_2\end{pmatrix} \qquad \alpha = \frac{b^H z_2}{\lambda_1 - \lambda_2}
$$

The same process is then repeated until all the eigenpairs of interests are found.

## Inverse power method

The inverse power method is used to compute the smallest (in modulo) eigenvalue (with the corresponding eigenvector).

The idea behind this method is that we can apply the power method to $A^{-1}$ to find $\lambda_n^{-1}$ except that if inverting a matrix were easy, the whole point of "Numerical Linear Algebra" would collapse on itself, letting infinitely powerful processors exist <!-- and leading to the extinction of the human species and to other fun stuff -->.

Starting from an unitary norm initial guess $q^{(0)}$ then, the update rule is

$$
\operatorname{solve}(Az^{k+1} = q^{(k)}) \\
q^{(k+1)} \gets \frac{z^{(k+1)}}{\|z^{(k+1)}\|} \\
\sigma^{(k+1)} \gets [q^{(k+1)}]^H A q^{(k+1)}
$$

::: {.callout .callout-note title="Note"}
The inverse power method is exactly the same as its non inverse counterpart except that instead of computing $z^{(k+1)} = A^{-1}q^{(k)}$, an equivalent system $Az^{(k+1)} = q^{(k)}$ is solved.
:::

## Inverse power method with shift

The inverse power method with shift can find the eigenvalue closest to a given parameter.

Let $\mu \ne \sigma(A)$ be the value for which we want to find the closest eigenvalue and $M_\mu = A - \mu\mathbb{I}$. Let $(\xi_i, w_i)$ be an eigenpair for $M_\mu$, this means that

$$
M_\mu w_i = \xi_i w_i \iff (A - \mu\mathbb{I})w_i = \xi_i w_i \iff A w_i = (\mu + \xi_i) w_i
$$

from which we can deduce that $(\mu + \xi_i, w_i)$ is an eigenpair for $A$.

Using the inverse power method we can then compute the smallest eigenvalue for $M_\mu$. To get the corresponding eigenvalue for the original matrix, $\mu$ should just be added back to the result.

It is possible to use the shift with the non inverted power method to find the eigenvalue that is the farthest from the provided $\mu$.

## QR factorization

QR factorization is used when _all_ the eigenpairs are needed.

_To be continued._

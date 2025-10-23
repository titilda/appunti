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

To have hands-on practice on the topics of this subject, it it suggested to play around with MATLAB/Octave, Eigen and LIS. A rudimental LIS cheatsheet is avaliable [here](./lis.html).

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

QR factorization is used when _all_ the eigenpairs are needed. The _full_ QR factorization consists in finding two matrices $Q \in \mathbb{R}^{m \times m}$ and $R \in \mathbb{R}^{m \times n}$ such that $Q$ is orthogonal and $R$ is upper trapeziodal.

Once the two matrices are found, one can chop them to obtain $\hat Q \in \mathbb{R}^{m \times n}$ (obtained chopping the rightmost columns) and $\hat R \in \mathbb{R}^{n \times n}$ (obtained by chopping the lowest rows): this is called **reduced QR factorization**.

It holds that

$$
A = QR = \hat Q \hat R
$$

The QR algorithm is based on the **Gram-Schmidt orthogonalization**: let $A = [a_1 | a_2 | \dots | a_n]$, then

- $$
  w_j = a_j - \sum\limits_{k=1}^{j-1} \left(\overline q_k^Ta_j\right)q_k \quad
  q_j = \frac{w_j}{\|w_j\|} \qquad \forall j
  $$
- $$
  r_{ij} = \begin{cases}
    \overline q_i^T a_j & i \ne j \\
    \left\| a_j - \sum_{i=1}^{j-1}r_{ij} qi \right\| & i = j
  \end{cases}
  $$

The existance and uniqueness of the QR factorization are guaranteed by the Gram-Schmidt algorithm.

This algorithm become unstable very quickly due to rounding errors: to stop it from exploding, the $r$ update rule is modified as follows.

$$
r_{ij} = \begin{cases}
  \overline q_i^T w_j & i \ne j \\
  \|w_j\| & i = j
\end{cases}
$$

To use the **QR algorithm** to actually compute all the eigenvalues, the Schur decomposition must be introduced.

Let $A \in \mathbb{C}^{n \times n}$, then there exists a unitary matrix $U \in \mathbb{C}^{n \times n}$ such that $U^HAU = T$ where $T$ is upper triangulas and contains the eigenvalues of $A$ in its diagonal. This decomposition is called **Schur Decomposition**. The vectors $u_i$ that composes the $U$ matrix are called **Schur vectors**.

From $U^HAU = T$ it follows that

$$
Au_k = \lambda_k u_k + \sum_{i=1}^{k-1} t_{ik} u_i \iff Au_k \in \operatorname{span}(u_1, u_2, \dots, u_k)
$$

This important property means that

- $u_1$ is an eigenvector of $A$;
- the first $k$ Schur vectors form an invariant subspace for $A$;
- the Schur decomposition is not unique.

Now that we know what the Schur decomposition is, we can now intriduce the basic **QR Algorithm**: an iterative method to find the Schur decomposition for a complex square matrix $A$.

The update rule for the basic QR algorithm is as follows:

$$
Q^{(k)}, R^{(k)} \gets QR(A^{(k-1)}) \\
A^{(k)} \gets R^{(k)} Q^{(k)} \\
U^{(k)} = U^{(k-1)}Q^{(k)}
$$

After the _stopping criteria_ decides that it is time to stop, the algorithm returns $T = A^{(k)}$ and $U = U^{(k)}$ such that $A = UTU^H$.

From the algorithm is follows that $A^(k)$ and $A^{(k-1)}$ are similar. **This relation is transitive**: from the algorithm we know that $A^{(k-1)} = Q^{(k)} R^{(k)}$ but $Q$ is orthogonal so $R^{(k)} = [Q^{(k)}]^H A^{(k)}$ but we also know that $A^{(k)} = R^{(k)} Q^{(k)}$ so

$$
\begin{align*}
  A^{(k)} &= [Q^{(k)}]^H A^{(k-1)} Q^{(k)} \\
  &= [Q^{(k)}]^H [Q^{(k-1)}]^H A^{(k-2)} Q^{(k-1)} Q^{(k)} \\
  &= \dots \\
  &= [Q^{(k)}]^H \dots [Q^{(1)}]^H A^{(0)} Q^{(1)} \dots Q^{(k)}
\end{align*}
$$

Assuming that all the eigenvalues in modulo are isolated, then $A$ converges to an upper triangular matrix. The convergence speed increases with the "isolatedness" of the eigenvalues in modulo.

The QR algorithm requires $O(n^3)$ operations per iteration. This number can decrease reducing $A$ to a similar Hessenberg matrix ($O(n^2)$) or by using shifts and deflations.

# Overdetermined linear systems

An **overdetermined system** can be expressed as $Ax = b$ where $A \in \mathbb{R}^{m \times n}$, $x \in \mathbb{R}^n$, $b \in \mathbb{R}^m$ and $m \gt \gt n$.

In the case of overdetermined systems, in general a solution in the _conventional_ sense does not exists so we need to extend the concept of _solution_ to the _least square sense_.

::: {.callout .callout-definition title="Solution in the leas square sense"}
A **Solution in the least square sense** for an overdetermined system is a vector $x^* \in \mathbb{R}^n$ such that $\Phi(x^*) = \min_{y \in \mathbb{R}^n} \Phi(y)$ where $\Phi(y) = \|Ay - b\|_2^2$.

In other words, the solution in the least square sense for an overdetermined system is the one which minimized the error.This is a generalization of the previous concept of _solution_: for a _not-underdeterminate-nor-overdeterminate_ system, this definition still works and corresponds to the exact solution.
:::

An exact solution for a rectangular system $Ax = b$ can be found only if $b \in \operatorname{Range}(A)$. In general, the solution in the least square sense for a rectangular system can be found by imposing $\nabla\Phi(y) = 0$ (since there may be multiple solutions that satisfy this condition, we want the one that also minimized its norm).

The rectangular system can be "squared" to get an equivalent system with the same solution:

$$
\Phi(y) = \|Ay - b\|_2^2 = (Ay - b)^T(Ay - b) = y^T A^T A y - 2y^T Ab + b^T b \\
\nabla \Phi(y) = 2A^T A y - 2A^T b \overset{!}{=} 0
$$

From this, it follows that the solution in the least square sense for the initial system is also the exact solution for $A^T Ax = A^T b$ (system of normal equations) except that $K_2(A^T A) = [K_2(A)]^2$ so this is practically useless.

We already briefly discussed about the reduced QR factorization: said factorization can also be used instead of the system of normal equations to approximate the solution in the least square sense.

::: {.callout .callout-theorem title="Theorem"}
Let $A \in \mathbb{R}^{m \times n}$ with $m \gt n$ and full-rank. Then the unique solution in the least square sense $x^*$ of $Ax^* = b$ is given by $$x^* = \hat R^{-1} \hat Q^T b$ (tat's easy to solve ar $\hat R$ is upper triangular) where $\hat Q$ and $\hat R$ comes from the reduced QR factorization of $A$. It is also true that $\Phi(x^*) = \sum\limits_{i = n+1}^{m}\left[(Q^Tb)_i\right]^2$.

Proof follows.

Assume $A$ is full rank. Since $A$ is full rank, there exists its QR decomposition. Since $Q$ is an orthogonal matrix, it preserves the scalar product (i.e. $\|Qz\|_2^2 = \|Q^Tz\|_2^2 = \|z\|_2^2$).

This means that

$$
\|Ax = b\|_2^2 = \|Q^t(Ax - b)\|_2^2 = \|Q^T(QRx - b)\|_2^2 = \|Rx - Q^Tb\|_2^2
$$

Since $R$ is upper trapezoidal

$$
\|Ax - b\|_2^2 = \|Rx - Q^Tb\|_2^2 = \|\hat Rx  -\hat Q^Tb\|_2^2 + \sum_{i=n+1}^m \left[ (Q^Tb)_i \right]^2
$$
:::

## Singular Value Decomposition (SVD)

If $A$ is full rank then the problem is **well posed** and the solution can be computed with the help of QR factorization. If $A$ is not full rank then if $x^*$ is a solution, $x^* + z$ (where $z \in \ker(A)$) is also a solution.

SVD is the Swiss Knife of matrix decomposition and it is basically a miracle backed by a theorem proof. The main problem is that SVD is really expensive. SVD is based on the following two theorems.

::: {.callout .callout-theorem title="Singular value decomposition"}
Let $A \in \mathbb{R}^{m \times n}$ and suppose we know how to factorize $A = U \Sigma V^T$ then $x^* = A^\dagger b$ where $A^\dagger \overset{\Delta}{=} V \Sigma^\dagger U^T$ (called **pseudoinverse** of $A$) with $\Sigma^\dagger$ that follows from $\Sigma$.
:::

::: {.callout .callout-theorem title="Singular value decomposition"}
Let $A \in \mathbb{R}^{m \times n}$ then there exists two orthogonal matrices $U$ ($m \times m$) and $V$ ($n \times n$) such that $\Sigma = U^T A V$ is diagonal with elements $\sigma_1, \sigma_2, \dots, \sigma_p, 0, \dots, 0$ (with $p = \min(m, n)$ and $\sigma_1 \ge \sigma_2 \ge \dots \ge \sigma_p$). $\sigma_i$ are called **singular values** of $A$.
:::

From the previous theorem, we know that $A = U \Sigma V^T \implies U^T A U = \Sigma \implies UU^T A VV^T = U \Sigma V^T$ and that $A^T = V \Sigma^T U^T$. From this we can deduce the first important property of the SVD: $\sigma_i = \sqrt{\lambda_i(A^T A)}$. This is because

$$
A^T A = V \Sigma U^T U \Sigma^T V^T = V (\Sigma^T \Sigma) V^T \implies \lambda_i(A^T A) = \lambda_i(V \Sigma^T \Sigma V^T) = \lambda_i(\Sigma^T \Sigma) = \left( \sigma_i(A) \right)^2
$$

::: {.callout .callout-note title="Complex SVD"} 
The SVD works even with complex matrices using the Hermitian Transpose instead of the normal transpose operator.
:::

::: {.callout .callout-definition title="Pseudoinverse"}
Let $A \in \mathbb{R}^{m \times n}$ with $U^T A V = \Sigma$ then the **pseudoinverse** of $A$ is $A^\dagger = V \Sigma^\dagger U^T$ where $\Sigma^\dagger = \operatorname{diag}\left( \frac{1}{\sigma_1}, \frac{1}{\sigma_2}, \dots, \frac{1}{\sigma_p}, 0, \dots, 0 \right)$

Note that if $A$ is invertible then $A^{-1} = A^\dagger$.
:::

# Multigrid Methods

Let $A$ be a square matrix coming from the discretization of a partial differential equations. The size of the matrix comes directly from the number of tiles in which we split the system during the discretization.

In this case, **multigrid method**s can be applied to get an approximation of the solution in the discrete points efficiently.

Multigrid methods are scalable: the number of iterations required to solve the system _does not_ depend on the size of the system.

We denote as $A_hx_h = b_h$ the system to solve at each iteration, where $h$ is the size of the discretization interval.

For said system, we use $h$ as a subscript also for the exact solution, the error and the residue: $e_h^{(k)} = x_h - x_h^{(k)}$, $r_h^{(k)} = b_h - A_hx_h^{(k)}$, $A_he_h^{(k)} = r_h^{(k)}$.

As thhe matrix $A_h$ comes from the discretization of a partial differential equation, we know that the error can be represented as a sum of sinusoidal components of different frequencies: iterative methods are usually really good at reducing high frequency components but are also really bad at reducong the low frequency ones. This means that after the first iterations, the decay speed of the error will start to decrease noticeably.

Multigrids methods try to solve this problem by exploiting the fact that, changing the _sampling rate_ (read as "changing the $h$ subscript"), lower frequencies become high.

A single multigrid iteration (denoted by $x_h^{(k+1)} = MG(x_h^{(k)}, b_h, \nu_1, \nu_2)$) is generally composed by 8 steps:

1. Perform $\nu_1$ iteration of whathever iterative method is available on $A_h x_h = b_h$ using $x^{(k)}$ as the initial guess (**pre-smoothing step**); the obtained solution is put into $y_h^{(\nu_1)}$.
2. Compute $r_h^{(\nu_1)} = b_h - A_h x_h^{(\nu_1)}$ (**fine grid residual**).
3. Compute $r_{2h}^{(\nu_1)} = I_{h}^{2h}r_h^{(\nu_1)}$ (**restriction** of the fine grid resitual to a coarser grid one).
4. Solve $A_{2h}e_{2h} = r_{2h}^{(\nu_1)}$ (direct solvers could also be used here as this is realistically a small system).
5. Compute $e_h = I_{2h}^h e_h^{(\nu_1)}$ (**interpolation** of the coarse grid residual to a finer grid one).
6. Compute $y_h^{(\nu_1 + 1)} = y_h^{(\nu_1)} + e_h$.
7. Perform $\nu_2$ iterations of whatever iterative method is available on $A_h x_h = b_h$ using $y^{(\nu_1 + 1)}$ as the initial guess (**post-smoothing step**). The obtained solution is put into $y^{(\nu_1 + 1 + \nu_2)}$.
8. Return $x_h^{(k+1)} \gets y_h^{(\nu_1 + 1 + \nu_2)}$

Usually, no more than three pre/post-smoothing steps are necessary.

Three things must be explained in order to understand the eight steps above: how to compute $A_{2h}$ and what are $I_{h}^{2h}$ and $I_{2h}^{h}$.

The operator $I_h^{2h}$ is called **restriction operator** and operates on $I_{h}^{2h} : \mathscr{T}_h \to \mathscr{T}_{2h}$ where $\mathscr{T}_h$ denotes a grid with $h$-wide discretization intervals. This operator maps a vector of elements to another vector with less elements, increasing the discretization interval to make the new vector _occupy the same space_ as the old one. This operator can be seen as a vector _sampling_ operator.

$I_{2h}^h$ is the exact opposite of the restriction operator and it is called **interpolation operator**. It operates on $I_{2h}^{h} : \mathscr{T}_{2h} \to \mathscr{T}_h$ and it just interpolates values on the old vector to fill-in the blanks in the longer new one.

It is now possible to define how to compute $A_{2h}$: let $I_{h}^{2h} = c(I_{2h}^{h})^T$ (with $c \in \mathbb{R}$ that is used to _tame_ the operator in order to achieve consistency), then $A_{2h} = I_{h}^{2h} A_h I_{2h}^{h}$ (**Galerkin conditioning**).

The above algorithm is used for the so-called **two grid scheme** where only 2 different granularities are used for the grid (the initial finer one and the other coarser one). It is possible to etend the algorithm to support  multiple levels of granularity by adding a parameter $j$ used to select the number of levels of granularity to traverse and modifying step 4 to "If $j$ is the coarsest level then solve $A_{2h} e_{2h} = r_{2h}^{(\nu_1)}$ otherwise compute $e_{2h} = MG(0, r_{2h}^{(\nu_1)}, \nu_1, \nu_2, j-1)$."

With the modification just described, we obtain the **V-cycle** iteration scheme. By employing more complex logic in the recursion, we can get to **F-cycle** and **W-cycle**.

No tail call optimization can be applied here: the entire state of each recursion call must be stored in memory: the storage cost is proportional to $\frac{2n^d}{1 - 2^{-d}}$, this means that, in storage terms, multigrids methods increase in efficiency with the dimensionality of the problem.

Computationally-wise, V-cycle cost scales like $2^d$.

Multigrid methods always converge.

# Algebraic multigrid methods

**Algebraic multigrid methods** are very similar to the normal multigrid methods except that we do not rely on the fact that we know the geometry of the problem. While with normal multigrid methods we use different-sized grids obtained by changing the _sampling rate_ of the function to sample, with purely algebraic methods, we go coarser and finer using pure math.

The same three operations as in the normal method version needs to be redefined, the algorithm is the same.

The coarsening of the $A$ matrix exploits the fact that (a) the matrix is usualy sparse (not a requirement, thought) and that (b) the matrix can be viewed as a wheighted adjancency matrix of a non directed graph (matrix is symmetric).

::: {.callout .callout-definition title="Strong connection"}
Given a treshold $\theta \in (0, 1)$, we say that two vertices $i$ and $j$ are **strongly connected** to each other if

$$
-a_{i,j} \ge \theta \max_{k \ne i}(-a_{i,k})
$$
:::

Let $S_i$ be the set of vertices the $i$-th vector is connected to, the with $S$ is denoted the **strenght matrix** and contains the $S_i$ vector at the $i$-th row.

The procedure to determine the coarser matrix is based on the classification of each vertex between _coarse_ and _fine_ using the $S$ matrix.

The algorithm is articulated in 4 points:

1. Choose a non categorized vertex $i$: this vertex is assigned to the C (coarse) category.
2. All vertices strongly connected to $i$ are assigned to the F (fine) category.
3. Choose another non categorized vertex that will be assigned to the C category, all the strongly connected vertices connected to it will be assigned to the F category.
4. Procedure is repeated until all vertices are cetagorized.

The "how to choose a vertex" depends on the algorithm used (e.g. C-AMG).

For each vertex $i$, let:

- $N_i = \left{ j \ne i : a_{i,j} \ne 0 \right}$: the set of vertices connected to $i$;
- $C$, $F$: the set of categorized vertices as per above algorithm;
- $C_i = C \cap N_i$: the set of C vertices strongly connected to $i$;
- $C_i^S = C \cap S_i$;
- $F_i^S = F \cap N_i$: the set of F vertices strongly connected to $i$;
- $N_i^W = N_i \backslash (C_i^S \cup F_i^S)$: all vertices weakly connected to $i$;

For what concerns the interpolation, all we want is a smooth error. This means that we want that

$$
Ae = 0 \iff a_{i,i}e_i + \sum_{j \in N_i} a_{i,j} e_j \simeq 0 \qquad i \in F
$$

We want to choose some weights $w_{i,j}$ such that

$$
e_i \simeq \sum_{j \in C} w_{i,j}e_j \qquad i \in F
$$

We can write the equations above as

$$
a_{i,i}e_i + \alpha \sum_{j \in C_i^S} a_{i,j}e_j = 0 \quad \alpha = \frac{\sum_{j \in N_i} a_{i,j}}{\sum_{j \in C_i^S} a_{i,j}}
$$

therefore

$$
w_{i,j} = \alpha \frac{s_{i,j}}{a_{i,i}} \qquad i \in F, j \in C_i^S
$$

The interpolation is not straightforward and standard, there are multiple variation of it but they wont be described here.

# Domain decomposition methods

Domain decomposition methods rely on the fact that we know the geometry of the EDO problem (even more that the multigrid methods).

Suppose you have a partial EDO $Lu = f$. Partition the domain into two partially overlapping subdomain $\Omega_1$ and $\Omega_2$ with boundaries $\Gamma_1$ and $\Gamma_2$ respectively.

The equivalent of the Gauss-Seidel method to solve said EDO is an iteratice method whose iterative step looks like 

$$
\begin{cases}
  Lu_1^{\left( k + \frac{1}{2} \right)} = f & \text{in } \Omega_1 \\
  u_1^{(\left( k + \frac{1}{2} \right))} = g & \text{in } \partial\Omega_1\backslash \Gamma_1 \\
  u_1^{\left( k + \frac{1}{2} \right)} = u_2^{(k)} & \text{in } \Gamma_1
\end{cases} \qquad \begin{cases}
  Lu_2^{(k+1)} = f & \text{in } \Omega_2 \\
  u_2^{(k+1)} = g & \text{in } \partial\Omega_2\backslash\Gamma_2 \\
  u_2^{(k+1)} = u_1^{\left( k + \frac{1}{2} \right)} & \text{in } \Gamma_2
\end{cases} \\
u^{(k+1)} = \begin{cases}
  u_1^{\left( k + \frac{1}{2} \right)} & \text{in } \Omega_1\backslash\Omega_2 \\
  u_2^{(k+1)} & \text{in } \Omega_2
\end{cases}
$$

The previously described algorithm (the **alternating Schwarz method**) is not inherently parallelizable: modifying it to use the previous solution (as with the jacobi) slowd down convergenge but the aglorithm become inherently massively parallelizable (one processor per subdomain).

## Discretized Schwarz methods

The discretization of a PDE returns a system $Ax = b$ with $A$ SDP. Let $S_i$ be the set of all the indices of the grid points belonging to $\Omega_i$ (subdomains may be overlapped so $\Omega_i \cap \Omega_{i+1} \ne \empty$).

There exist $R_i \in \mathbb{R}^{n_i \times n}$ matrices (**restriction matrices**) that can extract the $S_i$ points from $A$ and $b$. for those matrices, it holds that

$$
v_i = R_i v \qquad \forall v \\
A_i = R_1 A R_1^T \qquad \forall A
$$

The iteration step of the method (for a two-splits decomposition) is composed as follows

$$
x^{\left(k + \frac{1}{2}\right)} = x^{(k)} + R_1^T A_1^{-1} R_1 (b - Ax^{(k)}) \\
x^{(k+1)} = x^{\left( k + \frac{1}{2} \right)} + R_2^T A_2^{-1} R_2 (b - Ax^{\left( k + \frac{1}{2} \right)})
$$

In each iteration, the error is updates ad follows

$$
e^{(k+1)} = (I - R_2^TA_2^{-1}R_2A)(I - R_1^TA_1^{-2}R_1A)e{(k)}
$$

To achieve massive parallelizability you can modify the iteration step (**adaptive Schwarz method**):

$$
x^{\left( x + \frac{1}{2} \right)} = x^{(k)} + R_1^T A_1^{-1} R_1 (b - Ax^{(k)}) \\
x^{(k+1)} = x^{\left( k + \frac{1}{2} \right)} + R_2^T A_2^{-1} R_2 (b - Ax^{(k)}) \\
e^{(k+1)} = (R_2^T A_2^{-1} R_2 + R_1^T A_1^{-1} R_1) A e^{(k)}
$$

By substitution we obtain that $x^{(k+1)} = x^{(k)} + P^{-1}r^{(k)}$ which is just a preconditioned richardson iteration.

<!-- P7:11 -->

_To be continued._

---
title: "Numerical Methods for Partial Differential Equations Summary"
author: 
- "Andrea Oggioni"
---

# Introduction

A **Partial Differential Equation** (from now on, PDE) is an equation where both functions and their partial derivatives may appear.

PDEs are almost always used to study and predict physical phenomena: they usually consists of multiple variables and a close solution almost never exists, so we need to use computer to perform all the calculations.

Assume you want to study some physical phenomena, you cannot feed it directly into the computer: first you have to create a mathematical model of the phenomenon, deriving the euqtions that describe it (_modelling phase_), then you have to discretize the model so that it can be treated with numerical methods, obtaining an algebraic system (_discretization phase_) and only then you can feed it into the computer (_computation phase_).

Each of those three phases introduces an error: let $u_{ph}$ be the real solution (what you can observe in the real world), $u_m$ be the error introduced in the modelling phase, $u_n$ the error introduced in the discretization phase and $u_c$ the error introduced by the finite-precision computation, then

$$
\|u_{ph} - u_c\| \le \underbrace{\|u_{ph} - u_m\|}_\text{modelling error} + \underbrace{\|u_m - u_n\|}_\text{numerical error} + \underbrace{\|u_n - u_c\|}_\text{computation error}
$$

$u_c$ is the solution we effectively get out of all the simulation.

In this document we assume that there is no modelling error.

There exist three types of PDEs: **elliptic**, **parabolic** and **hyperbolic**. While the first type only depends on _space_, the other two depend also on _time_. Hyperbolic PDEs will not be considered in this document.

First, an overview about PDEs will be given and then, numerical methods for the approximation of their solution will be analyzed.

# Elliptic PDEs

A general elliptic PDE can be expressed as

$$
\begin{cases}
  Lu = f & \text{in } \Omega \\
  \text{Boundary conditions} & \text{in } \partial\Omega
\end{cases}
$$

where $\Omega \sub \mathbb{R}^d, d \in \mathbb{N}^+$ is called the **domain** of the PDE and $\partial\Omega$ is the **boundary** of $\Omega$, $f : \Omega \to \mathbb{R}$ is a given function and $u : \Omega \to \mathbb{R}$ is the unknown function.

Tipically, the boundary condition can be **Dirichlet conditions** and **Neumann conditions**:

$$
\begin{cases}
  u = g & \text{in } \Gamma_D \sube \partial\Omega \\
  \frac{\partial u}{\partial n} = q & \text{in } \Gamma_N \sube \partial\Omega
\end{cases}
$$

where $g : \Gamma_D \to \mathbb{R}$ is called **Dirichlet data** and $q : \Gamma_N \to \mathbb{R}$ is called **Neumann data**. $\frac{\partial}{\partial n}$ denotes the **normal derivative** (see [appendix](#normal-derivative)).

$\Gamma_D$ is called **Dirichlet boundary**, $\Gamma_D$ is called **Neumann boundary**. They form a partition of $\partial\Omega$:

$$
\begin{cases}
  \Gamma_D \cup \Gamma_N = \partial\Omega \\
  \Gamma_D \cap \Gamma_N = \empty
\end{cases}
$$

A problem can be a **Dirichlet problem** (if it has only Dirichlet conditions), **Neuman problem** (if it has only Neumann conditions) or **mixed problem**.

$L$ is a differential operator called **elliptic operator** and it is defined as a sum of three terms (in order, the **diffusion** term, the **advection/convection/transport** term and the **reaction** term):

$$
Lu \overset{\Delta}{=} -\operatorname{div}(\mu \nabla u) + \vec{b} \nabla u + \sigma u
$$

For this reason $L$ is also called **advection/diffusion/reaction** (ADR) operator.

See [appendix](#divergence) for an explanation of the various symbols.

If $\mu$ is constant then $-\operatorname{div}(\mu \nabla u) = -\mu \Delta u$.

## Monodimensional elliptic PDEs

In order to solve an elliptic PDE, the problem needs to be _massaged_ to get to something that can be treated with computers.

```mermaid
flowchart LR
    id1[Strong problem]
    id2[Weak problem]
    id3[Galerkin problem]
    id4[Algebraic problem]

    id1-->id2-->id3-->id4
```

### Getting to the algebraic problem

The **strong problem** is the problem as it was defined before:

$$
\text{Find $u$ s.t.} \begin{cases}
  Lu := -(\mu u')' + b u' + \sigma u = f & \text{on } \Omega \\
  \text{Boundary conditions}
\end{cases}
$$

where all the derivatives are [weak derivatives](#distributional-derivative).

The strong problem always has one unique solution.

In order to get to the **weak problem** we have to multiply both sides of the PDE by $v \in V$ and then integrate them on $\Omega$:

$$
\begin{align}
  \int_\Omega -(\mu u')' v + b u' v + \sigma u v &= \int_\Omega f v \\
  \int_\Omega \left( (\mu u')' v' + b u' v + \sigma u v \right) - \left[ \mu u' v \right]_{\partial\Gamma} &=\int_\Omega f v
\end{align}
$$

where $V$ is defined as follows:

$$
V = \left\{ v : \Omega \to \mathbb R \middle| v \in L^2(\Omega), v' \in L^2(\Omega), v(\Gamma_D) = 0 \right\}
$$

where $L^2$ is the set of [square-integrable functions](#square-integrable-functions).

The term in the square brackets has to be evaluated immediately: it is $0$ on the Dirichlet boundaries and a real number in the Neumann boundaries. The terms that comes out from the evaluation in the Neumann boundaries ($\sum \gamma v(x_D)$) must be moved to the right hand side of the equation.

The weak problem is then written as follows:

$$
\text{Find $u \in V$ s.t. } \underbrace{\int_\Omega (\mu u' v' + b u v' + \sigma u v) dx}_{a(u, v)} = \underbrace{\int_Omega f v \, dx+ \sum \gamma v(x_D)}_{F(v)} \qquad \forall v \in V 
$$

that is equivalent to

$$
\text{Find $u \in V$ s.t. } a(u, v) = F(v) \qquad \forall v \in V
$$

$a$ is a [bilinear form](#forms) and $F$ is a [linear functional](#functionals).

As the weak problem must be solved $\forall v \in V$, it has infinitely many solutions. We do not like that, so we restrict the space in which the solution lies to a smaller, finite dimension one.

Let $V_h \sub V$ be a finite-dimension function space, then the problem can be rewritten in the **Galerkin form**

$$
\text{Find $u_h \in V_h$ s.t. } a(u_h, v_h) = F(v_h) \qquad \forall v_h \in V_h
$$

Since $V_h$ has a finite number of dimensions, then it admits a basis $\{\varphi_j\}_{j=1}^{N_h}$ such that

$$
u_h(x) = \sum_{j=1}^{N_h} u_j \varphi_j(x)
$$

By exploiting the linearity of $a$ and $F$, then the problem can be rewritten again as multiple simpler equations, one for each element of the basis:

$$
a\left( \sum_{j=1}^{N_h} u_j \varphi_j, \varphi_i \right) = F(\varphi_i) \iff \sum_{j=1}^{N_h} u_j a(\varphi_j, \varphi_i) = F(\varphi_i) \qquad \forall i = 1, 2, \dots, N_h
$$

This can also be rewritten as $A \vec{u} = \vec{F}$ (**algebraic form**). The algebraic form is a _simple_ (and, usually, [megachonk](https://www.reddit.com/r/Chonkers/)) linear system so you can [have](/Fondamenti%20di%20Calcolo%20Numerico/index.html) [fun](/Numerical%20Linear%20Algebra/index.html) solving it.

### Construction of $V_h$

Let

$$
\begin{align*}
  \mathscr{T}_h &= \left\{ k_j : \bigcup_{i=1}^{N_h} k_i = \Omega, k_i \cap k_j = \empty \ \forall i, j \in 1, 2, \dots, N_h, i \ne j \right\} \\
  X_h &= \left\{ v_h \in \Omega \to \mathbb{R} \middle| v \in \mathcal{C}^0(\Omega), {v_h}\big|_{k_j} \in \mathbb{P}^g(k_j)\ \forall j \right\}
\end{align*}
$$

We split $\Omega$ in $N_h$ $h$-wide elements.

We want $v_h \in V_h$ to be a _piecewise polynomial_ (i.e. continuous in each element) of degree $g$ on $\Omega$ such that $v_h(\Gamma_D) = 0$:

$$
V_h = \left\{ v_h \in X_h : v_h(\Gamma_D) = 0 \right \}
$$

The basis of $V_h$ can be expressed as

$$
\varphi_h \in X_h : \varphi_j(x_i) = \delta_{ij} = \begin{cases}
  0 & i \ne j \\
  1 & \text{otherwise}
\end{cases}
$$

Therefore

$$
v_h(x) = \sum_{j=1}^{N_h} v_j \varphi_j(x) \implies v_h(x_i) = v_i
$$

The error in the discretization is bounded: $\|u - u_h\| \le C \cdot h$.

_To be continued_

## Multidimensional elliptic PDEs

Multidimensional PDEs are just like monodimensional PDEs, but multidimensional: 

$$
\begin{cases}
  Lu := -\operatorname{div}(\mu \nabla u) + \vec{b} \nabla u + \sigma v \\
  u = 0 & \text{on } \Gamma_D \\
  \mu \frac{\partial u}{\partial n} = \phi & \text{on } \Gamma_N
\end{cases}
$$

Integrals become multidimensional, derivatives become gradients and the integration by parts is performed through the **Green formula**:

$$
-\int_{\Omega} \operatorname{div}(\mu \nabla u) v= \int_{\Omega} mu \nabla u \nabla v - \int_{\partial\Omega} \mu \underbrace{\nabla u \vec{n}}_{\frac{\partial u}{\partial n}} v
$$

_Massaging the problem_ (if you get this joke, you are authorised to open the source of this file and to add your name in the comment below this paragraph) as in the monodimensional case and using the Green formula instead of the integration by parts, we can write that

<!--
Add your name here:
- 
-->

$$
\begin{align*}
  a(u, v) &= \int_{\Omega} \mu \nabla u \nabla v  \int_{\Omega} \vec{b} \nabla u v + \int_{\Omega} \sigma u v \\
  F(v) &= \int_{\Omega} fv + \int_{\partial\Omega} \mu \frac{\partial u}{\partial n} v \\
  &= \int_{\Omega} fv + \underbrace{\int_{\Gamma_D} \mu \frac{\partial u}{\partial n} v}_{=0} + \int_{\Gamma_N} \underbrace{\mu \frac{\partial u}{\partial n}}_{\psi} v \\
  &= \int_{\Omega} fv + \int_{\Gamma_N} \psi v
\end{align*}
$$

Continuing with the _massage_ we can get to the weak form, to the Galerkin approximation and finally, to the algebraic system.

The construction of $V_h$ follows the same logic: the definitions are the same as in the monodimensional case with $\Omega$ as the domain. It is notable that $X_h \sub H'(\Omega)$ and $V_h \sub H'_{\Gamma_D}$ where

$$
V \equiv H^1_{\Gamma_D}(\Omega) = \left\{ v \in H'(\Omega), v|_{\Gamma_D} = 0 \right\} \\
H^1(\Omega) = \left\{ v : \Omega \to  \mathbb{R}, v \in L^2(\Omega), \frac{\partial v}{\partial x_j} \in L^2(\Omega) \ \forall j = 1, 2, \dots, n \right\}
$$

## Lax-Milgram lemma

::: {.callout .callout-theorem title="Lax-Milgram lemma"}
Assume that $V$ is an Hilbert space with norm $\|\cdot\|$, $F$ is a linear functional on $V$ (i.e. $F \in V'$ and bounded), $a$ is a bilinear, continuous and coercive form (i.e. $\exists u \gt 0 : |a(u, v)| \le u \|u\|\|v\|\ \forall u, v \in V$, $\exists \alpha \gt 0 : a(v, v) \ge \alpha \|v\|^2\ \forall v \in V$).

Then there exists a unique solution $u$ to the weak problem that is also bounded (i.e. $\|u\| \lt \frac{1}{\alpha}\|F\|_{V'}$)
:::

In the previous lemma, $V'$ is the dual space of $V$ and is defined as

$$
V' = \left\{ F : V \to \mathbb{R} : F \text{ is linear, bounded and } \|F\|_{V'} = \frac{|F(v)|}{\|v\|_V} \le C \right\}
$$

::: {.callout .callout-property title="Corollary"}
If the assumptions of the Lax-Milgram lemma are satosfied, then the Galerkin problem has a unique solution and is bounded independently of $h$ (the solution is **stable**):

$$
\|u_h\| \lt \frac{1}{\alpha}\|F\|_{V'}
$$

Moreover, the resulting linear alegraic system will be nonsingular.
:::

We will not proove the Lax-Milgram lemma but we will only see the conditions a specific problem must satisfy to satisfy the assumptions of the lemma.

::: {.callout .callout-example title="Lax-Milgram lemma assumptions, 1D case"}
Assume we have to proove that the Lax-Milgram lemma applies to this problem (homogeneous mixed problem):

$$
a(u, v) = \int_a^b \mu u' v' + b u' v + \sigma u v \\
F(v) = \int fv + \gamma v(1) \\
\Omega = {x \in \mathbb{R} : a \le x \le b}, a = 0, b = 1
$$

The Lax-Milgram lemma requires that

1. $V$ is an Hilbert space
2. $a$ is bilinear
3. $a$ is continuous
4. $a$ is coercive
5. $F$ is linear
6. $F$ is bounded

For what concerns point (1), we take for granted that $V$ is an Hilbert space. We have to choose one of two norms to use in the proofs. We can either choose the **complete norm**

$$
\|v\| = \sqrt{\|v\|^2_{L^2}(0, 1) + \|v'\|_{L^2(0, 1)}}
$$

or the **reduced norm**

$$
|v| = \sqrt{\|v'\|_{L^2(0, 1)}}
$$

The reduced norm can be used only with a non-empty Dirichlet boundary. For this proof, we chose to use the complete norm.

Proving (2) and (5) is trivial.

We will now prove (3). By definition, $a$ is continuous if $\exists M \gt 0 : |a(u, v)| \le M \|u\| \|v\| \ \forall u, v \in V$.

$$
|a(u, v)| \le \left| \int_a^b \mu u' v' \right| + \left| \int_a^b b u' v \right| + \left| \int_a^b \sigma u v \right| = (*)
$$

We can write that

$$
\left| \int_a^b \mu u' v' \right| \le \underbrace{\max_{x} |\mu(x)|}_{\mu_{max}} \int_a^b |u' v'| \le \mu_{max} \|u'\|_{L^2(a, b)} \|v'\|_{L^2(a, b)} \\
\left| \int_a^b b u' v \right| \le \underbrace{\max_x |b(x)|}_{b_{max}} \int_a^b |u'v| \le b_{max} \|u'\|_{L^2(a, b)} \|v'\|_{L^2(a, b)} \\
\left| \int_a^b \sigma u v \right| \le \underbrace{\max_x |\sigma(x)|}_{\sigma_{max}} \int_a^b |u v| \le \sigma_{max} \|u\|_{L^2(a, b)} \|v\|_{L^2(a, b)}
$$

Knowing that $\|v\|_{L^2} \lt \|v\|$ and that $\|v'\|_{L^2} \lt \|v\|$, we can conclude that

$$
\begin{align*}
  (*) &\le \mu_{max} \|u'\|_{L^2(a, b)} \|v'\|_{L^2(a, b)} + b_{max} \|u'\|_{L^2(a, b)} \|v'\|_{L^2(a, b)} + \sigma_{max} \|u\|_{L^2(a, b)} \|v\|_{L^2(a, b)} \\
  &\le \mu_{max} \|u\| \|v\| + b_{max} \|u\| \|v\| + \sigma_{max} \|u\| \|v\| \\
  &= \underbrace{(\mu_{max} + b_{max} + \sigma_{max})}_{M} \|u\| \|v\| \\
  &= M \|u\| \|v\|
\end{align*}
$$

Assuming that $\mu$, $b$ and $\sigma$ are bounded from above, we can say that $a$ is continuous.

We will now prove (4).  By definition, $a$ is coercive if $\exists \alpha : a(v, v) \ge \alpha \|v\|^2$.

We know that $a(v, v) = \int_a^b \mu(v')^2 + \int_a^b b v' v + \int_a^b \sigma v^2$.

Assume that $\mu \gt 0$ then

$$
\int_a^b \mu (v')^2 \ge \mu_{min} \int_a^b (v')^2 = \mu_{min} \|v'\|_{L^2(a, b)}^2
$$

Assume that $b(1) \gt 0$ then

$$
\int_a^b b v' v = \int_a^b b \frac{1}{2} (v^2)' = \frac{1}{2} \int_a^b b (v^2)' = -\frac{1}{2} \int_a^b b v^2 + \underbrace{\frac{1}{2} [b v^2]_a^b}_{\gt 0} \ge -\frac{1}{2} \int_a^b b' v^2
$$

Assume $\delta = \sigma - \frac{1}{2}b' \ge 0$ then

$$
\begin{align*}
  a(v, v) &= \mu_{min} \|v'\|_{L^2(a, b)}^2 \int_a^b (\sigma - \frac{1}{2}b') v^2 \\
  &\ge \mu_{min} \|v'\|_{L^2(a, b)} + \delta \int_a^b v^2 \\
  &= \mu_{min} \|v'\|_{L^2(a, b)}^2 + \delta \|v\|_{L^2(a, b)}^2 \\
  &\ge \underbrace{\min(\mu_{min}, \delta)}_{\alpha \gt 0} \cdot (\|v'\|_{L^2(a, b)} + \|v\|_{L^2(a, b)}^2) \\
  &= \alpha\|v\|^2
\end{align*}
$$

We will now prove (6). Since $\|1\|_{L^2(a, b)} = \sqrt{\int_a^b 1^2 dx} = \sqrt{b - a}$ then

$$
v(b) = v(a) + \int_a^b v'(x) dx \implies |v(b)| = \cancel{|v(a)|} \int_a^b v'(x) \cdot 1 dx \le \|v'\|_{L^2(a, b)} \|1\|_{L^2(a, b)} = \|v'\|_{L^2(a, b)} \sqrt{b - a} \le \sqrt{b - a} \|v\|
$$

this means that

$$
\begin{align*}
  |F(v)| &= \left|\int_a^b fv + \gamma v(b) \right| \\
  &\le \left| \int_a^b fv \right| + |\gamma v(1)| \\
  &\le \|f\|_{L^2(a, b)} \|v\|_{L^2(a, b)} + |\gamma| |v(b)| \\
  &\le (\|f\|_{L^2(a, b)} + |\gamma|) \|v\| \\
  &= \alpha \|v\|
\end{align*}
$$

We can now conclude that

$$
\|F\|_{V'} = \sup_{v \in V} \frac{F(v)}{\|v\|} \le \alpha
$$

Since $f$ and $\gamma$ are given, the problem is said to be **bounded by the data**.

Since the assumptions of the Lax-Milgram lemma are satisfied, then we can say that there exists a unique solution for $a(u, v) = F(v) \ \forall v \in V$ that is also bounded.
:::

The Lax-Milgram lemma also holds with multidimensional domains.

If we take $u = v$ then

$$
\alpha \|u\|^2 \le a(u, u) = F(u) = \int fu + \gamma u(b) \le \|f\|_{L^2(a, b)} \|u\|_{L^2(a, b)} + |\gamma| \|u\| \le (\|f\|_{L^2(a, b)} + |\gamma|)\|u\|
$$

therefore

$$
\|u\| \le \frac{1}{\alpha} (\|F\|_{L^2(a, b)} + |\gamma|)
$$

This means that the solution is bounded independently on the value of $h$ (**stability** property).

# Practical numerical solution

_TODO_

# Appendix

## Normal derivative

Let $v : \Omega \sub \mathbb{R}^d \to \mathbb{R}$, then the **normal derivative** of $v$ is defined as

$$
\frac{\partial v}{\partial n} = \nabla v \cdot n
$$

where $n$ is the normal direction of $v$ in each point.

## Divergence

Let $\vec{w} \in \mathbb{R}^d, d \in \mathbb{N}^+$ then the **divergence operator** applied to $\vec{w}$ is defined as

$$
\operatorname{div}(\vec{w}) = \sum_{i = 1}^d \frac{\partial w_d}{\partial x_d}
$$

## Gradient

Let $v : \Omega \sub \mathbb{R}^d \to \mathbb{R}, d \in\mathbb{N}^+$, then the **gradient** of $v$ is defined as

$$
\nabla v = \begin{bmatrix}
  \frac{\partial v}{\partial x_1} \\
  \frac{\partial v}{\partial x_2} \\
  \vdots \\
  \frac{\partial v}{\partial x_d} \\
\end{bmatrix} \in \mathbb{R}^d
$$

Let $\vec{x} \in \Omega$ then $\nabla v(\vec{x})$ gives the direction of steepest ascent. If $\nabla v(\vec{x}) = 0$ then $\vec{x}$ can be either a local maximum, a local minimum or a saddle point.

## Laplacian

Let $v : \Omega \sub \mathbb{R}^d \to \mathbb{R}, d \in \mathbb{N}^+$, then the **laplacian** of $v$ is defined as

$$
\Delta v = \sum_{i = 1}^d \frac{\partial^2 v}{\partial x_i^2}
$$

## Distributional derivative

Let $v : \mathbb{R}^n \to \mathbb{R}$, then $D$ is the **distributional derivative** of $v$ if

$$
\int_{-\infty}^{+\infty} Dv \cdot w \, dx = -\int_{-\infty}^{+\infty} v \frac{dw}{dx} \, dx \qquad \forall w \in \mathcal{C}^\infty(\mathbb{R}) \text{ s.t. } w : \mathbb{R} \to \mathbb{R}, \lim_{x \to \pm \infty}w(x) = 0
$$

If $v \in \mathcal{C}^1$ then the distributional derivative is the same as the conventional derivative.

Intuitively, in the case of jump discontinuities, the distributional derivative is a piecewise derivative where at each discontinuity point $\bar x$, we can choose either $v(\bar x) = v(\bar x^-)$ or $v(\bar x) = v(\bar x^+)$.

Distributional derivatives are also called **weak derivatives**.

## Square-integrable functions

The **space of square-integrable functions** $L^2$ on $\Omega \sub \mathbb{R}^n$ is defined as

$$
L^2(\Omega) = \left\{ f : \Omega \to \mathbb{R} \middle| \int_\Omega f(x)^2 \, d\Omega \lt +\infty \right\}
$$

The integral is to be considered a _Lebesgue_ integral.

If $u, v \in L^2$ then $u' \cdot v' \in L^1$.

$L^2$ is an Hilbert space where

$$
\lang f, g \rang_{L^2(\Omega)} = \int_\Omega f(x)g(x) \, d\Omega \\
\|f\|_{L^2(\Omega)} = \sqrt{\lang f, f \rang_{L^2(\Omega)}}
$$

## Forms

A **form** $a$ is a relation $a : V \times V \to \mathbb R$ (where $V$ is a function space).

A form $a$ is called **bilinear** if

$$
a(\lambda u + \mu w, v) = \lambda a(u, v) + \mu a(w, v) \\
a(u, \lambda w + \mu v) = \lambda a(u, w) + \mu a(u, v)
$$

A form $a$ is called **coercive** if

$$
\exists \alpha \gt 0 : a(v, v) \gt \alpha \|v\|_V^2
$$

## Functionals

_TODO_

## Cauchy-Schwarz inequality

Let $u, v \in V$, then

$$
\left|\int_\Omega uv\right| \le \|u\|_{L^2(\Omega)} \|v\|_{L^2(\Omega)} \\
\left|\int_\Omega u'v'\right| \le \|u'\|_{L^2(\Omega)} \|v'\|_{L^2(\Omega)} \\
\left|\int_\Omega u'v\right| \le \|u'\|_{L^2(\Omega)} \|v'\|_{L^2(\Omega)} \\
$$

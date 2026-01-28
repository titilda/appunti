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
Lu \overset{\Delta}{=} -\operatorname{div}(\mu \nabla u) + \vec{b} \cdot \nabla u + \sigma u
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
\text{Find $u \in V$ s.t. } \underbrace{\int_\Omega (\mu u' v' + b u' v + \sigma u v) dx}_{a(u, v)} = \underbrace{\int_\Omega f v \, dx+ \sum \gamma v(x_D)}_{F(v)} \qquad \forall v \in V 
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
  Lu := -\operatorname{div}(\mu \nabla u) + \vec{b} \cdot \nabla u + \sigma u = f \\
  u = 0 & \text{on } \Gamma_D \\
  \mu \frac{\partial u}{\partial n} = \phi & \text{on } \Gamma_N
\end{cases}
$$

Integrals become multidimensional, derivatives become gradients and the integration by parts is performed through the **Green formula**:

$$
-\int_{\Omega} \operatorname{div}(\mu \nabla u) v = \int_{\Omega} \mu \nabla u \cdot \nabla v - \int_{\partial\Omega} \mu \frac{\partial u}{\partial n} v
$$

_Massaging the problem_ (if you get this joke, you are authorised to open the source of this file and to add your name in the comment below this paragraph) as in the monodimensional case and using the Green formula instead of the integration by parts, we can write that

<!--
Add your name here:
- 
-->

$$
\begin{align*}
  a(u, v) &= \int_{\Omega} \mu \nabla u \cdot \nabla v + \int_{\Omega} \vec{b} \cdot \nabla u v + \int_{\Omega} \sigma u v \\
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
7. $f \in L^2(\Omega)$

For what concerns point (1), we take for granted that $V$ is an Hilbert space. We have to choose one of two norms to use in the proofs. We can either choose the **complete norm**

$$
\|v\| = \sqrt{\|v\|^2_{L^2}(0, 1) + \|v'\|_{L^2(0, 1)}}
$$

or the **reduced norm**

$$
|v| = \sqrt{\|v'\|_{L^2(0, 1)}}
$$

The reduced norm can be used only with a non-empty Dirichlet boundary. For this proof, we chose to use the complete norm.

Proving (2), (5) and (7) is trivial.

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

_TODO: NDim generalization_

## Non homogeneous elliptic PDEs

Up until now, we have assumed that the Dirichlet boundary conditions were shaped like $u(\Gamma_D) = 0$. This is called **homogeneous problem**. If $u(\Gamma_D) = \varphi$ then it is called **heterogeneous problem**.

In the case of an heterogeneous problem, we introduce a **lifting** $R$ of $\varphi$ such that $u^0 = u - R\varphi$ and $u^0(a) = 0$.

The problem now become

$$
\begin{align*}
  a(u, v) &= F(v) \\
  a(u^0 + R\varphi, v) &= F(v) \\
  a(u^0, v) + \underbrace{a(R\varphi, v)}_{G(v)} = F(v) \\
  a(u^0, v) = F(v) - G(v)
\end{align*}
$$

A good choice for $R$ in 1D is a choice s.t. $R\varphi = \varphi$. In the multidimensional case, we choose an $R$ such that $u(\Gamma_D) = 0$.

## Galerkin approximation

_TODO_

# Practical numerical solution

_TODO_

# Parabolic PDEs

**Parabolic PDEs** are a generalization of elliptic PDEs that differs only for the fact that they are time-dependent.

A general parabolic problem can be expressed as

$$
\text{$\forall t \in (0, T)$, find $u$ : }\begin{cases}
  \frac{\partial u}{\partial t} + Lu = f & \text{in } \Omega \\
  \text{Boundary conditions} & \text{in } \partial\Omega, 0 \lt t \lt T \\
  \text{Initial condition} & \text{in } x \in \Omega, t = 0
\end{cases}
$$

where $L$ is the elliptic operator already defined for elliptic PDEs and so are the boundary conditions, except for the fact that they must be true $\forall t$. The solution $u$ also depends on time: $u = u(t, x)$.

The time-dependent domain where the Parabolic PDE applies is defined as $Q = \Omega \times (0, T)$ and, technically, it is a cylinder.

As usual, there are two types of boundary conditions: **Dirichlet** and **Neumann**:

$$
\begin{cases}
  u(x, t) = g(x, t) & x \in \Gamma_D \\
  \frac{\partial u(x, t)}{\partial n} = q(x, t) & x \in \Gamma_N
\end{cases}
$$

The procedure to get to "something that can be computed" is really similar to the one used for elliptic PDEs.

```mermaid
flowchart LR
    id1[Strong parabolic problem]
    id2[Weak parabolic problem]
    id3[Galerkin problem]
    id4[Algebraic problem]

    id1-->id2-->id3-->id4
```

In order to get to the weak parabolic problem we first multiply by a test function $v$ and then integrate, moving all the known terms to the right hand side of the equation.

$$
\frac{\partial u}{\partial t} -\operatorname{div}(\mu \nabla u) + \vec{b} \cdot \nabla u + \sigma u = f \\
\int_\Omega \frac{\partial u}{\partial t} v \ dx - \int_\Omega \operatorname{div}(\mu \nabla u) v \ dx + \int_\Omega \vec{b} \cdot \nabla u v \ dx + \int_\Omega \sigma uv = \int_\Omega fv \ dx \qquad \forall v \in V\\
\int_\Omega \frac{\partial u}{\partial t} v \ dx + \underbrace{\int_\Omega \mu \nabla u \cdot \nabla v + \int_\Omega \vec{b} \cdot \nabla u v \ dx + \int_\Omega \sigma u v \ dx}_{a(u, v)} = \underbrace{\int_\Omega f v \ dx + \int_{\Gamma_N} \psi v \ d\gamma}_{F(v)}  \qquad \forall v \in V \\
\int_\Omega \frac{\partial u}{\partial t} v \ dx  + a(u, v) = F(v) \qquad \forall v \in V
$$

Therefore, the **weak parabolic form** of the problem is

$$
\text{$\forall t \in (0, T)$, find $u \in V$ s.t. } \int_\Omega \frac{\partial u}{\partial t} v \ dx  + a(u, v) = F(v) \qquad \forall v \in V
$$

where $V$ is defined exactly in the same way it was defined for elliptic PDEs.

Let $V_h \sub V, \dim(V_h) = N_h$ then we can get to the **Galerkin approximation** of the problem:

$$
\text{$\forall t \in (0, T)$, find $u_h \in V_h$ s.t. } \begin{cases}
  \int_\Omega \frac{\partial u_h}{\partial t} v_h \ dx + a(u_h, v_h) = F(v_h) \quad \forall v_h \in V_h \\
  u_n|_{t=0} = u_{0h} \in V_h
\end{cases}
$$

Knowing that 

$$
u_h(x, t) = \sum_{j = 1}^{N_h} u_j(t) \varphi_j(x)
$$

we can apply even more algebraic transformations:

$$
\int_\Omega \frac{\partial}{\partial t} \left( \sum_{j=1}^{N_h} u_j(t) \varphi_j \right) \varphi_i \ dx + a\left( \sum_{j = 1}^{N_h} u_j(t) \varphi_j, \varphi_i \right) = F(\varphi_i) \qquad \forall i = 1, 2, \dots, N_h \\
\sum_{j=1}^{N_h} \frac{\partial}{\partial t} u_j(t) \int_\Omega \varphi_j \varphi_i \ dx + \sum_{j=1}^{N_h} u_j(t) a(\varphi_j, \varphi_i) = F(\varphi_i) \qquad \forall i = 1, 2, \dots, N_h \\
M \frac{\partial \vec{u}}{\partial t} + A \vec{u} = \vec{F}
$$

::: {.callout .callout-note title="Currying"}
In order to better understand what happens here, remember that $u_j(t)$ is defined as the partial application of $t$ to $u_h(x, t)$, therefore it is just a function of $x$.

Also note that $\forall v_h \in V_h$ can be replaced by "for all the functions $\varphi_i$ in the basis of $V_h$".
:::

In the last step we got to a system of ordinary differential equations (the actual **Galerkin approximation**).

The Galerkin approximation is also called **semidiscretization of the weak parabolic problem** because the time is still continuous.

To get to the **fully discretized** problem, the $\theta$-method must be applied.

::: {.callout .callout-definition title="$\Theta$-method"}
Let $y^n = y(t^n)$ then $\frac{\partial y}{\partial t}$ in an ODE can be approximated as follows:

$$
\frac{y^{n+1} - y^{n}}{\Delta t} = \theta f(t^{n+1}, y^{n+1}) + (1 - \theta)f(t^n, y^n) \qquad 0 \le \theta \le 1
$$

If $\theta = 0$ we get the **forward Euler** method, if $\theta = \frac{1}{2}$ we get the **Crank-Nicolson** method and if $\theta = 1$ we get the **backward Euler** method. The Crank-Nicolson method gives a second order convergence, while the Euler methods only gives first order convergence.
:::

With the application of the $\theta$-method, we can finally write the fully discretized problem:

$$
M \frac{\vec{u}^{n+1} - \vec{u}^n}{\Delta t} + \theta A \vec{u}^{n+1} + (1 - \theta) A \vec{u}^n = \theta \vec{F}^{n+1} + (1 - \theta)\vec{F}^{n} \qquad n = 1, 2, \dots, N \\
\underbrace{\left( \frac{1}{\Delta t}M + \theta A \right)}_{K} \vec{u}^{n+1} = \underbrace{\theta \vec{F}^{n+1} + (1 - \theta) \vec{F}^n + \frac{1}{\Delta t} M \vec{u}^n - (1 - \theta) A \vec{u}^n}_{\vec{G}^n} \qquad n = 1, 2, \dots, N \\
K \vec{u}^{n+1} = \vec{G}^n
$$

The fully discretized form is composed by a linear system for each timestep $n$. Since the solution of the $n+1$<sup>th</sup> timestep depends on the solution of the $n$<sup>th</sup> one, the linear systems must be resolved sequentially.

::: {.callout .callout-note title="$K$ factorization"}
As $K$ does not depend on time, it may be useful to apply LU or Cholesky factorization on it once and for all before solving the system for each timestep. For a big enough number of timesteps, the time spent performing the factorization will be masked by the performance gain.
:::

## Well-posedness of parabolic PDE problems

For the well-posedness of elliptic problems, coercivity was required. For parabolic problems, weak coercivity is enough.

Assume we want to prove the well-posedness of the following problem

$$
\begin{cases}
  \frac{\partial u}{\partial t} - \nu \frac{\partial^2 u}{\partial x^2} = f & \bar a \lt x \lt \bar b \\
  \nu \frac{\partial u}{\partial x}(\bar a) = \frac{\partial u}{\partial x}(\bar b) = 0 & \Gamma_N \\
  u(x, 0) = u_0(x) & \bar a \lt x \lt \bar b
\end{cases}
$$

with $\nu \gt 0$ and constant.

If we multiply by a test function and integrate, we get that

$$
\int_{\bar a}^{\bar b} \frac{\partial u}{\partial t} v + \underbrace{\int_{\bar a}^{\bar b} \nu \frac{\partial u}{\partial x} \frac{\partial v}{\partial x}}_{a(u, v)} - \underbrace{\left[ \nu \frac{\partial u}{\partial x} v \right]_{\bar a}^{\bar b}}_{0} = \int_{\bar a}^{\bar b} fv
$$

Consider $a(v, v) = \int_{\bar a}^{\bar b} \nu (\frac{\partial v}{\partial x})^2 = \nu \|\frac{\partial v}{\partial x}\|_{L^2(\bar a, \bar b)}^2$: there cannot be coercivity because $a(v, v)$ cannot be, in any way, larger than the complete norm of $v$ squared.

::: {.callot .callout-note title="Complete norm"}
Remember that the **complete norm of $v$** is expressed as

$$
\|v\| = \sqrt{\|v\|_{L^2(\bar a, \bar b)}^2 + \left\| \frac{\partial v}{\partial x} \right\|_{L^2(\bar a, \bar b)}^2}
$$
:::

Consider instead $a(v, v) + \lambda \|v\|_{L^2(\bar a, \bar b)}^2$ then

$$
a(v, v) + \lambda \|v\|_{L^2(\bar a, \bar b)} = \nu \left\| \frac{\partial u}{\partial x} \right\|_{L^2(\bar a, \bar b)}^2 + \lambda \|v\|_{L^2(\bar a, \bar b)}^2 \ge \underbrace{\min(\nu, \lambda)}_\alpha \cdot \underbrace{\left( \|v\|_{L^2(\bar a, \bar b)}^2 + \left\| \frac{\partial v}{\partial x}_{L^2(\bar a, \bar b)}^2 \right\| \right)}_{\|v\|^2} = \alpha \|v\|^2
$$

hence, we got that $a$ is weakly coercive.

### Stability of the $\theta$-method

The term **stability** refers to the fact that the solution does not explode and that is bounded in time.

Depending on the value of $\theta$, we can either have **unconditional stability** (the method is stable for an arbitrary $\Delta t$) or **conditional stability** (the method is stable only for small enough values of $\Delta t$).

To study the stability of the fully discretized problem, we apply the $\theta$-method to the Galerkin problem:

$$
\left( \frac{u_h^{k+1} - u_h^{k}}{\Delta t}, v_h \right) + a(\theta u_h^{k+1} + (1 - \theta)u_h^k, v_h) = \theta F^{k+1}(v_h) + (1 - \theta)F^k(v_h)
$$

for each timestep $k$ and where $(\cdot, \cdot)$ is the sclar product:

$$
(a, b) = \int a(x) \cdot b(x) \ dx
$$

::: {.callout .callout-example title="Unconditional stability of the backward Euler method"}
Assume for simplicity that $F = 0$ and that $\theta = 1$ (backward Euler). As we want to use coercivity, we take $a(u_h^{k+1}, u_h^{k+1})$.

$$
(u_h^{k+1}, u_h^{k+1}) + \Delta t \underbrace{a(u_h^{k+1}, u_h^{k+1})}_{\ge \alpha \|u_h^{k+1}\|_V^2} = (u_h^k, u_h^{k+1}) \\
\|u_h^{k+1}\|_{L^2(\Omega)}^2 + \Delta t \alpha \|u_h^{k+1}\|_V^2 \le \|u_h^k\|_{L^2(\Omega)} \|u_h^{k+1}\|_{L^2(\Omega)} \le \frac{1}{2} \|u_h^k\|^2_{L^2(\Omega)} + \frac{1}{2} \|u_h^{k+1}\|_{L^2(\Omega)}^2
$$

where we used the coercivity of $a$ (even if it is weakly coercive, it works anyway).

We will now prove **boundedess**:

$$
\|u_h^{k+1}\|_{L^2(\Omega)}^2 + 2 \Delta t \alpha \|u_h^{k+1}\|_{L^2(\Omega)}^2 \le \|u_h^k\|_{L^2(\Omega)}^2 \\
\left( \|u_h^{k+1}\|_{L^2(\Omega)}^2 - \|u_h^k\|_{L^2(\Omega)}^2 \right) + 2 \Delta t \alpha \|u_h^{k+1}\|_{L^2(\Omega)}^2 \le 0 \\
\left( \|u_h^{n+1}\|_{L^2(\Omega)}^2 - \|u_h^0\|_{L^2(\Omega)}^2 \right) + 2 \alpha \Delta t \sum_{k = 0}^n \|u_h^{k+1}\|_V^2 \le 0 \\
\underbrace{\|u_h^{n+1}\|_{L^2(\Omega)}^2 + 2 \alpha \Delta t \sum \|u_n^{k+1}\|_{L^2(\Omega)}^2}_{\text{One possible norm for $u_h^{n+1}$}} \le \|u^0_h\|_{L^2(\Omega)}
$$

where we used the fact that the $V$-norm of somethins is smaller or equal to the $L^2$ norm of the same thing.

We will now proove **stability**

$$
(1 + 2 \alpha \Delta t) \|u_h^{k+1}\|_{L^2(\Omega)}^2 \le \|u_h^k\| \\
\|u_h^k\|_{L^2(\Omega)} \le \left( \frac{1}{\sqrt{1 + 2 \alpha \Delta t}} \right)^k \|u_h^0\|_{L^2(\Omega)}
$$

therefore the backward Euler method is unconditionally stable.
:::

::: {.callout .callout-note title="Weak coercivity is still coercivity"}
In the previous example we used the fact that $a$ even if it was declared as _weakly_ coercive. This is perfectly fine because if we perform a specific change of variables on a weakly coercive form, we get a normally coercive form:

Take a weakly coercive form $a$:

$$
\frac{\partial u}{\partial t} - \nu \frac{\partial^2 u}{\partial x^2} = f \implies a(u, v) = \int_\Omega \frac{\partial u}{\partial x} \frac{\partial v}{\partial x}
$$

If we take $u = e^{\lambda t}w$ then the problem become

$$
\frac{\partial}{\partial t} \left( e^{\lambda t} w \right) - \nu \frac{\partial^2}{\partial t^2} \left( e^{\lambda t} w \right) = f \implies \tilde a(w, v) = \int_{\Omega} \frac{\partial w}{\partial x} \frac{\partial v}{\partial x} + \lambda \int_{\Omega} w^2 = a(w, v) + \lambda \|v\|_{L^2(\Omega)}^2 \ge \lambda \|w\|_{L^2(\Omega)}^2
$$
:::

While the backward Euler is unconditionally stable, in general, this property depends on the chosen value for $\theta$, in particular, the $\theta$-method is unconditionally stable only for $\theta \ge 0$, otherwise we must have that $\Delta t$ is small enough:

$$
\Delta t \lt \frac{2}{(1 - 2\theta) \lambda_{max}(A)} \simeq \frac{2}{1 - 2\theta} h^2
$$

::: {.collapsible title="Proof"}
Let $A$ and $M$ be two matrices. A **generalized eigenvalue problem** consists in finding all the eigenpairs $(\lambda, v)$ s.t. $Av = \lambda M v$. If $A$ is SPD then all the eigenvalues $\lambda$ are strictly positive and real numbers.

Let $a \in V \times V$ be a bilinear form. The eigenpairs of $a$ can be written as all the tuples $(\lambda, w)$ s.t.

$$
a(w, v) = \lambda(w, v) = \lambda \int_{\Omega} w v \qquad \forall v \in V
$$

If, instead of $V$, we take $V_h$, we get that $(\lambda_h, w_h)$ is an approximation of $(\lambda, w)$ if 

$$
a(w_h, v_h) = \lambda_h(w_h, v_h) \qquad \forall v_h \in V_h \\
a\left( \sum_j w_j \varphi_j, \varphi_i \right) = \lambda \left( \sum_j w_j \varphi_j, \varphi_i \right) \qquad \forall i = 1, 2, \dots, N_h\\
\sum_j w_j a(\varphi_j, \varphi_i) = \lambda_h \sum_j w_j (\varphi_j, \varphi_i) \qquad \forall i = 1, 2, \dots, N_h \\
A\vec{w} = \lambda_h M \vec{w}
$$

therefore the problem of the approximation of the eigenpairs of a bilinear form is equivalent to a generalized eigenvalue problem.

Let $(w_h^j, \lambda_h^j)$ be the $j$<sup>th</sup> eigenpair and assume that $w_h^k$ is normalized. It holds that

$$
(w_h^j, w_h^i) = \delta_{ij} \qquad \|w_h^j\|_{L^2(\Omega)}^2 = 1
$$

Let $u_h$ be the approximation of the solution, $v_h$ be the test function and $F = 0$, then, if we apply the $\theta$-method, we get that

$$
\int_\Omega \frac{u_h^{k+1} - u_h^k}{\Delta t} v_h + a(\theta u_h^{k+1} + (1 - \theta) u_h^k, v_h) = 0 \qquad \forall v_h \in V_h \\
\int_\Omega \frac{1}{\Delta t} \left( \sum_j (u_j^{k+1} - u_j^k) w_h^j \right) w_h^i + a\left(\sum_j u_j^{k+1} w_h^j + (1 - \theta)\sum_j u_j^k w_h^j, w_h^i\right) = 0 \qquad \forall i = 1, 2, \dots, N_h \\
\frac{1}{\Delta t} \sum_j (u_j^{k+1} - u_j^k) \underbrace{\int_\Omega w_h^j, w_h^i}_{\delta_{ij}} + \sum_j (\theta u_j^{k+1} - (1 - \theta) u_j^k) \underbrace{a(w_h^j, w_h^i)}_{\lambda_h^j(w_h^j, w_h^i) = \lambda_h^j \delta_{ij}} = 0 \qquad \forall i = 1, 2, \dots, N_h \\
\frac{1}{\Delta t} (u_i^{k+1} - u_i^{k}) + (\theta u_i^{k+1} + (1 - \theta) u_i^k) \lambda_h^i = 0 \qquad \forall i = 1, 2, \dots, N_h \\
u_i^{k+1} + \Delta t \theta u_i^{k+1} \lambda_h^i = u_i^k - \Delta t (1 - \theta) \lambda_h^i u_i^k \qquad \forall i = 1, 2, \dots, N_h \\
(1 + \Delta t \theta \lambda_k^i) u_i^{k+1} = [1 - \Delta t (1 - \theta) \lambda_h^i] u_i^k \qquad \forall i = 1, 2, \dots, N_h \\
u_i^{k+1} = \underbrace{\frac{1 - \Delta t (1 - \theta) \lambda_h^i}{1 + \Delta t \theta \lambda_h^i}}_{\rho_i} u_h^k \qquad \forall i = 1, 2, \dots, N_h
$$

Assume $|\rho_i| \lt 1$ then $|u_i^{k+1}| \lt |u_i^k| \lt \dots \lt |u_i^0|$, therefore

$$
\sum_{i = 1}^{N_h} |u_i^n|^2 \lt \sum_{i = 1}^{N_h} |u_i^0|^2
$$

Since

$$
u_h^n(x) = \sum_{j = 1}^{N_h} u_j^n w_h^j(x)
$$

then

$$
\|u_h^n\|_{L^2(\Omega)}^2 = \int_\Omega (u_h^n)^2 = \int_\Omega \left( \sum u_j^n w_h^j \right)\left( \sum u_j^n w_h^j \right) = \sum_j \sum_i u_j^n u_i^n \underbrace{\int_\Omega w_h^j w_h^i}_{\delta_{ij}} = \sum_j (u_j^n)^2
$$

therefore

$$
\|u_h^n\|_{L^2(\Omega)} \le \|u_h^n\|_{L^2(\Omega)}
$$

hence, stability of the $\theta$-method.

To prove stability, we assumed that $|\rho_i| < 1$, we will now study when that statement is true.

$$
|\rho_i| \lt 1 \iff -1 - \theta \lambda_h^i \Delta t \lt 1 - (1 - \theta) \lambda_h^i \Delta t \lt 1 + \theta \lambda_h^i \Delta t \iff 2 \theta - 1 \gt - \frac{2}{\lambda_h^i \Delta t}
$$

If $\theta \ge \frac{1}{2}$ the inequality is always satisfied and the method in unconditionally stable, otherwise we must impose that

$$
\Delta t \lt \frac{2}{(1 - 2 \theta) \lambda_h^i}
$$

Since that condition should hold for all the eigenvalies $\lambda_h^i$, it will suffice to impose it only for the bigger one, so

$$
\Delta t \gt \frac{2}{(1 - 2 \theta) \lambda_h^{N_h}} \simeq \frac{2}{1 - 2\theta} h^2
$$
:::

# Domain decomposition

**Domain Decomposition** methods are used to split a big problem into two or more smaller problems. Assume you have a problem so solve that looks like

$$
\begin{cases}
  Lu = f & \Omega = \{x : \bar a \lt x \lt \bar b\}\\
  u(\bar a) = \gamma_1 \\
  u(\bar b) = \gamma_2
\end{cases}
$$

We want to split $\Omega$ into two subdomains so that we can get a new, equivalent problem:

$$
\begin{cases}
  Lu_1 = f & \Omega_1 = \{x : \bar a \lt x \lt \Gamma\} \\
  Lu_2 = f & \Omega_2 = \{x : \Gamma \lt x \lt \bar b\} \\
  u_1(\bar a) = \gamma_1 \\
  u_2(\bar b) = \gamma_2 \\
  u_1(\Gamma) = u_2(\Gamma) \\
  (\mu u_1')(\Gamma) = (\mu u_2')(\Gamma)
\end{cases}
$$

$\Gamma$ is called the **interface** between $\Omega_1$ and $\Omega_2$.

We see in the new, equivalent problem, that on the interface, we need to impose continuity of both the solution and its derivative (**interface conditions**). Interface conditions are both Dirichlet and Neumann.

If we define

$$
u = \begin{cases}
  u_1 & \Omega_1 \\
  u_2 & \Omega_2
\end{cases}
$$

then we can recover the solution of the original problem from the unknowns of the equivalent one.

$u_1$ and $u_2$ are called **local solutions**.

We use an iterative method (the **Dirichlet-Neumann DD** method) to solve the new equivalent problem splitting it into two separate problems:

$$
\begin{cases}
  Lu_1^{(k)} = f & \Omega_1 \\
  u_1^{(k)} = u_2^{(k-1)} & \Gamma \\
  \text{Boundary conditions} & \partial\Gamma_1 \cap \partial\Gamma
\end{cases} \\
\begin{cases}
  Lu_2^{(k)} = f & \Omega_2 \\
  \mu u_2'^{(k)} = \mu u_1'^{(k)} & \Gamma \\
  \text{Boundary conditions} & \partial\Gamma_2 \cap \partial\Gamma
\end{cases}
$$

Lets call $P_1$ the problem on $\Omega_1$ and $P_2$ the one on $\Omega_2$.

$P_1$ is a Dirichlet problem, $P_2$ is a Neumann problem, hence the name of the iterative method.

The DN-DD method is sequential: $u_2^{(k)}$ (from $P_2$) depends on $u_1^{(k)}$ (from $P_1$) which in turn depends on $u_2^{(k-1)}$ (from $P_2$ on the previous iteration) and so on.

We modify $P_2$ to make the two $P_1$ and $P_2$ independent so that they can be solved in parallel (in red the changes to the algorithm):

$$
\begin{cases}
  Lu_2^{(k)} = f & \Omega_2 \\
  \mu u_2'^{(k)} = \mu \textcolor{red}{u_1'^{(k - 1)}} & \Gamma \\
  \text{Boundary conditions} & \partial\Gamma_2 \cap \partial\Gamma
\end{cases}
$$

With $k \to \infty$, we have that $u_i^{(k)} \to u_i$.

Depending on the choice for $\Gamma$, the algorithm may or may not converge. To improve convergence, we modify $P_1$ to have a relaxation parameter (changes in red) to get the DN-$\theta$ problem:

$$
\begin{cases}
  Lu_1^{(k)} = f & \Omega_1 \\
  u_1^{(k)} = \textcolor{red}{\theta u_2^{(k-1)} + (1-\theta) u_1^{(k-1)}} & \Gamma \\
  \text{Boundary conditions} & \partial\Gamma_1 \cap \partial\Gamma
\end{cases}
$$

where $\theta > 0$ is called **relaxation parameter** and can be chosen arbitrarily.

With $0 \lt \theta \lt \theta_{max} \lt 1$, convergence is guaranteed.

::: {.callout .callout-definition title="Convergence"}
In general, for a problem that has been split into $N$ subdomains, **convergence** means that

$$
\lim_{k \to \infty} u_i^{(k)} = u_i = u|_{\Omega_i} \qquad \forall i = 1, 2, \dots, N
$$
:::

::: {.callout .callout-note title="Single iteration convergence"}
With a 1D problem, splitting it in two domains, it exists $\theta_{opt}$ s.t. the algorithm converges in one single iteration.
:::

The same procedure is valid in the multidimensional generalization.

## Neumann-Neumann algorithm

There exists variations of the Dirichlet-Neumann algorithm that may outperform it in specific cases. The **Neumann-Neumann** algorithm outperforms the DN algorithm in the case of a large numbers of subdomains.

<!-- TODO: understand how to generalize to an arbitrary number of subdomains -->

The algorithm is structured as follows.

Let $\Gamma_{ij}$ be the interface between the $i$-th and $j$-th subdomain, then we define the **global interface skeleton** and the **subdomain interfaces** as

$$
\Gamma \overset{\Delta}{=} \bigcup_{i \lt j} \Gamma_{ij} \qquad \Gamma_i \overset{\Delta}{=} \partial \Omega_i \cap \Gamma = \bigcup_j \Gamma_{ij}
$$

First, for each subdomain $i$, we solve

$$
\begin{cases}
  Lu_i^{(k+1)} = f & \Omega_i \\
  u_i^{(k+1)} = \lambda^{(k)} & \Gamma_i \\
  u_i^{(k+1)} = 0 & \partial\Omega_i\backslash\Gamma
\end{cases}
$$

then, for each subdomain $i$ we solve

$$
\begin{cases}
  L\psi_i^{(k+1)} = 0 & \Omega_i \\
  \frac{\partial \psi_i^{(k+1)}}{\partial n} = \frac{\partial u_i^{(k+1)}}{\partial n} - \sum\limits_{j : \Gamma_{ij} \in \Gamma_i}\frac{\partial u_j^{(k+1)}}{\partial n} & \Gamma_i \\
  \psi_i^{(k+1)} = 0 & \partial\Omega_i\backslash\Gamma
\end{cases}
$$

where

$$
\lambda^{(k+1)} = \lambda^{(k)} - \theta \sum_i \sigma_i \psi_{i|\Gamma}^{(k+1)}
$$

In practice, the first step goes towards the direction of continuity of the solution while the second goes towards the direction of continuity of the derivative.

It can be demonstrated that with $0 \lt \theta \lt \theta_{max}$ (different $\theta$s w.r.t. before), this method always converges.

## Optimality and scalability

::: {.callout .callout-property title="Optimality"}
With the finite element method, the rate of convergence is not dependent of $h$.
:::

While this method is optimal, it is not scalable.

Given a triangulation on $\Omega$, we call **active nodes** the nodes whose degrees of freedom are to be determined in order to get to the solution.

All the active nodes can be split into **interface nodes** (that, intuitively, are nodes located on the interface between two subdomains) and **interior nodes** (all the other nodes).

Note that we do not have any active node on Dirichlet boundaries (the solution at those points is already known).

The solution $\vec{u}$ of a PDE problem can be interpreted as the vector of all the nodal values (i.e. the solution evaluated in the coordinates of the active nodes) of $\Omega$.

Consider a 2-subdomains problem. Let $u_1$, $u_2$ and $\lambda$ be the vector of the local solution on $\Omega_1$, $\Omega_2$ and $\Gamma$ respectively, then

$$
\vec{u} = \begin{bmatrix}
  \vec{u_1} \\ \vec{u_2} \\ \vec{u_3}
\end{bmatrix} \in \mathbb{R}^{N_h}
$$

In the same way, we can rewrite also the $A$ matrix and the $F$ vector:

$$
A = \begin{bmatrix}
  A_{11} & A_{12} & A_{1\Gamma} \\
  A_{21} & A_{22} & A_{2\Gamma} \\
  A_{\Gamma 1} & A_{\Gamma 2} & A_{\Gamma\Gamma}
\end{bmatrix} \in \mathbb{R}^{N_h \times N_h}
\qquad
\vec{F} = \begin{bmatrix}
  \vec{F_1} \\ \vec{F_2} \\ \vec{F_\Gamma}
\end{bmatrix} \in \mathbb{R}^{N_h}
$$

Since there is no direct coupling between different subdomains (except through the interface $\Gamma$) we can say that

$$
i \ne j, i \ne \Gamma, j \ne \Gamma \implies A_{ij} = 0
$$

therefore we can rewrite $A$ again:

$$
A = \begin{bmatrix}
  A_{11} & 0 & A_{1\Gamma} \\
  0 & A_{22} & A_{2\Gamma} \\
  A_{\Gamma 1} & A_{\Gamma 2} & A_{\Gamma\Gamma}
\end{bmatrix}
$$

With this new $A$, starting from $A \vec{u} = \vec{F}$, we can write that

$$
\begin{cases}
  A_{11} \vec{u_1} + A_{1\Gamma} \vec{\lambda} = \vec{F_1} \\
  A_{22} \vec{u_2} + A_{2\Gamma} \vec{\lambda} = \vec{F_2} \\
  A_{\Gamma 1} \vec{u_1} + A_{\Gamma 2} \vec{u_2} + A_{\Gamma\Gamma} \vec{\lambda} = \vec{F_\Gamma}
\end{cases} \implies \begin{cases}
  \vec{u_1} = A_{11}^{-1} (\vec{F_1} - A_{1\Gamma} \vec{\lambda}) \\
  \vec{u_2} = A_{22}^{-1} (\vec{F_2} - A_{2\Gamma} \vec{\lambda}) \\
  A_{\Gamma 1} A_{11}^{-1} (\vec{F_1} - A_{1\Gamma} \vec{\lambda}) + A_{\Gamma 2} A_{22}^{-1} (\vec{F_2} - A_{2\Gamma} \vec{\lambda}) = \vec{F_\Gamma}
\end{cases}
$$

thus, we get to a system that only depends on $\lambda$:

$$
\underbrace{\left[ -\left( A_{\Gamma 1} A_{11}^{-1} A_{1\Gamma} + A_{\Gamma 2} A_{22}^{-1} A_{2\Gamma} \right) + A{\Gamma\Gamma} \right]}_{\Sigma} \vec{\lambda} = \underbrace{\vec{F_\Gamma} - A_{\Gamma 1} A_{11}^{-1} \vec{F_1} - A_{\Gamma_2} A_{22}^{-1} \vec{F_2}}_{\chi}
$$

This is called the **Schur complement system** and it is a system in which we have eliminated all the internal nodes. We can further apply some mathematical transofrmation: since $A_{\Gamma\Gamma} = A_{\Gamma 1} + A_{\Gamma_2}$ then

$$
\Sigma = \Sigma^{(1)} + \Sigma^{(2)} = \left( A_{\Gamma\Gamma}^{(1)} - A_{\Gamma 1} A_{11}^{-1} A_{1 \Gamma} \right) + \left( A_{\Gamma\Gamma}^{(2)} - A_{\Gamma 2} A_{22}^{-1} A_{2 \Gamma} \right)
$$

$\Sigma$ is called **global Schur complement** and $\Sigma^{(i)}$ are called **local Schur complement**.

Assume we know how to compute $\lambda$ (we sill see it shortly), then, once we know $\lambda$, we can solve each subdomain indipendently from each other (a.k.a. in parallel).

### Solution of the Schur complement system

Let $\vec{\lambda}^{(0)}$ be given then, we start iterating with preconditioned Richardson iterations (you may notice that this is different from the method described [here](/Numerical%20Linear%20Algebra/index.html#stationary-richardson-method) but please notice that they are equivalent):

$$
P (\underbrace{\lambda^{(k+1)} - \lambda^{(k)}}_\text{increment}) = \theta (\underbrace{\chi - \Sigma \lambda^{(k)}}_\text{residual})
$$

where $P$ is the preconditioner matrix and $\theta$ is the step.

Instead of actually using Richardson, having rewritten the Richardson iteration in this new, equivalent way, we notice that we got to a new linear system that uses $P$ as a system matrix.

It can be denomstrated that, with specific choices for $P$ and $\theta$, one Richardson iteration is equivalent to one Dirichlet-Neumann/Neumann-Neumann iteration. In particular, the $\theta$ used in the algorithm iteration must be used as a Richardson step and, regarding $P$, for Dirichlet-Neumann it must be $P_{DN} = \Sigma^{(2)}$ while for Neumann-Neumann it must be $P{NN} = (\sigma_1 [\Sigma^{(1)}]^{-1} + \sigma_2 [\Sigma^{(2)}]^{-1})^{-1}$.

We can now compute the specral radius for the two methods:

$$
\rho_{DN} = \frac{k(P_{DN}^{-1} \Sigma) - 1}{k(P_{DN}^{-1} \Sigma) + 1} \qquad \rho_{NN} = \frac{k(P_{NN}^{-1} \Sigma) - 1}{k(P_{NN}^{-1} \Sigma) + 1}
$$

Instead of Richardson, it is possible to achieve better convergence through the use of Krylov space methods such as GMRES (if at least one of $P$ and $\Sigma$ are not symmetric) or conjugate gradient (if both $P$ and $\Sigma$ are symmetric). They may be better but they are not equivalent.

::: {.callout .callout-note title="Note"}
If $A$ is SPD, then, also $\Sigma$ is SPD. In general, if $k(A) = O(h^{-2})$ then $k(\Sigma) = O(h^{-1})$. $\Sigma^{(1)}$ and $\Sigma^{(2)}$ inherit the same properties of $\Sigma$.

It is really important that the condition number or the preconditioned Richarson matrix does not depend on $h$:

$$
k(P_{DN}^{-1} \Sigma) = O(1) \qquad k(P_{NN}^{-1}) = O(1)
$$
:::

As we will see, the multidomain extension does not play so nicely with domain decomposition.

<!-- 31:30 -->

### Multidomain extension

With more than two domains, the linear system can be expressed as

$$
\begin{bmatrix}
  A_{11} & 0 & \dots & A_{1\Gamma} \\
  0 & A_{22} & \dots & A_{2\Gamma} \\
  \vdots & \vdots & \ddots & \vdots \\
  A_{\Gamma 1} & A_{\Gamma 2} & \dots & A_{\Gamma\Gamma}
\end{bmatrix} \begin{bmatrix}
  \vec{u_1} \\ \vec{u_2} \\ \vdots \\ \vec{\lambda}
\end{bmatrix} = \begin{bmatrix}
  \vec{F_1} \\ \vec{F_2} \\ \vdots \\ \vec{F_\Gamma}
\end{bmatrix}
$$

We can get to the Schur complement system in the same way as before (write $u$ in function of everything else, then factor $\lambda$): $\Sigma \vec{\lambda} = \vec{\chi}$.

In general, it is not possible to _colorize_ every subdomain in "Dirichlet-color" and "Neumann-color" as every Dirichlet must be surrounded by Neumanns and vice-versa so we will only restrict ourselves to Neumann-Neumann algorithms.

Given this restriction, we can write the generic multidomain preconditioner. Let $D = \operatorname{diag}(\sigma_1, \sigma_2, \dots, \sigma_n)$ and $R$ be the global-to-local conversion matrix (like a permutation matrix but keeps only the relevant nodes), then

$$
P_{NN} = \left(\sum_j R_{\Gamma j}^T D [\Sigma^{(j)}]^{-1} D R_{\Gamma j} \right)
$$

The condition number of the preconditioned Schur matrix is

$$
k(P_{NN}^{-1} \Sigma) \le C H^{-2} \left( 1 + \log\frac{H}{h} \right)^2
$$

Since it depends on the subdomain size $H$, this new method is not really scalable. Also, since it depends on the mesh granularity $h$, this methos isn't even optimal.

We can restore both properties using a different preconditioner that can accelerate the propagation of information between the different subdomains:

$$
P_{NN}^{coarse} = \left[ \Sigma_H^{-1} + (\mathbb{I} - \Sigma_H^{-1} \Sigma)P_{NN}^{-1}(\mathbb{I} - \Sigma \Sigma_H^{-1}) \right]^{-1} \qquad \Sigma_H = (R_\Gamma^T A R_\Gamma)^{-1}
$$

In this case

$$
k((P_{NN}^{coarse})^{-1} \Sigma) \le C \left( 1 + \log\frac{H}{h} \right)
$$

while this quantity still depends on $H$ and $h$, it is a logarithmic dependence so it is negligible.

# Navier-Stokes equation

**Navier-Stokes equations** are the one used to precisely describe non-turbolent fluid evolution. We will start from the simple **Stokes equation** before moving on to the more complex **Navier-Stokes** ones in order to start from a simple numerical method that will then be adapted.

## Stokes equation

A stokes equation problem is written like

$$
\begin{cases}
  \sigma \vec{u} - \nu \Delta \vec{u} + \nabla p = \vec{f} & \Omega \\
  \operatorname{div} \vec{u} = 0 & \Omega \\
  \vec{u} = \vec{\varphi} & \Gamma_D \\
  \nu \frac{\partial \vec{u}}{\partial \vec{n}} - p \cdot \vec{n} = \psi & \Gamma_N
\end{cases}
$$

where $\sigma \ge 0$ is constant, $\nu \gt 0$ is the **viscosity coefficient** (assumed to be constant), $p = P(\vec{x})$ is the pressure of the fluid (unknown), $\vec{u} = \vec{u}(\vec{x})$ is the velocity of the fluid (unknown) and $f = f(\vec{x})$ is the forcing term (given). $\vec{\varphi}$ is the Dirichlet data and $\psi$ is the Neumann data. $\operatorname{div} \vec{u} = 0$ means that the fluid is incompressible.

The first equation imposes the conservation of momentum while the second imposes the conservation of mass.

We can now introduce time dependency to generalize this model to te **unsteady Stokes equations**:

$$
\begin{cases}
  \frac{\partial \vec{u}}{\partial t} + \sigma \vec{u} - \nu \Delta \vec{u} + \nabla p = \vec{f} & \Omega, 0 \lt t \lt T \\
  \operatorname{div} \vec{u} = 0 & \Omega, 0 \lt t \lt T \\
  \vec{u} = \vec{\varphi} & \Gamma_D \\
  \nu \frac{\partial \vec{u}}{\partial \vec{n}} - p \cdot \vec{n} = \psi & \Gamma_N
\end{cases}
$$

### Weak formulation

In order to obtain the weak formulation for the Stokes equations, we introduce two test functions: $v \in V$ for velocity and $q \in Q$ for pressure, then we integrate.

The momentum equation becomes

$$
\int_\Omega \sigma \vec{u} \cdot \vec{v} - \int_\Omega \nu \Delta \vec{u} \cdot \vec{v} + \int_\Omega \nabla p \cdot v = \int_\Omega \vec{f} \cdot \vec{v} \qquad \forall \vec{v} \in V \\
\int_\Omega \sigma \vec{u} \cdot \vec{v} + \int_\Omega \nu \nabla \vec{u} \cdot \nabla \vec{v} - \int_{\partial\Omega} \nu \frac{\partial \vec{u}}{\partial \vec{n}} - \int_\Omega p \operatorname{div} \vec{v} + \int_{\partial\Omega} p \vec{u} \cdot \vec{v} = \int_\Omega \vec{f} \cdot \vec{v} \qquad \forall \vec{v} \in V
$$


and the mass equation becomes

$$
\int_\Omega \operatorname{div} \vec{v} \cdot q = 0 \qquad \forall q \in Q
$$

notice that we do not integrate in the case of the mass equation.

Using the [properties of the nabla operator](#nabla-operator), we can now write in the componentwise form of the problem:

$$
\begin{cases}
  \frac{\partial u_i}{\partial t} + \sigma u_i - \nu \Delta u_i + \frac{\partial p}{\partial x} + \sum_j u_j \frac{\partial}{\partial x_j} u_i & \forall i = 0, 1, \dots, d \\
  \sum_j \frac{\partial}{\partial x_i} u_j = 0 & \forall i = 0, 1, \dots, d
\end{cases}
$$

Starting again from the weak formulation, we can reach quite easily the equivalent algebraic system. We start by moving all the known terms to the right hand side and factoring $v$ (so taht we can write $\psi$, that can be considered as _given_).

$$
\int_\Omega \sigma \vec{u} \cdot \vec{v} + \int_\Omega \nu \nabla \vec{u} \cdot \nabla \vec{v} - \int_{\partial\Omega} \nu \frac{\partial \vec{u}}{\partial \vec{n}} - \int_\Omega p \operatorname{div} \vec{v} + \int_{\partial\Omega} p \vec{u} \cdot \vec{v} = \int_\Omega \vec{f} \cdot \vec{v} \qquad \forall \vec{v} \in V \\
\int_\Omega \sigma \vec{u} \cdot \vec{v} + \int_\Omega \nu \nabla \vec{u} \cdot \nabla \vec{v} - \int_\Omega p \operatorname{div} \vec{v} = \int_\Omega \vec{f} \cdot \vec{v} + \int_\Omega \psi \vec{v} \qquad \forall \vec{v} \in V \\
$$

Therefore, the complete weak Stokes form is

$$
\begin{cases}
  \underbrace{\int_\Omega \sigma \vec{u} \cdot \vec{v} + \int_\Omega \nu \nabla \vec{u} \cdot \nabla \vec{v}}_{a(u, v)} \underbrace{- \int_\Omega p \operatorname{div} \vec{v}}_{b(u, v)} = \int_\Omega \vec{f} \cdot \vec{v} + \int_\Omega \psi \vec{v} & \forall \vec{v} \in V \\
  \underbrace{\int_\Omega \operatorname{u} \cdot q}_{b(v, u)} = 0 & \forall q \in Q
\end{cases} \iff \begin{cases}
  a(\vec{u}, \vec{v}) + b(\vec{v}, p) = F(\vec{v}) & \forall \vec{v} \in V \\
  b(\vec{u}, q) = 0 & \forall q \in Q
\end{cases}
$$

where

$$
V = \left\{ \vec{v} \in \left[ H^1 (\Omega)\right]^d : \vec{v}|_{}\Gamma_D  = \vec{0} \right\} \equiv \left[ H^1_{\Gamma_D}(\Omega) \right]^d \\
Q = \begin{cases}
  L^2(\Omega) & \Gamma_D \ne \emptyset \\
  L_0^2(\Omega) = \left\{ q \in L^2(\Omega) : \int_\Omega q = 0 \right\} & \Gamma_D = \emptyset
\end{cases}
$$

The distinction on $Q$ is due to the fact that when we are dealing with Neumann problems, there are infinite valid solutions defined up to a constant, therefore we choose to accept the only one that has a null average.

### Discretization

Discretizing the problem is easy. Let $\{\varphi_j\}_{j=1}^{N_v}$ and $\{\psi_j\}_{j=1}^{N_q}$ be basis for $V_h$ and $Q_h$, respectively, then

$$
\begin{cases}
  a(u_h, v_h) + b(u_h, p_h) = F(v_h) & \forall v_h \in V_h \\
  b(u_h, q_h) = G(q_h) & \forall q_h \in Q_h
\end{cases}
$$

Note that we used $G$ instead of $0$ to account for the case where lifting is needed.

If, as usual, 

$$
u_h = \sum_j u_j \varphi_j \qquad q_h = \sum_j q_j \psi_j
$$

then

$$
\vec{u} = \begin{bmatrix} u_1 \\ \vdots \\ u_{N_v} \end{bmatrix} \qquad \vec{q} = \begin{bmatrix} q_1 \\ \vdots \\ q_{N_q} \end{bmatrix} A_{ij} = a(\varphi_j, \varphi_i) \qquad B_{km} = b(\varphi_m, \varphi_k)
$$

therefore 

$$
\begin{cases}
  A \vec{u} + B \vec{p} = \vec{F} \\
  B^T \vec{u} = \vec{G}
\end{cases} \iff \begin{bmatrix}
  A & B \\ B^T & 0
\end{bmatrix} \begin{bmatrix}
  \vec{u} \\ \vec{q}
\end{bmatrix} = \begin{bmatrix}
  \vec{F} \\ \vec{G}
\end{bmatrix}
$$

We call the system matrix **Stokes matrix** and it is denoted with $S$.

### Well-posedness of the Stokes problem

The Stokes problem is well osed if and only if it has a unique solution, that is equivalent to asking wether $S$ is nonsingular.

::: {.callout .callout-theorem title="Existence and uniqueness of the solution of a Stokes problem"}
Let $a : V \times V \to \mathbb{R}$ be a bilinear, continuous (with constant $\gamma$) and coercive (with constant $\alpha$) form, $b : V \times Q \to \mathbb{R}$ be a bilienar, continuous (with constant $\delta$) and LBB (with constant $\beta$) (will see later) form, then it exists a unique solution $u_h, p_h$ to the discretized Stokes problem and the solution satisfies both

$$
\|\vec{u_h}\|_V \le \frac{1}{\alpha} \|F\|_{V'} \qquad \|p_h\|_Q \le \frac{1}{\beta} \left(1 + \frac{\gamma}{\alpha} \right) \|F\|_{V'}
$$

meaning that the solution is bounded w.r.t. $h$ (stability).

::: {.collapsible title="Proof"}
Since $a$ is continuous, then

$$
\exists \gamma \gt 0 : |a(\vec{u_h}, \vec{v_h})| \le M \|\vec{u_h}\|_V \|\vec{v_h}\|_V \qquad \forall \vec{u_h}, \vec{v_h} \in V_h
$$

Since $a$ is coercive, then

$$
\exists \alpha \gt 0 : a(\vec{u}, \vec{u}) \gt \alpha \|\vec{u}\|_V^2 \qquad \forall \vec{u_h} \in V_h
$$

Since $b$ is continuous, then

$$
\exists \delta \gt 0 : |b(\vec{v_h}, q_h)| \le M \|\vec{v_h}\|_V \|q_h\|_Q \qquad \forall \vec{v_h} \in V_h, q_h \in Q_h
$$

Since $b$ is LBB, then

$$
\exists \beta \gt 0 : \forall q_h \in Q_h \ \exists \vec{v_h} \in V_h \ b(\vec{v_h}, q) \ge \beta \|\vec{v_h}\|_V \|q_h\|_Q
$$

Since

$$
\begin{align*}
  |a(\vec{u}, \vec{v})| &= \left| \int_\Omega \sigma \vec{u} \cdot \vec{v} + \int_\Omega \nu \nabla\vec{u} \cdot \nabla\vec{v}\right| \\
  &\le \left| \int_\Omega \sigma \vec{u} \cdot \vec{v} \right| + \left| \int_\Omega \nu \nabla\vec{u} \cdot \nabla\vec{v}\right| \\
  &\le \sigma \|\vec{u}\|_{L^2(\Omega)} \|\vec{v}\|_{L^2(\Omega)} + \nu \|\nabla\vec{u}\|_{L^2(\Omega)} \|\nabla\vec{v}\|_{L^2(\Omega)} \\
  &\le \max(\sigma, \nu) (\|\vec{u}\|_{L^2(\Omega)} \|\vec{v}\|_{L^2(\Omega)} + \|\nabla\vec{u}\|_{L^2(\Omega)} \|\nabla\vec{v}\|_{L^2(\Omega)}) \\
  &= \max(\sigma, \nu) \left( \begin{bmatrix} \|\vec{u}\|_{L^2(\Omega)} \\ \|\nabla\vec{u}\|_{L^2(\Omega)} \end{bmatrix} \cdot \begin{bmatrix} \|\vec{v}\|_{L^2(\Omega)} \\ \|\nabla\vec{v}\|_{L^2(\Omega)} \end{bmatrix} \right) \\
  &\le \max(\sigma, \nu) \left( \sqrt{\|\vec{u}\|_{L^2(\Omega)} + \|\nabla\vec{u}\|_{L^2(\Omega)}} \sqrt{\|\vec{v}\|_{L^2(\Omega)} + \|\nabla\vec{v}\|_{L^2(\Omega)}} \right) & (\dagger) \\
  &= \max(\sigma, \nu) \|\vec{u}\|_V \|\vec{v}\|_V & (\ddagger)
\end{align*}
$$

where in $(\dagger)$ we used the Cauchy-Schwarz inequality in $\mathbb{R}^2$ and in $(\ddagger)$ we used the definition of seminorm in $V$, then $\alpha = \max(\sigma, \nu)$.

In the case of $\sigma \gt 0$, it holds that

$$
a(\vec{v}, \vec{v}) = \int_\Omega \sigma \vec{v}^2 + \int_\Omega \nu (\nabla\vec{v})^2 \ge \min(\sigma, \nu) (\|\vec{v}\|_{L^2(\Omega)}^2 + \|\nabla\vec{v}\|_{L^2(\Omega)}^2) = \min(\sigma, \nu) \|\vec{v}\|_V^2
$$

In the case of $\sigma = 0$, we can use the Pointcaré inequality:

$$
a(\vec{v}, \vec{v}) = \nu \|\nabla v\|_{L^2(\Omega)}^2 \ge \frac{1}{C} \|\vec{v}\|_{L^2(\Omega)} \simeq \|\vec{v}\|_V
$$

In either case we have coercivity, in the first case we can also say that $\alpha = \min(\sigma, \nu)$, in he other case, it is much more comples so we ignore that.

Since

$$
|b(\vec{v}, q)| = \left| -\int_\Omega q \operatorname{div} \vec{v} \right| \le \|q\|_{L^2(\Omega)} \|\operatorname{div}\vec{v}\|_{L^2(\Omega)} \le \|q\|_{L^2(\Omega)} \|\vec{v}\|_V
$$

then we have continuity of $b$ with constant $\delta = 1$.

By divinding both sides of the LBB condition by $\|\vec{v}\|_V \|q\|_Q$ we get that what that condition is asking is that the velocity space should be larger enough w.r.t. the pressure space. The largest the space of velocicies, the _more probable_ it is to find a suitable $\beta$ coefficient.

The LBB condition can be rewritten as the **inf sup condition**:

$$
\exists \beta \gt 0 : \inf_{q_h \in Q_h, q_h \ne 0} \sum_{\vec{v_h} \in V_h, \vec{v_h} \ne 0} \frac{b(\vec{v_h}, q_h)}{\|\vec{v}\|_V \|q_h\|_Q}
$$

We will not go into details about the LBB condition but if it is satisfied, along with all the other conditions listed here, then $S$ is non-singular, therefore the problem is well posed.
:::
:::

::: {.callout .callout-example title="Valid triangulations"}
Assume we are using triangulation to generate the mesh. Let $\mathbb{P}^n$ be the space of polynomials of degree $n$ and let the velocity space and the pressure space be $\mathbb{P}^r$ and $\mathbb{P}^s$ respectively. If we take $r = $ then we will never get any LBBness. If we take the Taylor-Hood elements (i.e. $r \ge 2, s = r - 1$) then we are guaranteed to always get LBBness. $s = 0, r = 1$ is not LBB. $r = 2, s = 0$ is good.
:::

### Convergence estimate for Taylor-Hood elements

As stated before, the most general formula to determine Taylor-Hood elements is $\mathbb{P}^{k+1}$ for velocity and $\mathbb{P}^k$ for pressure with $k \ge 2$.

Since Taylor-Hood elements are LBB, they allow for both stability and convergence.

For Taylor-Hood elements, it holds that

$$

$$

## Navier-Stokes equation

The Navier-Stokes equations are just unsteady Stokes equations with an extra nonlinear convection term:

$$
\begin{cases}
  \frac{\partial \vec{u}}{\partial t} + \sigma \vec{u} - \nu \Delta \vec{u} + \nabla p + (\vec{u} \cdot \nabla) \vec{u} = \vec{f} & \Omega, 0 \lt t \lt T \\
  \operatorname{div} \vec{u} = 0 & \Omega, 0 \lt t \lt T \\
  \vec{u} = \vec{\varphi} & \Gamma_D \\
  \nu \frac{\partial \vec{u}}{\partial \vec{n}} - p \cdot \vec{n} = \psi & \Gamma_N
\end{cases}
$$

We define the **Reynolds number**:

$$
R_e = \frac{|\vec{u}|L}{\nu}
$$

where $L$ is a representative for the length of the considered domain and $U$ is a representative for the speed of the system.

If $R_e \lt \lt 1$, then, the nonlinear term is negligible compared to the other ones so we can use the unsteady Stokes framework, otherwise we must use the Navier-Stokes one. In reality, if $R_e \gt \gt 1$, we are working with turbolent flow and even Navier-Stokes fails to work (we must use turbolence models).

::: {.callout .callout-note title="Reynolds number"}
Basically, $R_e$ is a measurement of the complexity of the phenomenon we are analyzing. If $R_e$ is really high, the phenomenon is really complex and needs to be addressed by really complex models. $R_e$ is the _condition number_ of fluid equations.
:::

# Appendix

## Nabla operator

The **nabla** operator (written as $\nabla$) is used to define multiple types of generalized n-dimensional derivatives.

Formally speaking, this operator is defined as

$$
\nabla = \sum_{i=1}^{d} \vec{u_i} \frac{\partial}{\partial x_i}
$$

This operator can be used to define **divergence**, **gradient**, **curl** (or **rotor**), **laplacian** and **normal derivative**.

All those types of derivatives, taken on a monodimensional domain, correspond to the simple derivative (you can convince yourself that this is true by explicitly computing a few of those in $d = 1$).

### Divergence

Let $\vec{w} : \mathbb{R}^d \to \mathbb{R}^d, d \in \mathbb{N}^+$ then the **divergence operator** applied to $\vec{w}$ is defined as

$$
\operatorname{div}(\vec{w}) = \nabla \cdot \vec{w} = 
\left( \sum_{i=1}^d \vec{u_i} \frac{\partial}{\partial x_i} \right) \cdot \left( \sum_{j = 1}^d \vec{u_j} w_j \right) = \sum_{i=1}^d \sum_{j_1}^d (\vec{u_i} \cdot \vec{u_j}) \frac{\partial}{\partial x_i} w_j = \sum_{i = 1}^d \frac{\partial w_i}{\partial x_i} \in \mathbb{R}
$$

### Gradient

Let $v : \Omega \sub \mathbb{R}^d \to \mathbb{R}, d \in\mathbb{N}^+$, then the **gradient** of $v$ is defined as

$$
\operatorname{grad}(\vec{v}) = \nabla v = \sum_{i = 1}^{d} \vec{u_i} \frac{\partial}{\partial x_i} v = \begin{bmatrix}
  \frac{\partial v}{\partial x_1} \\
  \frac{\partial v}{\partial x_2} \\
  \vdots \\
  \frac{\partial v}{\partial x_d} \\
\end{bmatrix} \in \mathbb{R}^d
$$

Let $\vec{x} \in \Omega$ then $\nabla v(\vec{x})$ gives the direction of steepest ascent. If $\nabla v(\vec{x}) = 0$ then $\vec{x}$ can be either a local maximum, a local minimum or a saddle point.

### Laplacian

Let $\vec{v} : \Omega \sub \mathbb{R}^d \to \mathbb{R}, d \in \mathbb{N}^+$, then the **laplacian** of $v$ is defined as

$$
\Delta \vec{v} = \nabla \cdot (\nabla v) = \left( \sum_{i=1}^{d} \vec{u_i} \frac{\partial}{\partial x_i} \right) \cdot \left( \sum_{j = 1}^{d} \vec{u_j} \frac{\partial \vec{v}}{\partial x_i} \right) = \sum_{i = 1}^d (\vec{i_j} \cdot \vec{u_j}) \frac{\partial}{\partial x_i} \frac{\partial v}{\partial x_i} = \sum_{i = 1}^d \frac{\partial^2 v}{\partial x_i^2}
$$

### Notes

A few equalities hold:

$$
\left[(\vec{u} \cdot \nabla) \vec{u} \right]_i = \sum_j u_j \frac{\partial}{\partial x_j} u_i \qquad \nabla \vec{u} : \nabla \vec{v} = \sum_{i, j} \frac{\partial u_i}{\partial x_j} \frac{\partial u_j}{\partial x_i}
$$

## Normal derivative

Let $v : \Omega \sub \mathbb{R}^d \to \mathbb{R}$, then the **normal derivative** of $v$ is defined as

$$
\frac{\partial \vec{v}}{\partial n} = \nabla \vec{v} \cdot n
$$

where $n$ is the normal direction of $v$ in each point.

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

A form $a$ is called **weakly coercive** if

$$
\exists \lambda \ge 0, \alpha \gt 0 : a(v, v) + \lambda \|v\|_{L^2}^2 \ge \alpha \|v\|_V^2
$$

A form $A$ is called **continuous** if

$$
\exists M \gt 0 : |a(u, v)| \le M \|u\|_V \|v\|_V
$$

A form $A$ is called **positive definite** if

$$
a(v, v) \gt 0
 $$

## Functionals

A **functional** is a relation $f : V \mapsto \mathbb{R}$ (where $V$ is a function space).

A functional $F$ is called **linear** if

$$
F(\lambda u + \mu v) = \lambda F(u) + \mu F(v)
$$

A functional $F $ is called **bounded** if

$$
\exist C \gt 0 : |F(v)| \le C\|v\|_V
$$

## Cauchy-Schwarz inequality

Let $u, v \in V$, then

$$
\left|\int_\Omega uv\right| \le \|u\|_{L^2(\Omega)} \|v\|_{L^2(\Omega)} \\
\left|\int_\Omega u'v'\right| \le \|u'\|_{L^2(\Omega)} \|v'\|_{L^2(\Omega)} \\
\left|\int_\Omega u'v\right| \le \|u'\|_{L^2(\Omega)} \|v'\|_{L^2(\Omega)} \\
$$

In $\mathbb{R}^n$, the inequality can be expressed as

$$
\left( \sum_i x_i y_j \right)^2 \le \left( \sum_i x_i^2 \right) \left( \sum_i y_i^2 \right)
$$

## Young inequality

Let $A, B \in \mathbb{R}$ then

$$
AB \le \varepsilon A^2 + \frac{1}{4\varepsilon}B^2 \qquad \forall \varepsilon \gt 0
$$

In particular, for $\varepsilon = \frac{1}{2}$, then

$$
AB \le \frac{A^2}{2} + \frac{B^2}{2}
$$

---
title: "Quantum Physics"
author: 
- "Andrea Oggioni"
---

# Introduction

[This](https://www.youtube.com/watch?v=Wzc0rCniHag) is the only introduction you need. In this document we will explain both the theory and the math behind quantum behaviors and the experiments that prove that they are not just dreams of scientists on drugs but that they are real and verifiable.

In this document, the following two assumptions will be made:

- there are no massless paticles;
- relativistic effects are ignored.

A good "popular" introduction to relativistic effects can be found [here](https://www.youtube.com/watch?v=Y-W-w8yNiKU).

# Fundamentals

We will now give the fundamental tools and concepts to understand more advanced topics.

## Wave-particle duality

The **wave-particle duality** is at the foundation of the entire field of quantum mechanics. Hoc can you prove that a particle is, in fact, a particle? Consider the following setup, where a beam of particles if thrown into a 50/50 beam-splitter:

![Image from author](assets/beamsplit-5050.png)

Since the beam-splitter is a 50/50 one (this also works with any generic type of beam-splitter), both detectors $A$ and $B$ will measure about the same numbers of particles. If, isntead of a beam, a single particle at a time is thrown into the setup, only one of the two detector will fire for each particle, with 50/50 probability (**randomness**). This means that the particle is effectively a particle that moves along a trajectory and that does not propagate like a wave.

Consider now the **Mach-Zender interferometer**:

![Image from author](assets/mach-zender.png)

Intuitively, sending a beam of particles inside this setup would make both detectors measure 50% beam intensity, but this does not happen. Assuming that both the upper path and the lower path are exactly the same length, detector $A$ will always measure 100% of the beam intensity.

Sending a particle at a time shows the same result (therefore no interference between different particles).

The mathematic modellization of the Mach-Zender interferometer will be presented [here](#translation-operator).

The Mach-Zender interferometer demonstrates that each particle also propagates like a wave, taking both the upper and the lower paths at the same time (**superposition**), interfering with itself at the second beam-splitter.

## Superposition

We saw with the Mach-Zender interferometer that a particle is also a wave, so it cannot be described with the usual **state of motion** tuple (position and velocity) like we do in classical mechanics, therefore a new kind of algebra is needed.

Let $|u\rangle$ be the **quantum state** (the equivalent of the state of motion) describing a particle.

Just as with the classical counterpart, knowing the quantum state of a particle at a give time, unless irreversible operations like [measurements](#measurements) are performed, is enough information to let us compute the quantum state of the same particle forever in the past and in the future.

$|u\rangle$ belongs to the Hilbert (i.e. a vector space with a scalar product) space $\mathcal{H}$ that contains all the possible quantum states for the particle. Since $\mathcal{H}$ is an Hilbert space, three operations are defined:

- $(+): \mathcal{H} \times \mathcal{H} \to H \overset{\Delta}{=} |u\rangle> + |v\rangle \mapsto |u+v\rangle$
- $(\cdot): \mathbb{C} \times \mathcal{H} \to \mathcal{H} \overset{\Delta}{=} \alpha |v\rangle \mapsto |\alpha v\rangle$
- $\langle \cdot | \cdot \rangle: \mathcal{H} \times \mathcal{H} \to \mathbb{C}$

The first operation is called the **superposition operation**, the third one is the **bracket**.

::: {.callout .callout-property title="Properties of Hilbert operators"}
Since $\mathcal{H}$ is an Hilbert space, a few properties holds:

- $\langle v | \alpha u \rangle = \alpha \langle v | u \rangle$, $\langle \alpha v | u \rangle = \alpha^* \langle v | u \rangle$
- $\langle v | u \rangle = \langle u | v \rangle^*$
- $\langle v | u + w \rangle = \langle v | u \rangle + \langle v | w \rangle$
- $\langle v | v \rangle \in \mathbb{R}$, $\langle v | v \rangle \ge 0$, $\langle v | v \rangle = \langle v | v \rangle^*$
- $\|v\| = \sqrt{\langle v | v \rangle}$
:::

A generic state can be represended as a weighted sum (a.k.a. **superposition** or **linear combination**) of other states. Weights are complex numbers.

Any superposition of valid states is another valid state for the system.

$$
|u\rangle = \sum_{j=1}^n \alpha_j |v_j\rangle \qquad |u\rangle, |v_j\rangle \in \mathcal{H}, \alpha_j \in \mathbb{C}
$$

::: {.callout .callout-example title="Schr&ouml;dinger's cat"}
The notorious Schr&ouml;dinger's cat is in a superposition of "being alive" and "being dead" at the same time:

$$
|cat\rangle = |alive\rangle + |dead\rangle
$$

If the cat is really sick, it is possible that

$$
|cat\rangle = \frac{1}{10}|alive\rangle + 100i|dead\rangle
$$
:::

## Measurements

A **measurement** is the act of extracting information about the quantum system we are working with. A measurement is always performed against an **observable**, i.e. a measurable quantity or property of the system.

A measurement is an irreversible operation: whenever a measurement is performed, the state collapses to one of the **proper states** of the observed quantity.

This means that a quantum experiment is generally not repeatable: if multiple measurements in the same conditions must be performed, the only way to do so is to create multiple replicas of the same experiments.

![Alpha - not Schr&ouml;dinger's cat but it'll work anyway.](assets/cat.png)

::: {.callout .callout-example title="Schr&ouml;dinger's cat"}
The _aliveness_ of the cat is a binary observable property that has two proper states (i.e. "alive" and "dead").

When we measure the cat aliveness, we can only observe if it is dead or alive, not that it is "42% alive".

When we perform the measure (e.g. looking at the cat), the system collapse to one of its proper state (i.e. "alive cat") and when we look away, the system starts evolving from that proper state.
:::

The probability of observing a specific observable value when performing a measurement depends on the quantum state of the system.

Let $|O_j\rangle$ be the quantum state in which a measurement on a given observable will always yield $O_j$ as a result and $|u\rangle$ the quantum state of the system we are working with, then

$$
|u\rangle = \sum_{j=1}^n \alpha_j |O_j\rangle
$$

The various $|O_j\rangle$s are therefore all proper states.

::: {.callout .callout-note title="Invariance w.r.t. multiplication by a phase factor"}
The same information encoded in $|u\rangle$ is also the one encoded in both $e^{i\varphi}|u\rangle$ and $A|u\rangle$. They all describe the exact same quantum state.
:::

The $\alpha_j$ coefficient are enough to determine the state of a quantum systemm therefore knowing $\alpha = [\alpha_1, \alpha_2, \dots]^T$ at a specific time allows us to determine the same vector forever in the past and in the future (unless irreversible operations are performed).

The probability of observing a specific value $O_j$ when performing a measurement is given by the Born rule.

::: {.callout .callout-definition title="Born rule"}
The **Born rule** gives the prorability of observing a given value $O_j$ when performing a measurement on $|u\rangle$:

$$
P(O = O_j) = \frac{a_j^* a_j}{\langle u | u \rangle} = \frac{}{}
$$
:::

Proper states are mutually exclusive: we cannot measure $O = 3.14$ and $O = 2.71$ at the same time. Let $|O_i\rangle, |O_j\rangle$ be two normalized proper states, then

$$
\langle O_i | O_j \rangle = \delta_{ji} = \begin{cases}
    0 & i \ne j \\
    1 & i = j
\end{cases}
$$

::: {.callout .callout-note title="Copenhagen interpretation"}
The Born rule comes from the Copenhagen interpretation of quantum mechanics. The _why_ quantum physic works like it does is still a mystery today and various _interpretations_ (hypothesis) exist and the Copenhagen one is one of those.

An interpretation is **consistent** when it does not break probability laws (i.e. when the probability of measuring _something_ when oerforming a measurement is 1):

$$
\sum_{j=1}^n P(O = O_j) = 1
$$

The Copenhagen interpretation is **consistent**.
:::

Let $|u\rangle$ be a quantum state, then $\alpha_j = \langle O_j | u \rangle$.

::: {.collapsible title="Proof"}
$$
\begin{align*}
    \langle O_j | u \rangle &= \langle O_j | \left( \sum \alpha_j | O_j \rangle \right) \\
    &= \langle O_j | \left( \alpha_1 | O_1 \rangle + \alpha_2 | O_2 \rangle + \dots \right) \\
    &= \langle O_j | \alpha_1 | O_1 \rangle + \langle O_j | \alpha_2 | O_2 \rangle + \dots \\
    &= \alpha_1 \langle O_j | O_1 \rangle + \alpha_2 \langle O_j | O_2 \rangle + \dots \\
    &= \alpha_1
\end{align*}
$$
:::

Since a measurement is a stochastic process, we can define the **expectation value** of an observable

$$
\langle O \rangle = \sum O_j \cdot P(O_j)
$$

and the measurement **variance**

$$
\sigma_O^2 = \sum (O_j - \langle O \rangle)^2 P(O_j)
$$

## Vector representation

_Vector representation is not to be confused with "position representation", "energy representation" et similia. The former is used to indicate the "data structure" used in the computations, the latters are used to indicate the "basis" for those data structures. This will become clear later._

To operate numerically with quantum states, we should choose an appropriate Hilbert space that is isomorphic to the space of quantum states allowable by the quantum system we are working with. $\{0\}$ and $\mathbb{C}^n \forall n \in \mathbb{N}$ are all good examples of Hilbert spaces that can be used to describe quantum systems. Vectors used to describe the state of a quantum system are called **statevector**s.

Orthogonal states must be associated to orthogonal vectors.

::: {.callout .callout-example title="Schr&ouml;dinger's cat"}
Schr&ouml;dinger's cat is both alive and dead at the same time. We associate

$$
|alive\rangle \to \begin{bmatrix} 1 \\ 0 \end{bmatrix} \qquad |dead\rangle \to \begin{bmatrix} 0 \\ 1 \end{bmatrix}
$$

therefore

$$
|cat\rangle = |alive\rangle + |dead\rangle \to \begin{bmatrix} 1 \\ 0 \end{bmatrix} + \begin{bmatrix} 0 \\ 1 \end{bmatrix} = \begin{bmatrix} 1 \\ 1 \end{bmatrix}
$$

It is always good to keep statevectors normalized to simplify algebra:

$$
|cat\rangle \to \frac{1}{\sqrt{2}} \begin{bmatrix} 1 \\ 1 \end{bmatrix}
$$

In this document we will frequently use the equal sign ($=$) instead of the association arrow ($\to$) to make syntax less cumbersome.
:::

The set of vectors associated to proper states must be a basis for the chosen Hilbert space, therefore multiple representations can coexist at the same time. Let

$$
|u\rangle = \sum \alpha_j |O_j\rangle = \sum \beta_j |v_j\rangle
$$

then 

$$
\begin{align*}
    \langle O_k | \sum \alpha_j | O_j \rangle &= \langle O_k | \sum \beta_i | v_i \rangle \\
    \sum \alpha_j \langle O_k | O_j \rangle &= \sum \beta_i \langle O_k | O_i \rangle \\
    \alpha_k &= \sum_i \langle O_k | v_i \rangle \beta_i
\end{align*}
$$

therefore, if $\alpha = [\alpha_1, \alpha_2, \dots]^T$ and $\beta = [\beta_1, \beta_2, \dots]^T$ then

$$
\begin{align*}
    \alpha = \mathcal{U}\beta &\qquad\qquad \mathcal{U} = [u_{ki}] = [\langle O_k | v_i \rangle] \\
    \beta = \mathcal{W} \alpha &\qquad\qquad \mathcal{W} = \mathcal{U}^T
\end{align*}
$$

Both $\mathcal{U}$ and $\mathcal{W}$ are unitary.

::: {.collapsible title="Proof"}
$$
\beta = \mathcal{W} \alpha = \mathcal{WU} \beta \implies \mathcal {WU} = \mathbb{I}
$$
:::

While $(+)$ and $(\cdot)$ are quite intuitive when using a complex vector space to describe quantum systems, the braket operator may not be so trivial:

$$
\langle u | v \rangle = \sum_{ji} \alpha_j^* \beta_i \delta_{ji} = \sum_j \alpha_j^* \beta_j
$$

_Vector representation will be the default for discrete systems for the remaining of this document._

## Discrete vs continuous systems

What we have seen so far was just for discrete systems (in the sense that observables can only assume quantized values). In reality, everything can be adapted to work for continuous systems with a few, intuitive tweaks.

Usually, it is just a matter of using integrals instead of sums and probability density functions instead of probabilities.

The quantum state describing a system can be written as

$$
|u\rangle = \int \alpha(\xi) |O(\xi)\rangle d\xi
$$

The statevector containing all the coefficients is now called **statefunction** (or, for a position observable, **wave function**):

$$
\psi(x) \overset{\Delta}{=} \langle O(x) | u \rangle
$$

The bracket is computed as

$$
\begin{align*}
    \langle u | v \rangle &= \int \alpha^*(\xi) \beta(\xi) d\xi \\
    \langle u | u \rangle &= \int |\alpha(\xi)| d\xi \\
    \langle O(\xi) | O(\eta) \rangle &= \delta(\xi - \eta) \\
\end{align*}
$$

$\delta$ is the continuous extension of the Dirac delta we've used before:

$$
f(\eta) = \int f(\xi) \delta(\xi - \eta) 
$$

Observables probabilities are now computed with

$$
f(\xi) = Pdf(O(\xi)) = \frac{|\alpha(\xi)|}{\langle u | u \rangle} \\
f(\xi_0) = \lim_{\Delta \xi \to 0} \frac{P(\xi_0 \lt \xi \lt \xi + \Delta \xi)}{\Delta \xi} \\
P(\xi_1 \lt \xi \lt \xi_2) = \int_{\xi_1}^{\xi_2} f(\xi) d\xi
$$

_This representation will be the default for continuous systems for the remaining of this document._

# Operators

Changes to a quantum systems are represented by changes in the corresponding statevector. Said changes are encoded in linear functions that act on the quantum state and return the modified quantum state.

## Commutators

We denote with $[\hat O_1, \hat O_2]$ the commutator of the two operators $\hat O_1$ and $\hat O_2$.

Two operators are said to **commute** if and only if $\hat O_1 \hat O_2 = \hat O_2 \hat O_1$. In this case, the order of application of the two operators does not matter.

::: {.callout .callout-property title="Properties of commuting operators"}

Two operators commute if and only if they share a common set of eigenvalues

::: {.collapsible title="Proof"}
We will now prove that if two operators share the same set of eigenvalues, they commute.

Let $|u\rangle$ belong to the common set of eigenstates, then

$$
\hat A \hat B |u\rangle = \hat A(B|u\rangle) = B(\hat A|u\rangle) = BA|u\rangle = AB|u\rangle = A(\hat B|u\rangle) = \hat B(A|u\rangle) = \hat B \hat A|u\rangle
$$

therefore $\hat B \hat A = \hat A \hat B$.

We will now prove that if two operators commute, they share the same set of eigenvalues

Let $\hat A$ and $\hat B$ be commuting operators without degenerate eigenvalues and $|u\rangle$ be one of their eigenstates, then

$$
\hat B|u\rangle = B|u\rangle \implies \hat A \hat B |u\rangle = \hat A B |u\rangle
$$

Since $\hat A$ and $\hat B$ commute, then $\hat A \hat B = \hat B \hat A$. Let $|v\rangle = \hat A|u\rangle$ so that $\hat B \hat A |u\rangle = B|v\rangle$ but there is only one eigenstate for a given eigenvalue so $|v\rangle = A|u\rangle$ therefore $\hat A|v\rangle = A|v\rangle$
:::
:::

Commutation between operators is an equivalence relation.

Let $\{\hat A, \hat B, \hat C, \dots\}$ be a set of operators such that if two operators are in that set, they commute. We denote one of the shared eigenstates with $|a_n, b_m, c_o, \dots \rangle$ to clearly indicate that

$$
\begin{cases}\begin{align*}
    \hat A |a_n, b_m, c_o, \dots \rangle &= a_n |a_n, b_m, c_o, \dots \rangle \\
    \hat B |a_n, b_m, c_o, \dots \rangle &= b_m |a_n, b_m, c_o, \dots \rangle \\
    \hat C |a_n, b_m, c_o, \dots \rangle &= c_o |a_n, b_m, c_o, \dots \rangle \\
    &\dots
\end{align*}\end{cases}
$$

## Operators associated to observables

If an operator is linear and also **hermitian** (a.k.a. **self adjoint**, i.e. it holds that $\langle u | \hat O v \rangle = \langle \hat O v | u \rangle$) then it is associated with an observable.

We can write the following eigenequation

$$
\hat O |u \rangle = O |u \rangle
$$

The solution of said eigenequation yields all possible observables $O$ (from now on, **eigenvalues**) associated with the respective proper states (from now on **eigenstates**).

An eigenvalue may be degenerate: in such a case, when that value is measured, the system will collapse in a superposition of the eigenstates associated to that eigenvalue.

If we knew all the possible eigenvalues and the associated eigenstates, any operator associated to an observable property can be described as

$$
\hat O = \sum O_j |O_j \rangle\langle O_j| \\
\hat O = \int O(\xi) |O(\xi) \rangle \langle O(\xi)| d\xi
$$

::: {.callout .callout-property title="Hermitianity test"}
Given an operator $\hat A$, then

$$
\exists \hat A^\dagger : \langle v | A u \rangle = \langle \hat A^\dagger v | u \rangle
$$

If $\hat A$ is hermitian, then $\hat A = \hat A^\dagger$.

To check if $\hat A$ it sufficies to chech whether $\langle v | \hat A u \rangle = \langle \hat A v | u \rangle$.

This is equivalent to check whether $A = A^H$.
:::

Since eigenvalues correspond to a measurable quantity, they must always be real (and they are).

::: {.collapsible title="Proof"}
Let $|u\rangle$ be an eigenvector for $\hat O$ that is associated with the eigenvalue $O$, then

$$
\langle u | (\hat O | u \rangle) = \langle u | (O | u \rangle) = O \langle u | u \rangle \\
\langle u | (\hat O | u \rangle) = \langle \hat O u | u \rangle = \langle O u | u \rangle = O \langle u | u \rangle
$$

therefore it must be true that $O = O^*$, that is equivalent to say that $\Im\{O\} = 0$.
:::

A really important relation exists between operators and expectation values:

$$
\langle O \rangle = \frac{\langle u | \hat O | u \rangle}{\langle u | u \rangle}
$$

::: {.collapsible title="Proof"}
Let $u\rangle = \sum \alpha_j |O_j\rangle$ and assume it is normalized (otherwise divide everyhting by $\langle u | u \rangle$), then

$$
\langle O \rangle = \sum_j O_j P(O_j) = \sum_j O_j |\alpha_j|^2 = \sum o_j \alpha_j^* \alpha_j = \sum_j O_j \langle u | O_j \rangle \langle O_j | u \rangle = \langle u | \left( \sum_j O_j | O_j \rangle \langle O_j | \right) u \rangle = \langle u | \hat O | \rangle
$$
:::

::: {.callout .callout-example title="Ammonia molecule"}
An ammonia molecule (NH<sub>3</sub>) is shaped like a tetrahedron with one atom at each vertex. Consider the plane described by the three hydrogen atoms and say that it is perpendicular to the Z axis, with the origin for said axis where it intersect the plane.

The nitrogen atom can be found either above the plane at position $+Z_0$ or below, at position $-Z_0$.

We associate this observable quantity to two orthogonal quantum states:

$$
|+Z_0\rangle = \begin{bmatrix} 1 \\ 0 \end{bmatrix} \qquad |-Z_0\rangle = \begin{bmatrix} 0 \\ 1 \end{bmatrix}
$$

The operator associated with the relative position of the nitrogen atom is

$$
\hat Z = Z_0 \begin{bmatrix} 1 \\ 0 \end{bmatrix} \begin{bmatrix} 1 & 0 \end{bmatrix} + (-Z_0) \begin{bmatrix} 0 \\ 1 \end{bmatrix} \begin{bmatrix} 0 & 1 \end{bmatrix} = \begin{bmatrix} Z_0 & 0 \\ 0 & -Z_0 \end{bmatrix}
$$

We now define the **reflection operator** $\hat R$ that, when applied to the ammonia molecule, it flips it

$$
\begin{cases}
    \hat R |+Z_0\rangle = |-Z_0\rangle \\
    \hat R |-Z_0\rangle = |+Z_0\rangle \\
\end{cases} \implies \hat R = \begin{bmatrix}
    0 & 1 \\ 1 & 0
\end{bmatrix}
$$

Since this is an hermitian operator, it is associated to some observabe property of the system that, in this case, is the parity of the molecule.

Its two eigenstates are **gerade**

$$
\lambda_g = 1 \qquad |g\rangle = \begin{bmatrix}\frac{1}{\sqrt{2}} \\ \frac{1}{\sqrt{2}}\end{bmatrix} \qquad \hat R |g\rangle = |g\rangle
$$

and **ungerade**

$$
\lambda_u = -1 \qquad |u\rangle = \begin{bmatrix}\frac{1}{\sqrt{2}} \\ -\frac{1}{\sqrt{2}}\end{bmatrix} \qquad \hat R |u\rangle = -|u\rangle
$$


We now introdice the energy observable (that will be discussed in detail later) associated with the operator $\hat E$. It is safe to assume that the molecule energy does not care about the orientation of the molecule, so

$$
\hat E \hat R |v\rangle = E \hat R |v\rangle \implies\hat R^{-1}\hat E\hat R |v\rangle = E|v\rangle = \hat E|v\rangle \implies \hat E \hat R = \hat R \hat E
$$

hence, $\hat R$ and $\hat E$ commutes.

Since $\hat E$ is associated with an observable, it is hermitian, therefore

<!-- TODO: why is this true? -->

$$
\hat E = \begin{bmatrix} a & b \\ b & a \end{bmatrix}
$$

The eigenvalues of $\hat E$ are $E = \pm b + a$. We take $a = 0$ to make the eigenvalues symmetric, therefore

$$
\hat E = \frac{\Delta E}{2}\begin{bmatrix}
    0 & 1 \\ 1 & 0
\end{bmatrix}
$$

Note that energy is quantized!

Usually, the lower energy state is associated with the gerade state and the higher one is associated with ungerade.

Assume that we let the molecule rest enough time to reach the gerade state, the expectation value of the orientation of the molecule in the gerade state can be computed as

$$
\langle Z \rangle = \langle g | \hat Z | g \rangle = g^H \cdot \hat Z \cdot g = 0
$$

<!-- TODO: is this representationally correct??? -->

This means that we have an equal probability to find the particle in any orientation.

We can reach this same conclusion, for example, by calculating the probabilities for the particle to be oriented in a specific way:

$$
P(Z_0) = \frac{|\langle +Z_0 | g \rangle|^2}{\langle g | g \rangle} = |\langle +Z_0 | g \rangle|^2 = \left| \begin{bmatrix} 1 & 0 \end{bmatrix}\begin{bmatrix}\frac{1}{\sqrt{2}} \\ \frac{1}{\sqrt{2}}\end{bmatrix}\right|^2 = \frac{1}{2}
$$
:::

# Heisenberg uncertainty principle

Given two operators $\hat A$ and $\hat B$ with the standard deviation associated to the measurement of the associated observables $\sigma_A, \sigma_B$, the Heisenberg uncertainty principle states that

$$
\sigma_A\sigma_B \ge \left|\frac{i}{2} \langle u | [\hat A, \hat B]|u\rangle \right|
$$

This meas that, in a joined measurement of $A$ and $B$, it is impossible to get bot standard deviations low, no matter the precision of the instruments, this uncertainty is intrinsicly built into quantum mechanics: the lowe is one, the higher the other one.

::: {.collapsible title="Proof"}
Let $|u\rangle$ be normalized, then

$$
\begin{align*}
    \sigma_A^2 &\overset{\Delta}{=} \langle u | (\hat A - \langle A \rangle)^2)|u\rangle = \langle u | \hat A^2 + \langle A \rangle^2 - 2 \hat A \langle A \rangle |u \rangle \\
    &= \langle u | \hat A^2 | u \rangle + \langle A \rangle^2\langle u|u \rangle - 2 \langle A \rangle \langle u|\hat A|u \rangle \\
    &= \langle u|\hat A^2|u \rangle - \langle A \rangle^2 \\
    &= \langle u|\hat A^2 - \langle A \rangle^2|u \rangle
\end{align*}
$$

Let $\hat A' = \hat A - \langle A \rangle$ so that $\sigma_A^2 = \sigma_{A'}$, then

<!-- This proof does not make any sense! -->
$$
\begin{align*}
    \sigma_A^2 \sigma_B^2 = \sigma_{A'} \sigma_{B'} &=\langle u |\hat A'^2| u \rangle \langle u | \hat B'^2| u \rangle = \langle u|\hat A' \hat A'|u \rangle\langle u|\hat B' \hat B'|u \rangle \\
    &= \langle \hat A' u | \hat A' u \rangle \langle \hat B' u | \hat B' u \rangle \overset{(1)}{\ge} |\langle \hat A' u | \hat B ' u \rangle|^2 \\
    &= \left[ \Re\{\langle \hat A'u | \hat B'u \rangle \} \right]^2 + \left[ \Im\{\langle \hat A'u | \hat B'u \rangle \} \right]^2 \\
    &\ge \left[ \Im\{\langle \hat A'u | \hat B'u \rangle \} \right]^2 \overset{(2)}{=} \left[ \frac{i}{2} \left(\langle \hat A'u | \hat B'u \rangle - \langle \hat B'u | \hat A'u \rangle\right) \right] \\
    &= \left[ \frac{i}{2} \left( \langle u|\hat A' \hat B'|u \rangle - \langle u|\hat B' \hat A'|u \rangle \right) \right]^2 \\
    &= \left[ \frac{i}{2} \left( \langle u | \hat A' \hat B' - \hat B' \hat A' | u \rangle \right) \right]^2 \\
    &\overset{(3)}{=} \left[ \frac{i}{2} \left( \langle u | [\hat A, \hat B] | u \rangle \right) \right]^2
\end{align*}
$$

where in $(1)$ we used the Cauchy-Schwarz inequality ($\langle v|v \rangle \langle u|u \rangle \ge |\langle v|u \rangle|^2$), in $(2)$ we used the fact that $\Im\{c\} = -i \frac{c - c^*}{2}$ and in $(3)$ we used the fact that $[\hat A, \hat B] = [\hat A', \hat B']$.

The thesis follows.
:::

# Complex systems

A complex system is a system in which we consider multiple degrees of freedom. It may be a system composed of multiple particles or a system in which we look at different properties of the same particle (or both).

To describe a complex system, quantum states needs to belong to a tensor product of multiple Hilbert spaces, one for each particle/properties observed: a combined state may be expressed as 

$$
|u\rangle_1|v\rangle_2 \in \mathcal{H}_1 \otimes \mathcal{H}_2
$$

::: {.callout .callout-property title="Property of tensor product"}
- $\alpha |\cdot\rangle_1 + \beta |\cdot\rangle_2 \in \mathcal{H}_1 \otimes \mathcal{H}_2$
- $\langle (|u\rangle_1 |v\rangle_2) | = \langle u|_1 \langle v|_2$
- $(\langle u|_1 \langle v|_2)(|w\rangle_1 |y\rangle_2) = \langle u|w \rangle_1 \langle v|y \rangle_2$
:::

A composite state is said to be **entangled** if it cannot be expressed as a product of states, all belonging to different Hilbert spaces (otherwise it is said to be **disentangled**).

::: {.callout .callout-example title="Entangled states"}
Let

$$
|u\rangle = (|T\rangle_1 + |H\rangle_1)(|T\rangle_2 + |H\rangle_2) \\
|v\rangle = |T\rangle_1 |T\rangle_2 + |H\rangle_1 |H\rangle_2
$$

then, the former is disentangled and the latter is entangled.
:::

The most general form to be used to express a generic complex system is

$$
|w\rangle_{1 \otimes 2} = \sum_{ji} \gamma_{ji} |O_j\rangle_1 |O_i\rangle_2
$$

while, if the system is disentangled, it can be expressed as

$$
|w\rangle_{1 \otimes 2} = \sum_{ji} \alpha_i \beta_j |O_i\rangle_1 |O_j\rangle_2
$$

It is possible to apply composite operators to composite systems: each operator is only applied to components of the state belonging to the same Hilbert space it belongs to.

If $|u\rangle = |a\rangle_1 |b\rangle_2$ then

$$
\hat O_1 \hat O_2 |u\rangle = (\hat O_1|a\rangle_1)(\hat O_2|b\rangle_2)
$$

It is possible to compute the probability of a joined measurement: let $|w\rangle$ be a general complex system composed of two degrees of freedom, then the probability of measuring $O_j$ for the first degree of freedom and, at the same time, $O_i$ for the second one is

$$
P = \frac{\|\gamma_{ji}\|}{\langle w|w \rangle}
$$

Let $|w\rangle = \sum_{ji} \gamma_{ji} |O_j\rangle_1 |O_i\rangle_2$ and $|y\rangle = \sum_{kl} \delta_{kl} |O_k\rangle_1 |O_l\rangle_2$, then

$$
\langle w|y \rangle = \sum_{jikl} \gamma_{ji}^* \delta_{kl} \langle O_j|O_k \rangle_1 \langle O_i|O_l \rangle_2 = \sum_{ji} \gamma_{ji} \delta_{ji}
$$

Continuous extension of compisite system use multidimensional integrals.

Compisite systems can be represented using vectors and the **Kroneker product** as tensor product.

# Momentum and position operators

Position and velocity are the basic properties to describe the state of motion of a particle. In quantum mechanics we use, instead of velocity, the momentum, due to some difficulties in handling velocities in complex systems.

Both the momentum operator and the position operators operates on the **position representation** of a quantum state, meaning that, the quantum state must be written like 

$$
|u\rangle = \int \psi(x) |O(x)\rangle dx
$$

where the statefunction $psi$ is the **wave function** of the system (i.e. one of the functions whose square norm is the probability density function of the position distribution).

Let $f(x) = |\psi(x)|^2$, then

$$
\langle x \rangle = \langle \psi(x) | \hat X | \psi(x) \rangle =  \int x f(x) dx = \int \psi^*(x) x \psi(x) dx = \int \psi^*(x) \hat X \psi(x) dx
$$

therefore, the position operator is just a "multiplication by scalar" operator:

$$
\hat X \psi = x \psi
$$

The n-dimensional exptension is trivial ($\hat{\vec{X}} = \hat X \cdot \vec{u_x} + \hat Y \cdot \vec{u_y} + \dots$).

Deriving the momentum operator is more difficult: a proof is provided below, for now just say that it can be written as

$$
\hat P_x = -i \hbar\frac{\partial}{\partial x} \qquad \hbar = 1.054 \cdot 10^{-34} Js
$$

::: {.collapsible title="Two-hours-long proof"}

I'm not joking, this proof is very long. It took me 1 hour and 27 minutes just to write it all down here, you can imagine how much time I spent trying to fully understand it, and I still do not think I have fully grasped everything. This is the fourth time I've rewritten this, finding new errors every time. This is probably the longest proof here on TiTilda.

We will now demonstrate and derive the momentum operator. The proof will be articulated in multiple steps for better clarity:

1. properties of an isolated system;
2. translation and momentum commute;
3. translation eigenstates;
4. distribution of translation eigenvalues;
5. momentum eigenvalues and operator derivation;
6. proof that momentum is hermitian.

**Properties of an isolated system**

We know that 

$$
F_x = ma_x = m \frac{dv_x}{dt} = \frac{d}{dt}(m v_x) = \frac{d}{dt}P_x
$$

and that

$$
F_x = -\frac{\partial V}{\partial x}(x)
$$

therefore we can state that if $F_x = 0$ (i.e. we are looking at an isolated system), it means that $P_x$ is constant and that the system is invariant w.r.t. translations.

**Translation and momentum operators commute**

Let $\hat T_R$ be the operator associated with translations

$$
\hat T_R \psi(x) \overset{\Delta}{=} \psi(x - R)
$$

and assume we already know how the momentum operator $\hat P$ works, then

$$
\hat P_x \varphi(x) = P_x \varphi(x)
$$

As momentum is invariant w.r.t. translations, then

$$
\begin{align*}
    \hat P_x(\hat T_R \varphi(x)) &= P_x(\hat T_R \varphi(x)) \\
    \hat T_{-R} \hat P_x \hat T_R \varphi(x) &=\hat T_{-R} P_x \hat T_R \varphi(x) \\
    &= P_X \hat T_{-R} \hat T_R \varphi(x) \\
    &= P_x \varphi(x) \\
    \hat P_x \hat T_R &= \hat T_R \hat P_x
\end{align*}
$$

hence, the translation operator and the translation operator commute, therefore they share the same set of eigenstates.

**Translation eigenvalues**

Intuitively, two translations $\hat T_R$ and $\hat T_{R'}$ commute, so they share the same set of eigenstates. Let $\alpha(x)$ be one of the tranlsation eigenvalues, then

$$
\hat T_{R + R'} \varphi(x) = \varphi(x - R - R') = \alpha(R + R') \varphi(x)
$$

and

$$
\hat T_{R + R'} \varphi(x) = \hat T_{R'} \hat T_R \varphi(x) = \hat T_{R'} \varphi(x - R) = \alpha(R) \hat T_{R'} \varphi(x) = \alpha(R) \varphi(x - R') = \alpha(R) \alpha(R') \varphi(x)
$$

from which, it must be true that

$$
\alpha(R + R') = \alpha(R) + \alpha(R')
$$

We also have to impose that translation does not vary the norm of the wave function:

$$
\begin{align*}
    1 \overset{!}{=} \int_{-\infty}^{+\infty} |\varphi(x)|^2 dx = \int_{-\infty}^{+\infty} |\hat T_R \varphi(x)|^2 dx &= \int_{-\infty}^{+\infty} |\varphi(x - R)|^2 dx = \int_{-\infty}^{+\infty} |\varphi(x')|^2 dx \\
    1 \overset{!}{=} \int_{-\infty}^{+\infty} |\varphi(x)|^2 dx = \int_{-\infty}^{+\infty} |\hat T_R \varphi(x)|^2 dx &= \int_{-\infty}^{+\infty} |\alpha(R) \varphi(x)|^2 dx = \int_{-\infty}^{+\infty} |\alpha(R)|^2|\varphi(x)|^2 dx = |\alpha(R)|^2 \int_{-\infty}^{+\infty} |\varphi(x)|^2 = |\alpha(R)|^2
\end{align*}
$$

from which $|\alpha(R)|^2 = 1$ regardless of the translation amount $R$.

One possible function that satisfies those requirements is

$$
\alpha(R) = e^{ikR} \qquad k \in \mathbb{R}
$$

Given a translation amount $R$, the associated operator $\hat T_R$ has an infinite amount of eigenvalues, indexed with the $k$ value.

**Distribution of translation eigenvalues** <!-- TODO: Hic sunt leones! - ci sono errori nella dimostrazione, capire. Dal paragrafo dopo, psi e phi sembrano wave functions, qui sembrano eigenvalues, come si fa? -->

Let $\psi_k(x) \overset{\Delta}{=} \varphi_k(x - R) = \hat T_R \varphi_k(x) = e^{-ikR} \varphi_k(x)$. This means that $\varphi_k(x) = \psi_k(x) e^{ikR}$ and that $\varphi_k(x - R) = \psi_k(x - R) e^{ikR}$.

From here, using substitution, we can write that

$$
\psi_k(x - R) e^{ik(x - R)} = e^{-ikR} \psi_k(x) e^{ikR}
$$

from which it follows that

$$
\psi_k(x - R) = \psi_k(x)
$$

regardless of the translation amount $R$, therefore $\psi_k$ must be a constant regardless of the $k$ index.

It follows that

$$
\varphi_k(x) = Ce^{ikx}
$$

where $C$ is the normalization constant.

Let $L$ be the size of the domain (e.g. th length of the laboratory). If we impose normalization, 

$$
\int_{-\frac{L}{2}}^{+\frac{L}{2}} |\varphi_k(x)|^2 \overset{!}{=} 1
$$

we get that 

$$
C = \frac{1}{\sqrt{L}}
$$

::: {.callout .callout-note title="Infinite domain"}
Techically, taking $L = \infty$ is possible but it breaks the algebra, so we assume that $L \lt +\infty$.
:::

We can now write that

$$
\hat P_x \varphi_k(x) = \frac{1}{\sqrt{L}} \hat P_x e^{ikx} = \frac{1}{\sqrt{L}} P_x e^{ikx}
$$

therefore, for a well defined momentum, each eigenstate is equally possible.

**Momentum eigenvalues and operator derivation**

We know that $P_x$ should be a function of $k$ and that it does not depend on the mass of the particle. Consider two non-interacting (a.k.a. disengangled) particles with momentums $P_{x_1}(k_1)$ and $P_{x_2}(k_2)$ respectively, then we associate the two momentum eigenvalues with the corresponding translation eigenvalues:

<!-- TODO: WHAT??? -->

$$
P_{x_1}(k_1) \to \varphi_{k_1}(x_1) = \frac{1}{\sqrt{L}} e^{i k_1 x_1} \\
P_{x_2}(k_2) \to \varphi_{k_2}(x_2) = \frac{1}{\sqrt{L}} e^{i k_2 x_2}
$$

We define

$$
\varphi_{tot}(x_1, x_2) = \varphi_{k_1}(x_1) \varphi_{k_2}(x_2) = \frac{1}{L} e^{i(k_1 x_1 + k_2 x_2)}
$$

If we apply a translation of the total $\varphi$, we get that

$$
\hat T_R \varphi_{tot}(x_1, x_2) = \varphi_{tot}(x_1 - R, x_2 - R) = \frac{1}{L} e^{i(k_1 + k_2)R} e^{i(k_1 x_1 + k_2 x_2)} = e^{-i k_{tot} R} \varphi(x_1, x_2)
$$

where $k_{tot} = k_1 + k_2$.

This means that the eigenvalues for the translation operator in a two-particle system depends only on the $k$ indices.

We know that momentum sums so $P_{tot}(k_1 + k_2) = P_{x_1}(k_1) + P_{x_2}(k_2)$, therefore $P$ must be the proportionality function.
It can be demonstrated that the proportionality constant is  the **reduced Plank constant**:

$$
\hbar = 1.054 \cdot 10^{-34} Js
$$

Finally, since

$$
\hat P_x = e^{ikx} = \hbar k e^{ikx}
$$

then

$$
\hat P_x = -i \hbar \frac{\partial}{\partial x}
$$

**Proof that momentum is hermitian**

To check whether $\hat P_x$ is hermitian, we must check if $\langle \hat P_x \psi | \varphi \rangle = \langle \psi | \hat P_x \varphi \rangle$.

$$
\begin{align*}
    \langle \psi | \hat P_x \varphi \rangle &= \int_{-\infty}^{+\infty} \psi^*(x) \left( -i \hbar \frac{\partial}{\partial x} \right) \varphi(x) dx \\
    &= -i \hbar \int_{-\infty}^{+\infty} \psi^*(x) \frac{\partial}{\partial x} \varphi(x) dx \\
    &= -i \hbar \left[ \underbrace{\left(\psi^*(x) \varphi(x)\right)_{-\infty}^{+\infty}}_{(1)} - \int_{-\infty}^{+\infty} \left( \frac{\partial}{\partial x} \psi^*(x) \right) \varphi(x) dx\right] \\
    &= \int_{-\infty}^{+\infty} \underbrace{\left( i \hbar \frac{\partial}{\partial x} \psi^*(x) \right)}_{(2)} \varphi(x) dx \\
    &= \langle \hat P_x \psi | \varphi \rangle
\end{align*}
$$

where $(1) = 0$ otherwise we would have had a non-sensical diverging integral, and $(2) = (-\hat P_x \psi^*) = (\hat P_x \psi)^*$.

Do we say QED? Nah, [this](https://sciencehumor.io/math-memes/how-to-properly-end-a-proof-ldj8) is better.
:::

Just as for the position operator, also the n-dimensional extension of the momentum operator is trivial:

$$
\hat{\vec{P}} = -i \hbar \vec{\nabla}
$$

Momentum and position operators do not commute: $[\hat{\vec{P}}_x, \hat X] = -i \hbar \ne 0$ therefore, according to the [Heisenberg uncertainty principle](#heisenberg-uncertainty-principle) we can say that

$$
\sigma_{P_x} \sigma_x \ge \frac{\hbar}{2}
$$

Knowing that $v = \frac{P}{m}$ then, we can also say that

$$
\sigma_{v_x} \sigma_x = \frac{\hbar}{2m}
$$

::: {.collapsible title="Proof"}
$$
\begin{align*}
    [\hat P_x, \hat X] \psi(x) &= \hat P_x(x\psi(x)) - x \hat P_x \psi(x) \\
    &=-i \hbar \frac{\partial}{\partial x}(x \psi(x)) + i \hbar x \frac{\partial}{\partial x} \psi(x) \\
    &= -i \hbar \left( \psi(x) + x \frac{\partial}{\partial x} \psi(x) \right) + i \hbar x \frac{\partial}{\partial x} \psi(x) \\
    &= -i \hbar \left( 1 + x \frac{\partial}{\partial x} \right) \psi(x) \\
    &= -i\hbar \psi(x)
\end{align*}
$$

therefore

$$
[\hat P_x, \hat X] = -i \hbar \ne 0
$$
:::

Since $\hbar$ is really small, the Heisenberg uncertainty principle is not a too strict of a limitation, even for systems as small as $1\r{A}$. This is exactly the reason why classical mechanics work. It is so big that quantum uncertainty is really negligible.

# Translation operator

The translation operator was already introduced in the two-hours-long proof in the previous section. We will now analyze it in more detail.

We saw that the translation operator is used to, well, translate stuff:

$$
\hat T_R \psi(x) = \psi(x - R) = \psi(x) \left.\frac{\partial \psi}{\partial x}\right|_{R = 0} + O(R)
$$

Since

$$
\frac{\partial \psi}{\partial x} = \frac{i \hat P_x}{\hbar} \psi
$$

then

$$
\psi(x - R) = \sum_{j = 0}^\infty \frac{1}{j!}\left( -\frac{i}{\hbar} \hat P_x R \right)^j \psi(x) = \underbrace{e^{\frac{-i\hat P_x R}{\hbar}}}_{\hat T_R} \psi(x)
$$

::: {.callout .callout-example title="Modellization of the Mach-Zender interferometer using the Translation operator"}
Assume we have a Mach-Zender interferometer like the one in the image below.

![](./assets/mach-zender.png)

We describe the beam as a quantum superposition of "entering from the top of the beam-splitter" or "from the right".

$$
|T\rangle = \begin{bmatrix} 1 \\ 0 \end{bmatrix} \qquad
|B\rangle = \begin{bmatrix} 0 \\ 1 \end{bmatrix}
$$

Assume that the beam-splitters are both 50/50.

The beam-splitter effect on the beam is represented by the following operator

$$
\hat B = \frac{1}{\sqrt{2}}\begin{bmatrix}
    1 & 1 \\ 1 & -1
\end{bmatrix}
$$

One can easily verify that, wherever the beam is coming from, it is always split in two equally intense beams.

In the setup depicted above, particles that exit the first beamsplit from the right enters the second from the top, so they are still described by $|T\rangle$. A similar reasoning can be performed for the other particles and $|B\rangle$.

Particles following the upper paths ($|T\rangle$) are translated by $L_{top}$ (hence, $\hat T^{top} = \hat T_{L_{top}}$), while the other ones ($|B\rangle$) are translated by $L_{bot}$ (hence, $\hat T^{bot} = \hat T_{L_{bot}}$).

$$
\hat T = \begin{bmatrix}
    \hat T^{top} & 0 \\ 0 & \hat T^{bot}
\end{bmatrix}
$$

The overll Mach-Zender interferometer can then be expressed as

$$
\psi_{fin} = \hat B \hat T \hat B \psi_{in}
$$

Assuming that the starting beam comes from the left and enters to the right of the first beam-splitter, then $\psi_{in} = [0, 1]^T$, therefore

$$
\begin{align*}
    \psi_{fin} &= \hat B \hat T \hat B \psi_{in} = \frac{1}{\sqrt{2}}\begin{bmatrix}
        1 & 1 \\ 1 & -1
    \end{bmatrix}
    \begin{bmatrix}
        e^{\frac{-i P L}{\hbar}} & 0 \\
        0 & e^{\frac{-i P L}{\hbar}}
    \end{bmatrix}
    \frac{1}{\sqrt{2}}\begin{bmatrix}
        1 & 1 \\ 1 & -1
    \end{bmatrix} \begin{bmatrix}
        0 \\ 1
    \end{bmatrix} \\
    &= \frac{1}{2} \begin{bmatrix}
        1 & 1 \\ 1 & -1
    \end{bmatrix}
    \begin{bmatrix}
        e^{\frac{-i P L}{\hbar}} & 0 \\
        0 & e^{\frac{-i P L}{\hbar}}
    \end{bmatrix} \begin{bmatrix}
        1 \\ -1
    \end{bmatrix} \\
    &= \frac{1}{2} \begin{bmatrix}
        1 & 1 \\ 1 & -1
    \end{bmatrix}
    \begin{bmatrix}
        e^{\frac{-i P L}{\hbar}} \\
        -e^{\frac{-i P L}{\hbar}}
    \end{bmatrix} \\
    &= \frac{1}{2} \begin{bmatrix}
        0 \\
        2e^{\frac{-i P L}{\hbar}}
    \end{bmatrix} \\
    &= \begin{bmatrix}
        0 \\ e^{\frac{-i P L}{\hbar}}
    \end{bmatrix}
\end{align*}
$$

As you can see, in the end, we get the same beam, just translated.
:::

_To be continued._



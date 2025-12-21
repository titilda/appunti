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

## Wave-particle duality

The **wave-particle duality** is at the foundation of the entire field of quantum mechanics. Hoc can you prove that a particle is, in fact, a particle? Consider the following setup, where a beam of particles if thrown into a 50/50 beam-splitter:

![Image from author](assets/beamsplit-5050.png)

Since the beam-splitter is a 50/50 one (this also works with any generic type of beam-splitter), both detectors $A$ and $B$ will measure about the same numbers of particles. If, isntead of a beam, a single particle at a time is thrown into the setup, only one of the two detector will fire for each particle, with 50/50 probability (**randomness**). This means that the particle is effectively a particle that moves along a trajectory and that does not propagate like a wave.

Consider now the **Mach-Zender interferometer**:

![Image from author](assets/mach-zender.png)

Intuitively, sending a beam of particles inside this setup would make both detectors measure 50% beam intensity, but this does not happen. Assuming that both the upper path and the lower path are exactly the same length, detector $A$ will always measure 100% of the beam intensity.

Sending a particle at a time shows the same result (therefore no interference between different particles).

The mathematica modellization of the Mach-Zender interferometer will be presented in the corresponding paragraph. <!-- TODO -->

The Mach-Zender interferometer demonstrates that each particle also propagates like a wave, taking both the upper and the lower paths at the same time (**superposition**), interfering with itself at the second beam-splitter.

## Superposition

We saw with the Mach-Zender interferometer that a particle is also a wave, so it cannot be described with the usual **state of motion** tuple (position and velocity) like we do in classical mechanics, therefore a new kind of algebra is needed.

Let $|u\rangle$ be the **quantum state** (the equivalent of the state of motion) describing a particle.

Just as with the classical counterpart, knowing the quantum state of a particle at a give time, unless irreversible operations like [measurements](#measurements) are performed, is enough information to let us compute the quantum state of the same particle forever in the past and in the future.

$|u\rangle$ belongs to the Hilbert space $\mathcal{H}$ that contains all the possible quantum states for the particle. Since $\mathcal{H}$ is an Hilbert space, three operations are defined:

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

A generic state can be represended as a weighted sum of other states. Weights are complex numbers.

$$
|u\rangle = \sum_{j=1}^n \alpha_j |v_j\rangle \qquad |u\rangle, |v_j\rangle \in \mathcal{H}, \alpha_j \in \mathbb{C}
$$

::: {.callout .callout-example title="Schr&ouml;dinger's cat"}
The notorious Schr&ouml;dinger's cat is in a superposition of "being alive" and "being dead" at the same time:

$$
|cat\rangle = |alive\rangle + |dead\rangle
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

# Operators

## Operators associated to observables

_To be continued._



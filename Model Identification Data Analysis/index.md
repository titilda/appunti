---
title: "Model Identification Data Analysis"
author:
  - "Andrea Lunghi"
---

## Stochastic Process

A **stochastic process** (SP) is a mathematical model that describes the evolution of a system over time, where the system's behavior is influenced by random factors. It can be thought of as an infinite sequence of random variables ($\text{sp}:x(t, s)$), all defined on the same probabilistic space.

The process is defined by two variables, t and s:

- **Time Index (t)**: represents the time at which the process is observed.
- **Outcome (s)**: represents the outcome of the random experiment that influences the process.

By fixing one of the two variables, we can get different perspectives on the process:

- By fixing s, we get a **deterministic signal** that evolves over time, representing a specific realization of the process.
- By fixing t, we get a **random variable** that represents the distribution of outcomes at that specific time point, which can change over time as the process evolves.

Different realizations of the same SP are considerate _stochastically equivalent_ if they have the same probability distribution.

by fixing s we get a deterministic signal, by fixing t we get a random variable, based on the outcome of the experiments (the random variable might change during the time).

Different realizations of the same SP they are _stochastically equivalent_.

To describe a SP need to know how the probability distribution evolves during the time t. Making it to complicated, we need to know the probability distribution of the random variable at each time t.

### Wide-sense characterization

A complete description of an SP requires the knowledge of the probability distribution of the random variable at each time t, which can be very complex. However, a simpler description is possible with **Wide-Sense Characterization** that only requires the knowledge of:

- **Mean**, the average behavior: $m(t) = \mathbb{E}_x[x(t, s)] = \int_\pi x(t, s) * pdf(s) ds$
- **Covariance Function**, describes the correlation between different points in time: $\gamma(t_1, t_2) = \mathbb{E}[(x(t_1, s) - m(t_1)) (x(t_2, s) - m(t_2))]$
  - **Variance**, the variability of the process at a specific time point: when $t_1 = t_2$, the covariance function reduces to the variance of the process at that time point: $\gamma(t_1, t_1) = \mathbb{E}[(x(t_1, s) - m(t_1))^2]$.
  - **Correlation Function**, describes the correlation between two points in time: $\mathbb{E}[x(t_1, s) * x(t_2, s)]$

### Stationary Stochastic Process

**Stationary Stocastic Process** (SSP) is a subclass of the SP where its statistical properties do not change over time.

- The mean is constant along the time ($m(t) = m, \forall t$)
- The covariance function only depends on the time difference $\tau = t_2 - t_1$ and not on the specific time points $t_1$ and $t_2$: $\gamma(t_1, t_2) = \gamma(t_1, t_1 + \tau) = \gamma(\tau)$, meaning that the correlation between two points in time only depends on how far apart they are, and not on their specific location in time.

**Properties of $\gamma(\tau)$:**

- **Positivity**: $\gamma(0) = \mathbb{E}[(x(t, s) - m)^2] \geq 0$
- **Boundedness**: $|\gamma(\tau)| \leq \gamma(0), \forall t \geq 0$
- **Symmetry** (even function): $\gamma(\tau) = \gamma(-\tau), \forall t$

> e.g. If temperature were a stationary process, today's temperature would tell us a lot about tomorrow's (high correlation, small $\tau$), but very little about the temperature in 100 days (low correlation, large $\tau$). The relationship between days remains the same regardless of whether we are looking at January or July.

#### White Noise

A **White Noise** (WN) is a purely unpredictable SSP where there is no correlation between different samples:

A process $e(t)$ is a white noise ($e(t) ~ \text{WN}(\mu, \lambda^2)$) if it satisfies the following properties:

- **Constant Mean**: $m = \mathbb{E}[e(t)] = \mu$
- **Constant Variance**: $\mathbb{E}[(e(t_1) - \mu)^2] = \lambda^2$
- **Zero Covariance**: $\gamma_e(\tau) = \mathbb{E}[(e(t_1) - \mu)(e(t_2) - \mu)] = 0, \forall t \neq 0$

#### Moving Average Processes

A **Moving Average Process** (MA) is a type of stochastic process that is defined as a linear combination of white noise terms.

$$y(t) = \sum_{i=0}^{n} b_i e(t - i)$$

where $e(t)$ is a white noise process, $b_i$ are the coefficients of the white processes, and $n$ is the order of the moving average process.

> Proof that $y(t)$ is a stationary process:
>
> $$m_y(t) = \mathbb{E}[y(t)] = \mathbb{E}[\sum_{i=0}^{n} b_i * e(t - i)]$$
>
> Since the the sum is a linear operation, we can exchange the sum with the expectation, and since $b_i$ are constants, we get:
>
> $$m_y(t) = \sum_{i=0}^{n} b_i * \mathbb{E}[e(t - i)]$$
>
> Since $e(t)$ is a white noise process, $\mathbb{E}[e(t - i)] = \mu$, so:
>
> $$m_y(t) = \sum_{i=0}^{n} b_i * \mu = \mu * \sum_{i=0}^{n} b_i$$
>
> This shows that the mean of $y(t)$ is constant over time, which is a property of stationary processes.
>
> ---
>
> To show that the covariance function of $y(t)$ only depends on the time difference $\tau$, we can compute the covariance function $\gamma_y(\tau)$:
>
> $$\gamma_y(\tau) = \mathbb{E}[(y(t) - m_y)(y(t + \tau) - m_y)]$$
>
> Substituting the definition of $y(t)$, we get:
>
> $$\gamma_y(\tau) = \mathbb{E}[(\sum_{i=0}^{n} b_i e(t - i) - m_y)(\sum_{j=0}^{n} b_j e(t + \tau - j) - m_y)]]$$
>
> Expanding the product, we get:
>
> $$\gamma_y(\tau) = \mathbb{E}[\sum_{i=0}^{n} \sum_{j=0}^{n} b_i b_j e(t - i) e(t + \tau - j) - m_y \sum_{i=0}^{n} b_i e(t - i) - m_y \sum_{j=0}^{n} b_j e(t + \tau - j) + m_y^2]$$
>
> Since the expectation is a linear operation, we can exchange the expectation with the sum, and since $b_i$ are constants, we get:
>
> $$\gamma_y(\tau) = \sum_{i=0}^{n} \sum_{j=0}^{n} b_i b_j \mathbb{E}[e(t - i) e(t + \tau - j)] - m_y \sum_{i=0}^{n} b_i \mathbb{E}[e(t - i)] - m_y \sum_{j=0}^{n} b_j \mathbb{E}[e(t + \tau - j)] + m_y^2$$
>
> Since $e(t)$ is a white noise process, $\mathbb{E}[e(t - i)] = m_y$, so we can simplify the expression to:
>
> $$\gamma_y(\tau) = \sum_{i=0}^{n} \sum_{j=0}^{n} b_i b_j \mathbb{E}[e(t - i) e(t + \tau - j)] - m_y^2 - m_y^2 + m_y^2$$
>
> Since $e(t)$ is a white noise process, $\mathbb{E}[e(t - i) e(t + \tau - j)] = \lambda^2$ if $t - i = t + \tau - j$ (i.e., if $j = i + \tau$), and $0$ otherwise. Therefore, we can simplify the expression to:
>
> $$\gamma_y(\tau) = \sum_{i=0}^{n} b_i b_{i + \tau} \lambda^2 - m_y^2$$
>
> This shows that the covariance function of $y(t)$ only depends on the time difference $\tau$, which is another property of stationary processes. Therefore, we can conclude that $y(t)$ is a stationary process.

In this case the covariance function $\gamma_y(\tau)$ is non-zero only for $\tau$ values between $-n$ and $n$, which means that the process has a finite memory of $n$ time steps, meaning that the process is _colored_ between $-n$ and $n$, and _white_ otherwise.

The generalization is with an infinite order ($n \to \infty$), where the process can have an infinite memory, and the covariance function can be non-zero for all $\tau$ values.

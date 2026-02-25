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

### White Noise

A **White Noise** (WN) is a purely unpredictable process where there is no correlation between different samples:

A process $e(t)$ is a white noise ($e(t) ~ \text{WN}(\mu, \lambda^2)$) if it satisfies the following properties:

- **Constant Mean**: $m = \mathbb{E}[e(t)] = \mu$
- **Constant Variance**: $\mathbb{E}[(e(t_1) - \mu)^2] = \lambda^2$
- **Zero Covariance**: $\gamma_e(\tau) = \mathbb{E}[(e(t_1) - \mu)(e(t_2) - \mu)] = 0, \forall t \neq 0$

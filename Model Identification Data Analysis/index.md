---
title: "Model Identification Data Analysis"
author:
  - "Andrea Lunghi"
---

## Stochastic Process

A **stochastic process** (SP) is a mathematical model that describes the uvolution of ausystem ovestationary, where the system's behavior is influenced by random factors. It can be thought of as an infinite sequence of random variables ($\text{sp}:x(t, s)$), all defined on the same probabilistic space.

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
- **Covariance Function**, describes the correlation between different points in time: $\gamma(t_1, t_2) = \mathbb{E}[(x(t_1, s) - m(t_1)) (x(t_2, s) - m(t_2))]$.
  - **Variance**, how data are spread around the mean ($t_1 = t_2$): $\sigma^2(t) = \gamma(t, t) = \mathbb{E}[(x(t, s) - m(t))^2]$
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

where $e(t)$ is a white noise process (for simplicity the mean will be 0), $b_i$ are the coefficients of the white processes, and $n$ is the order of the moving average process.

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
> $$\gamma_y(\tau) = \sum_{i=0}^{\max(0, n-|\tau|)} b_i b_{i + \tau} \lambda^2 - m_y^2$$
>
> This shows that the covariance function of $y(t)$ only depends on the time difference $\tau$, which is another property of stationary processes. Therefore, we can conclude that $y(t)$ is a stationary process.

In this case the covariance function $\gamma_y(\tau)$ is non-zero only for $\tau$ values between $-n$ and $n$, which means that the process has a finite memory of $n$ time steps, meaning that the process is _colored_ between $-n$ and $n$, and _white_ otherwise.

The generalization is with an infinite order ($n \to \infty$), where the process can have an infinite memory, and the covariance function can be non-zero for all $\tau$ values.

#### Autoregressive Processes

An $MA(n)$ is easy to compute but has "finite memory", an $MA(\infty)$ is more general but not practical.

An **Autoregressive Process** (AR) is a type of stochastic process that is defined as a linear combination of its own past values and a white noise term. This allow to shape $\gamma(\tau)$ for all $\tau$ values, with a finite number of parameters.

$$y(t) = \sum_{i=1}^{m} a_i y(t - i) + e(t)$$

where $e(t)$ is a white noise process , $a_i$ are the coefficients of the past values, and $m$ is the order of the autoregressive process.

##### Iterative Substitution

Considering the simplest case of an AR(1) process: $y(t) = a*y(t-1) + e(t)$. It's possible to substitute the expression for $y(t-1)$ into the equation for $y(t)$, and repeat this process until we get an expression for $y(t)$ that only depends on the white noise terms:

$$y(t) = \sum_{i=0}^{n} a^i e(t-i) + a^{n+1} y(t-n-1)$$

Where:

- The first term (external excitation) is a linear combination of the white noise terms, which is a moving average process.
- The second term (initial condition) only affect the initial transient, and it goes to zero as $n \to \infty$ if $|a| < 1$ (stability condition).

This is an AR(1), but is able to create a MA($\infty$) with $c_i = a^i$ coefficients, which means that the process can have an infinite memory, but is less expressive than a general MA($\infty$) since the coefficients are constrained to be powers of $a$.

##### Transfer Function

To handle AR Processes, we can use the concept of **transfer function**, which is a mathematical representation of the relationship between the input and output of a system in the frequency domain.

To derive the transfer function of an AR process, we need the **shift operator** $z^{i}$, which shifts the signal by one time step:

$$y(t - i) = z^{-i} y(t)$$

- If $i$ is posistive, the shift operator shifts the signal to the right (forward).
- If $i$ is negative, the shift operator shifts the signal to the left (backward).

**Properties of the shift operator:**

- **Linearity**: $z^{i} (a * y(t) + b * x(t)) = a * z^{i} y(t) + b * z^{i} x(t)$
- **Recursiveness**: $z^{i} z^{j} y(t) = z^{i+j} y(t)$
- **Linear composition**: $z^{i} y(t) = y(t - i)$

This allows us to rewrite the AR process in a more compact form:

$$y(t) = \frac{1}{1 - \sum_{i=1}^{m} a_i z^{-i}} e(t) = \underbrace{\frac{1}{A(z)} e(t)}_{\text{Noise Response}} = W(z) e(t)$$

Where:

- $m$: is the order of the autoregressive process, which represents the number of past values that influence the output.
- $a_i$ are the coefficients of the past values, which represent the influence of past values on the current output.
- $A(z)$ is the denominator polynomial, which roots represent the poles of the system.
- $W(z)$ is the _discrete-time transfer function_ of the system.

The stability condition for the AR process is that all poles of $W(z)$ must be within the unit circle in the complex plane, which ensures that the AR process is stationary.

It's possible to interconnect transfer functions:

- **Series Connection**: $W(z) = W_1(z) * W_2(z)$
- **Parallel Connection**: $W(z) = W_1(z) + W_2(z)$

#### ARMA Processes

An **Autoregressive Moving Average Process** (ARMA(m, n)) is a type of stochastic process that combines the properties of both autoregressive and moving average processes. It is defined as a linear combination of its own past values, a white noise term, and a linear combination of past white noise terms.

$$y(t) = \sum_{i=1}^{m} a_i y(t - i) + \sum_{j=0}^{n} c_j e(t - j)$$
$$y(t) = \underbrace{\frac{C(z)}{A(z)} e(t)}_{\text{Noise Response}}$$

- $n$ is the order of the moving average part, which represents the number of past white noise terms that influence the output.
- $c_j$ are the coefficients of the past white noise terms
- $C(z)$ is the numerator polynomial, which roots represent the zeros of the system.
- $e(t - j)$ are the past white noise terms

#### ARMAX Processes

An **Autoregressive Moving Average Exogenous Process** (ARMAX(m,n,k,p)) is a type of stochastic process that combines the properties of autoregressive, moving average, and exogenous processes, meaning that it includes an external input term in addition to the autoregressive and moving average components.

$$y(t) = \sum_{i=1}^{m} a_i y(t - i) + \sum_{j=0}^{n} c_j e(t - j) + \sum_{l=0}^{p} b_l u(t - k - l)$$
$$W(z) =  \underbrace{\frac{C(z)}{A(z)} e(t)}_{\text{Noise Response}} + \underbrace{\frac{B(z)}{A(z)} z^{-k} u(t)}_{\text{Exogenous Input Response}}$$

- $k$ is the pure input/output delay of the exogenous input, which represents the time it takes for the exogenous input to affect the output.
- $p$ is the order of the exogenous part, which represents the number of past exogenous input terms that influence the output.
- $b_l$ are the coefficients of the past exogenous input terms
- $u(t - k - l)$ are the past exogenous input terms, which are delayed by $k$ time steps and have a memory of $p$ time steps.
- $B(z)$ is the numerator polynomial of the exogenous input term, which roots represent the zeros of the system related to the exogenous input.

If there is a change in the input at time $t$, it will affect the output at time $t+k$ and for the next $p$ time steps, meaning that the exogenous input has a delayed effect on the output. The ARMAX model is useful for modeling systems where there is an external input that influences the output, such as in control systems.

The ARMAX transfer function can be interpreted as the parallel connection of two transfer functions, one for the white noise term and one for the exogenous input term.

#### N-ARMAX Processes

An **Nonlinear Autoregressive Moving Average Exogenous Process** (N-ARMAX) is a type of stochastic process that combines the properties of autoregressive, moving average, and exogenous processes, but allows for nonlinear relationships between the past values, white noise terms, and exogenous input terms.

$$y(t) = f(y(t-1), y(t-2), ..., y(t-m), e(t), e(t-1), ..., e(t-n), u(t-k), u(t-k-1), ..., u(t-k-p))$$

where $f$ is a nonlinear function that describes the relationship between the past values, white noise terms, and exogenous input terms.

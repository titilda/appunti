---
title: "Model Identification Data Analysis"
author:
  - "Andrea Lunghi"
---

## Stochastic Process

A **stochastic process** (SP) is a mathematical model that describes the evolution of a system over time, where the system's behavior is influenced by random factors. It can be thought of as an infinite sequence of random variables $x(t, s)$, all defined on the same probabilistic space.

A process is defined by two variables:

- **Time Index $t$**: the time at which the process is observed.
- **Outcome $s$**: the outcome of the random experiment.

By fixing one variable, we get:

- Fixing $s$ gives a **deterministic signal** (a single realization of the process).
- Fixing $t$ gives a **random variable** that represents the distribution at the time $t$ of all the possible outcomes.

Different realizations of the same SP are _stochastically equivalent_.

The description of an SP require the knowledge of the probability distribution at each time, making it too complex to compute.

### Wide-sense characterization

A simpler way to describe a SP is with the **Wide-Sense Characterization** that describe using only:

- **Mean**, the average behavior: $m(t) = \mathbb{E}_x[x(t, s)] = \int_\pi x(t, s) * pdf(s) ds$
- **Covariance Function**, describes the correlation between different points in time: $\gamma(t_1, t_2) = \mathbb{E}[(x(t_1, s) - m(t_1)) (x(t_2, s) - m(t_2))]$.
  - **Variance**, how data are spread around the mean ($t_1 = t_2$): $\sigma^2(t) = \gamma(t, t) = \mathbb{E}[(x(t, s) - m(t))^2]$
  - **Correlation Function**, describes the correlation between two points in time: $\mathbb{E}[x(t_1, s) * x(t_2, s)]$

### Stationary Stochastic Process

A **Stationary Stochastic Process** (SSP) is a subclass of SP where its statistical properties do not change over time.

- The mean is constant along the time ($m(t) = m, \forall t$)
- The covariance function depends only on the time difference $\tau = t_2 - t_1$: $\gamma(t_1, t_2) = \gamma(\tau)$, not on the specific time points $t_1$ and $t_2$.

**Properties of $\gamma(\tau)$:**

- **Positivity**: $\gamma(0) = \mathbb{E}[(x(t, s) - m)^2] \geq 0$
- **Boundedness**: $|\gamma(\tau)| \leq \gamma(0), \forall t \geq 0$
- **Symmetry** (even function): $\gamma(\tau) = \gamma(-\tau), \forall t$

> e.g. If temperature were a stationary process, today's temperature would tell us a lot about tomorrow's (high correlation, small $\tau$), but very little about the temperature in 100 days (low correlation, large $\tau$). The relationship between days remains the same regardless of whether we are looking at January or July.

#### White Noise

A **White Noise** (WN) is a purely unpredictable SSP where there is no correlation between different samples:

A process $e(t)$ is a white noise ($e(t) \sim \text{WN}(\mu, \lambda^2)$) if it satisfies the following properties:

- **Constant Mean**: $m = \mathbb{E}[e(t)] = \mu$
- **Constant Variance**: $\mathbb{E}[(e(t) - \mu)^2] = \lambda^2$
- **Zero Covariance**: $\gamma_e(\tau) = \mathbb{E}[(e(t_1) - \mu)(e(t_2) - \mu)] = 0, \forall \tau \neq 0$

> $$\mathbb{E}[e(t)^2] = \lambda^2 + \mu^2$$
>
> $$\mathbb{E}[e(t) e(t - \tau)] = \mu^2$$

#### Moving Average Processes

A **Moving Average Process** (MA) is a type of stochastic process that is defined as a linear combination of white noise terms.

$$y(t) = \sum_{i=0}^{n} b_i e(t - i)$$

where $e(t)$ is a white noise process (for simplicity the mean will be 0), $b_i$ are the coefficients of the white processes, and $n$ is the order of the moving average process.

> **Proof of stationarity:**
>
> **Mean** (by linearity of expectation):
>
> $$m_y = \mathbb{E}[y(t)] = \sum_{i=0}^{n} b_i \mathbb{E}[e(t-i)]$$
>
> Since $e(t)$ is a white noise has constant mean $\mu$,and the coefficients $b_i$ are constant, we get:
>
> $$\sum_{i=0}^{n} b_i \underbrace{\mathbb{E}[e(t-i)]}_{=\,\mu} = \mu \sum_{i=0}^{n} b_i = \text{const}$$
>
> **Covariance** (using $\mu = 0$ for simplicity):
>
> $$\gamma_y(\tau) = \mathbb{E}[y(t)\,y(t-\tau)] = \sum_{i=0}^{n}\sum_{j=0}^{n} b_i b_j \underbrace{\mathbb{E}[e(t-i)\,e(t-\tau-j)]}_{\lambda^2 \text{ if } j = i-\tau,\; \text{else } 0}$$
>
> $$\gamma_y(\tau) = \lambda^2 \sum_{i=0}^{n - |\tau|} b_i\, b_{i+\tau}$$
>
> Since this depends only on $\tau$ (not on $t$), $y(t)$ is stationary.

In this case the covariance function $\gamma_y(\tau)$ is non-zero only for $\tau$ values between $-n$ and $n$, which means that the process has a finite memory of $n$ time steps, meaning that the process is _colored_ between $-n$ and $n$, and _white_ otherwise.

The generalization is with an infinite order ($n \to \infty$), where the process can have an infinite memory, and the covariance function can be non-zero for all $\tau$ values.

#### Autoregressive Processes

An $MA(n)$ is easy to compute but has "finite memory", an $MA(\infty)$ is more general but not practical.

An **Autoregressive Process** (AR) is a stochastic process defined as a linear combination of its own past values and a white noise term. This allows shaping $\gamma(\tau)$ for all $\tau$ values with a finite number of parameters.

$$y(t) = \sum_{i=1}^{m} a_i y(t - i) + e(t)$$

where $e(t)$ is a white noise process , $a_i$ are the coefficients of the past values, and $m$ is the order of the autoregressive process.

##### Iterative Substitution

Considering the simplest case of an AR(1) process: $y(t) = a \cdot y(t-1) + e(t)$. Substituting $y(t-1)$ recursively:

$$y(t) = \sum_{i=0}^{n} a^i e(t-i) + a^{n+1} y(t-n-1)$$

Where:

- The first term (external excitation) is a linear combination of the white noise terms, which is a moving average process.
- The second term (initial condition) only affect the initial transient, and it goes to zero as $n \to \infty$ if $|a| < 1$ (stability condition).

This is an AR(1), but is able to create a MA($\infty$) with $c_i = a^i$ coefficients, which means that the process can have an infinite memory, but is less expressive than a general MA($\infty$) since the coefficients are constrained to be powers of $a$.

##### Transfer Function

To handle AR Processes, we can use the concept of **transfer function**, which is a mathematical representation of the relationship between the input and output of a system in the frequency domain.

To derive the transfer function of an AR process, we need the **shift operator** $z^{i}$, which shifts the signal by one time step:

$$y(t - i) = z^{-i} y(t)$$

- If $i$ is positive, the shift operator shifts the signal to the right (i.e., into the past).
- If $i$ is negative, the shift operator shifts the signal to the left (i.e., into the future).

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

##### Long Division

To find the coefficients of the moving average representation of an AR process, we can perform long division of the transfer function $W(z)$ by $A(z)$:

$$W(z) = \frac{1}{A(z)} = \frac{1}{1 - \sum_{i=1}^{m} a_i z^{-i}} = \sum_{i=0}^{\infty} c_i z^{-i}$$

Where $c_i$ are the coefficients of the moving average representation of the AR process, which can be computed iteratively by performing long division.

##### AR Mean

The mean of an AR process can be computed by taking the expectation of both sides of the AR equation:

$$m = \mathbb{E}[y(t)] = \sum_{i=1}^{m} a_i \mathbb{E}[y(t - i)] + \mathbb{E}[e(t)]$$

Since the process is stationary, $\mathbb{E}[y(t - i)] = m$, and since $e(t)$ is a white noise process with mean $\mu$, we get:

$$m = \sum_{i=1}^{m} a_i m + \mu$$

Rearranging the equation, we get:

$$\boxed{m = \frac{\mu}{1 - \sum_{i=1}^{m} a_i}}$$

##### AR Variance

> **Lemma:** $\mathbb{E}[e(t)\, y(t - \tau)] = 0$ for all $\tau > 0$.
>
> $y(t-\tau)$ depends only on noise inputs at times $\leq t - \tau$, i.e., $e(t-\tau), e(t-\tau-1), \ldots$
>
>All of these are independent from $e(t)$ by the white noise property. Therefore $e(t)$ and $y(t-\tau)$ are uncorrelated.

Using the AR(1) simplification ($m_y=0$, $\mathbb{E}[e(t)]=0$):

$$\gamma_y(0) = \mathbb{E}[y(t)^2] = \mathbb{E}[(a_1 y(t-1) + e(t))^2]$$

Expanding:

$$\gamma_y(0) = a_1^2 \underbrace{\mathbb{E}[y(t-1)^2]}_{\gamma_y(0)} + \underbrace{\mathbb{E}[e(t)^2]}_{\lambda^2} + 2a_1 \underbrace{\mathbb{E}[y(t-1)\, e(t)]}_{=\,0 \text{ (Lemma)}}$$

$$\gamma_y(0) = a_1^2\, \gamma_y(0) + \lambda^2$$

Solving for $\gamma_y(0)$:

$$\boxed{\gamma_y(0) = \frac{\lambda^2}{1 - a_1^2}}$$

##### AR Covariance

For AR(1) with $m = 0$ and $\tau \geq 1$:

$$\gamma_y(\tau) = \mathbb{E}[y(t)\, y(t-\tau)] = \mathbb{E}[(a_1 y(t-1) + e(t))\, y(t-\tau)]$$

$$= a_1 \underbrace{\mathbb{E}[y(t-1)\, y(t-\tau)]}_{\gamma_y(\tau - 1)} + \underbrace{\mathbb{E}[e(t)\, y(t-\tau)]}_{=\,0 \text{ (Lemma, } \tau \geq 1\text{)}}$$

This gives the recursion:

$$\gamma_y(\tau) = a_1\, \gamma_y(\tau - 1)$$

Applying it repeatedly from $\gamma_y(0)$, and using symmetry $\gamma_y(\tau) = \gamma_y(-\tau)$:

$$\boxed{\gamma_y(\tau) = a_1^{|\tau|}\, \gamma_y(0) = \frac{a_1^{|\tau|}\, \lambda^2}{1 - a_1^2}}$$

This will go to zero as $\tau \to \infty$ if $|a_1| < 1$, which is the stability condition for the AR(1) process.

#### ARMA Processes

An **Autoregressive Moving Average Process** (ARMA(m, n)) is a type of stochastic process that combines the properties of both autoregressive and moving average processes. It is defined as a linear combination of its own past values, a white noise term, and a linear combination of past white noise terms.

$$y(t) = \sum_{i=1}^{m} a_i y(t - i) + \sum_{j=0}^{n} c_j e(t - j)$$
$$y(t) = \underbrace{\frac{C(z)}{A(z)} e(t)}_{\text{Noise Response}}$$

- $n$ is the order of the moving average part, which represents the number of past white noise terms that influence the output.
- $c_j$ are the coefficients of the past white noise terms
- $C(z)$ is the numerator polynomial, which roots represent the zeros of the system.
- $e(t - j)$ are the past white noise terms

##### ARMA Mean

The mean of an ARMA process can be computed by:

$$m_y = \mathbb{E}[y(t)] = \sum_{i=1}^{m} a_i \underbrace{\mathbb{E}[y(t - i)]}_{= m_y} + \sum_{j=0}^{n} c_j \underbrace{\mathbb{E}[e(t - j)]}_{= \mu}$$

That rearranges to:

$$m_y(1 - \sum_{i=1}^{m} a_i) = \mu \sum_{j=0}^{n} c_j$$

$$\boxed{m_y = \frac{\mu \displaystyle\sum_{j=0}^{n} c_j}{1 - \displaystyle\sum_{i=1}^{m} a_i}}$$

> Note: if $\mu = 0$ (zero-mean white noise), then $m_y = 0$ regardless of the coefficients.

##### ARMA Variance

The variance is computed with $m_y = 0$, expand $\gamma_y(0) = \mathbb{E}[y(t)^2]$ by substituting the ARMA definition and grouping into three parts:

$$\gamma_y(0) = \mathbb{E}[(\sum_{i=1}^{m} a_i y(t - i) + \sum_{j=0}^{n} c_j e(t - j))^2]$$
$$ = \underbrace{\sum_{i,i'=1}^{m} a_i a_{i'} \mathbb{E}[y(t-i)\,y(t-i')]}_{\text{(1) AR} \times \text{AR}} + \underbrace{\sum_{j,j'=0}^{n} c_j c_{j'} \mathbb{E}[e(t-j)\,e(t-j')]}_{\text{(2) MA} \times \text{MA}} + \underbrace{2\sum_{i=1}^{m}\sum_{j=0}^{n} a_i c_j \mathbb{E}[y(t-i)\,e(t-j)]}_{\text{(3) AR} \times \text{MA}}$$

Simplifying each term:

**(1) AR×AR:** $\mathbb{E}[y(t-i)\,y(t-i')] = \gamma_y(|i-i'|)$:
$$\sum_{i=1}^{m} a_i^2\, \gamma_y(0) + 2\sum_{i < i'} a_i a_{i'}\, \gamma_y(i'-i)$$

**(2) MA×MA:** White noise is uncorrelated across time, so $\mathbb{E}[e(t-j)\,e(t-j')] = \lambda^2$ if $j = j'$, and 0 otherwise:
$$\lambda^2 \sum_{j=0}^{n} c_j^2$$

**(3) AR×MA:** Since $e(t-j)$ is uncorrelated with $y(t-i)$ for $j \neq i$, the only non-zero contributions come from terms where $j = i$.

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

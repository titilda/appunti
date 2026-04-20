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
  - **Variance**, how data are spread around the mean ($t_1 = t_2$): $\gamma(t, t) = \mathbb{E}[(x(t, s) - m(t))^2]$
- **Correlation Function**, describes the correlation between two points in time: $\tilde{\gamma} = \mathbb{E}[x(t_1, s) * x(t_2, s)]$

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

The variance and the covariance have been calculated using the _unbiased_ version of the WN that removes the mean from the samples. The biased versione would be:

- **Variance**: $\mathbb{E}[e(t)^2] = \lambda^2 + \mu^2$
- **Covariance**: $\mathbb{E}[e(t) e(t - \tau)] = \mu^2$

#### Transfer Function

A stochastic process can be represented using a **transfer function** that describes how the process responds to an input signal. The transfer function is a mathematical representation of the relationship between the input and output of a system in the frequency domain.

It is done by grouping the coefficients using the **shift operator** $z^{i}$, which allows us to rewrite the process in a more compact form:

$$y(t - i) = z^{-i} y(t)$$

**Properties of the shift operator:**

- **Linearity**: $z^{i} (a * y(t) + b * x(t)) = a * z^{i} y(t) + b * z^{i} x(t)$
- **Recursiveness**: $z^{i} z^{j} y(t) = z^{i+j} y(t)$
- **Linear composition**: $z^{i} y(t) = y(t - i)$

It's possible to interconnect transfer functions:

- **Series Connection**: $W(z) = W_1(z) * W_2(z)$
- **Parallel Connection**: $W(z) = W_1(z) + W_2(z)$

#### Moving Average Processes

A **Moving Average Process** ($\text{MA}(n)$) is a type of stochastic process that is defined as a linear combination of white noise terms.

$$\boxed{y(t) = \sum_{i=0}^{n} c_i e(t - i)}$$

Transfer function representation:

$$\boxed{y(t) = \sum_{i=0}^{n} c_i z^{-i} e(t) = \underbrace{C(z) e(t)}_{\text{Noise Response}}}$$

Where:

- $c_i$ are the coefficients of the white processes
- $n$ is the order of the moving average process
- $C(z)$ is the transfer function of the moving average process, which roots represent the zeros of the system

##### MA Mean

$$m_y = \mathbb{E}[y(t)] = \sum_{i=0}^{n} c_i \mathbb{E}[e(t-i)] = \sum_{i=0}^{n} c_i \underbrace{\mathbb{E}[e(t-i)]}_{=\,\mu}$$

Since $e(t)$ is a white noise has constant mean $\mu$, and the coefficients $c_i$ are constant, we get:

$$\boxed{m_y = \mu \sum_{i=0}^{n} c_i}$$

##### MA Covariance

For simplicity we can assume that the white noise has zero mean ($\mu = 0$), which is equivalent to subtracting the mean from the samples. In this case, the covariance function can be computed as:

$$\gamma_y(\tau) = \mathbb{E}[y(t)\,y(t-\tau)] = \sum_{i=0}^{n}\sum_{j=0}^{n} c_i c_j \underbrace{\mathbb{E}[e(t-i)\,e(t-\tau-j)]}_{\lambda^2 \text{ if } j = i-\tau,\; \text{else } 0}$$

$$\boxed{\gamma_y(\tau) = \lambda^2 \sum_{i=0}^{n - |\tau|} c_i\, c_{i+\tau}}$$

The covariance function depends only on the time difference $\tau$, and not on the specific time points $t$ and $t-\tau$, making it a stationary process.

The value of the covariance function $\gamma_y(\tau)$ is non-zero only for $\tau$ values between $-n$ and $n$, which means that the process has a finite memory of $n$ time steps, meaning that the process is _colored_ between $-n$ and $n$, and _white_ otherwise.

The generalization is with an infinite order ($n \to \infty$), where the process can have an _infinite memory_, and the covariance function can be non-zero for all $\tau$ values, making it less pratical.

#### Autoregressive Processes

An **Autoregressive Process** ($\text{AR}(m)$) is a stochastic process defined as a linear combination of its own past values and a white noise term. This allows shaping $\gamma(\tau)$ for all $\tau$ values with a finite number of parameters.

$$\boxed{y(t) = \sum_{i=1}^{m} a_i y(t - i) + e(t)}$$

Transfer function representation:
$$\boxed{y(t) = \frac{1}{1 - \sum_{i=1}^{m} a_i z^{-i}} e(t) = \underbrace{\frac{1}{A(z)} e(t)}_{\text{Noise Response}}}$$

Where:

- $m$: is the order of the autoregressive process, which represents the number of past values that influence the output.
- $a_i$ are the coefficients of the past values, which represent the influence of past values on the current output.
- $A(z)$ is the denominator polynomial, which roots represent the poles of the system.

##### Iterative Substitution

Considering the simplest case of an AR(1) process: $y(t) = a \cdot y(t-1) + e(t)$. Substituting $y(t-1)$ recursively:

$$y(t) = \sum_{i=0}^{n} a^i e(t-i) + a^{n+1} y(t-n-1)$$

Where:

- The first term (external excitation) is a linear combination of the white noise terms, which is a moving average process.
- The second term (initial condition) only affect the initial transient, and it goes to zero as $n \to \infty$ if $|a| < 1$ (stability condition).

This is an AR(1), but is able to create a MA($\infty$) with $c_i = a^i$ coefficients, which means that the process can have an infinite memory, but is less expressive than a general MA($\infty$) since the coefficients are constrained to be powers of $a$.

##### AR Mean

The mean of an AR process can be computed by taking the expectation of both sides of the AR equation:

$$m = \mathbb{E}[y(t)] = \sum_{i=1}^{m} a_i \underbrace{\mathbb{E}[y(t - i)]}_{= m} + \underbrace{\mathbb{E}[e(t)]}_{= \mu}$$

Rearranging the equation, we get:

$$\boxed{m = \frac{\mu}{1 - \sum_{i=1}^{m} a_i}}$$

##### AR Variance

> **Lemma:** $\mathbb{E}[e(t)\, y(t - \tau)] = 0$ for all $\tau > 0$.
>
> $y(t-\tau)$ depends only on noise inputs at times $\leq t - \tau$, i.e., $e(t-\tau), e(t-\tau-1), \ldots$
>
> All of these are independent from $e(t)$ by the white noise property. Therefore $e(t)$ and $y(t-\tau)$ are uncorrelated.

Using the AR(1) simplification ($m_y=0$, $\mathbb{E}[e(t)]=0$):

$$\gamma_y(0) = \mathbb{E}[y(t)^2] = \mathbb{E}[(a_1 y(t-1) + e(t))^2]$$

Expanding:

$$\gamma_y(0) = a_1^2 \underbrace{\mathbb{E}[y(t-1)^2]}_{\gamma_y(0)} + \underbrace{\mathbb{E}[e(t)^2]}_{\lambda^2} + 2a_1 \underbrace{\mathbb{E}[y(t-1)\, e(t)]}_{=\,0 \text{ (Lemma)}}$$

Solving for $\gamma_y(0)$:

$$\boxed{\gamma_y(0) = \frac{\lambda^2}{1 - a_1^2}}$$

##### AR Covariance

For AR(1) with $m = 0$ and $\tau \geq 1$:

$$\gamma_y(\tau) = \mathbb{E}[y(t)\, y(t-\tau)] = \mathbb{E}[(a_1 y(t-1) + e(t))\, y(t-\tau)]$$

$$= a_1 \underbrace{\mathbb{E}[y(t-1)\, y(t-\tau)]}_{\gamma_y(\tau - 1)} + \underbrace{\mathbb{E}[e(t)\, y(t-\tau)]}_{=\,0 \text{ (Lemma, } \tau \geq 1\text{)}}$$

Applying it repeatedly from $\gamma_y(0)$, and using symmetry $\gamma_y(\tau) = \gamma_y(-\tau)$:

$$\boxed{\gamma_y(\tau) = a_1^{|\tau|}\, \gamma_y(0) = \frac{a_1^{|\tau|}\, \lambda^2}{1 - a_1^2}}$$

This will go to zero as $\tau \to \infty$ if $|a_1| < 1$, which is the stability condition for the AR(1) process.

These are the **Yule-Walker equations** that describe the relationship between the coefficients of the AR process and its covariance function.

#### ARMA Processes

An **Autoregressive Moving Average Process** ($\text{ARMA}(m, n)$) is a type of stochastic process that combines the properties of both autoregressive and moving average processes. It is defined as a linear combination of its own past values, a white noise term, and a linear combination of past white noise terms.

$$\boxed{y(t) = \sum_{i=1}^{m} a_i y(t - i) + \sum_{j=0}^{n} c_j e(t - j)}$$

Transfer function representation:
$$\boxed{y(t) = \frac{\sum_{i=0}^{n} c_i z^{-i}}{1 - \sum_{i=1}^{m} a_i z^{-i}} = \underbrace{\frac{C(z)}{A(z)} e(t)}_{\text{Noise Response}}}$$

##### ARMA Mean

The mean of an ARMA process can be computed by:

$$m_y = \mathbb{E}[y(t)] = \sum_{i=1}^{m} a_i \underbrace{\mathbb{E}[y(t - i)]}_{= m_y} + \sum_{j=0}^{n} c_j \underbrace{\mathbb{E}[e(t - j)]}_{= \mu}$$

That rearranges to:

$$\boxed{m_y = \frac{\mu \displaystyle\sum_{j=0}^{n} c_j}{1 - \displaystyle\sum_{i=1}^{m} a_i}}$$

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

$$\boxed{y(t) = \sum_{i=1}^{m} a_i y(t - i) + \sum_{j=0}^{n} c_j e(t - j) + \sum_{l=0}^{p} b_l u(t - k - l)}$$

Transfer function representation:
$$\boxed{W(z) =  \underbrace{\frac{C(z)}{A(z)} e(t)}_{\text{Noise Response}} + \underbrace{\frac{B(z)}{A(z)} z^{-k} u(t)}_{\text{Exogenous Input Response}}}$$

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

### Frequency Domain Analysis

A single realization of a stochastic process can be analyzed in the time domain $y(t)$ or in the frequency domain ($\Gamma_y(\omega)$, spectrum) using the Fourier Transform.

The **power spectral density** (PSD) of a stationary process is:

$$\Gamma_y(\omega) = \mathbb{F}[\tilde{\gamma}_y(\tau)] = \sum_{\tau=-\infty}^{\infty} \tilde{\gamma}_y(\tau) e^{-j \omega \tau}$$
$$\Gamma_y(\omega) = \mathbb{F}[\gamma_y(\tau)] = \sum_{\tau=-\infty}^{\infty} \gamma_y(\tau) e^{-j \omega \tau} + \sum_{\tau=-\infty}^{\infty} m^2 e^{-j \omega \tau}$$

- $\Gamma_y(\omega)$ is the Discrete Fourier Transform of the covariance function $\gamma_y(\tau)$ so doesn't depend on the specific time points or realization of $y(t)$, but only on the time difference $\tau$.

**Properties:**

- $\Gamma_y(\omega)$ is real and non-negative for all $\omega$.
  - Even if is an imaginary function, $\mathbb{I}[\Gamma_y(\omega)] = 0$ for all $\omega$.
- $\Gamma_y(\omega)$ is an even function of $\omega$: $\Gamma_y(\omega) = \Gamma_y(-\omega)$.
- $\Gamma_y(w) = \Gamma_y(w + 2\pi k)$ for any integer $k$ (periodicity).

All interesting frequencies are between $0$ and $\pi$, which is called the **Nyquist frequency**.

#### Spectral Factorization

For a process $y(t) = W(z) e(t)$ where $e(t)$ is white noise, the output spectrum is related to the input spectrum through the transfer function:

$$\Gamma_y(\omega) = |W(e^{j \omega})|^2 \Gamma_e(\omega)$$

If $e(t) \sim \text{WN}(0, \lambda^2)$ (white noise), then $\Gamma_e(\omega) = \lambda^2$ for all $\omega$, and we get:

$$\Gamma_y(\omega) = \lambda^2 |W(e^{j\omega})|^2$$

1. set all z as positive in $W(z)$.
2. find all the roots of the numerator and denominator polynomials.
$$W(z) = \frac{(z - z_1)(z - z_2) \cdots (z - z_n)}{(z - p_1)(z - p_2) \cdots (z - p_m)}$$
3. replace $z$ with $e^{j \omega}$ to get the spectrum:
$$\Gamma_y(\omega) = \lambda^2 \frac{|e^{j \omega} - z_1|^2 |e^{j \omega} - z_2|^2 \cdots |e^{j \omega} - z_n|^2}{|e^{j \omega} - p_1|^2 |e^{j \omega} - p_2|^2 \cdots |e^{j \omega} - p_m|^2}$$
4. $|e^{j \omega} - z|^2 = (e^{j \omega} - z)(e^{-j \omega} - z^*)$ where $z^*$ is the complex conjugate of $z$.
5. using trigonometric identities, we can rewrite the spectrum as:
$$\Gamma_y(\omega) = \lambda^2 \frac{\prod_{i=1}^{n} (1 - 2|z_i| \cos(\omega - \angle z_i) + |z_i|^2)}{\prod_{j=1}^{m} (1 - 2|p_j| \cos(\omega - \angle p_j) + |p_j|^2)}$$

The spectrum can be computed analytically by decomposing $W(z)$ into its pole-zero factorization:

1. **Factor $W(z)$ into pole-zero form:** Express the transfer function in terms of its zeros ($z_i$) and poles ($p_j$):
$$W(z) = \frac{(z - z_1)(z - z_2) \cdots (z - z_n)}{(z - p_1)(z - p_2) \cdots (z - p_m)}$$

2. **Substitute the frequency variable:** Replace $z$ with $e^{j\omega}$ to evaluate the transfer function on the unit circle:
$$\Gamma_y(\omega) = \lambda^2 \frac{|e^{j \omega} - z_1|^2 |e^{j \omega} - z_2|^2 \cdots |e^{j \omega} - z_n|^2}{|e^{j \omega} - p_1|^2 |e^{j \omega} - p_2|^2 \cdots |e^{j \omega} - p_m|^2}$$

3. **Expand the magnitude squared terms:** where $z^*$ is the complex conjugate of $z$:
$$|e^{j \omega} - z_i|^2 = (e^{j \omega} - z_i)(e^{-j \omega} - z_i^*)$$

4. **Convert to real form using trigonometric identities:** $e^{j \omega} = \cos(\omega) + j\sin(\omega)$, apply:
$$|e^{j \omega} - z_i|^2 = 1 - 2|z_i| \cos(\omega - \angle z_i) + |z_i|^2$$

5. **Final spectrum expression:** The result is a real, analytical form:
$$\Gamma_y(\omega) = \lambda^2 \frac{\prod_{i=1}^{n} (1 - 2|z_i| \cos(\omega - \angle z_i) + |z_i|^2)}{\prod_{j=1}^{m} (1 - 2|p_j| \cos(\omega - \angle p_j) + |p_j|^2)}$$

#### Inverse Fourier Transform

It is possible to recover the covariance function from the spectrum:
$$\gamma_y(\tau) = F^{-1}[\Gamma_y(\omega)] = \frac{1}{2\pi} \int_{-\pi}^{\pi} \Gamma_y(\omega) e^{j \omega \tau} d\omega$$

### Canonical Representations

A stationary process can be represented in multiple equivalent ways. The **Canonical Representation** of a process is an unique pair of transfer function $W(z) = \frac{C(z)}{A(z)}$ and white noise $e(t)$ that can generate the process.

This is true if and only if:

- $C(z)$ and $A(z)$ are _monic_ polynomials (leading coefficient is 1, $1 + a_1 z^{-1} + \cdots + a_m z^{-m}$).
- $C(z)$ and $A(z)$ have _null_ relative degree ($\nu = \text{deg}(C) - \text{deg}(A) = 0$).
- $C(z)$ and $A(z)$ are _coprime_ (no common factors).
- $C(z)$ and $A(z)$ are _stable_ (all roots are inside the unit circle).

If the process is not in canonical form, it can be transformed into canonical form by applying a suitable transformation to the transfer function and the white noise.

$$y(t) = \frac{c z^n + c_1 z^{n-1}}{a z^m + a_1 z^{m-1}} e(t), \quad e(t) \sim \mathcal{N}(\mu, \lambda^2)$$

The first thing to do is to make the polynomials monic by dividing both the numerator and denominator by the leading coefficient of the denominator:

$$y(t) = \frac{1 + \frac{c_1}{c} z^{-1}}{1 + \frac{a_1}{a} z^{-1}} \frac{c}{a}z^{n-m} e(t) = \frac{1 + c' z^{-1}}{1 + a' z^{-1}} z^{n-m} e'(t), \quad e'(t) \sim \mathcal{N}(\frac{c}{a} \mu = \mu', \lambda^2 \cdot \left(\frac{c}{a}\right)^2 = \lambda'^2)$$

Than, if $c'/a' \gt 1$, it is possible to apply the spectral equivalence ($1 + az^{-1} = a(1 + \frac{1}{a}z^{-1})$) transformation to make the process stable:

$$y(t) = \frac{1 + c' z^{-1}}{1 + a' z^{-1}} z^{n-m} e'(t) = \frac{1 + \frac{1}{c'} z^{-1}}{1 + \frac{1}{a'} z^{-1}} z^{n-m} e''(t), \quad e''(t) \sim \mathcal{N}(\frac{c'}{a'} \mu', \lambda'^2 \cdot \left(\frac{c'}{a'}\right)^2)$$

## Sample-Based Estimation

Given a finite realization $(y_1, y_2, \ldots, y_N)$ of a stationary process, we must estimate the mean, covariance, and spectrum from data.

**Quality criteria:**

- **Correctness:** the expected value of the estimator equals the true parameter ($\mathbb{E}[\hat{\theta}] = \theta$).
- **Consistency:** The Mean Squared Error (MSE) vanishes as $N \to \infty$: $\lim_{N \to \infty} \mathbb{E}[(\hat{\theta}_N - \theta)^2] = 0$.

### Sample Mean Estimator

As the process is stationary, all samples have the same mean. The simpler estimator is the sample average:

$$\boxed{\hat{m}_N = \frac{1}{N} \sum_{t=1}^{N} y(t)}$$

**Correctness:**

$$\mathbb{E}[\hat{m}_N] = \frac{1}{N} \sum_{t=1}^{N} \mathbb{E}[y(t)] = \frac{1}{N} \cdot N \cdot m_y = m_y \quad \checkmark$$

**Consistency:**

$\hat{m}_N$ is consistent when $\gamma_y(\tau) \to 0$. This happens as $N \to \infty$.

### Sample Covariance Estimator

Assuming zero-mean,that is equivalent to subtract $\hat{m}_N$ from the samples, we compute the sample covariance for each lag $\tau$:

$$\boxed{\hat{\gamma}_N(\tau) = \frac{1}{N - |\tau|} \sum_{t=1}^{N - |\tau|} y(t)\, y(t + |\tau|)} \quad 0 \leq |\tau| < N$$

**Correctness:**

$$\mathbb{E}[\hat{\gamma}_N(\tau)] = \frac{1}{N - |\tau|} \sum_{t=1}^{N - |\tau|} \mathbb{E}[y(t) y(t + |\tau|)] = \frac{1}{N - |\tau|} \sum_{t=1}^{N - |\tau|} \gamma_y(\tau) = \gamma_y(\tau) \quad \checkmark$$

**Consistency:**

$\hat{\gamma}_N(\tau)$ is a consistent estimator of $\gamma_y(\tau)$ if $\gamma_y(\tau) \to 0$ as $\tau \to \infty$.

### Sample Spectrum Estimator

The sample of the spectrum is the **periodogram** and is computed as the DFT of the sample covariance:

$$\hat{\Gamma}_N(\omega) = \sum_{\tau=-(N-1)}^{N-1} \hat{\gamma}_N(\tau)\, e^{-j \omega \tau}$$

**Asymptotic Correctness:**

The spectrum should cover all lags $\tau$ from $-\infty$ to $\infty$, but this can only compute it for $|\tau| < N$.

This estimator is consistent as $N \to \infty$,
$$\mathbb{E}[\hat{\Gamma}_N(\omega)] \to \Gamma_y(\omega)$$

**Not Consistent:**

$$\lim_{N \to \infty} \mathbb{E}[(\hat{\Gamma}_N(\omega) - \Gamma_y(\omega))^2] \to \gamma_y(\omega)^2 \quad, \forall \omega$$

This estimator is not consistent because the variance does not vanish as $N \to \infty$.

#### Averaging

To achieve **consistency**, it is possible to average the periodogram over multiple segments of the data:

$$\hat{\hat{\Gamma}}_N(\omega) = \frac{1}{M} \sum_{m=1}^{M} \hat{\Gamma}_{N/M}^{(m)}(\omega)$$

where $M$ is the number of segments and each segment has length $N/M$.

Increasing $M$ (and thus decreasing the segment length) reduces the variance of the estimator, but increases the consistency.

#### Biased Periodogram

Using the **biased covariance estimator** (denominator $N$ instead of $N - |\tau|$):

$$\hat{\gamma}_N^{\text{biased}}(\tau) = \frac{1}{N} \sum_{t=1}^{N} y(t)\, y(t + |\tau|)$$

The spectrum becomes:

$$\hat{\Gamma}_N(\omega) = \sum_{\tau=-N+1}^{N-1} \hat{\gamma}_N(\tau) e^{-j \omega \tau} = \underbrace{\frac{1}{N} \left| \sum_{t=1}^{N} y(t) e^{-j \omega t} \right|^2}_{\text{DFT of } y(t)}$$

This make computation more efficient, but it is **biased** ($\mathbb{E}[\hat{\Gamma}_N(\omega)] \neq \Gamma_y(\omega)$), but becomes asymptotically unbiased as $N \to \infty$.

## Prediction

Given a stationary process in canonical form $y(t) = W(z)e(t)$ with $e(t) \sim \mathcal{N}(0, \lambda^2)$, and an observed realizations data up to time $t$, it is possible to predict $y(t + k)$ for $k \geq 1$.

Since the process is **linear**, the optimal predictor is also linear. The form of the predictor is a combination of the past observed data and the model coefficients $\alpha_i$ that we need to determine:
$$\hat{y}(t + k | t) = \sum_{i=0}^{\infty} \underbrace{\alpha_i}_{\text{Model}} \underbrace{y(t - i)}_{\text{Data}}$$

The **optimal choice** is the one that minimizes the Mean Squared Error (MSE) of the prediction, that is the expected value of the squared difference between the true value and the predicted value:
$$J(\hat{y}) = \mathbb{E}[\underbrace{(y(t + k) - \hat{y}(t + k))^2}_{\text{Prediction Error}}]$$

Since $y(t) = \sum_{j=0}^{\infty} w_j e(t-j)$, it is possible to rewrite the predictor as a linear combination of the _past noise terms_:

$$\hat{y}(t + k | t) = \sum_{i=0}^{\infty} \beta_i e(t - i)$$

It is possible to separate the MSE into two parts: one that depends on the **future** noise terms ($j < k$, which are independent of the past data and irreducible), and one that depends on the **past** noise terms ($j \geq k$, which can be reduced by choosing the optimal coefficients $\beta_i$).

$$J(\beta) = \underbrace{\mathbb{E}\left[\left(\sum_{j=0}^{k-1} w_j e(t + k - j)\right)^2\right]}_{\text{Future noise}} + \underbrace{\mathbb{E}\left[\left(\sum_{j=k}^{\infty} (w_j - \beta_{j-k}) e(t + k - j)\right)^2\right]}_{\text{Past noise}}$$

Since the future noise is independent of $\beta$, minimizing $J(\beta)$ is equal to minimize the past noise term ($\hat{\beta}_i = \arg\min_{\beta_i} \mathbb{E}[(\sum_{i=k}^{\infty} (w_{i+k} - \beta_i) e(t - i))^2]$). Setting the past coefficients optimally:

$$\beta_j = w_{j+k} \quad \forall j \geq 0$$

This gives the **optimal noise-based predictor**:

$$\hat{y}(t + k | t) = \sum_{j=0}^{\infty} w_{j+k} e(t - j)$$

This formula requires knowledge of the unobservable noise $e(t-j)$.

### Diophantine Decomposition

The key is to decompose the transfer function using _polynomial long division_. Divide $C(z)$ by $A(z)$ for exactly $k$ steps:

$$W(z) = \frac{C(z)}{A(z)} = E_k(z) + z^{-k} \frac{F_k(z)}{A(z)}$$

where:

- **$E_k(z) = e_0 + e_1 z^{-1} + \cdots + e_{k-1} z^{-(k-1)}$**: polynomial of degree $k-1$
- **$F_k(z)$**: remainder polynomial (degree $< m$)

This factorizes the MA coefficients into two parts:

$$y(t+k) = \underbrace{E_k(z) e(t + k)}_{\text{Prediction error, } \varepsilon(t+k)} + \underbrace{\frac{F_k(z)}{A(z)} e(t)}_{\text{Predictor}}$$

### Converting to Observable Form

The predictor still depends on unobservable noise. Using the **whitening filter** to recover $e(t)$ from $y(t)$:

$$e(t) = \frac{A(z)}{C(z)} y(t)$$

It is possible to rewrite the predictor in terms of the observed data:

$$\boxed{\hat{y}(t + k | t) = \frac{F_k(z)}{C(z)} y(t)}$$

### Prediction Error Evolution

The prediction error variance is:
$$\text{MSE}(k) = \mathbb{E}[\varepsilon(t+k)^2] = \left(\sum_{i=0}^{k-1} e_i^2\right) \lambda^2= var[\varepsilon(t+k)]$$

and as $k$ increases, more future noise terms $e(t+1), \ldots, e(t+k)$ are included in the prediction error, making the prediction less accurate.

1. $k = 1$ (one-step ahead): Error is minimal as it only considers the immediate future noise: $\text{MSE}(1) = e_0^2 \lambda^2 = \lambda^2$ (as in canonical form, $e_0 = 1$).
2. $k \to \infty$ (infinite horizon): No information remains useful and the best predictor reverts to the **trivial predictor** $\hat{y} = m_y$ (the mean), and $\text{MSE}(\infty) = \gamma_y(0)$ (the process variance).

The bounds are: $\lambda^2 \leq \text{MSE}(k) \leq \gamma_y(0)$ for all $k \geq 1$.

### Non-Zero Mean Processes

For a process with mean $m_y \neq 0$, the process is unbiased by subtracting the mean from the observed data, and then adding it back to the predictor:

$$\boxed{\hat{y}(t + k | t) = m_y + \frac{F_k(z)}{C(z)}(y(t) - m_y)}$$

### Prediction with Exogenous Inputs

If the system is affected by an exogenous input $u(t)$ (ARMAX model), the optimal predictor decomposes into two components:

$$\boxed{\hat{y}(t + k | t) = \frac{F_k(z)}{C(z)} y(t) + \frac{B_k(z)E_k(z)}{C(z)} u(t + k - d)}$$

where:

- **First term:** Response to past observations (same as before)
- **Second term:** Response to future inputs, requires knowledge of the input.

## Identification

Given an input-output dataset $\{u(t), y(t)\}$ of a dynamic system $S$, we want to find a model that can explain the data and predict future values of the output.

The model is found with the **Parametric System Identification** approach, which consists of the following steps:

### Experimental Design

Design an experiment to collect input-output data $\{u(t), y(t)\}_{t=1}^N$ that accurately reflects the system dynamics.

Some key decisions in experimental design are:

- **Experiment length $N$:** Long enough to capture system dynamics and provide sufficient data for reliable estimation, but balancing cost and time constraints.
- **Input design $u(t)$:** Choose a signal that excites the relevant frequencies: white noise, a constant, a step input, or other sequences that depends on prior knowledge of the system.

### Model class selection

Select a parametric model structure $M(\theta)$ that can represent the system dynamics, where $\theta$ is the vector of parameters to be estimated.

It can be related to:

- _Discrete_ or continuous time.
- _Linear_ or nonlinear.
- _Time-invariant_ or time-varying.
- Static or _dynamic_.

The vector of parameters $\theta$ can contains:

- $a_i$: coefficients of the autoregressive terms.
- $b_i$: coefficients of the exogenous input terms.
- $c_i$: coefficients of the moving average terms.

To characterize the model it is also necessary to specify:

- $\lambda^2$: the variance of the white noise.
- $d$: the pure input/output delay.
- $m$: the order of the autoregressive part.
- $n$: the order of the moving average part.
- $p$: the order of the exogenous input part.

$\Theta$ is the set of admissible models, which is the set of all possible values of $\theta$ that can be used to represent the system dynamics.

### Identification criterion

Choose the identification criterion ($J_N(\theta) \geq 0$) that quantifies the error between the model output and the observed data.

This use predictive approach to system identification, where the model output is compared with the observed data, and the parameters are adjusted to minimize the prediction error.

The ideal objective is $J(\theta) = \mathbb{E}[(y(t + 1) - \hat{y}(t + 1 | t, \theta))^2]$, but it is not computable as it depends on the true system dynamics and the noise distribution.

Practical criteria is:
$$J_N(\theta) = \frac{1}{N} \sum_{t=1}^{N} (y(t) - \hat{y}(t | t-1, \theta))^2$$

A one-step ahead predictor will have an error $\varepsilon(t+1) = y(t + 1) - \hat{y}(t + 1 | t, \theta) = e(t)$, then $J_N(\hat{\theta}_N) = \mathbb{E}[(\varepsilon(t + 1))^2] = \lambda^2$.

### Minimization

Minimization of $J_N(\theta)$ to find the optimal parameters $\hat{\theta} = \arg\min_{\theta} J_N(\theta)$.

#### Minimization of AR(X) models

For AR(X) models, the prediction error is a linear function of the parameters, so the minimization can be solved in closed form by setting the gradient of $J_N(\theta)$ to zero and solving the resulting equations.

$$M_\theta: \quad y(t) = \frac{B(z)}{A(z)} u(t) + \frac{C(z)}{A(z)} e(t)$$

- $C(z) = 1$
- $A(z) = 1 - a_1 z^{-1} - \cdots - a_m z^{-m}$
- $B(z) = b_1 z^{-1} + \cdots + b_p z^{-p}$
- $e(t) \sim \text{WN}(0, \lambda^2)$
- $\theta = [a_1, a_2, \ldots, a_m, b_1, b_2, \ldots, b_p]^T$

The objective function is:
$$\hat{\theta}_N = \arg\min_{\theta} \frac{1}{N} \sum_{t=1}^{N} (y(t) - \hat{y}(t | t-1, \theta))^2$$

The regression vector is:
$$\varphi(t) = [y(t-1), \ldots, y(t-m), u(t-d), \ldots, u(t-d-p+1)]^T$$

meaning that the parametric optimal predictor is a linear combination of the past values of the output and the past values of the input:
$$\hat{y}(t | t-1, \theta) = \theta^T \varphi(t)$$

Being linear in $\theta$, the cost function is quadratic in $\theta$, making the minimization problem convex and solvable.

The condition for $\hat{\theta}_N$ to be the global minimum are:

- $\hat{\theta}_N$ is a stationary point: $\frac{\partial J_N(\theta)}{\partial \theta} |_{\theta = \hat{\theta}_N} = 0$
- The Hessian of $J_N(\theta)$ is positive definite (minimum point) at $\hat{\theta}_N$: $\frac{\partial^2 J_N(\theta)}{\partial \theta^2} |_{\theta = \hat{\theta}_N} \succ 0$

To satisfy the first condition, we can set the gradient of $J_N(\theta)$ to zero and solve for $\hat{\theta}_N$ (least-squares normal equations):
$$\frac{\partial J_N(\theta)}{\partial \theta} = -\frac{2}{N} \sum_{t=1}^{N} (y(t) - \theta^T \varphi(t)) \varphi(t) = 0$$

obtaining the **ordinary least squares**:
$$\hat{\theta}_N = \left( \underbrace{\sum_{t=1}^{N} \varphi(t) \varphi(t)^T}_\text{information matrix - non singular} \right)^{-1} \left( \sum_{t=1}^{N} y(t) \varphi(t) \right)$$

This allows to compute the optimal model as a function of the data.

To verify that the second condition is satisfied, we can compute the Hessian of $J_N(\theta)$:
$$\frac{\partial^2 J_N(\theta)}{\partial \theta^2} = \frac{2}{N} \sum_{t=1}^{N} \varphi(t) \varphi(t)^T$$

For any vector $x \neq 0$:
$$x^T \frac{\partial^2 J_N(\theta)}{\partial \theta^2} x = \frac{2}{N} \sum_{t=1}^{N} (\varphi(t)^T x)^2 \geq 0$$

> This is always non-negative. However, it equals zero if and only if $\varphi(t)^T x = 0$ for all $t$, which indicates a **degenerate case**:
>
> - **Experimental identifiability:** Data are not informative enough to capture the system dynamics (e.g., the input doesn't excite relevant frequencies).
> - **Structural identifiability:** The model class is too flexible; multiple parameter values produce identical predictions, leading to non-unique solutions.

#### Minimization of ARMA(X) models

For ARMA(X) models, the prediction error is a nonlinear function of the parameters, so the minimization problem is non-convex and cannot be solved in closed form.

$$M_\theta: \quad y(t) = \frac{B(z)}{A(z)} u(t) + \frac{C(z)}{A(z)} e(t)$$

- $C(z) = 1 + c_1 z^{-1} + \cdots + c_n z^{-n}$
- $A(z) = 1 - a_1 z^{-1} - \cdots - a_m z^{-m}$
- $B(z) = b_1 z^{-1} + \cdots + b_p z^{-p}$
- $e(t) \sim \text{WN}(0, \lambda^2)$
- $\theta = [a_1, a_2, \ldots, a_m, b_0, b_1, \ldots, b_{p-1}, c_1, c_2, \ldots, c_n]^T$

The **1-step-ahead predictor** is:
$$\hat{y}(t | t - 1, \theta) = \frac{B(z)}{A(z)} u(t - d) + \frac{C(z) - A(z)}{C(z)} y(t)$$

where $\frac{A(z)}{C(z)}$ and $\frac{B(z)}{C(z)}$ are rational functions depending on all parameters $\theta$.

The **prediction error** is:
$$\varepsilon(t | \theta) = y(t) - \hat{y}(t | t - 1, \theta) = \frac{A(z)}{C(z)} y(t) - \frac{B(z)}{C(z)} u(t-d)$$

Because $\varepsilon(t | \theta)$ depends on $\theta$ through rational functions (not just linear combinations), the cost function $J_N(\theta)$ is **nonlinear** in $\theta$. This makes the problem non-convex and prevents closed-form solutions.

#### Iterative Solution Strategy

Since a closed-form solution does not exist, we use **iterative numerical optimization** to find local minima.

At each iteration, approximate the cost function $J_N(\theta)$ locally using a **second-order Taylor expansion** (quadratic approximation that creates a paraboloid tangent to the cost function). Minimize this simpler quadratic model to obtain the next point, then repeat.

**Quadratic approximation** at iteration $i$:
$$V^{(i)}(\theta) = J_N(\theta^{(i)}) + g^{(i)}(\theta - \theta^{(i)}) + \frac{1}{2} (\theta - \theta^{(i)})^T H^{(i)} (\theta - \theta^{(i)})$$

where:

- $g^{(i)} = \frac{\partial J_N(\theta)}{\partial \theta}\big|_{\theta = \theta^{(i)}}$ (gradient at current point)
- $H^{(i)} = \frac{\partial^2 J_N(\theta)}{\partial \theta^2}\big|_{\theta = \theta^{(i)}}$ (Hessian at current point)

By setting the gradient to zero, we find the minimum of the quadratic approximation:
$$\frac{\partial V^{(i)}(\theta)}{\partial \theta} = g^{(i)} + H^{(i)} (\theta - \theta^{(i)}) = 0$$

Solving for $\theta$:
$$\boxed{\theta^{(i + 1)} = \theta^{(i)} - [H^{(i)}]^{-1} g^{(i)}}$$

This is the **Newton-Raphson update rule**.

**Gradient computation:**
$$g^{(i)} = \frac{\partial J_N}{\partial \theta} = \frac{2}{N} \sum_{t = 1}^N \varepsilon(t | t-1, \theta^{(i)}) \frac{\partial \varepsilon(t | t-1, \theta^{(i)})}{\partial \theta}$$

**Hessian computation:**
$$H^{(i)} = \frac{\partial^2 J_N}{\partial \theta^2} = \frac{2}{N} \sum_{t = 1}^N \left( \frac{\partial \varepsilon(t | t-1, \theta^{(i)})}{\partial \theta} \left(\frac{\partial \varepsilon(t | t-1, \theta^{(i)})}{\partial \theta}\right)^T + \varepsilon(t | t-1, \theta^{(i)}) \frac{\partial^2 \varepsilon(t | t-1, \theta^{(i)})}{\partial \theta^2} \right)$$

The gradient with respect to each parameter $\theta_j$:
$$\frac{\partial \varepsilon(t | \theta)}{\partial \theta} = \begin{bmatrix} \frac{\partial \varepsilon(t | t-1, \theta)}{\partial a_1} \\ \vdots \\ \frac{\partial \varepsilon(t | t-1, \theta)}{\partial a_m} \\ \frac{\partial \varepsilon(t | t-1, \theta)}{\partial b_1} \\ \vdots \\ \frac{\partial \varepsilon(t | t-1, \theta)}{\partial b_p} \\ \frac{\partial \varepsilon(t | t-1, \theta)}{\partial c_1} \\ \vdots \\ \frac{\partial \varepsilon(t | t-1, \theta)}{\partial c_n} \end{bmatrix} = \begin{bmatrix} \alpha(t - 1) \\ \vdots \\ \alpha(t - m) \\ \beta(t - d) \\ \vdots \\ \beta(t - d - p + 1) \\ \gamma(t - 1) \\ \vdots \\ \gamma(t - n) \end{bmatrix}$$

##### Algorithm Variants

Different balance between accuracy and computational cost:

| **Method** | **Update Rule** | **Pros** | **Cons** |
| --- | --- | --- | --- |
| **Newton** | $\theta^{(i+1)} = \theta^{(i)} - [H^{(i)}]^{-1} g^{(i)}$ | Fast convergence near optimum | Hessian inversion expensive as requires 2nd derivatives |
| **Gradient Descent** | $\theta^{(i+1)} = \theta^{(i)} - \nu g^{(i)}$ | Simple, only 1st derivatives needed | Slow convergence with fixed step-size $\nu$ |
| **Quasi-Newton** | $\theta^{(i+1)} = \theta^{(i)} - [\tilde{H}^{(i)}]^{-1} g^{(i)}$ | Good balance, approximate Hessian from gradients | Less accurate than Newton |

In the Quasi-Newton method, the second Hessian term (involving $\frac{\partial^2 \varepsilon}{\partial \theta^2}$) is often neglected in practice because it decays faster than the first term as optimization progresses.

$$\boxed{\theta^{(i + 1)} = \theta^{(i)} - \left( \sum_{t = 1}^N \frac{\partial \varepsilon(t | t-1, \theta^{(i)})}{\partial \theta} \left(\frac{\partial \varepsilon(t | t-1, \theta^{(i)})}{\partial \theta}\right)^T \right)^{-1} \left(\sum_{t = 1}^N \varepsilon(t | t-1, \theta^{(i)}) \frac{\partial \varepsilon(t | t-1, \theta^{(i)})}{\partial \theta} \right)}$$

### Model Validation

The objective is to determine whether the identified model $M(\hat{\theta}_N)$ adequately captures the system dynamics and can reliably predict future outputs, meaning that the assumptions made during identification are valid:

- Correct model structure chosen (system $S$ belongs to model class $\mathcal{M}$)
- Model order parameters $(m, n, p, k, d)$ are correct
- Input signal sufficiently exciting (experimental identifiability)
- Data informative and representative of system dynamics

As $N \to \infty$, the sample cost function converges:
$$J_N(\theta, S) \to \bar{J}(\theta) = \mathbb{E}[\varepsilon(t | t-1, \theta)^2]$$

The optimal parameters converge to the set:
$$\Delta = \{\theta^* | \bar{J}(\theta^*) \leq \bar{J}(\theta), \, \forall \theta\}$$

If $\Delta = \{\theta^*\}$ is a singleton, then $\hat{\theta}_N \to \theta^*$ with probability 1. The true system parameters $\theta°$ that generated the data are always in $\Delta$.

Based on whether the true system $S$ is in the model class $\mathcal{M}$ and whether the solution is unique, we can have four possible outcomes:

| System in $\mathcal{M}$? | Unique Solution? | Outcome |
| --- | --- | --- |
| **Yes** ($S \in \mathcal{M}$) | **Yes** ($\Delta = \{\theta°\}$) | Model converges to true parameters; prediction error $\to \lambda^2$ |
| **Yes** ($S \in \mathcal{M}$) | **No** ($\|\Delta\| > 1$) | Model converges to some $\theta^* \in \Delta$ (possibly $\neq \theta°$) and cannot guarantee true solution |
| **No** ($S \notin \mathcal{M}$) | **Yes** ($\Delta = \{\theta^*\}$) | Converges to best approximation $\theta^* \neq \theta°$ as the true system is not in the model class |
| **No** ($S \notin \mathcal{M}$) | **No** ($\|\Delta\| > 1$) | The model converges to one of the multiple solutions in $\Delta$, but the true solution is outside the set |

#### Model Order Selection

The **Model Order Selection** process determines the degrees of polynomials $m$ (AR order), $n$ (MA order), and $p$ (exogenous order) that balance _fit quality_ and _model complexity_.

We cannot guarantee the true system $S$ is in the model set, so order selection requires balancing two competing errors:

| Order too low | Order too high |
| --- | --- |
| **Under-fitting:** Model cannot capture system dynamics | **Over-fitting:** Model fits noise instead of signal |
| Prediction error $> \lambda^2$ (systematic component missed) | Prediction error $< \lambda^2$ (noise fitted as signal) |
| Leads to biased parameter estimates | Leads to poor generalization on new data |

We do not know the true noise variance $\lambda^2$ a priori, so we cannot simply vary the order and check when $J_N(\hat{\theta}_N) \approx \lambda^2$. Instead, we need to use one of three approaches:

##### Whiteness Test

The **whiteness test on residuals** checks whether the identified model has captured all dynamics by testing if residuals are consistent with white noise.

If the true system $S \in \mathcal{M}$, parameters converge $\hat{\theta}_N \to \theta°$, then:
$$\varepsilon(t | t-1, \theta°) = y(t) - \hat{y}(t | t-1, \theta°) = e(t) \sim \mathcal{N}(0, \lambda^2)$$

The residuals should be **white noise**, meaning:

- Autocorrelation $\gamma_\varepsilon(\tau) \approx 0$ for all lags $\tau > 0$
- Power spectrum nearly flat across all frequencies

This means that starting from a low-order model, we can increase the order until the residuals pass the whiteness test, indicating that all systematic dynamics have been captured.

##### Cross-Validation

**Cross-validation** evaluates model performance on unseen data to select the order that generalizes best.

1. Split dataset into training ($k$ samples) and validation ($N-k$ samples)
2. For each candidate order:
   - Train on first $k$ samples: $\hat{\theta}_k^{(n)} = \arg\min_\theta J_k(\theta)$
   - Evaluate on remaining data: $J_v(\hat{\theta}_k^{(n)}) = \frac{1}{N-k} \sum_{t=k+1}^N (y(t) - \hat{y}(t|t-1, \hat{\theta}_k^{(n)}))^2$
3. Select order that minimizes validation error:
$$\hat{n} = \arg\min_n J_v(\hat{\theta}_k^{(n)})$$

##### Identification using Model Order Penalties

Select model order by balancing fit quality and complexity using a **single dataset** with a penalty term. Each criterion decomposes into:
$$\text{Criterion} = \text{(fit term)} + \text{(penalty term)}$$

Where when $n$ increases the fit term decreases, but the penalty term increases.

**Final Prediction Error (FPE):**
$$FPE(n) = \underbrace{\frac{N + n}{N - n}}_{\text{complexity penalty}} \cdot \underbrace{J_N(\hat{\theta}_n)}_{{\text{in-sample fit}}}$$

Approximates the out-of-sample prediction error. Penalty is linear in $n$.

**Akaike Information Criterion (AIC):**
$$AIC(n) = \underbrace{\ln(J_N(\hat{\theta}_n))}_{{\text{decreases as } n \uparrow}} + \underbrace{2\frac{n}{N}}_{\text{penalty increases as } n \uparrow}$$

Asymptotically equivalent to FPE.

**Minimum Description Length (MDL):**
$$MDL(n) = \underbrace{\ln(J_N(\hat{\theta}_n))}_{{\text{Fit term}}} + \underbrace{\frac{n}{N} \ln(N)}_{\text{Penalty}}$$

Stronger penalty for model complexity. Tends to select lower order models than AIC/FPE.

## Data Preprocessing

When raw data contains non-stationary components it is necessary to separate the stationary part from the deterministic part before applying identification techniques.

$$y(t) = \underbrace{\tilde{y}(t)}_{\text{stationary}} + \underbrace{D(t)}_{\text{deterministic}}$$

### Trend Removal

**Trend** is a long-term increase or decrease in the data. It can be modeled as a deterministic function of time:
$$y(t) = \tilde{y}(t) + T(t)$$

where $T(t)$ is the trend component (e.g., polynomial, exponential).

To remove the trend:

1. Estimate trend using polynomial regression (e.g., linear: $T(t) = a + bt$, quadratic: $T(t) = a + bt + ct^2$)
2. Subtract to obtain stationary residual:
$$\tilde{y}(t) = y(t) - \hat{T}(t)$$

### Seasonality Removal

**Seasonality** is a repeating pattern in the data with a fixed period. It can be modeled as a periodic function of time:
$$y(t) = \tilde{y}(t) + S(t)$$

where $S(t)$ is periodic with known period $T$ (if unknown, use spectral analysis to estimate it).

To remove seasonality we need to estimate the seasonal component $S(t)$, which can be done by averaging over complete periods. With $M$ periods in the data:
$$\hat{S}(t) = \frac{1}{M} \sum_{k=0}^{M - 1} y(t + kT) = \underbrace{\frac{1}{M} \sum_{k=0}^{M - 1} \tilde{y}(t + kT)}_{\mathbb{E}[\tilde{y}(t)] \approx 0} + \frac{1}{M} \sum_{k=0}^{M - 1} S(t + kT) = S(t)$$

Then remove the seasonal component:
$$\tilde{y}(t) = y(t) - \hat{S}(t)$$

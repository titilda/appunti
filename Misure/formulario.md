# Formulario di Misure

## Formule di base

Siano $x_i$, con $i = 1, \dots, N$, le $N$ misurazioni effettuate, allora posso calcolare la media del misurando ($\overline x$), la varianza campionaria ($S^2(x)$), la varianza del valor medio ($S^2(\overline x)$) e la sua incertezza o deviazione standard ($S(\overline x) = u(x)$).

$$
\overline x = \frac{1}{N} \sum_{i = 1}^N [x_i] \\
S^2(x) = \frac{1}{N - 1} \sum_{i = 1}^{N} \left[(x_i - \overline x)^2\right] \\
S^2(\overline x) = \frac{S^2(x)}{N} \\
S(\overline x) = u(x) = \sqrt{S^2(\overline x)}
$$

### Derivate dalle precedenti

Ovviamente, ci sono alcune formule comode che sono derivate dalle precedenti, che permettono, in alcuni casi, conti più veloci.

$$
u(x) = \frac{S(\overline x)}{\sqrt N} \\
S(x) = \sqrt{\frac{1}{N - 1} \sum_{i = 1}^N \left[(x_i - \overline x)^2 \right]} \\
u(x) = \sqrt{\frac{1}{N(N - 1)}\sum_{i = 1}^N \left[(x_i - \overline x)^2\right]}
$$

## Formule riguardanti l'incertezza

Ci sono due tipi di incertezze: (A) calcolate con metodi statistici (vedere [formule di base](#formule-di-base)) e (B) conosciute a priori e/o in altro modo.

Nel caso in cui una misurazione coinvolga entrambi i tipi di incertezze, è necessario calcolare l'incertezza composta ($u_C(x)$).

$$
u_C(x) = \sqrt{u_A^2(x) + u_B^2(x)}
$$

Se si devono inserire i valori calcolati in delle formule, è possibile calcolare l'incertezza del risultato conoscendo l'incertezza dei vari valori utilizzati (se i valori utilizzato sono correlati tra loro, vedere [misure indirette](#misure-indirette)).
Ad esempio, sia $R = R(a, b, \dots)$ la formula e $a, b, \dots$ i parametri, allora

$$
u(R) = \sqrt{\left(\frac{\partial R}{\partial a}\right)^2 \cdot u^2(a) + \left( \frac{\partial R}{\partial b} \right)^2 \cdot u^2(b) + \dots}
$$

Può essere necessario conoscere l'incertezza relativa ($u_r(x)$) di una misurazione rispetto alla misurazione stessa.

$$
u_r(x) = \frac{u(x)}{\overline x}
$$

### Misure indirette

Nel caso in cui si debba compiere una misurazione indiretta, il cui risultato è descritto dalla funzione $R = R(x_1, x_2, \dots, x_N)$ dove $x_1, x_2, \dots, x_N$ sono i valori utilizzati nel calcolo, l'incertezza finale è data dalla formula

$$
u_C(R) = \sqrt{\underbrace{\sum_{i=1}^N \left[ \left( \frac{\partial R}{\partial x_i} \right)^2 \cdot u^2(x_i) \right]}_{\text{Somma pesata varianze}} + \underbrace{2 \sum_{i = 1}^{N - 1} \sum_{j = i + 1}^{N} \left[ \left( \frac{\partial R}{\partial x_i} \right) \left( \frac{\partial R}{\partial x_j} \right) \cdot u^2(x_i, x_j) \right]}_{\text{Somma pesata covarianze}}}
$$

Se si vuole calcolare quanto due misure siano correlate tra loro, è necessario calcolare il coefficiente di correlazione ($r_{ij}$).

$$
r_{ij}(x_i, x_j) = \frac{u(x_i, x_j)}{u(x_i) \cdot u(x_j)} \in [-1, 1]
$$

Se $x_i$ e $x_j$ sono statisticamente indipendenti allora $r_{ij} = 0$.

### Incertezza estesa

E' possibile che un'incertezza venga espressa come percentuale. In tal caso viene data la probabilità che una misurazione ricada in un dato range.

Data un'incertezza estesa $U(x)$ e la sua percentuale, è possibile calcolare l'incertezza $u(x)$.

$$
u(x) = \frac{U(x)}{k}
$$

| $k$ (fattore di copertura) | Percentuale |
| -------------------------- | ----------- |
| 1                          | 68.3%       |
| 2                          | 95.5%       |
| 3                          | 99.7%       |

Due misure $x_a$ e $x_b$ sono dette compatibili se è possibile trovare un $k$ inferiore a 3 tale per cui

$$
|x_a - x_b| \le k \sqrt{u^2(x_a) + u^2(x_b) - \underbrace{2r_{ab}u(x_a)u(x_b)}_{\text{Nullo se indipendenti}}}
$$

### Incertezza di tipo B

L'incertezza di tipo B si basa su dati conosciuto a priori (ad esempio ci viene data dal manuale).

In molti casi è fornita la PDF con l'errore massimo da cui è possibile estrapolare i vari valori che interessano.

#### PDF normale (gaussiana)

| $\sigma$  | Intervallo                          | Probabilità |
| --------- | ----------------------------------- | ----------- |
| $1\sigma$ | $\mu - \sigma < x < \mu + \sigma$   | 68.3%       |
| $2\sigma$ | $\mu - 2\sigma < x < \mu + 2\sigma$ | 95.5%       |
| $3\sigma$ | $\mu - 3\sigma < x < \mu + 3\sigma$ | 99.7%       |

#### PDF quadrata

Sia $\Delta x$ l'errore massimo e quindi anche la base della PDF. L'altezza è $\frac{1}{\Delta x}$.

$$
p(x) = \begin{cases}
    0 & x \lt \mu - \frac{\Delta x}{2} \\
    \frac{1}{\Delta x} & \mu - \frac{\Delta x}{2} \le x \le \mu + \frac{\Delta x}{2}
    0 & x \gt \mu + \frac{\Delta x}{2}
\end{cases} \\
\mu(x) = \int_{-\infty}^{+\infty} x \cdot p(x) \cdot dx \\
\sigma = \frac{\Delta x}{\sqrt {12}}
$$

#### PDF triangolare

Sia $\Delta x$ l'errore massimo e quindi anche la base della PDF. L'altezza è $\frac{2}{\Delta x}$.

$$
\sigma = \frac{\Delta x}{\sqrt {24}}
$$


### Derivate dalle precedenti

In caso si voglia calcolare l'incertezza del risultato di una formula che sia una produttoria, è possibile utilizzare un formula semplificata. Sia $R = R(a, b, \dots) = a^{e_a} \cdot b^{e_b} \cdot \dots$ allora

$$
u_r(R) = \sqrt{(e_a)^2 \cdot u_r^2(a) + (e_b)^2 \cdot u_r^2(b) + \dots}
$$

E' possibile calcolare una media pesata di più misurazioni con le relative incertezze e l'incertezza della media pesata.

$$
x_{mp} = \frac{x_1 \cdot \frac{1}{u^2(x_1)} + x_2 \cdot \frac{1}{u^2(x_2)} + \dots}{\frac{1}{u^2(x_1)} + \frac{1}{u^2(x_2)}} + \dots \\
u(x_{mp}) = \sqrt{\frac{1}{\frac{1}{u^2(x_1)}+\frac{1}{u^2(x_2)} + \dots}}
$$

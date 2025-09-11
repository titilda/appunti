---
title: "Riassunto di Fondamenti di Ricerca Operativa"
author: 
- "Andrea Oggioni"
---

# Introduzione

Un **Problema di Ottimizzazione** consiste nel trovare una $x \in X \sube \mathbb{R}^n$ che minimizza o massimizza una certa funzione, detta **Funzione Obiettivo**, dove $X$ è detto **Insieme Ammissibile**.

Per risolvere un problema di ottimizzazione bisogna quindi trovare una $x^*$ tale per cui $x^* \in X$ e $f(x^*)$ è minimo o massimo.

::: {.callout .callout-definition title="Ottimo globale"}
$x^* \in \mathbb{R}^n$ è ottimo globale se $x^* \in X$ e $\not \exists \bar x \in X : f(\bar x) \lt f(x^*)$.
:::

::: {.callout .callout-definition title="Ottimo locale"}
$x^* \in \mathbb{R}$ è un ottimo locale se $x^* \in X$ e $\exists \varepsilon \gt 0 : \not \exists \overline x \in \mathcal{N}_\varepsilon(x^*) \cap X : f(\overline x) \lt f(x^*)$
:::

La notazione

$$
\begin{align*}
    \min & \qquad f(x) \\
    \text{s.t.} & \qquad x \in X
\end{align*}
$$

è completamente equivalente alla notazione

$$
\min\left\{ f(x) : x \in X \right\}
$$

e possono essere utilizzate interscambievolmente.
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

## Modelli di ottimizzazione

Il modello di un problema (ovvero la rappresentazione matematica di un problema reale) è composto da **variabili**, **vincoli** e **funzione obiettivo**.

Le **variabili** sono quelle che descrivono la decisione che deve essere presa.

I **vincoli** sono le restrizioni si tali variabili e possono essere di tre tipi:

- vincoli fisici (ad esempio la capacità di un rimorchio)
- vincoli di tipo (ad esempio nonnegatività o appartenenza ad un certo dominio)
- vincoli legali (tutti quelli che ci sono perchè sta scritto così)

La **funzione obiettivo** è una funzione che dipende dalle decisioni che deve essere minimizzata (o massimizzata).

::: {.callout .callout-example title="Lattina"}
Supponiamo di voler costruire una lattina cilindrica standard da 330 ml che sia composta dalla minor quantità di alluminio.

Le variabili in gioco sono $r$ (il raggio della lattina) e $h$ (l'altezza della lattina).

Il vincolo da rispettare è $\pi r^2 h = 330 ml$ con $r \ge 0$ e $h \ge 0$ (i vincoli stretti non si possono utilizzare per motivi che vedremo più avanti anche se è palese che una lattina di raggio nullo, in questo contesto, sarebbe una soluzione non valida).

La funzione obiettivo da minimizzare è l'area della lattina ovvero $2 \pi r^2 + 2 \pi hr$.
:::

Formalmente, un problema di minimopuò essere espresso come problema di massimo e viceversa:

$$
\min \{ f(x) : x \in X \} = - \max \{ -f(x) : x \in X \}
$$

Ne segue che tutto ciò che è applicabile su un problema di un tipo può essere applicato anche su problemi del tipo inverso con le divute accortezze.

## Problemi di ottimizzazione

Sia $P$ un problema generico del tipo $\min \{ f(x) : x \in X \}$.

::: {.callout .callout-definition title="Inammissibilità di un problema"}
$P$ si dice **inammissibile** $X = \emptyset$.
:::

::: {.callout .callout-definition title="Illimitatezza di un problema"}
$P$ si dice **illimitato** se $x \ne \emptyset$ e $\forall M \lt 0 \exists \bar x \in X : f(\bar x) \le M$.
:::

In altre parole, un problema è illimitato non se non ci sono soluzioni ma se, presa una qualunque soluzione, ce n'è sempre una migliore.

### Rilassamento di un problema di ottimizzazione

::: {.callout .callout-definition title="Rilassamento"}
Dato un generico problema di ottimizzazione

$$
\begin{align*}
\min & \qquad f(x) \\
\text{s.t.} & \qquad x \in X
\end{align*}
$$

un **rilassamento** è un altro problema di ottimizzazione del tipo

$$
\begin{align*}
\min & \qquad \tilde f(x) \\
\text{s.t.} & \qquad x \in \tilde X
\end{align*} 
$$

tale che
1. $\tilde X \supseteq X$
2. $\forall x \in X \quad \tilde f(x) \le f(x)$
:::

La definizione è analoga per problemi di massimo se nella seconda condizione si usa $\ge$.

Nella pratica, un rilassamento di un problema è tale se i vincoli sono più tenui ma le soluzioni sono migliori.

Siano $P$ un problema di minimo $\min \left\{f(x) : x \in X\right\}$ e $R$ un suo rilassamento $\min\left\{ \tilde f(x) : x \in \tilde X \right\}$, allora è possibile esprimere alcune proprietà di cui il rilassamento gode:

1. Se $x^*$ è ottimo per $P$ allora $x^*$ è ammissibile per $\tilde x$ ma non è necessariamente ottimo per $R$.
2. Se $x^*$ è ottimo per $R$ allora non è necessariamente ammissibile (e quindi nemmeno ottimo) per $P$.
3. Se $x^*$ è ottimo per $R$ e ammissibile per $P$ allora $x^*$ non è necessariamente ottimo per $P$, a meno che non si abbia che anche $f(x^*) = \tilde f(x^*)$; in tal caso allora $x^*$ è anche ottimo per $P$.
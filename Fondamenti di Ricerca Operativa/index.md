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

# Analisi convessa

Sia dato un insieme di $m$ vettori $x^i \in \mathbb{R}^n \forall i \in M = \{ 1, \dots, m \}$, allora

::: {.callout .callout-definition title="Definizione"}
Dati $m$ scalari $\lambda_i \ \forall i \in M$, $\sum\limits_{i \in M} \lambda_iv^i \in \mathbb{R}$ è

1. una **combinazione lineare** se $\lambda_i \in \mathbb{R} \forall i \in M$
2. una **combinazione affine** se $\sum\limits_{i \in M} \lambda_i = 1$
3. una **combinazione conica** se $\lambda_i \ge 0 \forall i \in M$
4. una **combinazione convessa** se è sia affine che conica
:::

::: {.callout .callout-definition title="Span lineare"}
Dati $m$ punti $x^1, x^2, \dots, x^m \in \mathbb{R}^n$, lo **span lineare** di $x^1, x^2, \dots, x^m$ è l'insieme di tutte le combinazioni lineari di $x^1, x^2, \dots, x^m$ costruite con coefficienti $\lambda_i \in \mathbb{R}$.
:::

::: {.callout .callout-definition title="Span affine"}
Dati $m$ punti $x^1, x^2, \dots, x^m \in \mathbb{B}^n$, lo **span affine** di $x^1, x^2, \dots, x^m$ è l'insieme di tutte le combinazioni affini di $x^1, x^2, \dots, x^m$.
:::

Matematicamente, lo span affine di un insieme di punti $x^1, x^2, \dots, x^m$ può essere definito come

$$
\left\{ \sum_{i=1}^m \lambda_i x^i : \sum_{i=1}^m \lambda_i = 1 \right\}
$$

::: {.callout .callout-definition title="Inviluppo conico"}
Dati $m$ punti $x^1, x^2, \dots, x^m \in \mathbb{R}^n$, l'**inviluppo conico** di $x^1, x^2, \dots, x^m$ è l'insieme di tutte le combinazioni coniche di $x^1, x^2, \dots, x^m$.
:::

Matematicamente, l'inviluppo conico di $x^1, x^2, \dots, x^m$ può essere descritto come

$$
\left\{ \sum_{i=1}^m \lambda_ix^i : \lambda_i \ge 0 \ \forall i = i \dots m \right\}
$$

e rappresenta la minima piramide con vertice nell'origine e base all'infinito che contiene tutti i punti dati.

Intersecando lo span affine e l'inviluppo conico, si ottiene l'inviluppo complesso.

::: {.callout .callout-definition title="Inviluppo complesso"}
Dati $m$ punti $x^1, x^2, \dots, x^m \in \mathbb{R}^n$, l'**inviluppo convesso** di $x^1, x^2, \dots, x^m$ è l'insieme di tutte le combinazioni convesse di $x^1, x^2, \dots, x^m$.
:::

Matematicamente, l'inviluppo complesso di un insieme di punti $x^1, x^2, \dots, x^m$ può essere descritto dall'intersezione dell'insieme dhe descrive lo span affine con quello che descrive l'inviluppo conico:

$$
\left\{ \sum_{i=1}^m \lambda_ix^i : \lambda_i \ge 0 \ \forall i = 1, \dots, m, \ \sum_{i=1}^m \lambda_i = 1 \right\}
$$

Come si pu osservare trascinando i punti nell'embed qui sotto, l'inviluppo convesso di un insieme di punti è il minimo luogo convesso dello spazio che contiene tutti i punti dati (un po' come mettere un elastico $n$-dimesionale attorno ai punti dati).

<iframe src="https://www.geogebra.org/calculator/cm5rvv7w/?embed" width="700" height="600" allowfullscreen style="border: 1px solid #e4e4e4;border-radius: 4px;" frameborder="0"></iframe>

## Funzioni convesse

::: {.callout .callout-definition title="Funzione concava"}
Una funzione $f : \mathbb{R}^n \to \mathbb{R}$ è **convessa** se $\forall x',x'' \in \mathbb{R}^n,\forall \lambda \in [0,1]$ vale che $f(\underbrace{\lambda x' + (1 - \lambda)x''}_{\text{combinazione convessa}}) \le \lambda f(x') + (1-\lambda)f(x'')$.
:::

In generale, sia $\sum\limits_{i=1}^m \lambda_i x^i$ una combinazione convessa, allora una funzione è convessa se

$$
f\left(\sum_{i=1}^m \lambda_i x^i\right) \le \sum_{i=1}^m \lambda_i f(x^i)
$$

Le funzioni convesse godono di alcune proprietà (che possono essere dimostrate tutte con la definizione):

1. La somma di funzioni convesse è a sua volta convessa.
2. Il prodotto di uno scalare non negativo per una funzione convessa è a sua volta convesso.
3. Il massimo tra funzioni convesse è a sua volta una funzione convessa.

::: {.callout .callout-example title="Funzione convessa"}
Una funzione $f$ è **concava** se e solo se $-f$ è convessa.
:::

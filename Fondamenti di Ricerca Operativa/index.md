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

::: {.callout .callout-definition title="Funzione convessa"}
Una funzione $f$ è **concava** se e solo se $-f$ è convessa.
:::

::: {.callout .callout-note title="Nota"}
Una funzione lineare è sia concava che convessa.

Una forma quadratica $x^TQx$ è convessa se $Q$ è semidefinita positiva.
:::

::: {.callout .callout-defintion title="Insieme convesso"}
Un insieme $S \sube \mathbb{R}^n$ è **convesso** se $\forall x', x'' \in S, \forall \lambda \in [0,1]$ vale che $\lambda x' + (1 - \lambda)x'' \in S$.
:::

Gli insiemi convessi godono di alcune proprietà:

1. L'intersezione di insiemi convessi è a sua volta convessa (non vale per l'unione).
2. Un insieme definito come $S = \left\{ x \in \mathbb{R}^n : g(x) \le \alpha \right\}$ con $g$ convessa è a sua volta convesso.
3. Un insieme definito come $S = \left\{x \in \mathbb{R}^n : g(x) \ge \alpha \right\}$ con $g$ concava è convesso.

Dalle prime due proprietà appena citate, segue che se per ciascun vincolo di un problema dato costruisco un insieme che lo rispetta, se poi interseco tutti questi insiemi, ho l'insieme degli elementi ammissibili per tale problema.

::: {.callout .callout-definition title="Problema convesso"}
Un problema $\min \{f(x) : x \in X\}$ è **convesso** se $f$ è una funzione convessa e $X$ è un insieme convesso.
:::

Analogamente, se ho un problema come quello nella definizione ma di massimo, tale problema rimane convesso se $f$ è concava ($X$ deve rimanere convesso).

# Ottimizzazione lineare

L'ottimizzazione lineare spesso approssima il modello su cui lavorare perchè il mondo reale contiene molte non linearità, però rende molto più semplici i conti.

::: {.callout .callout-definition title="Funione lineare"}
Una funzione $f : \mathbb{R}^n \to \mathbb{R}$ è **lineare** se è della forma $f(x) = \sum\limits_{i=1}^n a_ix_i$.
:::

Dalle proprietà delle [funzioni convesse](#funzioni-convesse) e dalle proprietà degli [insiemi convessi](#analisi-convessa) segue che gli insiemi della forma $S = \left\{x \in \mathbb{R}^n : \sum\limits_{i=1}^n a_ix_i \gtreqless b\right\}$ è un insieme convesso.

I problemi della forma

$$
\begin{align*}
\min \qquad & c_1x_1 + c_2x_2 + \dots + c_nx_n \\
\text{s.t.} \qquad & a_{11}x_1 + a_{12}x_2 + \dots + a_{1n}x_n \le b_1 \\
& a_{21}x_1 + a_{22}x_2 + \dots + a_{2n}x_n \le b_2 \\
& \qquad \vdots \\
& a_{m1}x_1 + a_{m2}x_2 + \dots + a_{mn}x_n \le b_n
\end{align*}
$$

sono detti **problemi di ottimizzazione lineare** e possono essere espressi come

$$
\begin{align*}
\min \qquad & c_1x_1 + c_2x_2 + \dots + c_nx_n \\
\text{s.t.} \qquad & \begin{bmatrix}
a_{11} & a_{12} & \dots & a_{1n} \\
a_{21} & a_{22} & \dots & a_{2n} \\
\vdots & \vdots & \ddots & \vdots \\
a_{m1} & a_{m2} & \dots & a_{mn}
\end{bmatrix} \cdot \begin{bmatrix}
x_1 \\ x_2 \\ \vdots \\ x_n
\end{bmatrix} \le \begin{bmatrix}
b_1 \\ b_2 \\ \vdots \\ b_n
\end{bmatrix}
\end{align*}
$$

## Ottimizzazione lieare intera

Un problema di ottimizzazione lineare intera è esattamente come un problema di ottimizzazione lineare, tranne che ha vincoli aggiuntivi della forma $x_i \in \mathbb{Z} \ \forall i \in I \subseteq \{1, \dots, n\}$ e/o vincoli della forma $x_i \in \{0, 1\} \ \forall i \in I \subseteq \{1, \dots, n\}$ (le variabili vincolate in questo modo vengono dette **variabili booleane**).

### Variabili booleane

::: {.callout .callout-example title="Problema dello zaino"}
Un ladro deve rubare oggetti da una casa. Ciascun oggetto ha un valore e un peso associati, lo zaino del ladro ha una capacità massima che non può essere superata. Si vuole trovare la combinazione di oggetti che massimizzi il valore portato a casa.
I parametri di questo problema sono:

- $v_i$: il valore dell'oggetto $i$-esimo;
- $p_i$: il peso dell'oggetto $i$-esimo;
- $c$: la capacità dello zaino.

Le variabili da trovare per questo problema sono:

- $x_i \in \{0, 1\}$: indica se l'oggetto $i$-esimo è da prendere ($x_i = 1$) o meno ($x_i = 0$).

Il problema da ottimizzare è dunque

$$
\begin{align*}
\max \qquad & \sum_{i=1}^n v_ix_i \\
\text{s.t.} \qquad & \sum_{i=1}^n p_ix_i \le c \\
& x_i \in \{0, 1\} \quad \forall i = {1, \dots, n}
\end{align*}
$$

Immaginiamo che alcuni oggetti siano inutili se rubati da soli e che dunque vadano rubati insieme ad altri o che due oggetti, per qualche motivo, non possano essere rubati e messi nello zaino assieme (ad esempio una sfera di plutonio ed un pezzo di carburo di tungsteno): vengono aggiunti dei vincoli aggiuntivi a titolo di esempio (assumendo che $n$ sia abbastanza grande).

1. Se viene rubato l'oggetto 1, allora si devono rubare anche gli oggetti 7 e 8;
2. Se viene rubato l'oggetto 3, allora non deve essere rubato l'oggetto 5;
3. Se vengono rubati gli oggetti 4 e 6 assieme, allora deve essere rubato anche l'oggetto 5;
4. Se vengono rubati gli oggetti 1 e 3 assieme, allora non deve essere rubato l'oggetto 8;
5. Viene rubato l'oggetto 4 se e solo se viene rubato l'oggetto 7.
:::

Tutti i vincoli sulle variabili booleane sono esprimibili in maniera abbastanza intuitiva e standard (tranne qualche caso): solitamente basta sostituire il "$\implies$" con il "$\le$" e le "$x_i = 0$" con $(1 - x_i)$ e lavorare un po' con le proprietà della logica per arrivare ad una funzione lineare utilizzabile nei vincoli. Segue tabella con esempi di conversioni.

| Vincolo logico                             | Vincolo lineare                                |
| ------------------------------------------ | ---------------------------------------------- |
| $x_1 = 1 \implies x_2 = 1$                 | $x_1 \le x_2$                                  |
| $x_1 = 1 \implies x_2 = 0$                 | $x_1 \le 1 - x_2$                              |
| $x_1 = 0 \implies x_2 = 0$                 | $1 - x_1 \le 1 - x_2 \equiv x_2 \le x_1$       |
| $x_1 = 1 \lor x_2 = 1$                     | $x_1 + x_2 > 1$                                |
| $x_1 = 0 \lor x_2 = 1$                     | $(1 - x_1) + x_2 \ge 1 \equiv x_1 \le x_2$     |
| $x_1 = 0 \land x_2 = 1$                    | $\begin{cases} x_1 = 0 \\ x_2 = 0 \end{cases}$ |
| $(x_1 = 1 \land x_2 = 1) \implies x_3 = 1$ | $x_3 \ge x_1 + x_2 - 1$                        |
| $(x_1 = 0 \land x_2 = 1) \implies x_3 = 1$ | $x_3 \ge (1 - x_1) + x_2  -1$                  |
| $x_1 = 1 \oplus x_2 = 1$                   | $x_1 + x_2 = 1$                                |

### Attivazione condizionale di variabili, intervalli e vincoli

E' possibile attivare e disattivare variabili, intervalli e vincoli a seconda della verità di alcune condizioni.

::: {.callout .callout-example title="Esempio"}
Si vuole modellare la quantità prodotta da un macchinario tenendo conto del fatto che esso possa essere acceso o spento. Nel primo caso questo potrà produrre una quantità positiva non infinita mentre nel secondo la produzione sarà nulla.

Si indica con $y \in \{0, 1\}$ lo stato di accensione del macchinario e con $x \in \mathbb{R}^+$ la quantità da esso prodotta. Per modellare tale situazione si può dire che

$$
\begin{cases}
y = 0 \implies x \le 0\\
y = 1 \implies x \le M
\end{cases}
$$

che può essere riscritto come $x \le My$.
:::

Il genere di approssimazione visto nell'esempio si chiama **big-M constraint** ed è necessario per non cadere nell'utilizzo di valori quali $\infty$ che non sono facilmente trattabili (ovviamente $M$ deve di un ordine di grandezza sufficientemente grande/piccolo per non rischiare di escludere una soluzione ma non talmente grande/piccolo da essere difficile da trattare).

E' possibile anche accendere e spegnere vincoli di appartenenza ad un intervallo:

$$
\begin{cases}
y = 0 \implies x = 0 \\
y = 1 \implies x \in [a, b]
\end{cases}
$$

diventa

$$
\begin{cases}
x \ge ay \\
x \le by
\end{cases}
$$

In generale, è necessario trovare un vincolo in funzione della variabile booleana che lo controlla e fare in modo che tale vincolo diventi ridondante (ad esempio qualcosa come $0 = 0$) in caso questo debba essere spento.

E' possibile spegnere vincoli nella loro interezza seguendo la stessa logica.

::: {.callout .callout-example title="Esempio"}
Si vuole attivare il vincolo $3x_1 + 4x_2 - 7x_3 \le 11$ solo nel caso in cui $y_1 = 1$ (con $x_1 \in [1, 3], x_2 \in \{0, 1\}, x_3 \in [-2, 6]$).

Nel "caso peggiore" l'espressione vale 27 dunque si può scrivere che
$$
\begin{align}
	3x_1 + 4x_2 - 7x_3 &\le \begin{cases}
		11 & y_1 = 1 \\
		27 & y_1 = 0
	\end{cases} \\
	&= 27 - 16y_1
\end{align}
$$
:::

### Ottimizzazione di problemi min-max, max-min e min-abs

::: {.callout .callout-definition title="Problemi di min-max"}
Un problema è detto di **min-max** se è della forma

$$
\begin{align*}
\min & \qquad \max\{e_1, e_2, \dots, e_n\}\\
\text{s.t.} & \qquad x \in X
\end{align*}
$$

dove le varie $e_i$ sono funzioni nelle variabili di controllo. La definizione è analoga per i problemi di **max-min**.
:::

In caso di problemi di _min-max_ si ha che la funzione da minimizzare non è lineare in quanto la funzione $\max$ non è lineare. In casi del genere, si introduce un nuova variabile $y$ e, nei vincoli, si aggiunge che $y \ge e_1, y \ge e_2, \dots, y \ge e_n$. Per il problemi di _max-min_, il procedimento è analogo.

::: {.callout .callout-property title="Proprietà"}
Il massimo di un insieme di funzioni convesse, è a sua volta convessa.
:::

::: {.callout .callout-definition title="Problemi di min-abs"}
Un problema è detto di **min-abs** se è della forma

$$
\begin{align}
\min \qquad & |e| \\
\text{s.t.} \qquad & x \in X
\end{align}
$$

dove $e$ è una funzione nelle variabili di controllo.
:::

Anche nel caso di problemi di _min-abs_ la funzione da minimizzare non è lineare, dunque la si sostituisce con una variabile $y$ e, nei vincoli, si impone che $y \ge e$ e che $y \ge -e$. Questo procedimento è giustificato dal fatto che $|e| = \max\{e, -e\}$.

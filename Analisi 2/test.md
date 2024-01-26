---
title: "Riassunto di Analisi II"
author: 
- "Alessandro Modica"
- "Joele Andrea Ortore"
- "Andrea Oggioni"
---

# Equazioni differenziali

## Equazioni differenziali ordinarie (EDO)

Risolvere un'equazione differenziale vuol dire trovare una funzione $y(t)$ che soddisfa un'equazione del tipo $y'(t) = f(t, y(t))$ (un equazione del genere è detta **in forma normale**).

Si dice **integrale generale** l'insieme di tutte le soluzioni di una EDO.

Una EDO si dice **del primo ordine** se contiene al massimo occorrnze della derivata prima della funzione incognita.

Per trovare il dominio di una EDO, si scrive $f(t, s)$ e si trovano le condizioni di esistenza sul piano t-s.

## Soluzioni costanti

Una **soluzione costante** è una soluzione della forma $y(t) = k \quad \forall t$.
Per trovare le soluzioni costanti, so sostituisce nella EDO $y(t)$ con $k$ e $y'(t)$ con $0$ (infatti la derivata di una costante è nulla) e si risolve per $k$ l'equazione ottenuta.

## EDO a variabili separabili

Una EDO del primo ordine si dice **a variabili separabili** se è della forma $y'(t) = h(t) \cdot g(y(t))$ con $h : J_1 \sube \mathbb{R} \to \mathbb{R}$ e $g : J_2 \sube \mathbb{R} \to \mathbb{R}$ continue.

Si dice _a variabili separabili_ perchè è possibile portare da un lato tutte le occorrenze di $y(t)$ e $y'(t)$ lasciando dall'altro tutte le occorrenze di $t$.

Le soluzioni costanti di una EDO a variabili separabili sono $y(t) = c$ dove $c$ sono le soluzioni di $g(c) = 0$.

Per risolvere una EDO a variabili separabili, è necessario seguire una procedura predefinita:

1.  Da $y'(t) = h(t) \cdot g(y(t))$ trovo che $f(t, y(t)) = h(t) \cdot g(y(t))$ quindi $f(t, s) = h(t) \cdot g(s)$.
2.  Trovo le soluzioni costanti $y(t) = s$ imponendo $g(s) = 0$.
3.  Divido l'equazione originale da entrambi i lati per $g(y(t))$ e integro da entrambi i lati da $t_0$ a $t$, ottenendo
    $$
    \int_{t_0}^t \frac{y'(t)}{g(y(t))} dt = \int_{t_0}^t h(t) dt
    $$
    Il lato destro viene risolto normalmente, il lato sinistro diventa
    $$
    \int_{t_0}^t \frac{y'(t)}{g(y(t))} dt = \int_{y(t_0)}^{y(t)} \frac{1}{g(y)} dy
    $$
    e si risolve per $t(t)$, inglobando tutte le costanti in $C$ (o altre lettere se la $C$ è già stata utilizzata).

## Problema di Cauchy per una EDO del primo ordine

Data una EDO del primo ordine $y'(t) = f(t, y(t))$, sia $(t_0, y_0)$ un punto appartenente al dominio di $f$. Il **problema di Cauchy** consiste nel determinare una soluzione dell'equazione che passi per quel punto.

$$
\begin{cases}
    y'(t) = f(t, y(t)) \\
    y(t_0) = y_0
\end{cases}
$$

Per una EDO del primo ordine, viene imposta quindi una sola condizione aggiuntiva.

Per risolvere un problema di Cauchy di tale genere, si seguono i seguenti passaggi:

1.  Trovo l'integrale generale.
2.  Impongo la condizione aggiuntiva per trovare la $C$.
3.  Sostituisco la $C$ nella soluzione originale.

Se il dominio di $f$ è diviso in più parti, è sufficiente considerare solo la regione in cui si trova il punto $(t_0, y_0)$.

## EDO del primo ordine lineari

Una EDO del primo ordine **lineare** in forma normale è una EDO della forma $y'(t) = a(t) \cdot y(t) + b(t)$ con $a,b : J \sub \mathbb{R} \to R$ continue su $J$.

Per le EDO del primo ordine lineari, è possibile enunciare il **teorema di esistenza e unicità globale per il problema di Cauchy**:
>   Siano $J \sub \mathbb{R}$, $a,b : J \to \mathbb{R}$ continue su $J$. Per ogni $t_0 \in J$ e $y_0 \in \mathbb{R}$ il problema di Cauchy ha un'unica soluzione definita $\forall t \in J$.

Da questo ne derivano 3 cose molto importanti:

1.  Dato un qualunque $(t_0, y_0) \in J \times \mathbb{R}$, c'è una soluzione della EDO che passa per tale punto; i grafici delle soluzioni riempiono tutto $J \times \mathbb{R}$.
2.  La soluzione è unica: i grafici non si intersecano.
3.  Tutte le soluziono sono definite $\forall t \in J$.

<div id="Dim1"></div>

Per risolvere una EDO del primo ordine lineare della forma $y'(t) = a(t) \cdot y(t) + b(t)$, considero $A(t)$ primitiva di $a(t)$, successivamente, moltiplico da entrambi i lati per $e^{-A(t)}$ e porto a sinistra tutti i termini che contengono y:

$$
y'(t) \cdot e^{-A(t)} = a(t) \cdot e^{-A(t)} \cdot y(t) + b(t) \cdot e^{-A(t)} \\
y'(t) \cdot e^{-A(t)} - a(t) \cdot e^{-A(t)} \cdot y(t) = b(t) \cdot e^{-A(t)} \\
$$

Ora riconosco che $(y(t) \cdot e^{-A(t)})' = y'(t) \cdot e^{-A(t)} - y(t) \cdot e^{-A(t)} \cdot a(t)$ dunque posso sostituire la parte a sinistra dell'uguale con $(y(t) \cdot e^{-A(t)})'$:

$$
(y(t) \cdot e^{-A(t)})' = b(t) \cdot e^{-A(t)}
$$

Per il teorema fondamentale del calcolo integrale, posso integrare da ambo i lati, poi isolo $y(t)$:

$$
\int_{t_0}^t \left(y(x) \cdot e^{-A(x)} \right)' dx = \int_{t_0}^t b(x) \cdot e^{-A(x)} dx \\
\left[ y(x) \cdot e^{-A(x)} \right]_{t_0}^{t} = \int_{t_0}^t b(x) \cdot e^{-A(x)} dx \\
y(t) \cdot e^{-A(t)} - \underbrace{y(t_0) \cdot e^{-A(t_0)}}_{= C} = \int_{t_0}^t b(x) \cdot e^{-A(x)} dx \\
y(t) \cdot e^{-A(t)} = \int_{t_0}^t b(x) \cdot e^{-A(x)} dx + C \\
y(t) = e^{A(t)} \left( \int_{t_0}^t b(x) \cdot e^{-A(x)} dx + C \right)
$$

E' detta omogenea una EDO lineare con $b(t) = 0 \forall t$.

In tal caso, l'EDO è anche a variabili separabili e si può risolvere come si preferisce.

## Equazioni di Bernoulli

Un'equazione di Bernoulli è una EDO del primo ordine non lineare della forma $y'(t) = k(t) \cdot y(t) + h(t) \cdot y(t)^\alpha$ con $\alpha \in \mathbb{R}$, $\alpha \ne 0$, $\alpha \ne 1$, $h, k$ continue.

Se $\alpha$ è irrazionale oppure è razionale con denominatore pari, allora $y^\alpha$ ha senso solo se $y \ge 0$. Per semplificare, verrà trattato solo il caso di $y \gt 0 \quad \forall \alpha$.
Per $\alpha \in \{0, 1\}$, non vale il teorema di unicità.
Se $\alpha \lt 0$ non ha senso $y = 0$.

Per risolvere un'equazione di Bernoulli, c'è un procedimento da seguire.

Inizio crcando le soluzioni costanti:

-   $y(t) = 0$ è sempre soluzione.
-   Se sia $h$ che $k$ sono costanti, posso raccogliere $y(t)$e trovare un'altra soluzione costante:
    $$
    k \cdot y(t) + h \cdot y(t)^\alpha = 0 \\
    y(t) \cdot (k + h \cdot y(t)^{\alpha - 1}) = 0 \\
    k + h \cdot y(t)^{\alpha - 1} = 0 \\
    y(t)^{\alpha - 1} = -\frac{k}{h} \\
    y(t) = \left( -\frac{k}{h} \right)^{\frac{1}{\alpha - 1}}
    $$

Procedo cercando le soluzioni non costanti dividendo da entrambi i lati per $y(t)^\alpha$:

$$
\frac{y'(t)}{y(t)^\alpha} = k(t) \cdot y(t)^{1 - \alpha} + h(t)
$$

Pongo $z(t) = y(t)^{1 - \alpha}$ e calcolo $z'(t)$:

$$
\begin{align*}
    z'(t) &= (1 - \alpha) \cdot y(t)^{1 - \alpha + 1} \cdot y'(t)\\
    &= (1 - \alpha) \cdot \frac{y'(t)}{y(t)^\alpha} \\
    &= (1 - \alpha) \cdot \left( k(t) \cdot y(t)^{1 - \alpha} + h(t) \right) \\
    &= (1 - \alpha) \cdot \left( k(t) \cdot z(t) + h(t) \right) \\
    &= \underbrace{(1 - \alpha) \cdot k(t)}_{a(t)} \cdot z(t) + \underbrace{(1 - \alpha) \cdot h(t)}_{b(t)}
    &= a(t) \cdot z(t) + b(t)
\end{align*}
$$

Ora che ho ottenuto un'equazione lineare, la risolvo per $z(t)$ e poi ritorno in $y$:

$$
y(t) = z(t)^\frac{1}{1 - \alpha}
$$

## EDO del secondo ordine lineari

Una **EDO del secondo ordine lineare** è un'equazione della forma $a(t) \cdot y''(t) + b(t) \cdot y'(t) + c(t) \cdot y(t) = f(t)$ con $a, b, c, f : J \sube \mathbb{R} \to \mathbb{R}$ e $a \ne 0$.

## Problema di Cauchy per una EDO del secondo ordine lineare

Il problema di Cauchy per una EDO del secondo ordine è analogo al problema di Cauchy per una EDO del primo ordine, ma vengono imposte, al posto che una, due condizioni aggiuntive che vanno a vincolare sia la $y$ che la $y'$.
In particolare, un problema di Cauchy per una EDO lineare del secondo ordine è un sistema della forma

$$
\begin{cases}
    a(t) \cdot y''(t) + b(t) \cdot y'(t) + c(t) \cdot y(t) = f(t) \\
    y(t_0) = y_0 \\
    y'(t_0) = v_0
\end{cases}
$$

E' possibile enunciare il **teorema di esistenza e unicità globale per il problema di Cauchy** anche per le EDO del secondo ordine lineari:
>   Data la EDO $a(t) \cdot y''(t) + b(t) \cdot y'(t) + c(t) \cdot y(t) = f(t)$ con $a, b, c, f, : J \sube \mathbb{R} \to \mathbb{R}$ continue e $a \ne 0$ in $J$ e assegnati $t_0 \in J$ e $y_0, v_0 \in \mathbb{R}$ allora il problema di Cauchy definito come sopra ha un'unica soluzione $y(t)$ definita $\forall t \in J$.

Per il **principio di sovrapposizione**, se $y_1$ e $y_2$ sono soluzione della stessa EDO, $y_1 - y_2$ è soluzione della EDO omogenea associata e tutte le soluzioni di quest ultima formano uno spazio vettoriale

## Teorema di struttura dell'integrale generale di EDO del secondo ordine lineari omogenee

Siano $a,b,c : J \sube \mathbb{R} \to \mathbb{R}$ continue in $J$ con $a \ne 0$.

L'integrale generale dell'equazione omogenea $a(t) \cdot y''(t) + b(t) \cdot y'(t) + c(t) \cdot y(t) = 0$ è uno spazio vettoriale di dimensione 2, cioè le soluzioni sono della forma $y_0(t) = C_1 \cdot y_{o1}(t) + C_2 \cdot y_{o2}(t)$ con $C_1, C_2 \in \mathbb{R}$ dove $y_{o1}$ e $y_{o2}$ sono due soluzioni linearmente indipendenti.

### Dimostrazione richiesta

Sia $V$ lo spazio vettoriale delle funzioni di classe $\mathcal{C}^2$ su $J$.

L'integrale generale dell'omogenea è il seguente sottoinsieme di $V$:

$$
W = \left\{ y \in V : ay'' + by' + cy = 0 \right\} = \ker L
$$

dove $L$ è l'operatore definito come $Ly = ay'' + by' + cy$.

In quanto kernel di un'applicazione lineare, $W$ è sottospazio vettoriale.

Per dimostrare che $W$ ha dimensione 2, devo 

1.  Esibire due soluzioni linearmente indipendenti
2.  Dimostrare che ogni altra soluzione si scrive come combinazione lineare delle due soluzioni trovate al punto precedente.

Cerco due soluzioni linearmente indipendenti.

Sia $t_0 \in J$, per il teorema di esistenza e unicità globale, posso scegliere due punti linearmente indipendenti e cercare, per ciascun punto, una soluzione che passa per tale punto.

Scelgo i punti $(1, 0)$ e $(0, 1)$

$$
\begin{cases}
    ay''_{o1} + by'_{o1} + cy_{o1} = 0 \\
    y_{o1}(t_0) = 1 \\
    y'_{o1}(t_0) = 0
\end{cases}
\qquad
\begin{cases}
    ay''_{o2} + by'_{o2} + cy_{o2} = 0 \\
    y_{o2}(t_0) = 0 \\
    y_{o2}(t_0) = 1
\end{cases}
$$

Per verificare che le due soluzioni sono linearmente indipendenti, suppongo, per assurdo, che non lo siano, quindi che $y_{o1}(t) = k \cdot y_{o2}(t) \quad \forall t \in J$.

Per $t = t_0$ si ha che $y_{o1} = k \cdot y_{o2} \iff 1 = k \cdot 0$ che è impossibile, pertanto le due soluzioni sono linearmente indipendenti.

Ora dimostro che qualunque soluzione è compbinazione lineare delle due precedenti.

Sia $y_o$ una soluzione qualunque dell'equazione omogenea:

$$
y_o(t) = C_1 \cdot y_{o1}(t) + C_2 \cdot y_{o2}(t) \implies \begin{cases}
    y_o(t_0) = C_1 \cdot y_{o1}(t_0) + C_2 \cdot y_{o2}(t_0) = C_1 \cdot 1 + C_2 \cdot 0 = C_1\\
    y'_o(t_0) = C_1 \cdot y'_{o1}(t_0) + C_2 \cdot y'_{o2}(t_0) = C_1 \cdot 0 + C_2 \cdot 1 = C_2
\end{cases}
$$

Definisco ora una funzione $z(t) = C_1 \cdot y_{o1}(t) + C_2 \cdot y_{o2}(t) = y_o(t)$.
$z(t)$ risolve lo stesso problema di Cauchy di $y_o(t)$ è quindi per il teorema di esistenza e unicità globale del problema di Cauchy, sono la stessa cosa.

## Struttura dell'integrale generale di EDO del secondo ordine lineari non omogenee

Siano $a,b,c : J \sube \mathbb{R} \to \mathbb{R}$ continue in $J$ con $a \ne 0$.

L'integrale generale dell'equazione completa $a(t)y''(t) + b(t)y'(t) + c(t)y(t) = f(t)$ è $y(t) = y_o(t) + y_p(t) = C_1 \cdot y_{o1}(t) + C_2 \cdot y_{o2}(t) + y_p(t)$ con $y_o$ soluzione dell'omogenea associata, $y_{o1}, y_{o2}$ soluzioni particolari linearmente indipendenti dell'omogenea associata e $y_p$ soluzione particolare dell'equazione completa.

In pratica, l'integrale generale di una EDO del secondo ordine lineare non omogenea è uno spaizo affine di dimensione due che consiste dello span delle soluzioni dell'omogenea associata, translata di $y_p$.

## Risoluzione di EDO omogenee a coefficienti costanti

Per risolvere una edo omogenea a coefficienti costanti, scrivo il **polinomio caratteristico** $p(\lambda) = a \lambda^2 + b \lambda + c$ e risolvo **l'equazione caratteristica** $p(\lambda) = 0$ con $\Delta = b^2 - 4ac$.

A seconda del segno di $\Delta$, l'integrale generale assume forme diverse:

| $\Delta$       | Integrale Generale                                                            |
| -------------- | ----------------------------------------------------------------------------- |
| $\Delta \gt 0$ | $y(t) = C_1 e^{\lambda_1 t} + C_2 e^{\lambda_2 t}$                            |
| $\Delta = 0$   | $y(t) = C_1 e^{\lambda t} + C_2 t e^{\lambda t}$                              |
| $\Delta \lt 0$ | $y(t) = e^{\Re \lambda t}(C_1 \cos(\Im \lambda t) + C_2 \sin(\Im \lambda t))$ |

## Sistemi differenziali lineari

Un sistema differenziale lineare è un'equazione della forma $\underline y'(t) = A \underline y(t) + \underline b(t)$ con $\underline y', \underline y, \underline b \in (\mathbb{R} \to \mathbb{R})^n$, $A \in \mathcal{M}_\mathbb{R}(n)$.

Tale equazione può essere espansa:

$$
\begin{pmatrix}
    y'_1 \\ \vdots \\ y'_n
\end{pmatrix} = A \cdot \begin{pmatrix}
    y_1 \\ \vdots \\ y_n
\end{pmatrix} + \begin{pmatrix}
    b_1 \\ \vdots \\ b_n
\end{pmatrix}
$$

Il problema di Cauchy per un sistema differenziale lineare consiste nell'imporre una condizione per ogni $y_i$:

$$
\begin{cases}
    \underline y'(t) = A \underline y(t) + \underline b(t) \\
    \underline y(t_0) = \underline y_0
\end{cases}
$$

Anche nel caso dei sistemi differenziali possibile enunciare il **teorema di esistenza e unicità globale per il problema di Cauchy**:
> Sia $A \in \mathcal{M}_\mathbb{R}(n)$ e $b_i : j \sube \mathbb{R} \to \mathbb{R}$ continue. Dati $t_0 \in J$ e $\underline y_0 \in \mathbb{R}$, il problema di Cauchy enunciato come sopra, ha un'unica soluzione $\underline y(t)$ definita $\forall t$.

Questo teorema ha 2 conseguenze importanti:

1. Se le funzioni $b_i$ sono definite e continue in tutto $\mathbb{R}$ allora le soluzione $y_i(t)$ sono definite in tutto $\mathbb{R}$.
2. Se il sistema è omogeneo ($b_i = 0$) allora
   $$
    \begin{cases}
        \underline y'(t) = A \cdot \underline y(t) \\
        \underline y(t_0) = \underline 0
    \end{cases}
   $$
   ha una sola soluzione.

Dato un sistema differenziale lineare $n \times n$ omogeneo, chiamiamo **sistema fondamentale di soluzioni** una famiglia di $n$ soluzioni linearmente indipendenti $\underline y_{o1}(t), \dots, \underline y_{on}(t)$ che costituiscono una base dello spazio vettoriale delle soluzioni del sistema.

## Determinante wronskiano

Supponiamo di conoscere $n$ soluzioni $\underline y_{o1}(t), \dots, \underline y_{on}(t)$ di un sistema differenziale lineare $n \times n$ omogeneo: il sistema fondamentale esiste se e solo se esiste un $t_0$ (tipicamente 0) tale che $\det(\underline y_{o1}(t) | \dots | \underline y_{on}(t)) \ne 0$.

Si chiama **matrice wronskiana** la matrice ottenuta affiancando un sistema fondamentale:

$$
W = \underline y_{o1}(t), \dots, \underline y_{on}(t)
$$

Con questa notazione, l'integrale generale del sistema omogeneo diventa $\underline y_o(t) = W(t) \cdot \underline C$ con $\underline C \in \mathbb{R}$ e la soluzione del problema di Cauchy si ottiene scegliendo $\underline C = W(t_0)^{-1} \cdot \underline y_o$ quindi $\underline y(t) = W(t) \cdot (t_0)^{-1} \cdot \underline y_o$.

## Risoluzione esplicita di sistemi con $A$ diagonalizzabile reale

Se $A \in \mathcal{M}_\mathbb{R}(2)$ è diagonalizzabile reale ($A$ è diagonalizzabile reale $\iff$ esistono $n$ autovettori di $A$ che formano una base di $\mathbb{R}^n$ $\iff$ tutti gli autovalori sono regolari; se $A$ è simmetrica o ha $n$ autovalori reali distinti, allora è diagonalizzabile reale), detta $S$ la matrice ottenuta affiancando una base di $\mathbb{R}^n$, vale che 

$$
S = (\underline v_1 | \dots | \underline v_n) \qquad S^{-1} \cdot A \cdot S = \Lambda \qquad \Lambda = \text{diag}(\lambda_1, \dots, \lambda_s) \qquad A = S \cdot \Lambda \cdot S^{-1}
$$

Una matrice wronskiana relativa al sistema omogeneo è $W(t) = (e^{\lambda_1 t} \underline v_1| \dots | e^{\lambda_n t} \underline v_n)$.

Eqivalentemente, l'integrale generale è $\underline y_0(t) = C_1 e^{\lambda_1 t} \underline v_1 + \dots + C_n e^{\lambda_n t} \underline v_n$ con $C_1, \dots, C_n \in \mathbb{R}$.

## Esponenziale di una matrice

Sia $A \in \mathcal{M}_\mathbb{R}(n)$, la **matrice esponenziale** $e^A$ è definita dalla serie

$$
e^A = \sum_{k = 0}^{+ \infty} \frac{A^k}{k!} = I + A + \frac{1}{2} A^2 + \frac{1}{3} A^3 + \dots
$$

e risulta convergente $\forall A$.

Se $A$ è diagonalizzabile reale, allora è più semplice calcolare la matrice esponenziale:

$$
e^A = S \cdot \text{diag}(e^{\lambda_1} , \dots, e^{\lambda_n}) \cdot S^{-1}
$$

ove $S$ è è la matrice ottenuta affiancando gli autovettori di $A$ è i vari $\lambda_i$ sono gli autovalori.

Se $A$ è diagonalizzabile reale allora $e^{At}$ è una matrice wronskiana relativa al sistema omogeneo $\underline y' = A \underline y$ e l'integrale generale di tale sistema si scrive come $\underline y_o = e^{At} \cdot \underline C$ con $\underline C \in \mathbb{R}$.

La matrice esponenziale è comoda per risolvere il problema di Cauchy: dato che $(e^{At_0})^{-1} = e^{-At_0}$, la soluzione del problema di Cauchy è $\underline t(t) = e^{A(t - t_0)}\underline y_0$.

## Risoluzione esplicita di sistemi con $A$ $2 \times 2$ con autovalori complessi coniugati

Sia $A \in \mathcal{M}_{\mathbb{R}}(2)$ con autovalori $\lambda$ e $\overline \lambda$ complessi coniugati, $\Im \lambda = 0$ e $\underline v \in \mathbb{C}^2$ autovalore associato a $\lambda$.
Un sistema fondamentale di soluzioni di un sistema omogeneo è dato da

$$
y_{o1}(t) = \Re (e^{\lambda t} \underline v) \qquad y_{o2}(t) = \Im(e^{\lambda t} \underline v)
$$

Equivalentemente, l'integrale generale è $y_o(t) = C_1 \Re(e^{\lambda t} \underline v) + C_2 \Im(e^{\lambda t} \underline v)$

# Varie ed eventuali

Di seguito lista delle dimostrazioni da conoscere per l'esame:

1.  [Formula risolutiva EDO del primo ordine lineari](#Dim1)
2.  [Teorema di struttura dell'integrale generale di EDO del secondo ordine lineari omogenee](#teorema-di-struttura-dellintegrale-generale-di-edo-del-secondo-ordine-lineari-omogenee)
3.  AAAAAAAAAAAA
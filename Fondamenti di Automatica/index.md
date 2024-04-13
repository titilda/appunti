---
title: "Riassunto di Fondamenti di Automatica"
author:
- "Andrea Oggioni"
---

# Classificazione di sistemi

Un sistema è una rappresentazione di un oggetto fisico che interagisce col mondo esterno attraverso un vettore di input ($u(t)$) e un vettore di output ($y(t)$). Lo stato del sistema è memorizzato dentro il vettore di stato ($x(t)$)

Un sistema è detto a **tempo continuo** se è della forma

$$
\frac{dx(t)}{dt} = f(x(t), u(t), t) \quad y(t) = g(x(t), u(t), t) \qquad t \in \mathbb{R}
$$

mentre è detto a **tempo discreto** se è della forma

$$
x(k + 1) = f(x(k), u(k), k) \quad y(k) = g(x(k), u(k), k) \qquad k \in \mathbb{N} 
$$

_D'ora in poi, si considereranno equivalenti le notazioni $\frac{dx(t)}{dt}$ e $\dot x(t)$_

Se l'uscita di un sistema dipende in ogni istante dal solo ingresso allora viene detto **statico**, altrimenti è **dinamico** (nel primo caso, il sistema non è dotato di stato).

Se un sistema ha esattamente un solo input ed un solo output, allora è detto **SISO**, altrimenti è **MIMO**.

Se l'uscita di un sistema dipende sia dallo stato che dall'ingresso, allora tale sistema è detto **proprio**, altrimenti, se l'uscita dipende solamente dallo stato, è detto **strettamente proprio**.

Se le funzioni $f$ e $g$ non dipendono strettamente dal parametro $t$ o $k$, allora il sistema viene detto **tempo-invariante** o **stazionario** , altrimenti è **tempo-variante** o **dinamico**.

Se il sistema è dinamico, è possibile studiarne gli equilibri: un equilibrio è una coppia $(\overline x(t), \overline u(t))$ tale che

$$
\frac{d \overline x(t)}{dt} = 0 \quad f(\overline x(t), \overline u(x)) = 0 \qquad \forall t
$$

La stabilità di un equilibrio determina la risposta del sistema in equilibrio ad una perturbazione.

Ci sono tre tipologie di equilibrio:

- equilibrio **stabile**: in risposta alla perturbazione, lo stato non si allontana più di una zona predefinita
- equilibrio **instabile**: in risposta alla perturbazione, lo stato si allontana sempre di più
- equilibrio **asintoticamente stabile**: dopo una perturbazione, all'infinito, lo stato tende a ritornare nella posizione in cui si trovava prima della perturbazione

Se le funzioni $f$ e $g$ sono funzioni lineari su $x(t)$ e $u(t)$ (vale anche per il tempo discreto), allora il sistema viene detto **lineare** e può essere scritto nella forma

$$
f(x(t), u(t)) = A \cdot x(t) + B \cdot u(t) \\
g(x(t), u(t)) = C \cdot x(t) + D \cdot u(t)
$$

con $A \in \mathcal{M}_\mathbb{R}(n)$, $B \in \mathcal{M}_\mathbb{R}(n, m)$, $C \in \mathcal{M}_\mathbb{R}(p, n)$ e $D \in \mathcal{M}_\mathbb{R}(p, m)$.

# Analisi di sistemi a tempo continuo

## Sistemi LTI

Un sistema **LTI** (Lineare, Tempo-Invariante) è un sistema della forma

$$
\frac{dx(t)}{dt} = Ax(t) + Bu(t) \quad y(t) = Cx(t) + Du(t)
$$

Dato uno stato iniziale $x(t_0) = x_0$, i movimenti dello stato e dell'uscita si calcolano come segue (**formula di Lagrange**):

$$
x(t) = e^{A(t - t_0)} x(t_0) + \underbrace{\int_{t_0}^{t} e^{A(t - \tau)}B u(\tau) \, d\tau}_{\text{Convoluzione}} \qquad t \ge t_0 \\
y(t) = Ce^{A(t - t_0)} x(t_0) + C \int_{t_0}^t e^{A(t - \tau)}Bu(\tau) \, d\tau + Du(t) \qquad t \ge t_0
$$

Come si può notare, per lo studio dei sistemi LTI possono risultare comode le proprietà della matrice esponenziale: vedere [Analisi II](https://appunti.titilda.org/Analisi%202/index.html#esponenziale-di-una-matrice) oppure l'[appendice in fondo](#proprietà-matrice-esponenziale).

Di un sistema, è possibile studiare separatamente il **movimento libero** (spegnendo gli ingressi) e il **movimento forzato** (imponendo stato iniziale nullo).

I due movimenti costituiscono i due addendi nella formula di Lagrange.

### Meta variabili

Per sfruttare appieno le potenzialità delle matrici esponenziali, è necessario che la matrice $A$ sia diagonale dunque si trasforma il sistema dato in uno equivalente che opera su meta-variabili non fisiche con matrici $A', B', C', D'$ di cui $A'$ è diagonale.

Sia $T$ una matrice non singolare e sia $z(t) = Tx(t)$ lo stato del sistema equivalente, allora

$$
x(t) = T^{-1}z(t) \\
\dot z(t) = T \dot x(t) = T(Ax(t) + Bx(t)) = TAT^{-1}z(t) + TBu(t) = A'z(t) + B'u(t) \\
y(t) = Cx(t) + Du(t) = CT^{-1}z(t) + Du(t)
$$

da cui segue che 

$$
\begin{cases}
    A' = TAT^{-1} \\
    B' = TB \\
    C' = CT^{-1} \\
    D' = D
\end{cases}
$$

Ottenere la matrice $T$ è un semplice esercizio di algebra lineare: se $A$ è diagonalizzabile, $A'$ è la matrice diagonale con composta dagli autovalori di $A$ e $T^{-1}$ è la matrice diagonalizzante ottenuta affiancando gli autovettori di $A$.

### Risoluzione di casi particolari

#### Sistemi con matrice $A$ diagonale
Se si vuole studiare il movimento libero di un sistema con $A$ diagonale, è sufficiente calcolare $x(t) = e^{At}x_0$ con $t_0 = 0$.

#### Sistemi con matrice $A$ triangolare

Se si vuole studiare il movimento libero di un sistema con matrice $A$ triangolare superiore (analogo per matrici triangolari inferiori), serve utilizzare più volte volte la formula di Lagrange; segue algoritmo:

1. Siano
   $$
    x(t) = \begin{bmatrix}
        x_1(t) \\ x_2(t) \\ \vdots \\ x_n(t)
    \end{bmatrix} \quad \dot x(t) = \begin{bmatrix}
        \dot x_1(t) \\ \dot x_2(t) \\ \vdots \\ \dot x_n(t)
    \end{bmatrix}
   $$
   il vettore di stato e la sua derivata e $A = [a_{ij}]$ la matrice $A$.
2. Si calcoli $x_n(t) = e^{a_{nn}t}x_n(0)$
3. $\dot x_{n-1}$ dipende da $x_{n-1}$ e da $x_n$ dunque si tratta $x_n$ come ingresso virtuale e si calcola $x_{n-1}$ con la formula di Lagrange.
4. In generale, si procede a ritroso, calcolando una variabile per volta con la formula di Lagrange e considerando tutte le altre variabili di stato da cui dipende come ingresso virtuale. 

#### sistemi con matrice $A$ diagonalizzabile

Se si vuole studiare il movimento libero di un sistema con matrice $A$ diagonalizzabile, si sfrutta la tecnica delle meta variabili descritto appena sopra ottenendo che

$$
T^{-1} = \left[ v_1 \middle| v_2 \middle| \dots \middle| v_n \right] \\
\dot z = TAT^{-1}z = \text{diag}(\lambda_1, \lambda_2, \dots, \lambda_n) z
$$

da cui

$$
z(t) = \begin{bmatrix}
    e^{\lambda_1 t} z_1(0) \\ e^{\lambda_2 t} z_2(0) \\ \vdots \\ e^{\lambda_n t} z_n(0)
\end{bmatrix} = e^{\text{diag}(\lambda_1, \lambda_2, \dots, \lambda_n)}z(0)
$$

Successivamente si torna allo stato in $x$:

$$
x(t) = T^{-1}z(t) = \underbrace{T^{-1}e^{\text{diag}(\lambda_1, \lambda_2, \dots, \lambda_n)}T}_{e^{At}}x(0)
$$

da cui segue che il movimento libero per un sistema LTI con matrice $A$ diagonalizzabile è 

$$
x(t) = T^{-1} \text{diag}(e^{\lambda_1 t}, e^{\lambda_2 t}, \dots, e^{\lambda_n t}) x_0
$$

I termini che compaiono nella matrice al centro sono detti **modi**.

#### Sistemi con matrice $A$ non diagonalizzabile

Nel caso in cui la matrice $A$ non sia diagonalizzabile, $\exists T_J \in \mathcal{M}_\mathbb{R}(n) : A_J = T_JAT_J^{-1}$ tale che $A_J$ sia una matrice di blocchi di Jordan con gli autovalori di $A$ sulla diagonale (non ci interessa conoscere $T_J$, ci basta sapere che esiste):

$$
A_J = \underbrace{\left[ \begin{array}{c|c}
    \begin{matrix}
        \lambda_1 & 1 & 0 \\ 0 & \lambda_1 & 1 \\ 0 & 0 & \lambda_1
    \end{matrix} & 0 \\
    \hline
    0 & \begin{array}{c|c}
        \lambda_2 & 0 \\
        \hline
        0 & \begin{array}{cc}
            \lambda_3 & 1 \\
            0 & \lambda_3
        \end{array}
    \end{array}
\end{array} \right]}_{\text{Matrice esemplare di blocchi di Jordan}} \implies e^{A_Jt} = \begin{bmatrix}
    e^{\lambda t} & t e^{\lambda t} & \frac{t^2}{2!} e^{\lambda t} \\
    0 & e^{\lambda t} & t e^{\lambda t} \\
    0 & 0 & e^{\lambda t}
\end{bmatrix}
$$

### Studio degli equilibri

Una volta trovati i modi del sistema sotto esame, è possibile studiarne gli equilibri.

I punti di equilibrio sono le soluzioni dell'equazione

$$
A \overline x + B \overline u = 0
$$

Se $A$ non è invertibile, tale equazione può avere infinite soluzioni (se $-Bu \in \text{Cols}(A)$) oppure nessuna.

Se $A$ è invertibile, la soluzione esiste sempre ed è della forma

$$
\overline x = -A^{-1}B \overline u
$$

Nel caso di sistemi con più punti di equilibrio, un equilibrio $(\overline x, \overline u)$ è **stabile** se e solo se tutti gli equilibri $(x_i, u_i)$ sono stabili quindi se $\Re(\lambda_i) \le 0 \ \forall i$ e $\Re(\lambda_i) = 0 \implies \lambda_i$ è regolare.

Un sistema LTI è **asintoticamente stabile** se $\Re(\lambda_i) \lt 0 \ \forall i$.

Un sistema LTI è **instabile** se $\exists \lambda_i : Re(\lambda_i) \gt 0 \lor \Re(\lambda_i) = 0 \land \lambda_i$ non è regolare.

Sia
$$
\varphi(\lambda) = \varphi_0 \lambda^n + \varphi_1 \lambda^{n-1} + \dots + \varphi_{n-1} \lambda + \varphi_n = \varphi_0 \prod_{i=1}^n (\lambda - \lambda_i)
$$

il polinomio caratteristico di $A$.

Se $\varphi_0 \gt 0$ allora $\Re(\lambda_i) \lt 0 \ \forall i \implies \forall i \ \varphi_i \gt 0$

Una condizione necessaria per avere stabilità asintotica è che tutti i coefficienti $\varphi_i$ abbiano tutti lo stesso segno.

Per sistemi di alto ordine, è possibile applicare un procedimento meccanico che consente, con un numero limitato di passaggi, di distinguere tra sistemi asintoticamente stabili ed instabili: il criterio do Routh.

Dato un polinomio caratteristico per un sistema di ordine $n$, si inizia mettendo, in una tabella, sulla prima riga tutti i coefficienti di indice pari e sulla seconda tutti i coefficienti di ordine dispari. Entrambe le righe sono seguite da infiniti zeri. Successivamente si prenda la matrice ottenuta affiancando la prima e la seconda colonna, se ne calcoli il determinante e lo si divida per il numero in basso a sinistra della matrice appena ottenuta. Si faccia lo stesso con la prima e la terza riga, poi con la prima e la quarta e cos' via fino ad arrivare agli infiniti zeri. Con i numeri ottenuti, si componga una riga poi aggiunta alla tabella. Si ignori la prima riga e si ripeta il procedimento fino ad ottenere un solo numero seguito da infiniti zeri.

Si dis-ignorino tutte le righe ignorate:

- se tutti gli elementi della prima colonna hanno lo stesso segno allora il sistema è asintoticamente stabile
- se, percorrendo la prima colonna dall'alto verso il basso, si incontrano dei cambiamenti di segno, il sistema è instabile e il numero di cambiamenti di segno corrisponde al numero di autovalori con parte reale positiva
- se c'è uno zero nella prima colonna, vuol dire che ci sono autovalori puramente immaginari e non è possibile distinguere se il sistema in questione sia semplicemente stabile o instabile con blocchi di Jordan
  
## Sistemi nonlineari, tempo-invarianti

Si vuole studiare il movimento dello stato di un sistema nonlineare vicino ai punti di equilibrio:

$$
u(t) = \overline u + \delta u(t) \quad x(t) = \overline x + \delta x(t) \quad y(t) = \overline y + \delta y(t)
$$

Per fare ciò si deve linearizzare il sistema nell'intorno di ciascun punto di equilibrio:

$$
\dot{\delta x}(t) = \underbrace{\left. \frac{\partial f}{\partial x} \right|_{\overline x, \overline u}}_{A} \delta x(t) + \underbrace{\left. \frac{\partial f}{\partial u} \right|_{\overline x, \overline u}}_{B} \delta u(t) \\
\dot{\delta y}(t) = \underbrace{\left. \frac{\partial g}{\partial x} \right|_{\overline x, \overline u}}_{C} \delta x(t) + \underbrace{\left. \frac{\partial g}{\partial u} \right|_{\overline x, \overline u}}_{D} \delta u(t)
$$

Una volta fatto ciò, si vanno ad analizzare gli autovalori della matrice $A$:

- se tutti gli autovalori hanno parte reale negativa, vi è stabilità asintotica
- se esiste un autovalore a parte reale positiva, vi è instabilità
- se vi sono autovalori puramente immaginari, non si può dire nulla

Per un sistema di ordine 1, un po' come nella [linea delle fasi](https://appunti.titilda.org/Analisi%202/index.html#studio-qualitativo-di-edo-del-primo-ordine), se $\frac{\partial f}{\partial x} \gt 0$ allora si tende ad andare a destra mentre se $\frac{\partial f}{\partial x} \lt 0$ allora si tende ad andare a sinistra. I punti in cui $\frac{\partial f}{\partial x} = 0$ sono i punti di equilibrio e sono stabili o meno a seconda della direzione in cui tendono ad andare le derivate come visto prima (il tutto è valido per un $\overline u$ fissato).

# Appendice

## Proprietà matrice esponenziale

$$
e^a = \sum_{i=0}^{+ \infty} \frac{a^i}{i!} \implies e^{At} = \sum_{i=0}^{+ \infty} \frac{A^it^i}{i!} = \mathbb{I} + at + \frac{A^2t^2}{2} + \dots \\
\frac{de^{At}}{dt} = Ae^{At} \quad e^{A0} = \mathbb{I} \quad e^{A(t_1 + t_2)} = e^{At_1} \cdot e^{At_2} \quad e^{-At} = \left( e^{At} \right)^{-1}
$$
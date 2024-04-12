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

# Analisi di sistemi Lineari Tempo-Invarianti (LTI)

Un sistema **LTI** è un sistema della forma

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

## Meta variabili

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

## Risoluzione di casi particolari

Se si vuole studiare il movimento libero di un sistema con $A$ diagonalizzabile, è sufficiente calcolare $x(t) = e^{At}x_0$ con $t_0 = 0$.

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



# Appendice

## Proprietà matrice esponenziale

$$
e^a = \sum_{i=0}^{+ \infty} \frac{a^i}{i!} \implies e^{At} = \sum_{i=0}^{+ \infty} \frac{A^it^i}{i!} = \mathbb{I} + at + \frac{A^2t^2}{2} + \dots \\
\frac{de^{At}}{dt} = Ae^{At} \quad e^{A0} = \mathbb{I} \quad e^{A(t_1 + t_2)} = e^{At_1} \cdot e^{At_2} \quad e^{-At} = \left( e^{At} \right)^{-1}
$$
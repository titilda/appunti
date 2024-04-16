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
\delta y(t) = \underbrace{\left. \frac{\partial g}{\partial x} \right|_{\overline x, \overline u}}_{C} \delta x(t) + \underbrace{\left. \frac{\partial g}{\partial u} \right|_{\overline x, \overline u}}_{D} \delta u(t)
$$

Una volta fatto ciò, si vanno ad analizzare gli autovalori della matrice $A$:

- se tutti gli autovalori hanno parte reale negativa, vi è stabilità asintotica
- se esiste un autovalore a parte reale positiva, vi è instabilità
- se vi sono autovalori puramente immaginari, non si può dire nulla

Per un sistema di ordine 1, un po' come nella [linea delle fasi](https://appunti.titilda.org/Analisi%202/index.html#studio-qualitativo-di-edo-del-primo-ordine), se $\frac{\partial f}{\partial x} \gt 0$ allora si tende ad andare a destra mentre se $\frac{\partial f}{\partial x} \lt 0$ allora si tende ad andare a sinistra. I punti in cui $\frac{\partial f}{\partial x} = 0$ sono i punti di equilibrio e sono stabili o meno a seconda della direzione in cui tendono ad andare le derivate come visto prima (il tutto è valido per un $\overline u$ fissato).

# Analisi di sistemi a tempo discreto

Mentre i modelli a tempo continuo utilizzano la variazione dello stato, i modelli a tempo discreto ne analizzano direttamente l'evoluzione. Continuano a valere le stesse categorizzazioni che valevano per i sistemi a tempo continuo.

La condizione di equilibrio è la seguente:

$$
\overline x = f(\overline x, \overline u) \quad \overline y = g(\overline x, \overline u)
$$

Noti lo stato al tempo $k = 0$ ($x(0) = x_0$) e $u(k)$ allora 

$$
x(k) = A^k x(0) + \sum_{i=0}^{k-1} A^{k-i-1}Bu(i) \\
y(k) = CA^kx(0) + C \sum_{i=0}^{k-1}A^{k-i-1}Bu(i) + Du(k)
$$

Come per i sistemi a tempo continuo, è possibile ottenere un sistema equivalente applicando un cambio di variabili:

$$
z(k) = Tx(k) \iff x(k) = T^{-1}z(k) \\
\begin{cases}
    z(k+1) = \underbrace{TAT^{-1}}_{A'} z(k) + \underbrace{TB}_{B'} u(k) \\
    y(k) = \underbrace{CT^{-1}}_{C'} z(k) + \underbrace{D}_{D'} u(k)
\end{cases}
$$

Logicamente, si deve scegliere la matrice $T$ per fare in modo di arrivare ad una matrice $A'$ diagonale.

Per stabilire la stabilità di ciascun modo, mentre pe ri sistemi a tempo continuo si andava ad analizzare la parte reale degli autovalori, per i sistemi a tempo discreto si va ad analizzarne il modulo.

Per un sistema con $A$ diagonalizzabile

- se tutti gli atuovalori hanno norma inferiore ad 1 oppure pari ad uno ma senza blocchi di Jordan annessi, vi è stabilità asintotica
- se esiste almeno un autovalore con norma maggiore di uno oppure con norma pari ad uno che genera un blocco di Jordan, vi è instabilità

Per un sistema con $A$ non diagonalizzabile, i modi sono della forma $\frac{k^i}{i!}\lambda_i^k$ ed in tal caso

- se tutti gli autovalori hanno norma inferiore ad 1, vi è stabilità asintotica
- se esiste un autovalore con norma maggiore di 1, vi è instabilità
- nulla si può dire in presenza di autovalori con norma esattamente pari ad 1

Nel caso di sistemi nonlineari, si linearizza in maniera identica ai sistemi lineari:

$$
\delta x(k + 1) = \underbrace{\left. \frac{\partial f}{\partial x} \right|_{\overline x, \overline u}}_{A} \delta x(k) + \underbrace{\left. \frac{\partial f}{\partial u} \right|_{\overline x, \overline u}}_{B} \delta u(k) \\
\delta y(k) = \underbrace{\left. \frac{\partial g}{\partial x} \right|_{\overline x, \overline u}}_{C} \delta x(k) + \underbrace{\left. \frac{\partial g}{\partial u} \right|_{\overline x, \overline u}}_{D} \delta u(k)
$$

E' possibile andare a lavorare sul polinomio caratteristico solo dopo aver applicato la trasformazione bilineare che manda $\varphi(z)$ in $\varphi(s)$: se $\varphi(z)$ è il polinomio caratteristico originale, il polinomio con la trasformazione applicata è $\varphi(\frac{1+s}{1-s})$.

Se tutte le radici di $\varphi(z)$ hanno modulo inferiore ad 1 allora tutte le radici di $\varphi(s)$ hanno parte reale minore di zero.

Su $\varphi(s)$ è possibile applicare il criterio di Routh.

# Sistemi a tempo campionato

A seconda della continuità di ampiezza e tempo, ci sono 4 tipi diversi di segnali:

1. **analogico** (tempo discreto, ampiezza continua)
2. **quantizzato** (tempo discreto, ampiezza discreta)
3. **campionato** (tempo continuo, ampiezza continua)
4. **digitale** (tempo continuo, ampiezza discreta)

La conversione tra segnali a tempo continuo e discreto avviene tramite DAC e ADC.

La rappresentazione matematica di un ADC è il campionatore:

$$
\delta(t) = \begin{cases}
    0 & t \ne 0 \\
    \int_{-\infty}^{+\infty} \delta(t) \, dt = 1
\end{cases} \\
r^*(t) = \sum_{k = -\infty}^{+\infty} r(kT) \delta(t - kT)
$$

La rappresentazione matematica di un DAC è il mantienitore:

$$
u(t) = u(kT) \quad kT \le t \lt (k + 1)T
$$

Può essere necessario convertire sistemi a tempo continuo in sistemi a tempo discreto e viceversa.

Dato un sistema a tempo continuo, è possibile costruire le matrici che descrivono il sistema equivalente a tempo discreto: sia $T_S$ il sampling time, allora

$$
A_D = e^{AT_S} \qquad B_D = \int_{0}^{T_S} e^{A\tau} \, d\tau \qquad C_D = D \qquad D_D = D
$$

Dato che $A_D = e^{AT_S}$ allora gli autovalori sono $\lambda_D = e^{\lambda T_S}$.

$$
\begin{cases}
    \frac{dx}{dt}(t) = Ax(t) + Bu(t) \\
    y(t) = Cx(t) + Du(t)
\end{cases} \iff \begin{cases}
    x(k+1) = A_D x(k) + B_D u(k) \\
    y(k) = C_D x(k) + D_D u(k)
\end{cases}
$$

# Trasformata di Laplace

Dato un qualsiasi sistema LTI a tempo continuo

$$
\begin{cases}
    \dot x(t) = Ax(t) + Bu(t) \\
    y(t) = Cx(t) + Du(t)
\end{cases}
$$

i movimenti dello stato e dell'uscita si calcolano come

$$
x(t) = e^{A(t - t_0)}x(t_0) + \int_{t_0}^t e^{A(t - \tau)}Bu(\tau) \, d\tau \qquad t \ge 0 \\
y(t) = Ce^{A(t - t_0)}x(t_0) + C \int_{t_0}^t e^{A(t - \tau)} Bu(\tau) \, d\tau + Du(t) \qquad t \ge t_0
$$

Per semplificare i conti si introducono la trasformata di Laplace e alcune funzioni ausiliarie:

| Funzione     | Definizione                                                                                                 |
| ------------ | ----------------------------------------------------------------------------------------------------------- |
| Impulso      | $\text{Imp}(t) = \begin{cases} 0 & t \ne 0 \\ \int_{-\infty}^{+\infty} \text{Imp}(t) \, dt = 1 \end{cases}$ |
| Scalino      | $\text{Sca}(t) = \begin{cases} 0 & t \lt 0 \\ 1 & t \ge 0 \end{cases}$                                      |
| Rampa        | $\text{Ram}(t) = \begin{cases} 0 & t \lt 0 \\ t & t \ge 0 \end{cases}$                                      |
| Parabola     | $\text{Par}(t) = \begin{cases} 0 & t \lt 0 \\ \frac{t^2}{2} & t \ge 0 \end{cases}$                          |
| Esponenziale | $e^{\alpha t} \text{Sca}(t)$                                                                                |
| Sinusoide    | $\sin(\omega t) \text{Sca}(t) \\ \cos(\omega t) \text{Sca}(t)$                                              |

La **trasformata di Laplace** permette di trasformare un'equazione differenziale in una algebrica:

$$
f(t) \overset{\mathcal L}{\to} F(s) = \mathcal{L} \{f(t)\} = \int_0^{+\infty} f(t) e^{-st} \, dt
$$

La trasformata esiste se esiste un valore di $s$ tale per cui l'integrale converge.

Una trasformata è detta **razionale** se può essere scritta nella forma

$$
F(s) = \frac{N(s)}{D(s)} = \frac{b_0s^m + b_1s^{m-1} + \dots + b_{m-1}s + b_m}{a_0s^n + a_1s^{n-1} + \dots + a_{n-1}s + a_n}
$$

con $N(s)$ e $D(s)$ primi tra loro. Le radici di $D$ vengono dette **poli** mentre le radici di $N$ vengono dette **zeri**. Il **grado relativo** è dato da $n - m$.

La trasformata di Laplace è lineare:

$$
\mathcal{L}\{\alpha x(t) + \beta y(t)\} = \alpha \mathcal{L}\{x(t)\} + \beta \mathcal{L}\{y(t)\} = \alpha X(s) + \beta Y(s)
$$

Dato che la $\mathcal{L}$ passa dal dominio del tempo a quello della frequenza, è possibile applicare delle translazioni sia in tempo che in frequenza:

$$
\mathcal{L}\{f(t - t_0)\} = F(s) e^{-st_0} \qquad \mathcal{L}\{e^{at} f(t)\} = F(s - a)
$$

Si può derivare nel tempo...

$$
\mathcal{L} \left\{ \frac{df(t)}{dt} \right\} = sF(s) - f(0) \qquad \mathcal{L}\left\{ \frac{d^nf(t)}{dt^n} \right\} = s^nF(s) - \left. \sum_{i=1}^n s^{n-i} \frac{d^{i-1}f(t)}{dt^{i-1}}\right|_{t=0} \\
\mathcal{L}\left\{ -tf(t) \right\} = \frac{dF(s)}{ds} \qquad \mathcal{L}\left   \{ t^nf(t) \right\} = (-1)^n \frac{d^nF(s)}{ds^n}
$$

... e anche integrare e convolvere

$$
\mathcal{L} \left\{ \int_0^t f(\tau) \, d\tau \right\} = \frac{1}{s}F(s) \\
f(t) \ast f(t) = \int_0^t f(t - \tau)g(\tau) \, d\tau \implies \mathcal{L}\left\{ f(t) \ast g(t) \right\} = F(s) \cdot G(s)
$$

Per una tabella con una buona dose di trasformate, vedere l'[appendice](#tabella-trasformate-di-laplace).

Vale il **teorema del valore iniziale**: se $g \ge 1$ allora 

$$
\lim_{t \to 0} = \lim_{s \to +\infty} sF(s)
$$

da cui deriva, per la priprietà di derivazione, il **teorema della pendenza iniziale**:

$$
\lim_{t \to 0} \frac{df(t)}{dt} = \lim_{s \to +\infty} s(sF(s) - f(0))
$$

Vale il **teorema del valore finale**:

$$
\lim_{t \to +\infty} f(t) = \lim_{s \to 0} sF(s) \qquad \text{(Se esiste)}
$$

A scopo informativo, viene riportata la formula dell'**antitrasformata di Laplace**, che però è molto scomoda e si preferisce utilizzare la tabella sopracitata al contrario:

$$
f(t) = \mathcal{L}^{-1} \{F(s)\} = \frac{1}{2 \pi j} \int_{\sigma - j \infty}^{\sigma + j \infty} F(s) e^{st} \, ds \qquad \sigma \in \text{Regione di convergenza}
$$

Per utilizzare la tabella al contrario, si deve scomporre la $F(s)$ in una somma di frazioni.

Se $F(s)$ è razionale, se il grado relativo è maggiore di 1, allora

$$
F(s) = \sum_{i=1}^n \frac{r_i}{s - p_i}
$$

se invece il grado relativo è zero

$$
F(s) = k + \frac{N'(s)}{D(s)} \qquad N' = N \% D
$$

Successivamente, per ogni addendo, si va a vedere l'antitrasformata corrispondente sulla tabella.

Applicando la trasformata di Lablace alla formula che descrive l'uscita di un sistema, si ottiene la **rappresentazione esterna**:

$$
Y(s) = \underbrace{(S \mathcal{L}\{e^{At}\}B + D)}_{G(s)} \cdot U(s) \\
G(s) = C(s \mathbb{I} - A)^{-1}B + D
$$

Si può esprimere $G$ come $G = [g_{ij}]$; in tal caso, $g_{ij}(s)$ è la funzione di trasferimento tra $U_j(s)$ e $Y_i(s)$.

## Risposta all'impulso

Si vuole analizzare la risposta all'impulso di un sistema LTI:

Se $U(s) = \mathcal{L}\{\text{Imp}(t)\}$ allora $Y(s) = G(s) \cdot U(s) = G(s) \cdot 1 = G(s)$: un impulso non va a modificare in alcun modo l'uscita

## Caratterizzazione della funzione di trasferimento

La funzione di trasferimento può essere scritta in **forma poli-zeri** 

$$
G(s) = \frac{\rho \prod_i(s - z_i)\prod_i(s^2 + 2 \zeta \alpha_{ni} s + \alpha_{ni}^2)}{s^g \prod_i(s - p_i) \prod_i(s^2 + 2 \xi \omega_{ni}s+\omega_{ni}^2)}
$$

o in forma **costanti di tempo**

$$
G(s) = \frac{\mu \prod_i(1 + \pi_i s) \prod_i \left(1 + 2\frac{\zeta_i s}{\alpha_{ni}} + \frac{s^2}{\alpha_{ni}^2}\right)}{s^g \prod_i (1 + T_i s) \prod_i\left(1 + 2 \frac{\xi_i s}{\omega_{ni}} + \frac{s^2}{\omega_{ni}^2}\right)}
$$

dove

| Parametro                 | Spiegazione                                                                                                             |
| ------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| $\rho$                    | Costante di trasferimento                                                                                               |
| $g$                       | Per la forma poli-zeri è il numero di poli (o zeri) nell'origine; per la forma costanti di tempo, è il tipo del sistema |
| $p_i$                     | Numero di poli reali                                                                                                    |
| $z_i$                     | Numero di zeri reali                                                                                                    |
| $\zeta_i, \xi_i$          | Costanti di smorzamento                                                                                                 |
| $\alpha_i, \omega_i$      | Pulsazioni/frequenze naturali                                                                                           |
| $\mu$                     | Guadagno statico                                                                                                        |
| $\tau_i = -\frac{1}{z_i}$ | Costanti di tempo degli zeri                                                                                            |
| $T_i = -\frac{1}{p_i}$    | Costanti di tempo dei poli                                                                                              |

# Appendice

## Proprietà matrice esponenziale

$$
e^a = \sum_{i=0}^{+ \infty} \frac{a^i}{i!} \implies e^{At} = \sum_{i=0}^{+ \infty} \frac{A^it^i}{i!} = \mathbb{I} + at + \frac{A^2t^2}{2} + \dots \\
\frac{de^{At}}{dt} = Ae^{At} \quad e^{A0} = \mathbb{I} \quad e^{A(t_1 + t_2)} = e^{At_1} \cdot e^{At_2} \quad e^{-At} = \left( e^{At} \right)^{-1}
$$

$$
E[Y] = \int_{-\infty}^{+\infty} y p_Y(y)\,dy = \int_{-\infty}^{+\infty} x^2 p_X(x) \, dx
$$

## Tabella trasformate di Laplace

| $f(t)$                                     | $F(s)$                                                  |
| ------------------------------------------ | ------------------------------------------------------- |
| $\text{Imp}(t)$                            | $1$                                                     |
| $\text{Sca}(t)$                            | $\frac{1}{s}$                                           |
| $\text{Ram}(t)$                            | $\frac{1}{s^2}$                                         |
| $\text{Par}(t)$                            | $\frac{1}{s^3}$                                         |
| $e^{\alpha t}\text{Sca}(t)$                | $\frac{1}{s-\alpha}$                                    |
| $te^{\alpha t}\text{Sca}(t)$               | $\frac{1}{(s-\alpha)^2}$                                |
| $\sin(\omega t)\text{Sca}(t)$              | $\frac{\omega}{s^2+\omega^2}$                           |
| $\cos(\omega t)\text{Sca}(t)$              | $\frac{s}{s^2+\omega^2}$                                |
| $t\sin(\omega t)\text{Sca}(t)$             | $\frac{2\omega s}{(s^2+\omega^2)^2}$                    |
| $t\cos(\omega t)\text{Sca}(t)$             | $\frac{s^2+\omega^2}{(s^2+\omega^2)^2}$                 |
| $e^{\alpha t}\sin(\omega t)\text{Sca}(t)$  | $\frac{\omega}{(s-\alpha)^2+\omega^2}$                  |
| $e^{\alpha t}\cos(\omega t)\text{Sca}(t)$  | $\frac{s-\alpha}{(s-\alpha)^2+\omega^2}$                |
| $te^{\alpha t}\sin(\omega t)\text{Sca}(t)$ | $\frac{2\omega(s-\alpha)}{((s-\alpha^2)+\omega^2)^2}$   |
| $te^{\alpha t}\cos(\omega t)\text{Sca}(t)$ | $\frac{(s-\alpha)^2-\omega^2}{((s-\alpha)^2+\omega^2)}$ |

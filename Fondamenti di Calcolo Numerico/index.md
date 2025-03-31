---
title: "Riassunto di Fondamenti di Calcolo Numerico"
author:
- "Andrea Oggioni"
---

_Nota: questa pagina è work in progress. I contenuti potrebbero variare rapidamente._

# Introduzione

La disciplica chiamata _calcolo numerico_ (che si contrappone al _caclolo simbolico_) si occupa di trovare modi di risolvere problemi al computer.
I computer, essendo macchine finite (sia in termini di memoria che in termini di velocità), non sono in grado di rappresentare numeri con una precisione illimitata (causando quindi problemi di approssimazione) e, a seconda del tipo di problema da risolvere e della metodologia scelta, si dovrà attendere un tempo più o meno lungo prima di arrivare ad una soluzione.

Per fare un esempio, la rappresentazione numerica di una funzione non è una curva ma una serie di campioni più o meno vicini tra loro. Spesso bisogna scescliere un compromesso tra _grandezza del numero_ e _precisione della rappresentazione_: per questo motivo esistono casi nei quali le classiche proprietà delle operazioni non sono valide (ad esempio, $(1 + 10^{20}) - 10^{20} = 0$ ma $1 + (10^{20} - 10^{20}) = 1$).

E' denominato `eps` di un calcolatore il più piccolo numero tale per cui, con la precisione di tale calcolatore, vale che $1 + eps = 1$.

_Nota: in questa pagina verranno fatti frequenti riferimenti a MATLAB. Nel caso in cui non si possieda una licenza MATLAB, è possibile utilizzare l'alternativa open-source gratuita [GNU Octave](https://octave.org/) che ha una sintassi quasi completamente compatibile._

# Risoluzione di sistemi lineari

Sia $Ax = b$ un sistema lineare ($A \in \mathcal M_\mathbb R(n)$): tale sistema è risolvibile, tra gli altri, col [metodo di Cramer](#metodo-di-cramer), che richiede la risoluzione di $n$ determinanti i quali, a loro volta, richiedono ciascuno la risoluzione di circa $n!$ operazioni. E' evidente come questo metodo sia computazionalmente molto oneroso.

Di seguito, dopo aver introdotto i concetti necessari per la loro comprensione, verranno presentati alcuni metodi per risolvere sistemi lineari in maniera più veloce.

## Fattorizzazione LU

**Teorema**: Sia $A \in \mathcal M_\mathbb R(n)$. Se $\det(A_{p,i}) \ne 0 \forall i = 1, \dots, n$ allora esistono uniche una matrice triangolare inferiore $L$ (con tutti elementi unitari sulla diagonale) ed un'altra triangolare superiore $U$ tali che $A = LU$. Le matrici $A_{p,i}$ sono le sottomatrici di $A$ ottenute considerando solo le prime $i$ righe e colonne di $A$.

Si ha quindi che il sistema $Ax = b$ può essere riscritto come $LUx = b$ e può essere risolto come

$$
\begin{cases}
    Ly = b \\
    Ux = y
\end{cases}
$$

Con MATLAB è possibile verificare l'applicabilità del teorema ad una generica matrice quadrata $A$:

```matlab
accumulatore = 1;
for i = 1:n
    accumulatore = accumulatore * det(A(1:i, 1:i));
end
```

Se, alla fine del ciclo, `accumulatore ~= 0` vuol dire che la matrice è fattorizzabile.

Esistono delle condizioni sufficienti:

- la matrice $A$ è a dominanza diagonale stretta per righe (o per colonne);
- la matrice $A$ è simmetrica e definita positiva (SDP).

La fattorizzazione LU si trova applicando il **Metodi di Eliminazione Gaussiana (MEG)**.

Nel caso di elemento nullo sulla diagonale di $A$, il MEG calcolerà una divisione per zero che non è consentita: per questo motivo viene effettuato il **Pivoting** ovvero uno scambio di righe (o colonne) per fare in modo di ovviare al problema.

Il pivoting consiste nel moltiplicare a sinistra la matrice di partenza $A$ per una matrice $P$ ottenuta partendo dalla matrice identitaria e scambiando le righe che bisogna scambiare nella matrice $A$.

E' utile notare come $P = P^T = P^{-1}$ da cui $PP^T = PP^{-1} = I$.

Il sistema $Ax = B$ si può dunque riscrivere come $PLUx = Pb$ e può essere risolto come

$$
\begin{cases}
    Ly = Pb \\
    Ux = y
\end{cases}
$$

Per trovare la fattorizzazione LU in MATLAB si procede come segue:

```matlab
[L, U, P] = lu(A);
```

In caso di matrici SDP esiste unica anche la fattorizzazione di Cholesky: $A = R^TR$. In MATLAB:

```matlab
R = chol(A);
```

Sia $A = PLU$, allora è facile calcolarne il determinante:

$$
\det(A) = \det(PLU) = \det(P) \det(L) \det(U) = \pm 1 \cdot 1 \cdot \prod_{n=1}^n u_{ii} = \pm \prod_{n=1}^n u_{ii}
$$

## Condizionamento, errori di arrotondamento e residuo.

Sia dato un sistema $Ax = b$ e la sua soluzione calcolata $\hat x$. Dato che il calcolatore non dispone di precisione infinita, nella realtà, $\hat x$ è soluzione approssimata del sistema approssimato $(A + \delta A)x = b + \delta b$ ed è stato introdotto un errore computazionale $\delta x = x - \hat x$.

E' evidente che l'errore non sia calcolabile: se lo fosse, sarebbe anche possibile ottenendo un risultato esatto.

E' definito **numero di condizionamento** di una matrice il valore $K_k(A) = \|A\|_k \cdot \|A^{-1}\|_k$.

In MATLAB è possibile ottenere il numero di condizionamento di una matrice tramite la funzione `cond`: `k = cond(A)`.

Tale valore è computazionalmente oneroso da calcolare e dunque, spesso, ci si accontenta di una stima.

In caso di matrice $A$ semidefinita positiva, vale che

$$
\| A \|_2 = \lambda_{max}(A) = \max_{1 \le i \le n} \lambda_i \qquad \| A^{-1} \|_2 = \frac{1}{\lambda_{min}(A)} = \frac{1}{\min\limits_{1 \le i \le n} \lambda_i}
$$

da cui segue che

$$
K_2(A) = \frac{\lambda_{max}(A)}{\lambda_{min}(A)}
$$

Il numero di condiionamento descrive _quanto malamente_ si propagano gli errori di arrotondamento durante l'esecuzione del MEG.

Una matrice è detta **ben condizionata** se $K_2(A) \tilde \le 10^4$.

Una classe di matrici mal condizionate sono le **matrici di Hilbert**:

$$
a_{ij} = \frac{1}{i+j-1} \qquad K(A) \simeq e^{3.5n}
$$

In MATLAB è possibile ottenere una matrice di Hilbert di lato $n$ attraverso la funzione `hilb`: `H = hilb(n)`.

Ipotizzando che $\|\delta A\|_2 \|A^{-1}\|_2 \lt 1$ vale che

$$
\frac{\|\delta x\|}{\|x\|} \le \frac{K(A)}{1 - K(A)\frac{\| \delta A \|}{\| A \|}} \left( \frac{\| \delta b \|}{\| b \|} + \frac{\| \delta A \|}{\| A \|} \right)
$$

Nella ragionevole ipotesi in cui $\delta A = 0$ (ovvero nell'ipotesi che $A$ non abbia subito approssimazioni), vale che

$$
\underbrace{\frac{\| \delta x \|}{\| x \|}}_{\text{Errore relativo}} \le \underbrace{K(A)}_{\text{Coefficiente}} \cdot \underbrace{\frac{\| \delta b \|}{\| b \|}}_{\text{Errore sui dati}}
$$

L'errore diminuisce con l'aumentare dell valore degli ingressi sulla diagonale di $A$: per questo motivo viene eseguito un pivoting adeguato per avere i coefficienti più grandi possibili sulla diagonale.

Pur migliorando l'approssimazione del risultato, il pivoting non può fare nulla per migliorare il condizionamento.

E' definita **residuo** la quantità $r = b - A \hat x$. Al contrario dell'errore, il residuo, è perfettamente calcolabile (a meno di errori di troncamento nel calcolo del residuo dal quale può essere derivato il residuo del residuo dal cui conto può esser derivato il residuo del residuo del residuo e così via).

Il legame tra errore e residuo è espresso dalla seguente disuguaglianza:

$$
\frac{\|\delta x\|}{\|x\|} \le K(A) \cdot \frac{\|r\|}{\|b\|}
$$

## Metodi diretti

I metodi diretti sono quei metodi che terminano sempre arrivando ad una soluzione più o meno precisa.

### Sostituzione in avanti ed indietro

Risolvere un sistema nel quale la matrice è triangolare superiore (inferiore) è semplice e computazionalmente poco oneroso con la tecnica della sostituzione all'indietro (in avanti).

Il metodo di sostituzione in avanti si può scrivere matematicamente come segue:

$$
x_i = \frac{b_i - \sum\limits_{j=1}^{i-1}l_{ij}x_j}{l_{ii}}
$$

Il metodo di sostituzione all'indietro si può scrivere matematicamente come segue:

$$
x_i = \frac{b_i - \sum\limits_{j=i+1}^{n} u_{ij}x_j}{u_{ii}}
$$

Complessivamente, calcolare la fattorizzazione LU di una matrice e risolvere il sistema equivalente con le due passate di sostituzioni è _molto_ più veloce dell'utilizzo del metodo di Cramer (con Cholesky la velocità di fattorizzazione raddoppia).


<!-- ## Metodi itarativi -->


# Richiami di algebra lineare

In questa sezione verranno ripresi concetti di algebra lineare necessari per la comprensione di quanto scritto nelle sezioni precedenti.

## Metodo di Cramer

Sia $Ax = b$ un sistema lineare. I membri del vettore soluzione $x$ vengono calcolati col metodo di Cramer attraverso la seguente formula:

$$
x_j = \frac{\det(A_j)}{\det(A)} \qquad A_j = [a_1 | \dots | a_{j-1} | b | a_{j+1} | \dots | a_n ]
$$

dove $a_i$ è la $i$-esima colonna di $A$.

## Norma

Sia $x \in \mathbb{R}^n$, allora la norma-$k$ di tale vettore è data da
$$
\| x \|_k = \left( \sum_{i=1}^n x_n^k \right)^{\frac{1}{k}}
$$

Alcune norme utili sono le seguenti:

- $\|\cdot\|_1$: ottenuta _contando i quadretti_ tra il vettore e l'origine;
- $\|\cdot\|_2$: distanza euclidea tra il vettore e l'origine;
- $\|\cdot\|_\infty$: il massimo tra i membri del vettore presi in valore assoluto.

Sia $A$ una matrice quadrata, allora la norma-$A$ del vettore $x$ definito sopra è
$$
\|x\|_A = \sqrt{x^TAx}
$$

Sia $A$ una matrice quadrata, allora la norma-$k$ di tale matrice è definita come

$$
\|A\|_k = \sup_{v \in \mathbb R^n, v \ne 0} \frac{\|Av\|_k}{\|v\|_k} = \sqrt[k]{\lambda_{max}(A^TA)}
$$

## Disuguaglianze di Cauchy-Schwartz e triangolare

Siano $x, y \in \mathbb R^n$ allora

- per Cauchy-Schwartz vale che $| \langle x, y \rangle | \le \| x \| \cdot \| y \|$;
- per la triangolare vale che $\| x + y \| \le \| x \| + \| y \|$.

Entrambe valgono per qualsiasi norma.

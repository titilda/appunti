---
title: "Riassunto di Fondamenti di Calcolo Numerico"
author:
- "Andrea Oggioni"
---

_Nota: questa pagina è work in progress. I contenuti potrebbero variare rapidamente._

# Introduzione

La disciplica chiamata _calcolo numerico_ (che si contrappone al _calcolo simbolico_) si occupa di trovare modi di risolvere problemi al computer.
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

La fattorizzazione LU si trova applicando i **Metodi di Eliminazione Gaussiana (MEG)**.

Nel caso di elemento nullo sulla diagonale di $A$, il MEG calcolerà una divisione per zero che non è consentita: per questo motivo viene effettuato il **Pivoting** ovvero uno scambio di righe (o colonne) per fare in modo di ovviare al problema.

Il pivoting consiste nel moltiplicare a sinistra la matrice di partenza $A$ per una matrice $P$ ottenuta partendo dalla matrice identitaria e scambiando le righe che bisogna scambiare nella matrice $A$.

E' utile notare come $P = P^T = P^{-1}$ da cui $PP^T = PP^{-1} = I$.

Il sistema $Ax = B$ si può dunque riscrivere come $LUx = Pb$ e può essere risolto come

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

Il numero di condizionamento descrive _quanto malamente_ si propagano gli errori di arrotondamento durante l'esecuzione del MEG.

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

In MATLAB è possibile definire le funzioni di sostituzione in avanti e sotituzione all'indietro come segue:

```matlab
function [x] = forward_sub(L, b)
    N = length(b);
    x = zeros(N, 1);

    for i = 1:N
        x(i) = (b(i) - L(i, 1:i-1) * x(1:i-1)) / L(i, i);
    end
end


function [x] = backward_sub(U, b)
    N = length(b);
    x = zeros(N, 1);

    x(N) = b(N) / U(N, N);

    for i = N-1:-1:1
        x(i) = (b(i) - U(i, i+1:N) * x(i+1:N)) / U(i, i);
    end
end
```

La procedura completa per la risoluzione di un sistema lineare è quindi codificata come segue:

```matlab
function [x] = solve_by_sub(A, b)
    [L, U, P] = lu(A);
    y = forward_sub(L, P*b);
    x = backward_sub(U, y);
end
```

## Metodi iterativi

Mentre i metodi diretti terminano restituendo una soluzione, questi non sono adatti per sistemi eccessivamente grossi o sparsi. Per queste tipologie di sistemi si usano i metodi iterativi. Questi metodi generano una successione infinita di soluzioni $x^{(k)}$ sempre più precise. 
E' compito dell'utilizzatore di questi metodi decidere quale criterio utilizzare per determinare il momento di fermare l'algoritmo.

A differenza dei metodi diretti, i metodi iterativi sono soggetti ad errore anche in aritmetica esatta.

Un metodo iterativo generico si può esprimere come

$$
x^{(k+1)} = Bx^{(k)} + f
$$

$B$ è detta **matrice d'iterazione**.

Un metodo iterativo sensato gode di due proprietà:

- proprietà di **consistenza**: $x = Bx + f$ da cui si ricava che una consizione necessaria ma non sufficiente per la consistenza è che $f = (I - B)A^{(-1)}b$;
- proprietà di **convergenza**: $\lim\limits_{k \to \infty} x^{(k)} = x$.

Dalle due proprietà appena citate segue che, definendo l'errore come $e^{(k)} = x - x^{(k)}$ allora $\|e^{(k+1)}\| \le \|B\|^{k+1}\|e^{(0)}\|$, da cui segue che una condizione sufficiente di convergenza è che $\|B\| \le 1$.

Notare che, per identificare un metodo iterativo e assumento che sia sensato, basta la matrice $B$.

Viene definita **raggio spettrale** la quantità $\rho(B) = \max\limits_j |\lambda_j(B)|$. Vale anche l'approssimazione $\lim\limits_{k \to \infty} \|B^k\|^{\frac{1}{k}} = \rho(B)$.

La velocità di convergenza è tanto più alta quanto è più piccolo il raggio spettrale.

Il raggio spettrale gode di due proprietà:

- $\rho(B) \le \|B\|$;
- $\rho(B) \lt 1$ è condizione necessaria e sufficiente per la convergenza del metodo iterativo.

Se $B$ è SDP allora $\rho(B) = \|B\|_2$.

Per comodità, vengono definite alcune matrici ampiamente utilizzate nei paragrafi successivi (viene utilizzata la sintassi MATLAB).

```matlab
D = diag(diag(A));
E = -tril(A, -1);
F = -triu(A, +1);
```

### Metodo di Jacobi

Il metodo di Jacobi è un metodo iterativo facilmente parallelizzabile esprimibile come segue:

$$
x_i^{(k+1)} = \frac{b_i - \sum_{j \ne i} a_{ij}x_j^{(k)}}{a_{ii}} \quad \forall i = 1, \dots, n
$$

Il metodo di Jacobi è competitivo con MEG se il numero di iterazioni programmate è inferiore a $n$.

Il metodo di Jacobi può essere identificato dalla seguente matrice d'iterazione

$$
B_J = I - D^{-1}A \\
$$

In MATLAB, è possibile implementare il metodo di Jacobi (con criterio di arresto sul residuo, che verrà spiegato [più avanti](#criteri-di-arresto)) nel seguente modo:

```matlab
function [x, n_iters] = solve_jacobi(A, b, x0, tolerance)
    N = length(b);

    D = diag(diag(A));

    B = eye(N) - D\A;
    f = (eye(N) - B) / A * b;

    n_iters = 0;
    normalized_residue = tolerance + 1;
    x = x0;

    while normalized_residue > tolerance
        n_iters = n_iters + 1;
        x = B * x + f;
        residue = b - A*x;
        normalized_residue = norm(residue)/norm(b);
    end
end
```

### Metodo di Gauss-Seidel

Il metodo di Gauss-Seidel è un metodo iterativo non facilmente parallelizzabile ma più veloce (in termini di numero di iterazioni) del metodo di Jacobi esprimibile matematicamente come segue:

$$
x_i^{(k+1)} = \frac{b_i - \sum\limits_{j \lt i} a_{ij}x_j^{(k+1)} - \sum\limits_{j \gt i} a_{ii}x_j^{(k)}}{a_{ii}}
$$

Il metodo di Gauss-Seidel può essere identificato dalla seguente matrice d'iterazione:

$$
B_{GS} = (D - E)^{-1} F
$$

In MATLAB, è possibile implementare il metodo di Gauss-Seidel (con criterio di arresto sull'incremento, che verrà spiegato [più avanti](#criteri-di-arresto)) nel seguente modo:

```matlab
function [x, n_iters] = solve_gauss_seidel(A, b, x0, tolerance)
    N = length(b);

    D = diag(diag(A));
    E = -tril(A, -1);
    F = -triu(A, +1);

    B = (D - E) \ F;
    f = (eye(N) - B) / A * b;

    increment = tolerance + 1;
    n_iters = 0;

    x = x0;

    while increment > tolerance
        n_iters = n_iters + 1;
        x_new = B * x + f;
        increment = norm(x_new - x);
        x = x_new;
    end
end
```

Per entrambi i metodi visti precedentemente, valgono le seguenti proprietà:

- se $A$ strettamente dominante diagonale per righe allora convergono entrambi;
- se $A$ è SDP allora Gauss-Seidel converge;
- se $A$ è tridiagonale allora convergono entrambi o non convergono entrambi; Se convergono, Gauss-Seidel è più veloce.

A scopo puramente illustrativo, nel seguente script, viene generata casualmente una matrice $A$ strettamente dominante diagonale per righe ed un vettore $b$, anch'esso casuale, poi, per ciascuno dei metodi visti, viene risolto il sistema $Ax = b$ e viene stampato il numero di iterazioni necessarie:

```matlab
N = 10;

A = generate_converging_matrix(N);
b = rand(N, 1);

x0 = zeros(N, 1);

[x_j, n_j] = solve_jacobi(A, b, x0, 1e-12);
[x_gs, n_gs] = solve_gauss_seidel(A, b, x0, 1e-12);

fprintf('Iterazioni Jacobi: %d\n', n_j);
fprintf('Iterazioni Gauss-Seidel: %d\n', n_gs);
```

La funzione `generate_converging_matrix` è disponibile [nell'appendice](#funzioni-matlab).

### Metodo di Richardson stazionario

Il metodo di Richaradson stazionario è basato sulla seguente legge di aggiornamento:

$$
x^{(k+1)} = x^{(k)} + \alpha r^{(k)}
$$

La matrice d'iterazione corrispondente è la seguente:

$$
B_\alpha = I - \alpha A
$$

**Teorema**: il metodo di Richardson stazionario con $A$ SDP converge se e solo se

$$
0 \lt \alpha \lt \frac{2}{\lambda_{max}(A)}
$$

Il parametro $\alpha$ ottimale è calcolato come

$$
\alpha_{opt} = \frac{2}{\lambda_{min}(A) + \lambda_{max}(A)}
$$

Ne segue che, con $\alpha$ ottimale, 

$$
\rho_{opt} = \frac{K(A) - 1}{K(A) + 1}
$$
E' possibile aumentare ulteriormente la velocità di convergenza moltiplicando la matrice $A$ per l'inverso di una matrice di **precondizionamento** invertibile $P$.

Il sistema da risolvere diventa dunque $P^{-1}Ax = P^{-1}b$ e nelle formule tutte le occorrenze di $A$ vengono sostituite con $P^{-1}A$.

Logicamente, per fare in modo che il precondizionamento abbia effetto, deve essere scelta una $P$ tale per cui $K(P^{-1}A) \lt\lt K(A)$.

Per matrici $A$ sparse, esiste la **fattorizzazione LU inesatta** che si trova ponendo $l_{ij} = u_{ij} = 0$ dove $a_{ij} = 0$. La fattorizzazione LU inesatta restituisce due matrici $\tilde L$ e $\tilde U$ il cui prodotto viene utilizzato per costruire la matrice $P$: $P = \tilde L \tilde U$.

Un'implementazione MATLAB del metodo appena analizzato è la seguente:

```matlab
function [x, n_iters] = solve_richardson(A, b, alpha, x0, tolerance)
    N = length(b);
    
    B = eye(N) - alpha * A;

    f = (eye(N) - B) / A * b;

    n_iters = 0;
    normalized_residue = tolerance + 1;
    x = x0;

    while normalized_residue > tolerance
        n_iters = n_iters + 1;
        x = B*x + f;
        residue = b - A*x;
        normalized_residue = norm(residue)/norm(b);
    end
end
```

### Metodo del gradiente

Sia

$$
\Phi(y) = \frac{1}{2} y^T A y - y^T b
$$

allora

$$
\nabla \Phi(y) = Ay - b
$$

e i punti in cui $\nabla \Phi(y) = 0$ sono esattamente le soluzioni di $Ax = b$.

La legge di aggiornamento è dunque la seguente:

$$
x^{(k+1)} = x^{(k)} - \alpha_k\nabla\Phi(x^{(k)})
$$

Per scegliere $\alpha$ (che varia ad ogni iterazione, ottenendo una successione), si impone che

$$
\frac{d\Phi(x^{(k+1)})}{d\alpha_k} = 0
$$

da cui si ottiene che

$$
\alpha_k = \frac{(r^{(k)})^Tr^{(k)}}{((r^{(k)}))^TAr^{(k)}}
$$

L'errore per il metodo del gradiente si misura come

$$
\|e^{(k)}\|_A \le \left( \frac{K(A) - 1}{K(A) + 1} \right)^k \|e^{(0)}\|_A
$$

Di seguito compare il listato di un'implementazione di una funzione MATLAB che risolve un sistema $Ax = b$ col metodo del gradiente:

```matlab
function [x, n_iters] = solve_gradient(A, b, x0, tolerance)
    x = x0;
    n_iters = 0;
    residue = b - A*x;
    normalized_residue = tolerance + 1;

    while normalized_residue > tolerance
        n_iters = n_iters + 1;
        alpha = (residue' * residue) / (residue' * A * residue);
        x = x - alpha * (A * x - b);
        residue = b - A * x;
        normalized_residue = norm(residue) / norm(b);
    end
end
```

### Metodo del gradiente coniugato

La direzione di aggiornamento utilizzata dal metodo del gradiente è sempre ortogonale alla direzione precedente: in questo modo ci si avvicina al punto di convergenza in maniera non ottimale.

Il metodo del gradiente coniugato (che non verrà spiegato ulteriormente, almeno per ora), risolve questo problema: basti sapere in aritmentica esatta questo metodo converge alla soluzione esatta in al più $n$ iterazioni e che

$$
\|e^{(k)}\|_A \le \frac{2c^k}{1 + 2c^k} \|e^{(0)}\|_A \qquad c = \frac{\sqrt{K(A)} - 1}{\sqrt{K(A)} + 1}
$$

### Gradiente coniugato precondizionato

Analogamente al [metodo di Richardson precondizionato](#metodo-di-richardson-stazionario) si vanno a sostituire tutte le occorrenze di $A$ con $P^{-1}A$ avendo cura di scegliere una $P$ invertibile e che faccia ridurre il numero di condizionamento.

### Criteri di arresto

Per decidere quando fermarsi, i criteri più sensati sono due:

- **criterio sul residuo**: ci si ferma quando $\frac{\|r^{(k)}\|}{\|b\|} \le \varepsilon$;
- **criterio sull'incremento**: sia $\delta^{(k)} = x^{(k+1)} - x^{(k)}$ allora ci si ferma quando $\|\delta^{(k)}\| \le \varepsilon$.

Mentre il secondo si usa per $\rho(B)$ piccoli, il primo è buono per bassi condizionamenti; la versione modificata del criterio sul residuo per metodi precondizionati è la seguente:

$$
\frac{\|z^{(k)}\|}{\|b\|} \qquad z^{(k)} = P^{-1}r^{(k)}
$$

# Ricerca di radici di funzioni non lineari

Sia $f : (a, b) \to \mathbb R$, l'obiettivo è trovare $\alpha \in (a, b): f(\alpha) = 0$.

## Metodi iterativi locali

I metodi iterativi locali funzionano in maniera simile ai metodi iterativi per la soluzione di sistemi lineari: si sceglie un $x^{(0)}$ iniziale e da lì si procede generando una successione $x{(k)}$ che converge ($\lim\limits_{k \to \infty} x^{(k)} = \alpha$).

I metodi che verranno spiegati nelle sezioni successive sono detti anche **metodi di locali** e la loro convergenza (tranne per una singola eccezione) è garantita dai teoremi di esistenza e unicità del punto fisso e di convergenza locale che verranno illustrati nel paragrafo successivo.

### Iterazioni di punto fisso

L'iterazione di punto fisso è un modo _standard_ di identificare ed esprimere la legge di aggiornamento per un metodo iterativo locale per la ricerca di radici di una funzione non lineare.

Dato un $x^{(0)}$ iniziale, la funzione di iterazione di punto fisso è espressa come $\varphi : [a, b] \to \mathbb R$ tale che

$$
x^{(k+1)} = \varphi(x^{(k)}) \qquad k \ge 0
$$

**Teorema** (esistenza e unicità del punto fisso): sia $\varphi : [a, b] \to \mathbb R$ continua e sia $x^{(0)} \in [a, b]$ assegnato. Considerando $x^{(k+1)} = \varphi(x^{(k)}) \qquad k \ge 0$ allora

1. se $\forall x \in [a, b]$ vale che $\varphi(x) \in [a, b]$ allora $\exists \alpha \in [a, b]$ punto fisso;
2. se inoltre $\exists L \lt 1 : |\varphi(x_1) - \varphi(x_2)| \le L|x_1 - x_2|$ per ogni $x_1, x_2 \in [a, b]$ allora
   1. $\exists ! \alpha \in [a, b] : \varphi(\alpha) = \alpha$;
   2. $\forall x^{(0)} \in [a, b] \quad \lim\limits_{k \to \infty}x^{(k)} = \alpha$.

**Teorema** (convergenza locale): sia $\varphi : [a, b] \to \mathbb R$ una funzione di iterazione con $\alpha$ punto fisso, $I_\alpha$ un intorno di $\alpha$ e $\varphi \in \mathcal C^1(I_\alpha)$, allora

1. se $|\varphi'(\alpha)| \lt 1$ allora $\exists \delta \gt 0 : \forall x^{(0)} |x^{(0)} - \alpha| \lt \delta, x^{(k)} \to \alpha$. Inoltre
   $$
   \lim_{k \to \infty} \frac{x^{(k+1)} - \alpha}{(x^{(k)} - \alpha)} = \varphi'(\alpha)
   $$
2. se inoltre $\varphi \in \mathcal C^2(I_\alpha), \varphi'(\alpha) = 0, \varphi''(\alpha) \ne 0$ allora $x^{(k)} \to \alpha$ e
   $$
   \lim_{k \to \infty} \frac{x^{(k+1)} - \alpha}{(x^{(k)} - \alpha)^2} = \frac{\varphi''(\alpha)}{2}
   $$

Dal precedente teorema si possono ricavare due spunti interessanti: se i requisiti del punto 1 sono soddisfatti, allora l'errore scende linearmente mentre se i requisiti del punto 2 sono soddisfatti allora l'errore scala quadraticamente (e la convergenza è molto più veloce).

### Metodo di Newton

Il metodo di Newton è un metodo iterativo identificato dalla seguente legge di aggiornamento:

$$
x^{(k+1)} = x^{(k)} - \frac{f(x^{(k)})}{f'(x^{(k)})} \qquad k \ge 0
$$

Questo aggiorna la successione prendendo come valore successivo la $x$ nella quale la retta tangente al punto $(x^{(k)}, f(x^{(k)}))$ interseca l'asse x.

Il metodo di Newton può essere visto come metodo di punto fisso:

$$
\varphi_N(x) = x - \frac{f(x)}{f'(x)} \qquad \varphi'_N(x) = \frac{f(x)f''(x)}{[f'(x)]^2}
$$

<!-- NOTA: in base a cosa $f'(\alpha) \ne 0$??? -->
Se $\alpha$ è una radice allora $f(\alpha) = 0$ e $f'(\alpha) \ne 0$ di conseguenza

$$
\varphi''(\alpha) = \frac{f''(\alpha)}{f'(\alpha)} \ne 0
$$

Sono soddisfatte le condizioni del punto 2 del teorema di convergenza locale: il metodo di Newton converge localmente.

Il metodo di Newton richiede la conoscenza della derivata prima della funzione di cui si vuole trovare la radice: i metodi successivi rimuovono questa limitazione.

Se la radice $\alpha$ ha molteplicità maggiore, allora le derivate superiori si annullano e Newton converge ma non più del secondo ordine: per ovviare a questo problema lo si modifica introducendo una variabile $m$ che è pari alla molteplicità della radice (si comincia con $m = 1$ e si aumenta se si vede che converge lentamente):

$$
x^{(k+1)} = x^{(k)} - m \frac{f(x^{(k)})}{f'(x^{(k)})} \implies \varphi(x) = x - m \frac{f(x)}{f'(x)}
$$

Il listato di un'implementazione MATLAB di una funzione che trova lo zero di una funzione è riportato di seguito:

```matlab
function [x, iters] = find_root_newton(f, df, x0, toll)
    x = x0;
    iters = 0;

    increment =  toll;

    phi = @(x_) x_ - f(x_)/df(x_);

    while increment >= toll
        iters = iters + 1;
        x_new = phi(x);
        increment = norm(x_new - x);
        x = x_new;
    end
end
```

### Metodo delle corde

Il metodo delle corde è un metodo iterativo identificato dalla seguente legge di aggiornamento:

$$
x^{(k+1)} = x^{(k)} - \frac{f(x^{(k)})}{q} \qquad q = \frac{f(b) - f(a)}{b - a} \ne 0
$$

Il metodo delle corde può essere visto come metodo di punto fisso:

$$
\varphi_C(x) = x - \frac{1}{q} f(x)
$$

Sia $\alpha$ punto fisso di $f$, allora

$$
\varphi'(\alpha) = 1 - \frac{1}{q} f'(\alpha)
$$

Se $f'(\alpha) \ne 0$ (se valesse l'uguaglianza, non saprei dire nulla) allora il metodo converge solo se

$$
\begin{cases}
    q \text{ e } f'(\alpha) \text{ hanno lo stesso segno} \\
    b - a \lt \frac{2}{f'(\alpha)}[f(b) - f(a)]
\end{cases}
$$

Segue listato di un'implementazione MATLAB di una funzione che trova lo zero di una funzione:

```matlab
function [x, iters] = find_root_chord(f, a, b, x0, toll)
    x = x0;
    iters = 0;

    q = (f(b) - f(a)) / (b - a);

    phi = @(x_) x_ - f(x_)/q;

    increment = toll;

    while increment >= toll
        iters = iters + 1;
        x_new = phi(x);
        increment = norm(x_new - x);
        x = x_new;
    end
end
```

### Metodo delle secanti

Il metodo delle secanti va a sostituire la derivata prima di $f$ con un rapporto incrementale tra i due ultimi valori della successione $x^{(k)}$:

$$
x^{(k+1)} = x^{(k)} - (x^{(k)} - x^{(k-1)})\frac{f(x^{(k)})}{f(x^{(k)}) - f(x^{(k-1)})}
$$

Questo metodo necessita, oltre di $x^{(0)}$, anche di $x^{(-1)}$.

Questo metodo non può essere visto come metodo di punto fisso perchè $x^{(k+1)}$ non dipende solo da $x^{(k)}$ ma anche da $x^{(k-1)}$.

Di seguito viene riportata una possibile implementazione MATLAB del metodo delle secanti:

```matlab
function [x, iters] = find_root_secant(f, x0, x_prec, toll)
    xs = [x_prec; x0];

    iters = 0;

    while abs(f(xs(2))) >= toll
        iters = iters + 1;

        disp(f(xs(2)));

        xs = [
            xs(2);
            xs(2) - (xs(2) - xs(1)) * f(xs(2)) / (f(xs(2)) - f(xs(1)));
        ];
    end

    x = xs(2);
end
```

### Metodo di Newton per sistemi non lineari

Siano $x = [x_1, x_2, \dots, x_n]^T$ e

$$
f(x) = \begin{bmatrix}
    f_1(x_1, x_2, \dots, x_n) \\
    f_2(x_1, x_2, \dots, x_n) \\
    \vdots \\
    f_n(x_1, x_2, \dots, x_n) \\
\end{bmatrix}
$$

Si supponga di voler trovare $x : f(x) = \underline 0$: il metodo di Newton può essere applicato, con alcune piccole modifiche anche alla risoluzione di questo problema.

Ogni iterazione è composta da 

1. La risoluzione del sistema $J(x^{(k)})\delta x^{(k)} = -f(x^{(k)})$
2. Il calcolo di $x^{(k+1)} = x^{(k)} + \delta x^{(k)}$.

Nei pinti precedenti, $J$ è la matrice Jacobiana di $f$ calcolata come

$$
j_{ij} = \frac{\partial f_i}{\partial x_l} \qquad \forall i = 1, \dots, n \quad \forall j = 1, \dots, n
$$

Ovviamente il metodo converge: se $\exists \delta \gt 0 : \|\alpha - x^{(0)}\| \lt \delta$ allora

$$
\lim_{k\ to \infty} \|\alpha - x^{(k)}\| = 0
$$

Se il metodo converge e $J$ è derivabile, allora la convergenza è del secondo ordine:

$$
\frac{\|\alpha - x^{(k+1)}\|}{\|\alpha - x^{(k)}\|^2} \le C
$$

Il costo computazionale per questo metodo è dato dal numero di iterazioni ($\#iter$) moltiplicato per il costo di costruzione della matrice $J$ ($C_{cos}$) sommato al costo di risoluzione del corrispondente sistema lineare ($C_{sl}$).

$$
C = \#iter \times (C_{cos} + C_{sl})
$$

Questo numero, spesso, è molto grande: per ridurlo si potrebbe pensare di aggiornare la matrice jacobiana non a tutte le iterazioni oppure di utilizzare il metodo di Broyden (che non verrà analizzato).

### Criteri di arresto

Come qualsiasi metodo iterativo, anche i metodi iterativi locali non terminano autonomamente: bisogna quindi scegliere un criterio sensato per decidere quando far terminare la computazione.

I due criteri più utilizzati sono

- **criterio sul residuo** (buono per $|f'(\alpha)| \simeq 1$): ci si ferma se $|f(x^{(k)})| \lt \varepsilon$;
- **criterio sull'incremento** (buono per Newton o per $-1 \lt \varphi'(\alpha) \lt 0$): ci si ferma quando $|x^{(k+1)} - x^{(k)}| \lt \varepsilon$.

Per il [Metodo di Newton per sistemi non lineari](#metodo-di-newton-per-sistemi-non-lineari) i due criteri sono applicabili sostituendo i valori assoluti con norme vettoriali.

Esistono anche altri tipi di criteri, più o meno robusti, come, ad esempio, il criterio tale per cui entrambi i criteri devono essere soddisfatti. Logicamente, più un criterio è robusto, più il risultato numerico si avvicinerà a quello simbolico, sempre a discapito del numero di iterazioni.

# Approssimazione ed interpolazione

Siano date $n+1$ coppie $(x_i, y_i)$ con $i = 0, \dots, n$. Tali coppie sono definite genericamente **dati**. Un dato può rappresentare una **misura** nel caso in cui $y$ misura la risposta di un sistema fisico ad una sollecitazione $x$ (in questo caso il dato viene chiamato anche **nodo**) oppure un **valore di una funzione** nel caso in cui la si vuole campionare o semplificare (in questo caso il dato ciene chiamato anche **punto**). Di seguito, si farà riferimento a dati, nodi e punti come se fossero sinonimi.

Le principali motivazioni dietro alla scelta di voler approssimare una funzione sono due:

- si vuole trovare un modo di descrivere il comportamento di un sistema a partire dai dati a disposizione per una sollecitazione arbitraria senza effettivamente andare a disturbare il sistema ogni volta;
- si vuole trovare una funzione molto simile ad una già conosciuta ma che sia più semplice da maneggiare.

La differenza tra interpolazione ed approssimazione non è trascurabile:

- **interpolare** significa trovare una **funzione interpolatoria** $\tilde f(x)$ tale che $\tilde f(x_i) = y_i \quad \forall i=0, \dots, n$;
- **approssimare** significa minimizzare la distanza tra la $\tilde f$ e i punti (ad esempio col metodo dei **minimi quadrati**: $\min \sum\limits_{i=0}^n\left| \tilde f(x_i) - y_i \right|^2$).

## Interpolazione polinomiale

Si supponga di voler trovare interpolatore polinomiale $p(x) = a_nx^n + a_{n-1}x^{n-1} + \dots + a_1x + a_0$. Questo polinomio esiste sempre ed è unico: per ipotesi $p(x_i) = y_i \quad \forall i$ quindi si ha un sistema di $n+1$ equazioni in $n+1$ incognite. Tale sistema è detto **sistema di Vandermonde** ed è garantito che la sua matrice $A$ sia invertibile dunque ha una ed una sola soluzione. Il problema di questo metodo è che il numero di condizionamento di $A$ cresce molto velocemente con la sua dimensione quindi si usa un altro metodo.

## Interpolazione Lagrangiana

_Nota: il signor Lagrange era italiano, non francese e, di cognome, faceva Lagrangia. Però si faceva chiamare Lagrange perchè, all'epoca, se un matematico era francese, allora era automaticamente più credibile._

Sia definito il **polinomio di Lagrange** come

$$
\varphi_k(x) = \prod_{j = 0, j \ne k}^n \frac{x - x_j}{x_k - x_j} = \frac{(x - x_0)(x - x_1) \dots (x - x_{k-1})(x - x_{k+1}) \dots (x - x_n)}{(x_k - x_0)(x_k - x_1) \dots (x_k - x_{k-1})(x_k - x_{k+1}) \dots (x_k - x_n)}
$$

Valutando il polinomio di Lagrange in $x_i$ si ottiene che 

$$
\varphi_k(x_i) = \begin{cases}
    0 & i \ne k \\
    1 & \text{Altrimenti}
\end{cases}
$$

Sfruttando questa interessante proprietà, si può riscrivere il polinomio interpolatore del paragrafo precedente come 

$$
p(x) = \sum_{j=0}^n y_j \varphi_j(x)
$$

Per convenzione, un polinomio interpolatore è indicato da $\Pi_n(x)$ (se $y_i = f(x_i)$ si può scrivere anche $\Pi_nf(x)$).

La dimostrazione dell'unicità di $\Pi_n(x)$ (e della conseguente equivalenza tra l'interpolazione Lagrangiana e quella polinomiale) è presente nell [appendice](#appendice).

Sia $I$ un intervallo limitato e si considerino $n+1$ nodi di interpolazione distinti $x_i \in I$. Sia $f \in \mathcal C^{n+1}(I)$ allora $\forall x \in I, \exists \xi_i \in I$ tale che

$$
E_nf(x) = f(x) - \Pi_nf(x) = \frac{f^{(n+1)}(\xi_n)}{(n+1)!}\prod_{i=0}^n(x - x_i)
$$

Vale la seguente maggiorazione:

$$
\max_{x \in I} |E_nf(x)| \le \frac{\left|\max\limits_{x \in i}f^{(n+1)}(x)\right|}{4(n+1)}h^{(n+1)}
$$

Nella formula precedente, il denominatore e l'esponenziazione di $h$ tendono a mandare a zero il risultato. Questo non accade per il modulo del massimo al numeratore quando si tenta di creare un interpolatore per alcune funzioni (**fenomeno di Runge**).

Il fenomeno di Runge consiste in ampie oscillazioni nel valore della funzione interpolante verso gli estremi del dominio. Per risolvere questo problema si può utilizzare l'**interpolatore lagrangiano composito** o l'**interpolazione con ubicazione specifica dei nodi**.

Per trovare il polinomio interpolatore in MATLAB, si usa la funzione `polyfit`.

## Interpolazione Lagrangiana composita

La differenza fondamentale tra l'interpolazione lagrangiana e l'interpolazione lagrangiana composita è che mentre la prima interpola su tutto il dominio ottenendo una sola funzione interpolante, la seconda interpola su piccoli sottodomini che contengono ciascuno $k+1$ nodi e di lunghezza $H = kh$ dove $h$ è la distanza tra due nodi.

L'interpolatore globale composto dai piccoli interpolatori è denotato con $\Pi_k^H(x)$ o $\Pi_k^Hf(x)$.

I piccoli interpolatori non sono mai di grado superiore al 3 e questo ne garantisce la convergenza.

L'interpolatore globale perde in regolarità (la derivata non è continua in corrispondenza delle _giunture_ tra interpolatori piccoli) rispetto all'interpolatore di Lagrange.

In MATLAB, si può implementare l'interpolazione Lagrangiana composita come segue:

```matlab
function [f_approx] = composite_polyfit(f, xs, H, k)
    nodes_x = (xs(1):H/k:xs(end))';
    nodes_y = f(nodes_x);

    f_approx = zeros(length(xs), 1);

    for j = 1:k:length(nodes_x)-k
        p = polyfit(nodes_x(j:j+k), nodes_y(j:j+k), k);
        subset = (nodes_x(j) <= xs) & (xs <= nodes_x(j+k));
        xx_subset = xs(subset);
        f_approx(subset) = polyval(p, xx_subset);
    end
end
```

<!--
Nota per il futuro: questo codice è quasi 1:1 di parte di quello usato per la mia soluzione del "progetto 2".
E' la funzione qui sopra che è presa dal progetto, non il contrario (vorrei far notare che la data del commit è 13/05 a circa le 22:15, ovvero circa 34 ore DOPO la chiusura delle consegne).
-->

Dato che i parametri di questa funzione possono risultare non immediatamente chiari, si riporta che `f` è la funzione da approssimare e che `xs` è il vettore di valori nei quali valutare sia `f` che la sua approssimazione. `H` e `k`, invece, mantengono lo stesso significato visto nei paragrafi precedenti.

Col seguente script si decide di confrontare il valore di una funzione $f$ con la sua interpolazione Lagrangiana composita:

```matlab
xs = (0:1e-5:10)';

f = @(x) exp(3 * x) ./ (log(x) + x);
g = @(x) composite_polyfit(f, xs, 0.1, 3);

subplot(3, 1, 1);
plot(xs, f(xs));

subplot(3, 1, 2);
plot(xs, g(xs));

subplot(3, 1, 3);
plot(xs, abs(g(xs) - f(xs)));
```

Vengono riportati anche i grafici prodotti dallo script precedente:

![**La funzione `f` (riga 1), la sua approssimazione (riga 2) e l'errore (riga 3)** <br /> Si noti la differenza di ben 5 ordini di grandezza tra i valori della funzione e l'errore.](./assets/composite_polyfit.svg)

## Interpolazione con ubicazione specifica dei nodi

Dato che il fenomeno di Runge si verifica in prossimità degli estremi del dominio, una soluzione potrebbe essere quella di aumentare il numero di nodi solo dove necessario: si usano i **nodi di Chebyshev**

$$
\hat x = -\cos\left( \frac{\pi j}{n} \right) \quad j=0, \dots, n
$$

La formula precedente genera punti nell'intervallo $[-1, 1]$ ma è possibile scalarlo e translarlo su di un intervallo generico $[a, b]$ con la seguente mappa:

$$
x_{ab} = \frac{a+b}{2} + \frac{b-a}{2} \hat x
$$

L'interpolatore generato utilizzando i nodi di Chebyshev si denota con $\Pi_n^C(x)$ o $\Pi_n^Cf(x)$.

Supponiamo che $f \in \mathcal{C}^{s+1}([-1, 1])$ allora vale che

$$
\max_{x \in [-1, 1]} |f(x) - \Pi_n^Cf(x)| \le \tilde C \frac{1}{n^s}
$$

Ne seguono tre proprietà:

- se $s > 1$ allora si ha sempre convergenza;
- la velocità di convergenza cresce con $s$;
- se l'ipotesi precedente è valida per qualunque $s$ allora la velocità di covergenza è esponenziale.

## Approssimazione

Approssimare una funzione significa minimizzare l'errore (lo scarto) tra i dati a disposizione e la funzione (solitamente un polinomio) trovata.

Si supponga di avere a disposizione $n$ coppie di dati e di volerne trovare un'approssimazione polinomiale: il risultato sarà un polinomio di grado $m$ e vale che se $m = n$ allora si trova esattamente il polinomio interpolatore mentre se $m \lt n$ se ne rova un'approssimazione e il polinomio trovato, in generale, **non** passa per i dati.

Solitamente si sceglie $m$ di valore pari a 2 o 3 per evitare di incorrere nel fenomeno di Runge.

In MATLAB è possibile trovare il polinomio approssimate o interpolatore con la funzione `polyfit`: `polinomio = polyfit(X, Y, m);`.

Il metodo più spesso utilizzato per cercare il polinomio approssimante è quello dei **minimi quadrati**: sia $q(x)$ il polinomio in $x$ di grado $m$ che meglio approssima i dati a disposizione, allora $q$ è il polinomio di grado $m$ tra tutti i polinomi di grado $m$ che minimizza l'errore tra i dati e se stesso valutato nelle stesse posizioni.

$$
\sum_{i=0}^n \left[ y_i - q(x_i) \right]^2 \le \sum_{i=0}^n \left[ y_i - p_m(x_i) \right]^2 \qquad \forall p_m \in \mathbb{P}_m
$$

Con $m = 1$ il polinomio assume la forma di una retta chiamata **retta di regressione** per la quale esiste una formula analitica:

$$
q(x) = a_1x + a_0 \\
a_0 = \frac{1}{D} \left[ \sum_{i=0}^n y_i \cdot \sum_{j=0}^n x_j^2 - \sum_{j=0}^n x_j \cdot \sum_{i=0}^n x_iy_i\right] \\
a_1 = \frac{1}{D} \left[ (n+1) \sum_{i=0}^n x_iy_i - \sum_{j=0}^n x_j \sum_{i=0}^n y_i\right] \\
D = (n+1) \sum_{i=0}^n x_i^2 - \left( \sum_{i=0}^n x_i \right)^2
$$

Nel caso generale, trovare minimizzare una funzione significa trovarne i punti a gradiente nullo. La funzione **errore quadratico** è definita come segue:

$$
\Phi(a_0, a_1, \dots, a_m) = \sum_{n=0}^n \left[ y_i - (a_0 + b_1x_i + \dots + b_mx_i^m) \right]^2
$$

# Integrazione numerica

Attualmente non esistono metodi numerici per calcolare in maniera esatta (a meno di errori dovuti alla quantizzazione del numero) valori di integrali (salvo casi molto particolari e specifici).

Per calcolare numericamente integrali, si usano le **formule di quadratura** che riconducono il problema del calcolo ad un problema di calcolo di aree (introducendo un errore).

In questa sezione si farà uso della seguente notazione:

$$
I(f) = \int_a^b f(x) dx
$$

Sia $I_H(f)$ il valore dell'integrale calcolato utilizzando una formula di quadratura: si dice che la formula di quadratura è di **ordine** $p$ se

$$
E_H = |I(f) - I_H(f)| \le CH^p
$$

Si dice anche che la formula di quadratura ha **grado di esattezza** pari ad $r$ se, quando applicata a polinomi di grado pari od inferiore ad $r$ vale che

$$
E_H = |I(f) - I_H(f)| = 0 \quad \forall f \in \mathbb{P}^r (a, b)
$$

Dei tre metodi visti, esistono sia la versione _composita_ (analizzate di seguito) che quella _semplice_ (ottenuta ottenuta utilizzando l'intero intervallo invece che tanti intervallini).

## Formula del punto medio composita

Questa formula si basa sul separare l'intervallo $(a, b)$ in molteplici intervallini uguali (di dimensione $H$) per poi calcolarne l'area sottesa dalla funzione considerando come altezza il valore $f(\bar x_k)$ dove $x_k = \frac{x_k + x_{k+1}}{2}$:

$$
I_{pm}(f) = H \sum_{k=1}^M f(\bar x_k)
$$

Una stima per l'errore di questa formula è la seguente:

$$
|I(f) - I_{pm}(f)| \le \max_x |f''(x)|\frac{b - a}{24} H^2
$$

da cui deriva chela formula del punto medio ha grado di esattezza pari a 1.

Sia `f` una funzione, allora è possibile calcolarne l'integrale in MATLAB con la formula del punto medio composita come `I = H * sum(f((xs(1:end-1) + xs(2:end)) / 2))`.

## Formula dei trapezi composita

La formula dei trapezi composita si basa sull'approssimazione dell'area sottesa dalla curva come somma di trapezi:

$$
I_{tr}(f) = \frac{H}{2} \sum_{k=1}^M \left( f(x_{k-1}) + f(x_k) \right) = \frac{H}{2} \left( f(a) + f(b) \right) + H \sum_{k=1}^{M-1}f(x_k) = \int_a^b \Pi_1^Hf(x)dx
$$

L'integrale dell'ultimo passaggio è calcolabile in maniera esatta anche numericamente.

Una stima per l'errore di questa formula è la seguente:

$$
|I(f) - I_{tr}(f)| \le \frac{1}{12} \max_x |f''(x)|(b-a)H^2
$$

da cui deriva che la formula dei trapezi composita ha grado di esattezza pari a 1.

Sia `f` una funzione, allora è possibile calcolarne l'integrale in MATLAB con la formula dei trapezi composita come `I = sum(f(xs(1:end-1)) + f(xs(2:end))) / 2 * H`.

## Formula di Simpson composita

La formula di Simpson composita è l'equivalente della formula dei trapezi composita che utilizza l'interpolatore Lagrangiano composito di grado 2 invece che di grado 1:

$$
I_{sim}(f) = \frac{H}{6} \sum_{k=1}^M \left( f(x_{k-1}) + 4f(\bar x_k) + f(x_k) \right) = \int_a^b \Pi_2^Hf(x)dx \qquad \bar x_k = \frac{x_{k-1} + x_k}{2}
$$

Una stima per l'errore di questa formula è la seguente:

$$
|I(f) - I_{sim}(f)| \le \frac{b - a}{2880} \max_x \left| f''''(x) \right| H^4
$$

da cui deriva che la formula di Simpson composita ha grado di esattezza pari a 3.

Sia `f` una funzione, allora è possibile calcolarne l'integrale in MATLAB con la formula di simpson composita come `I = H / 6 * sum(f(xs(1:end-1) + f(xs(2:end) + f((xs(1:end-1) + xs(2:end))/2))))`.

# Approssimazione di derivate

Esistono cinque modi principali di approssimare una derivata e sono tutti e cinque abbastanza intuitivi: il **metodo delle differenze in avanti**, il **metodo delle differenze all'indietro**, il **metodo dell'approssimazione centrata**, il **metodi di Crank-Nicolson** ed il **metodo di Heun**. Tutti e cinque, alla fine, sono riconducibili ad una rielaborazione di un rapporto incrementale o alla soluzione dell'equzione di volterra tramite formula di quadratura.

Nelle definizioni seguenti, si considererà $\tilde T = \{t_n | i = 1, \dots, N; \ t_i - t_{i-1} = h \ \forall i = 2, \dots, N\}$.

Sia $v : \tilde T \to \mathbb{R}^n$ una funzione, allora la sua derivata approssimata con il metodo delle differenze in avanti è denotata con $D^+v(t_n)$ e si calcola come

$$
D^+v(t_n) = \frac{v(t_{n+1}) - v(t_n)}{h}
$$

Sia $v : \tilde T \to \mathbb{R}^n$ una funzione, allora la sua derivata approssimata con il metodo delle differenze all'indietro è denotata con $D^-v(t_n)$ e si calcola come

$$
D^-v(t_n) = \frac{v(t_n) - v(t_{n-1})}{h}
$$

Sia $v : \tilde T \to \mathbb{R}^n$ una funzione, allora la sua derivata approssimata con il metodo dell'approssimazione centrata è denotata con $D^cv(t_n)$ e si calcola come

$$
D^cv(t_n) = \frac{v(t_{n+1}) - v(t_{n-1})}{2h}
$$

La tabella seguente mostra, per ogni tipologia di approssimazione, il loro errore:

| Approssimazione          | Errore   |
| ------------------------ | -------- |
| Differenze in avanti     | $O(h)$   |
| Differenze in indietro   | $O(h)$   |
| Approssimazione centrata | $O(h^2)$ |

La dimostrazione, per ciascuna riga della tabella, è presente nell'[appendice](#dimostrazioni).

# Risoluzione di equazioni differenziali (problemi di Cauchy)

## Metodo di Eulero in avanti

Sia data l'equazione $y'(t_n) = f(t_n, y(t_n))$ e sia $u_n$ la successione numerica che approssima la soluzione, allora si può scrivere che

$$
D^+y(t_n) = \frac{y(t_{n+1}) - y(t_n)}{h} \simeq f(t_n, y(t_n))
$$

da cui 

$$
\frac{u_{n+1} - u_n}{h} = f(t_n, u_n)
$$

da cui si deriva il passo ricorsivo per l'applicazione del metodo di Eulero in avanti:

$$
u_{n+1} = u_n + hf(t_n, u_n)
$$

Dato che la conoscenza della soluzione al tempo $t_n$ permette di calcolare direttamente la soluzione all'istante successivo, il metodo di Eulero in avanti è detto _esplicito_.

## Metodo di Eulero all'indietro

Analogamente al metodo precedente, sia data l'equazione $y'(t_{n+1}) = f(t_{n+1}, y(t_{n+1}))$ e sia $u_n$ la successione numerica che approssima la soluzione, allora si può scrivere che

$$
D^-y(t_{n+1}) = \frac{y(t_{n+1}) - y(t_n)}{h} \simeq f(t_{n+1}, y(t_{n+1}))
$$

da cui

$$
\frac{u_{n+1} - u_n}{h} = f(t_{n+1}, u_{n+1})
$$

da cui si deriva il passo ricorsivo per l'applicazione del metodo di Eulero all'indietro:

$$
u_{n+1} = u_n + hf(t_{n+1}, u_{n+1})
$$

Dato che non è possibile, data la soluzione al tempo $t_n$, calcolare la soluzione all'istante successivo senza prima risolvere un'equazione, non necessariamente lineare, il metodo di Eulero all'indietro è detto _implicito_.

## Approssimazione tramite differenze centrate

Analogamente ai metodi precedenti, sia data l'equazione $y'(t_{n+1}) = f(t_{n+1}, y(t_{n+1}))$ e sia $u_n$ la successione numerica che approssima la soluzione, allora si può scrivere che

$$
D^cy(t_n) = \frac{y(t_{n+1}) - y(t_{n-1})}{2h} \simeq f(t_n, y(t_n))
$$

da cui

$$
\frac{u_{n+1} - u_{n-1}}{2h} = f(t_n, u_n)
$$

da cui si deriva il passo ricorsivo per l'applicazione del metodo di approssimazione tramite differenze centrate:

$$
u_{n+1} = u_{n-1} + 2hf(t_n, u_n)
$$

La particolarità di questo metodo è che non serve conoscese solo la condizione iniziale $u_0 = y(t_0) = y_0$ ma serve anche calcolare $u_1$ col metodo di Eulero in avanti. Dopo aver calcolato $u_1$, il metodo è _esplicito_.

Questo metodo può essere derivato anche dall'**equzione di Volterra** prendendo $\Delta t = 2h$ e sostituendo l'integrale con la [formula del punto medio](#formula-del-punto-medio-composita) ottenendo che

$$
y(t_{n+2}) \simeq u_{n+2} = u_n + 2hf(t_{n+1}, u_{n+1})
$$

## Metodo di Crank-Nicolson

Sia data l'equazione di Volterra corrispondente alla soluzione del problema di Cauchy sotto esame:

$$
y(t_{n+1}) = y(t_n) + \int_{t_n}^{t_{n+1}} f(t, y(t)) dt
$$

Applicando la formula dei trapezi per risolvere l'integrale a destra, si ottiene che

$$
y(t_{n+1}) = y(t_n) + h \left( \frac{f(t_n, y(t_n)) + f(t_{n+1}, y(t_{n+1}))}{2} \right)
$$

da cui si deriva il passo ricorsivo per l'applicazione del metodi di Crank-Nicolson:

$$
u_{n+1} = u_n + \frac{h}{2} \left( f(t_n, u_n) + f(t_{n+1}, u_{n+1}) \right)
$$

Come si può osservare, questo metodo è esattamente un media tra il metodo di Eulero in avanti ed il metodo di Eulero all'indietro e quindi converge meglio.

Il metodo di Crank-Nicolson è _implicito_.

## Metodo di Heun

Il metodo di Heun è molto simile al metodo di Crank-Nicolson ma, approssimando $u_{n+1}$ con il metodo di Eulero in avanti, riesce a rimanere esplicito pur mantenendone lo stesso ordine di convergenza.

Il passo ricorsivo per l'applicazione del metodo di Heun è il seguente:

$$
u_{n+1} = u_n + \frac{h}{2} \left( f(t_n, u_n) + f(t_{n+1}, u_n + hf(t_n, u_n)) \right)
$$

## Assoluta stabilità

Il concetto di **assoluta stabilità** si riferisce al fatto che, facendo scelte sbagliate in termini di $h$, la soluzione di un problema di Cauchy, approssimata con i metodi appena visti potrebbe oscillare e/o esplodere.

Sia dato il seguente problema modello con la sua soluzione esatta:

$$
\begin{cases}
    y'(t) = -\lambda y(t) \quad \lambda \gt 0 \\
    y(0) = 1
\end{cases} \qquad y(t) = e^{-\lambda t}
$$

Dato un $h \gt 0$, un metodo numerico si dice assolutamente stabile per tale valore di $h$ se

$$
|u_{n+1}| \le C_{AS} |u_n| \qquad |C_{AS}| \lt 1
$$

il che implica che

$$
\lim_{n \to + \infty} |u_n| = 0
$$

Viene analizzata ora la stabilità per ciascuno dei cinque metodi analizzati nelle sezioni precedenti.

Risolvendo il problema modello con metodo di Eulero in avanti si ottiene che

$$
u_{n+1} = u_n - h \lambda u_n = (1 - h \lambda)u_n \implies C_{AS} = 1 - h \lambda
$$

da cui si ottiene che, per garantire stabilità, bisogna che valga che

$$
-1 \lt 1 - h \lambda \lt 1 \implies h \lt \frac{2}{\lambda}
$$

Risolvendo il problema modello con il metodo di Eulero all'indietro si ottiene che

$$
u_{n+1} = u_n + h \lambda u_{n+1} \implies u_{n+1} = \frac{1}{1 + h \lambda} u_n \implies C_{AS} = \frac{1}{1 + h \lambda}
$$

da cui si ottiene che, per garantire assoluta stabilità, bisogna che valga

$$
-1 \lt \frac{1}{1 + h \lambda} \lt 1
$$

che è sempre vero.

<!--
Risolvendo il problema modello con il metodo di approssimazione tramite differenze centrate si ottiene che
 Ma si può ottenere qualcosa?? non si può esprimere mica -->

Risolvendo il problema modello con il metodo di Crank-Nicolson si ottiene che

$$
u_{n+1} = u_n + \frac{h \lambda}{2}(u_n + u_{n+1}) \implies u_{n+1} = \frac{2 + h \lambda}{2 - h \lambda} u_n
$$

da cui si ottiene che, per garantire assoluta stabilità, bisogna che valga

$$
-1 \lt \frac{2 + h \lambda}{2 - h \lambda} \lt 1
$$

che è sempre vero.

Risolvendo il problema modello con il metodo di Heun si ottiene che

$$
u_{n+1} = u_n + \frac{h}{2} \left( -\lambda u_n + (-\lambda)\left( u_n + h(-\lambda u_n) \right) \right) = \left( 1 - h\lambda + \frac{(h\lambda)^2}{2} \right)u_n
$$

da cui si ottiene che, per garantire assoluta stabilità, bisogna che valga

$$
-1 \lt \frac{(h\lambda)^2}{2} - h\lambda + 1 \lt 1
$$

ovvero 

<!--
Completare con il C_AS di Heun 
-->

Di seguito viene proposta una tabella che riassume le principali caratteristiche dei cinque metodi appena visti:

| Metodo              | Esplicitezza | Ordine di convergenza | Stabilità                                 |
| ------------------- | ------------ | --------------------- | ----------------------------------------- |
| Eulero in Avanti    | Esplicito    | 1                     | $h \lt \frac{2}{\lambda}$                 |
| Eulero all'Indietro | Implicito    | 1                     | Incondizionatamente assolutamente stabile |
| Differenze centrate | 50/50        | 2                     | N/A                                       |
| Crank-Nicolson      | Implicito    | 2                     | Incondizionatamente assolutamente stabile |
| Heun                | Esplicito    | 2                     | $h \lt \frac{2}{\lambda}$                 |

Nel caso in cui il problema di Cauchy sotto esame non sia riconducibile alla forma del problema modello, si prende

$$
\lambda = \max_{t,y} \left| \frac{\partial f}{\partial y} \right|
$$

## Consistenza

Si definisce **errore di troncamento** la quantità

$$
\tau_n = \left| f(t_n, y(t_n)) - Dy(t_n) \right|
$$

dove $Dy(t)$ è l'approssimazione data dal metodo numerico scelto.

Il metodo numerico scelto è **consistente** se

$$
\lim_{h \to 0} \tau_n = 0 \qquad \forall n
$$

E' possibile affermare che un metodo numerico è **consistente di ordine $p$** se

$$
\tau_n = O(h^p) \qquad \forall n
$$

In generale, per un metodo convergente, l'ordine di convergenza è uguale all'ordine di consistenza.

# Appendice

## Richiami di algebra lineare ed analisi

In questa sezione verranno ripresi concetti di algebra lineare necessari per la comprensione di quanto scritto nelle sezioni precedenti.

### Metodo di Cramer

Sia $Ax = b$ un sistema lineare. I membri del vettore soluzione $x$ vengono calcolati col metodo di Cramer attraverso la seguente formula:

$$
x_j = \frac{\det(A_j)}{\det(A)} \qquad A_j = [a_1 | \dots | a_{j-1} | b | a_{j+1} | \dots | a_n ]
$$

dove $a_i$ è la $i$-esima colonna di $A$.

### Norma

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

### Disuguaglianze di Cauchy-Schwartz e triangolare

Siano $x, y \in \mathbb R^n$ allora

- per Cauchy-Schwartz vale che $| \langle x, y \rangle | \le \| x \| \cdot \| y \|$;
- per la triangolare vale che $\| x + y \| \le \| x \| + \| y \|$.

Entrambe valgono per qualsiasi norma.

### Problema di Cauchy

Risolvere un problema di Cauchy consiste nel trovare una funzione $y : I \subseteq \mathbb R \to \mathbb{R}^n$ tale che

$$
\begin{cases}
    y'(t) = f(t, y(t)) \\
    y(t_0) = y_0
\end{cases}
$$

con $t_0$ e  $y_0$ dati e $f : I \times \mathbb{R}^n \to \mathbb{R}^n$.

Sia $f$ continua e limitata rispetto ad entrambi gli argomenti e lipschitziana rispetto al secondo, allora la soluzione $y$ esiste ed è unica ed inoltre $y \in \mathcal{C}^1(I)$.

Qualunque soluzione di qualunque problema di Cauchy può essere scritta nella forma dell'**Equazione di Volterra**:

$$
y(t) = y(t_0) + \int_{t_0}^{\Delta t} f(s, y(s)) ds
$$

## Dimostrazioni

### Unicità del polinomio interpolatore

sia $\Psi_n(x)$ un polinomio interpolante di grado $n$ e $D(x) = \Pi_n(x) - \Psi_n(x)$, anch'esso di grado $n$.$\forall i = 0, \dots, n$ vale che $D(x_i) = \Pi_n(x_i) - \Psi_n(x_i) = 0$.Questo significa che $D$ ha almeno $n+1$ radici ma essendo di grado $n$ allora per forza vale che $D \equiv 0$ da cui $\Pi_n(x) = \Psi_n(x)$.

### Errore di approssimazione di derivate

Di seguito viene dimostrato che l'errore dell'approssimazione di derivate tramite il metodo delle differenze in avanti è un O-grande di $h$ (il procedimento è analogo per il metodo delle differenze all'indietro).

Siano $\tilde T = \{t_n | i = 1, \dots, N; \ t_i - t_{i-1} = h \ \forall i = 2, \dots, N\}$ e $v : \tilde T \to \mathbb{R}^n$.

$$
v(t_{n+1}) = v(t_n) + hv'(t_n) + \frac{h^2}{2}v''(t_n) + \frac{h^3}{6}v'''(t_n) + O(h^3)
$$

Dalla formula precedente deriva che

$$
v'(t_n) = \frac{v(t_{n+1}) - v(t_n)}{h} \underbrace{- \frac{h^2}{2}v''(t_n) - \frac{h^3}{6}v'''(t_n) + O(h^3)}_{O(h)}
$$

Da cui la tesi.

Per l'approssimazione centrata, il procedimento è simile ma con un passaggio in più.

Sapendo che

$$
\begin{cases}
    v(t_{n+1}) = v(t_n) + hv'(t_n) + \frac{h^2}{2}v''(t_n) + \frac{h^3}{6}v'''(t_n) + O(h^3) \\
    v(t_{n-1}) = v(t_n) - hv'(t_n) + \frac{h^2}{2}v''(t_n) - \frac{h^3}{6}v'''(t_n) + O(h^3)
\end{cases}
$$

allora si può calcolare

$$
v(t_{n+1}) - v(t_{n-1}) = 2hv'(t_n) + \frac{h^3}{3}v'''(t_n) + O(h^3)
$$

da cui, isolando $v'(t_n)$, si deduce la tesi.

## Funzioni MATLAB

Di seguito vengono riportati i listati per alcune delle funzioni utilizzate nella presente pagina o che possono comunque risultare utili per mettere in pratica quanto appena riassunto.

```matlab
function [A] = generate_converging_matrix(N)
    T = rand(N);
    v = sum(T, 2);
    A = T + diag(v);
end
```

```matlab
function [A] = generate_sdp_matrix(N)
    A = rand(N);
    A = A * A';
end
```

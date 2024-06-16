---
title: "Riassunto estremamente sintetico di Informazione e Stima"
author:
- "Andrea Oggioni"
- "Niccolò Papini"
- "Emanuel Mihali"
---

# Introduzione

Lo **spazio campionario** $\Omega$ di un esperimento aleatorio è l'insieme di tutti i risultati possibili di tale esperimento. Lo spazio campionario deve essere **completo** (deve coprire tutti i possibili esiti) e i suoi elementi devono godere di **mutua esclusività** (l'avvenire di un evento implica il non avvenire degli altri).

Siano $A$ e $B$ due eventi, allora valgono i seguenti assiomi:

1. Assioma di **non-negatività**: $P(A) \ge 0$
2. Assioma di **normalizzazione**: $P(\Omega) = 1$
3. Assioma di **additività**: se $A \cap B = \emptyset$ allora $P(A \cup B) = P(a) + P(B)$

Dal terzo assioma segue il **teorema delle probabilità totali**: siano $A_1, A_2, \dots, A_n$ $n$ eventi disgiunti, allora

$$
P \left( \bigcup_{i = 1}^n A_i \right) = \sum_{i = 1}^{n} P(A_i)
$$

Esiste una versione estesa dello stesso teorema che vale anche per eventi non necessariamente disgiunti: siano $A_1, A_2, \dots, A_n$ $n$ eventi, allora

$$
P \left( \bigcup_{i = 1}^n A_i \right) = \sum_{r = 1}^{n} \left( (-1)^{r+1} \sum_{i_1 \lt i_2 \lt \dots \lt i_r} P(A_{i_1} \cap A_{i_2} \cap \dots \cap A_{i_r}) \right)
$$

E' importante notare che l'assioma 3 (e il teorema che ne segue) si applicano anche ad un infinità di eventi, purchè tale infinità sia numerabile.

# Probabilità condizionate

Condizionare una probabilità significa andare a mappare lo spazio campionario su un altro evento.

$$
P(A | B) = \frac{P(A \cap B)}{P(B)}
$$

Se un evento non è condizionato, allora lo si può considerare condizionato ad $\Omega$.

Dalla formula inversa segue che

$$
P(A \cap B) = P(A | B) \cdot P(B) = P(B | A) \cdot P(A)
$$

Le probabilità condizionate seguono gli assiomi esattamente come le probabilità normali.

Siano $A$ e $B$ due eventi. Vale che $P(B) = P(\{B \cap A\} \cup \{B \cap A^C \})$.

Vale la regola detta **chain-rule**: $P(A_1 \cap A_2 \cap \dots \cap A_n) = P(A_1) \cdot P(A_2 | A_1) \cdot P(A_3 | A_1 \cap A_2) + \dots + P(A_n | A_1 \cap A_2 \cap \dots \cap A_{n-1})$.

<a id="probabilita_totali"></a>
Vale il **teorema delle probabilità totali**: sia $\{A_1, A_2, \dots, A_n\} \sube \Omega$ una partizione di $\Omega$ e $B \sube \Omega$, allora

$$
P(B) = \sum_{i = 1}^{n} P(B | A_i) \cdot P(A_i)
$$

Nelle ipotesi del teorema precedente, per rispondere a domande del tipo "Sapendo che $B$ è accaduto, qual è la probabilità che sia stato causato dall'evento $i$-esimo?" si usa la **regola di Bayes**:

$$
P(A_i | B) = \frac{P(B \cap A_i)}{P(B)} = \frac{P(B | A_i) \cdot P(A_i)}{P(B)} = \frac{P(B | A_i) P(A_i)}{\sum\limits_{j=1}^n P(B | A_j) \cdot P(A_j)}
$$

# Eventi indipendenti

Due eventi sono **indipendenti** se e solo se $P(A \cap B) = P(A) \cdot P(B)$.

Se due eventi sono indipendenti, allora è anche vero che $P(B | A) = P(B)$.

## Indipendenza condizionata

Siano $A$ e $B$ due eventi non necessariamente indipendenti. Tali eventi sono indipendenti condizionatamente ad un evento $C$ se e solo se $P(A \cap B | C) = P(A | C) \cdot P(B | C)$.

L'indipendenza condizionata non è legata in alcun modo a quella non condizionata.

## Indipendenza tra insiemi di eventi

Sia $\{A_1, A_2, \dots, A_n\}$ un insieme di eventi. Tali eventi sono detti **indipendenti** se e solo se $P(A_i \cap A_j \cap \dots \cap A_q) = P(A_i) \cdot P(A_j) \cdot \dots \cdot P(A_q)$ per ogni sottoinsieme $\{A_i, A_j, \dots, A_q\} \sube \{A_1, A_2, \dots, A_n\}$.

Se la condizione di cui sopra vale solo per i sottoinsiemi di arità 2 allora l'insieme $\{A_1, A_2, \dots, A_n\}$ viene detto **indipendente a coppie**.

L'indipendenza totale implica l'indipendenza a coppie ma non vale il viceversa.

# Principi di calcolo combinatorio

Se si ha un esperimento composto da $r$ stadi, ognuno con $n_i$ scelte possibili, il numero totale di scelte è dato da

$$
\prod_{i=1}^r n_i
$$

Con le **permutazioni** si contano i possibili modi di ordinare un insieme di elementi: tale numero è dato da $n!$ dove $n$ è l'arità dell'insieme considerato.

Il **Numero di sottoinsiemi di un insieme** è dato da $2^n$ dove $n$ è l'arità dell'insieme considerato.

Se si vogliono solamente sottoinsiemi di $k$ elementi, si utilizza il **coefficiente binomiale**:

$$
{n \choose k} = \frac{n!}{k!(n - k)!}
$$

<a id="probabilita_binomiali"></a>

Sia $P(A) = p$. Per calcolare la probabilità di avere $k$ successi su $n$ si deve prima calcolare la probabilità di avere una particolare configurazione che abbia $k$ successi e poi si moltiplica per il numero di tutte le possibili configurazioni possibili che abbiano $k$ successi:

$$
P(k \text{ successi su } n) = {n \choose k} p^k (1 - p)^{n - k}
$$

Questa formula calcola le cosiddette **probabilità binomiali**.

Per calcolare quali sono i modi di partizionare un insieme di arità $n$ in $r$ sottoinsiemi di arità $k_1, k_2, \dots, k_r$, si usa il **coefficiente multinomiale**:

$$
{n \choose k_1, k_2, \dots, k_r} = \frac{n!}{k_1! k_2! \dots k_r!}
$$

Lo stesso metodo funziona anche per problemi quali dover calcolare il numero di **anagrammi** di una parola: $n$ è la lunghezza della parola e i vari $k_i$ sono il numero di occorrenze di ciascuna lettera della parola all'interno della stessa.

Se si vogliono contare i modi di estrarre elementi da insiemi di elementi diversi in un certo numero, si può ricorrere alle **probabilità ipergeometriche**:

$$
\frac{{n_1 \choose k_1} {n_2 \choose k_2} \dots {n_r \choose k_r}}{n_1 + n_2 + \dots + n_r \choose k_1 + k_2 + \dots + k_r}
$$

# Variabili aleatorie discrete

Una **variabile aleatoria** è una funzione $H : \Omega \to \mathbb{R}$.

Tutte le funzioni deterministiche di una variabile aleatoria sono a loro volta variabili aleatorie.

Per risolvere problemi de tipo "Qual è la probabilità di ottenere il primo successo al $k$-esimo tentativo?" si usano le variabili aleatorie **geometriche**:

$$
X \sim \text{Geom}(p) \iff P_X(k) = \begin{cases}
    p(1-p)^{k-1} & k = 1, 2, \dots \\
    0 & \text{Altrimenti}
\end{cases}
$$

Per descrivere le distribuzioni di probabilità binomiali viste [sopra](#probabilita_binomiali) esistono le variabili aleatorie **binomiali**:

$$
X \sim \text{Bin}(n, p) \iff P_X(k) = \begin{cases} 
    {n \choose k} p^k (1-p)^{n-k} & k = 0, 1, 2, \dots \\
    0 & \text{Altrimenti}
\end{cases}
$$

# Valore atteso e varianza

Il **valore atteso** rappresenta la media delle varie realizzazioni, pesate per la loro probabilità di realizzarsi:

$$
E[X] = \sum_{x \in \mathbb{R}} x \cdot p_X(x)
$$

Se $Y = g(X)$ e $g$ è una funzione deterministica, allora

$$
E[Y] = \sum_{x \in \mathbb{R}} g(x) \cdot P_X(x) = \sum_{y \in \mathbb{R}} y \cdot P_Y(y)
$$

In generale $E[g(X)] \ne g(E[X])$, tranne nel caso in cui $g(x) = \alpha x + \beta$. In tal caso, $E[\alpha X + \beta] = \alpha E[X] + \beta$ (il valore atteso è un'operazione lineare).

La **varianza** descrive quanto i valori sono dispersi rispetto alla media:

$$
Var[X] = E[(X - E[X])^2] = E[X^2] - E[X]^2 = \sum_{x \in \mathbb{R}} (x - E[X])^2 P_X(x)
$$

Vale che $Var[\alpha X + \beta] = \alpha^2 Var[X]$

Dato che la varianza ha un'unità di misura quadrata rispetto all'unità di misura della variabile aleatoria, è stato introdotto un altro indicatore, la **deviazione standard** o **scarto quadratico medio** che si ottiene estraendo la radice qudrata della varianza:

$$
\sigma_X = \sqrt{Var[X]}
$$

Applicando le formule date, è possibile estrarre una formula veloce per calcolare il valore atteso e la varianza di variabili aleatorie geometriche e binomiali.

$$
X \sim \text{Geom}(p) \implies \begin{cases}
    E[X] = \frac{1}{P} \\
    Var[X] = \frac{1 - p}{p^2}
\end{cases}
$$

$$
X \sim \text{Bin}(n, p) \implies \begin{cases}
    E[X] = np \\
    Var[X] = np(1 - p)
\end{cases}
$$

## Valore atteso condizionato

Il **valore atteso condizionato** è come il valore atteso ma condizionato ad un altro evento:

$$
E[X|B] = \sum_{x \in \mathbb{R}} x \cdot P_{X|B}(x)
$$

Il valore atteso condizionato gode di tutte le proprietà del valore atteso normale.

Per la **legge della perdita di memoria**, che vale solo per le variabili aleatorie geometriche, $E[X - t | X > t] = E[X]$, cioè, se ho già effettuato alcuni tentativi senza successo, il numero di tentativi che ci si aspetta dover effettuare (il valore atteso, appunto) è pari al valore atteso che si avrebbe senza aver già effettuato altri tentativi.

In poche parole, il numero di tentativi fallimentari già fatti non va a influenzare il valore atteso.

Vale la legge dell'**aspettativa totale**: sia $\{A_1, A_2, \dots, A_n\}$ una partizione di $\Omega$ e $X \sube \Omega$, allora 

$$
E[X] = \sum_{i=1}^n P(A_i) \cdot E[X | A_i]
$$

# Variabili aleatorie discrete multiple

Le **variabili aleatorie discrete multiple** funzionano esattamente come quelle singole ma vanno a descrivere eventi composti da più variabili aleatorie invece che una sola.

Valgono le stesse proprietà e le stesse regole:

$$
\begin{cases}
    P_{X,Y}(x, y) \ge 0 & \forall (x, y) \in \mathbb{R} \\
    \sum\limits_{(x, y) \in \mathbb{R}^2} P_{X,Y}(x, y) = 1
\end{cases}
$$

Anche i condizionamenti funzionano in maniera molto simile a quanto visto [precedentemente](#probabilità-condizionate) e vale la chain-rule:

$$
P_{X|Y}(x | y) = \frac{P_{X,Y}(x, y)}{P_Y(y)} = \frac{P_{X,Y}(x, y)}{\sum\limits_{t} P_{X, Y}(t, y)} \\
P_{X,Y}(x, y) = P_{X|Y}(x| y) \cdot P_Y(y) = P_{Y|X}(y| x) \cdot P_X(x)
$$

Due variabili aleatorie sono dette **indipendenti** ($X \perp Y$) se e solo se $P_{X,Y}(x, y) = P_X(x) \cdot P_Y(y)$. Questo ragionamento può essere esteso ad un numero arbitrario di variabili.

L'**indpendenza condizionata** è come l'indipendenza normale ma si applica su un sottoinsieme dello spazio $\mathbb{R^2}$. Due variabili aleatorie possono essere indipendenti condizionatamente ad un certo evento ma non in generale.

Il **valore atteso per variabili aleatorie multiple** si calcola come segue:

$$
E[g(X, Y)] = \sum_x \sum_y g(x, y) \cdot P_{X,Y}(x, y)
$$

La funzione $E[x^j y^k]$ viene detta momento congiunto di $X$ e $Y$.

(Ri)valgono le seguenti proprietà:

$$
E[\alpha X + \beta Y + \gamma] = \alpha E[X] + \beta E[Y] + \gamma \\
X \perp Y \implies E[X \cdot Y] = E[X] \cdot E[Y]
$$

Siano $X$ e $Y$ due variabili aleatorie e $Z = X + Y$, allora la varianza di $Z$ si calcola come

$$
Var[Z] = Var[X + Y] = E[(X + Y)^2] + E[X + Y]^2 = Var[X] + Var[Y] - 2(E[X \cdot Y] - E[X] \cdot E[Y])
$$

Se $X \perp Y$ allora $Var[X + Y] = Var[X] + Var[Y]$.

Sia $X \sim \text{Bin}(n, p)$ e $X_i$ la variabile aleatoria che vale 1 se si è avuto un successo nella $i$-esima prova e 0 altrimenti. In questo caso, si dice che $X_i \sim \text{Bern}(p)$.

Dato che le varie $X_i$ sono tutte indipendenti tra di loro e la probabilità di successo non cambia in base al numero della prova, le variabili $X_i$ sono dette **indipendenti e identicamente distribuite** (**IID**). Da questo segue che

$$
E[X_i] = E[X_{\overline i}] = p \qquad \forall i, \text{ Con $\overline i$ fissato}
$$

Da tutto ciò segue che

$$
E[X] = E \left[\sum_{i=1}^n X_i \right] = \sum_{i=1}^n E[X_i] = n \cdot E[X_1] = np
$$

La varianza invece è calcolata come

$$
Var[X] = np(1 - p)
$$

# Variabili aleatorie continue

Le **variabili aleatorie continue** sono come quelle discrete ma le realizzazioni possono assumere un qualsiasi valore compreso in un un intervallo continuo. Le probabilità di ogni realizzazione sono descritte dalla funzione continua **probability density function** (**pdf**). In realtà, la frase appena scritta non è proriamente corretta: la pdf viene utilizzata per descrivere la probabilità che una realizzazione rientri entro un certo intervallo, non che assuma un certo valore:

$$
P(a \le X \le b) = \int_{a}^{b} f_X(x) dx
$$

Nella formula precedente, è utile notare come le varie occorrenze di $\le$ possano essere sostituite (completamente o in parte) da $\lt$ senza che il risultato cambi.

Valgono le seguenti proprietà:

$$
f_X(x) \ge 0 \qquad \forall x \in \mathbb{R} \\
\int_{-\infty}^{+\infty} f_X(x) dx = 1
$$

Valore atteso e varianza non cambiano ma si utilizza l'integrazione al posto della sommatoria:

$$
E[X] = \int_{-\infty}^{+\infty} x \cdot f_X(x) dx \\
Var[X] = \int_{-\infty}^{+\infty} (x - E[X])^2 \cdot f_X(x) dx
$$

Avendo introdotto le variabili aleatorie continue, è possibile introdurre le **variabili aleatorie uniformi**:

$$
X \sim U[a, b] \implies f_X(x) = \begin{cases}
    \frac{1}{b - a} & a \le x \le b \\
    0 & \text{altrimenti}
\end{cases}
$$

$$
E[X] = \frac{b - a}{2} \\
Var[X] = \frac{(b - a)^2}{12}
$$

La **funzione cumulata** è semplicemente l'integrale da $-\infty$ a $x$ della pdf:

$$
F_X(x) = \int_{-\infty}^{x} f_X(x) dx
$$

E' intuitivo come segua che 

$$
\lim_{x \to \infty} F_X(x) = 1
$$

## Variabili aleatorie gaussiane (normali)

Le **variabili aleatorie gaussiane (normali)** meritano un paragrafo a parte perchè sono un concetto molto importante.

Sia $X \sim \mathcal{N}(\mu, \sigma^2)$ una variabile aleatoria gaussiana. La sua pdf è data da

$$
f_X(x) = \frac{1}{\sigma \sqrt{2 \pi}}e^{-\left(\frac{x - \mu}{\sigma}\right)^2\frac{1}{2}}
$$

Le distribuzioni gaussiane sono onnipresenti in natura ([meme obbligatorio](https://www.reddit.com/r/funny/comments/m4aaee/how_to_explain_normal_distribution_to_a_bro_at/)) e consentono di descrivere una pdf data la media e la varianza:

$$
E[X] = \mu \\
Var[X] = \sigma^2
$$

Esiste una particolare distribuzione gaussiana che prende il nome di $Z \sim \mathcal{N}(0, 1)$ alla qule tutte le altre distribuzioni possono essere ricondotte tramite $X = \sigma Z + \mu \implies Z = \frac{X - \mu}{\sigma}$.

La cumulata di una gaussiana generica si scrive come

$$
F_X(x) = \int_{-\infty}^{x} \frac{1}{\sigma \sqrt{2 \pi}}e^{-\left(\frac{x - \mu}{\sigma}\right)^2\frac{1}{2}} dx
$$

Solo 3 valori sono costanti per questa funzione: $F_X(-\infty) = 0$, $F_X(0) = 0.5$ e $F_X(+\infty) = 1$

La cumulata della gaussiana $Z$ prende il nome di $\Phi(x)$. Per calcolare $\Phi$ senza ricorrere a integrali, è possibile utilizzare specifiche tabelle (come [questa, dal sito dell'università di Chieti](http://www.biostatistica.unich.it/mat_didattica/Odont/Tavole.pdf)) che riportano una grande quantità di valori con buona precisione.

Come già detto, è possibile ricondurre una qualsiasi distribuzione alla distribuzione $Z$. La cumulata di qualsiasi gaussiana è pari a $\Phi(\frac{x - \mu}{\sigma})$.

Valgono le seguenti uguaglianze (che si possono dedurre dalle proprietà degli integrali ma che vengono riportate comunque):

$$
P(X \le a) = F_X(a) \\
P(a \le X \le b) = F_X(b) - F_X(a) \\
P(X \ge a) = 1 - F_X(a) \\
F_X(a) = 1 - F_X(-a) \\
$$

E' possibile calcolare la **marginale** di una combinazione di variabili gaussiane andando ad imporre quanto segue:

$$
\frac{(Y - \mu_X)^2}{\sigma_X^2} - \frac{(Y - \mu_Y)^2}{\sigma_Y^2} = k
$$

# Densità di probabilità congiunta e trasformazioni di variabili aleatorie

Una **densità di probabilità congiunta** è una pdf che mappa un $n$-upla di elementi ad un valore reale. Valgono ancora una volta se solite proprietà:

$$
f_{X,Y}(x, y) \ge 0 \qquad \forall (x, y) \in \mathbb{R}^2 \\
{\int \int}_{\mathbb{R}^2} f_{X,Y}(x, y) dxdy = 1 \\
E[g(X, Y)] = {\int\int}_{\mathbb{R}^2}g(x, y)F_{X,Y}(x, y) dxdy
$$

E' possibile calcolare le **distribuzioni marginali** per $X$ e per $Y$:

$$
F_X(x) = \int_{\mathbb{R}} f_{X,Y}(x, y) dy \\
F_Y(y) = \int_{\mathbb{R}} f_{X,Y}(x, y) dx
$$

Due variabili aleatorie continue sono **indipendenti** ($X \perp Y$) se 

$$
f_{X,Y}(x, y) = f_X(x) \cdot f_Y(y) \qquad \forall (x, y) \in \mathbb{R}^2
$$

Anche per i condizionamenti, valgono le formule sopra ma riadattate con l'integrale al posto della sommatoria:

$$
f_{Y|X} = \frac{f_{X,Y}(x, y)}{f_X(x)} = \frac{f_{X,Y}(x, y)}{\int_\mathbb{R} f_{X,Y}(x, y) dy}
$$

Continua a valere la regola di Bayes ma con delle caratteristiche nuove:

$$
P_{X|Y}(x | y) = \frac{P_{Y|X}(y | x) \cdot P_X(x)}{P_Y(y)}
$$

$P_X$ viene detta **legge a priori**, $P_{Y|X}$ viene detta **legge di causa-effetto** (o **di verosimiglianza**) e $P_{X|Y}$ viene detta **legge a posteriori**.

E' possibile combinare più variabili aleatorie in una funzione deterministica di esse: siano $f_{X,Y}(x, y)$ la pdf combinata di $X$ e $Y$ e $Z = g(X, Y)$ una funzione deterministica delle due variabili aleatorie precedenti. Vale che 

$$
E[Z] = {\int\int}_{\mathbb{R}^2} g(x, y) \cdot f_{X,Y}(x, y) dxdy
$$

Se $Y = \alpha X + \beta$ (quindi $Y$ è una trasformazione lineare di $X$) e $f_X$ è la pdf di $X$, allora 

$$
f_Y(y) = f_X\left(\frac{y - \beta}{\alpha}\right)\frac{1}{|\alpha|}
$$

Se invece $Y = g(X)$ con $g$ monotona, vale che

$$
f_Y(y) = \frac{f_X(x)}{\left| \frac{dg}{dx} (x) \right|} = \frac{f_X(g^{-1}(y))}{\left| \frac{dg}{dx} \left( g^{-1}(y) \right) \right|}
$$

Se $g$ non è monotona, si può dividerla in casi monotoni.

La legge della **somma di due variabili indipendenti** è la convoluzione delle leggi di probabilità.

Siano $X$ e $Y$ due variabili aleatorie indipendenti e $W = X + Y$. Si vuole calcolare la probabilità che $W = w$:

$$
P(W = w) = P(X + Y = w) = \sum_{(x, y) : x + y = w} P_{X,Y}(x, y) = \sum_{(x, y) : x + y = w} P_X(x) \cdot P_Y(y) = \underbrace{\sum_{(x, y) : x + y = w} P_X(x) \cdot P_Y(w - x)}_{\text{Somma di convoluzione}}
$$

Nel caso continuo, si sostituisce la sommatoria con l'integrale:

$$
P(W = w) = \int_{-\infty}^{\infty} f_X(x) \cdot f_Y(w - x) dx
$$

Se le due variabili $X$ e $Y$ sono gaussiane, si dimostra con la formula appena sopra che la pdf della loro somma è a sua volta gaussiana. In particolare se $X \sim \mathcal{N}(\mu_X, \sigma_X^2)$ e $Y \sim \mathcal{N}(\mu_Y, \sigma_Y^2)$ allora $X + Y \sim \mathcal{N}(\mu_X + \mu_Y, \sigma_X^2 + \sigma_Y^2)$.

# Covarianza

La **covarianza** descrive quanto due variabili sono correlate tra loro.

$$
Cov[X, Y] = E[(X - E[X]) \cdot (Y - E[Y])] = E[X \cdot Y] - E[X] \cdot E[Y]
$$

E' utile notare che 

- $Cov[X, X] = Var[X]$
- $E[X] = 0 \lor E[Y] = 0 \implies Cov[X, Y] = E[X \cdot Y]$
- $X \perp Y \implies Cov[X, Y] = 0$ ma non vale il contrario

## Varianza della somma di variabili aleatorie qualunque

$$
Var \left[ \sum_{i = 1}^{n} X_i \right] = \sum_{i = 1}^{n} Var[X_i] + 2 \sum_{i \lt j} Cov[X_i, X_j]
$$

## Coefficiente di correlazione lineare

Il **coefficiente di correlazione lineare** funziona un po' come la varianza ma è adimensionale e normalizzato rispetto alle variabili aleatorie.

$$
\rho[X, Y] = \frac{Cov[X, Y]}{\sigma_X, \sigma_Y} = E \left[ \frac{(X - E[X])}{\sigma_X} \cdot \frac{(Y - E[Y])}{\sigma_Y} \right]
$$

Valgono alcune proprietà:

- $-1 \le \rho[X, Y] \le 1$
- $|\rho[X, Y]| = 1 \iff X - E[X] = c(Y - E[Y]), \ Y = aX + b$
- $X \perp Y \implies \rho[X, Y] = 0$

# Valore atteso condizionato e varianza condizionata

Il **valore atteso condizionato** è identico al valore atteso ma condizionato ad una specifica realizzazione (o un insieme di esse) ma è esso stesso una variabile aleatoria in quanto funzione di altre variabili aleatorie.

Si supponga di voler calcolare il valore atteso della variabile aleatoria $E[Y|X] = g(X)$:

$$
E[E[Y|X]] = E[g(X)] = \int_{-\infty}^{+\infty} g(X) \cdot f_X(x) dx = \int_{-\infty}^{+\infty} E[Y | X = x] \cdot f_X(x) dx = E[Y]
$$

Questa formula prende il nome di **legge delle aspettative iterate** e spesso la si ripercorre da destra verso sinistra: se $Y$ dipende da $X$ allora $E[Y] = E[Y | X]$.

Per la varianza condizionata, si procede in modo simile:

$$
Var[X | Y = y] = E[X^2 | Y = y] + E[X | Y = y]^2 \\
Z = Var[X | Y] = g(Y) = \begin{cases}
    Var[X | Y = y] & \text{Con pdf $F_Y(y)$} \\
    \not \exists & \text{Altrimenti}
\end{cases}
$$

$Var[X | Y]$ è a tutti gli effetti una variabile aleatoria che vale $Var[X | Y = y]$ con pdf $f_Y(y)$.

Per la **legge della variazione totale** vale che

$$
Var[X] = E[Var[X | Y]] + Var[E[X | Y]]
$$

In pratica, è come se il primo termine descrivesse la variabilità all'interno di ciascuna realizzazione di $Y$ mentre il secondo, la variabilità tra le diverse realizzazioni.

# Somma di un numero casuale di variabili aleatorie

Sia $N$ una variabile aleatoria discreta e $X_1, X_2, \dots, X_N$ variabili aleatorie continue e $X$ la variabile aleatoria che descrive la somma delle varie $X_i$, allora

$$
E[X] = E \left[ \sum_{i=1}^N X_i \right] = E\left[ E\left[ \sum_{i=1}^N X_i \middle| N \right] \right]
$$

dove

$$
E\left[ \sum_{i=1}^N X_i \middle | N = n \right] = E \left[ \sum_{i=1}^n X_i \middle| N = n \right] = \sum_{i=1}^n E[X_i | N = n] = \sum_{i=1}^n E[X_i] = nE[X_1]
$$

quindi

$$
E[X] = E[N \cdot E[X_1]] = E[N] \cdot E[X_1]
$$

La varianza totale si calcola tramite la legge della varianza totale:

$$
Var[X] = Var[E[X | N]] + E[Var[X | N]] = Var[N] \cdot E[X_1]^2 + E[N] \cdot Var[X_1]
$$

# Disuguaglianza di Markov e di Chebyshev

La **disuguaglanza di Markov** afferma che, se $X \ge 0$ allora $E[X] \gt a \cdot P(X \ge a)\ \forall a \ge 0$.

La **disuguaglianza di Chebyshev** (che è derivata da quella di Markov) afferma che $Var[X] \ge a \cdot P((X - E[X])^2 \ge a)\ \forall a \ge 0$.

# Convergenza in probabilità

Sia $\{A_k\}$ una successione di variabili aleatorie ed $a$ un numero. Si dice che $\{A_k\}$ **converge in probabilità** ad $a$ ($A_k \overset{P}{\to} a$) se 

$$
\lim_{k \to \infty} P(|A_k - a| \gt \varepsilon) = 0 \qquad \forall \varepsilon \gt 0
$$

Questo tipo di convergenza è anche detta **convergenza debole** in quanto non dà garanzie sulla convergenza dei momenti di $A_k$.

# Media campionaria e legge dei grandi numeri

La media campionaria si utilizza per calcolare la media di una variabile aleatoria avendo a disposizione un numero limitato di campioni.

Siano $X_1, X_2, \dots, X_n$ variabili aleatorie indipendenti e identicamente distribuite (quindi $n$ campioni). La media campionaria è a sua volta una variabile aleatoria:

$$
M_n = \frac{X_1 + X_2 + \dots + X_n}{n}
$$

E' possibile dimostrare che

$$
E[M_n] = E[X] \qquad Var[M_n] = \frac{X_1}{n} \overset{n \to \infty}{\to} 0
$$

Dato che

$$
0 \le \lim_{n \to \infty}{P(|M_n - E[X]| \gt \varepsilon)} \le \lim_{n \to \infty} \frac{Var[M_n]}{\varepsilon^2} = \lim_{n \to \infty} \frac{Var[X_1]}{n \varepsilon^2} = 0
$$

allora $M_n \overset{P}{\to} E[M_n] = E[X]$. Questo risultato viene detto **legge debole dei grandi numeri** (**WLLN**) e dice che la media campionaria converge in probabilità al proprio valore atteso.

# Problema del sondaggista

Si ha una popolazione nella quale ogni persona può soddisfare o meno un evento $A$. Si vuole stimare la frazione $f$ delle persone che soddisfano l'evento senza intervistare l'intera popolazione.

Sia $X_i$ la variabile aleatoria che, per ogni persona $i$-esima, rappresenta con $1$ il fatto che si è verificato l'evento e con $0$ il fatto opposto, allora si può stimare $f$ attraverso la media campionaria:

$$
P_{X_i}(1) = f \quad X_i \sim \text{Bern}(f) \quad E[X_i] = f \quad Var[X_i] = f(1-f) \\\
\hat f = M_n = \frac{X_1 + X_2 + \dots + X_n}{n}
$$

L'obiettivo è quello di cadere in un buon intervallo di accuratezza $l_a$ con un buon livello di fiducia $l_f$ (cioè avere un'alta probabilità di cadere in un intorno "piccolo" di $f$).

$$
P(|M_n - f| \le l_a) \ge l_f
$$

Dati $l_a$ e $l_f$, è necessario trovare un modo di avere $n$ più piccolo possibile. Per trovare $n$ si usa la [disuguaglianza di Chebyshev](#disuguaglianza-di-markov-e-di-chebyshev).

$$
P(|M_n - f| \le l_a) \ge l_f \iff P(|M_n - f| \ge l_a) \le \frac{Var[M_n]}{l_a^2} \le \frac{1}{4nl_a^2} \le 1 - l_f
$$

dunque basta imporre $\frac{1}{4nl_a^2} \le 1 - l_f$ per trovare il minimo $n$ tale per cui si raggiungono i livelli di accuratezza e fiducia desiderati.

Per diminuire $n$ è possibile abbassare l'accuratezza (ottenendo un vantaggio quadratico) o il livello fiduciario, oppure è possibile utilizzare approssimazioni migliori rispetto a quella derivata dalla disuguaglianza di Chebyshev.

# CLT - Teorema fondamentale del limite

Se $Z_n$ è la media campionaria $n$ variabili aleatorie $X_i$ indipendenti e identicamente distribuite con varianza finita, allora $F_{Z_n}(c) \overset{n \to \infty}{\to} \Phi(c)$. Questo teorema vale per qualsiasi distribuzione di probabilità delle $X_i$.

E' possibile applicare il CLT al problema del songaggista, ottenendo che la condizione da imporre per ottenere $n$ è 

$$
2 \left( 1 - \Phi\left(l_a \sqrt{4n}\right) \right) \le 1 - l_f
$$

E' anche possibile utilizzare questo teorema per approssimare il valore di una binomiale i cui fattoriali sono troppo grandi per essere calcolati utilizzando metodi classici: sia $S_n \sim \text{Bin}(n, p)$ allora

$$
\frac{S_n - E[S_n]}{\sqrt{Var[S_n]}} = \frac{S_n - np}{\sqrt{np(1-p)}} \overset{n \to \infty}{\to} Z
$$

Con quanto appena visto si possono facilmente calcolare probabilità della forma $P(S_n \lesseqgtr s)$. Per l'uguaglianza si considera $P(s - 0.5 \le S_n \le s + 0.5)$.

# Processi di Bernoulli

Un **processo di Bernoulli** $BP(p)$ può essere considerato come una serie di slot ordinati, nei quali un evento ha sempre la stessa probabilità $p$ di accadere.

Il numero $S$ di successi in $n$ slot è distribuito come una $\text{Bin}(n, p)$.

I tempi di interarrivo $T_i$ sono distribuiti come una $\text{Geom}(p)$.

Il tempo $Y_k$ al $k$-esimo arrivo è distribuito come una $\text{Pascal-}k(p)$.

<!--
Si abbia un $BP(p)$; se ad ogni successo, con una probabilità $q$, accade un'altro evento, l'accadere di questo secondo evento è descritto da un $BP(pq)$.

Si abbiano due processi di Bernoulli $BP(p)$ e $BP(q)$; l'accadere di un evento, indipendentemente da quale processo l'ha causato, è a sua volta un $BP(p + q - pq)$.
-->

Lo splitting di un $BP(p)$ che avviene con probabilità $q$ genera un $BP(pq)$.

Il merging di $BP(p)$ con $BP(q)$ genera un $BP(p + q - pq)$.

# Processi di Poisson

I **processi di Poisson** sono l'equivalente tempo-continuo dei processi di Bernoulli: gli eventi possono accadere in un qualsiasi istante. Un processo di Poisson non è descritto dalla probabilità che un evento accada ma da quanti eventi accasono in media per unità di tempo.

I processi di Poisson si indicano con $PP(\lambda)$.

Il numero di eventi $N_{[a, b]}$ in un intervallo di tempo di durata $\tau = b - a$ è distribuito come $\text{Poisson}(\lambda \tau)$. Logicamente, l'intervallo di tempo deve essere espresso nella stessa unità di misura utilizzata nel denominatore di $\lambda$.

I tempi di interarrivo $T_i$ sono distribuiti come $\text{Exp}(\lambda)$.

Il tempo $Y_k$ al $k$-esimo arrivo è distribuito come $\text{Erlang-}k(\lambda)$.

Lo splitting di un $PP(\lambda)$ che avviene con probabilità $\delta$ genera un $PP(\lambda \delta)$.

Il mergind di $PP(\lambda)$ con $PP(\delta)$ genera un $PP(\lambda + \delta)$.

# Stimatori

Si ha una variabile aleatoria $\theta$, misurata con uno strumento di misura impreciso, che legge il valore $X$.

Come stimare il valore reale di $\theta$ conoscendo il valore di $X$? Per la formula di Bayes vale che (ed è analogo per le variabili aleatorie discrete)

$$
f_{\theta|X}(\theta, x) = \frac{f_{X|\theta}(x|\theta) f_\theta(\theta)}{f_X(x)}
$$

## Stimatore MAP

Lo **stimatore MAP** consiste nel selezionare il $\theta$ con maggior probabilità:

$$
\hat \theta_{\text{MAP}}(x) = \underset{\theta}{\argmax} (f_{\theta|X}(\theta|x))
$$

Lo stimatore MAP massimizza la probabilità a posteriori, cioè prende la $\theta$ con maggior probabilità di essere quella vera dopo aver effettuato la lettura.

## Stimatore LMS

Lo **stimatore LMS** tende a minimizzare l'errore quadratico medio.

Siano $c = \hat \theta_{\text{LMS}}(X)$ e $h(c) = E[(\theta - c)^2]$. L'obiettivo è minimizzare l'errore quadratico medio $h(c)$, trovando il valore di $c$ tale per cui

$$
\frac{d}{dc} h(c) = 0
$$

ovvero

$$
\hat \theta_{\text{LMS}}(X) = c = E[\theta]
$$

L'errore quadratico medio corrispondente è $Var[\theta]$. Questo stimatore è quello che ha il minimo errore quadratico medio.

Sia $\tilde \theta = \hat \theta_{\text{LMS}} - \theta$, $E[\tilde \theta | X = x] = 0 \implies [\tilde \theta] = 0 \implies E[\hat \theta_{\text(LMS)}] = E[\theta]$, $E[\tilde \theta \cdot h(X)] = 0$, $Cov[\tilde \theta, \hat \theta_{\text{LMS}}] = 0$.

## Stimatore LIN

Lo **stimatore LIN** consente di trovare una correlazione lineare tra il valore stimato di $\theta$ ed $X$:

$$
\hat \theta_{\text{LIN}}(X) = E[\theta] + \frac{Cov[X, \theta]}{Var[X]}(X - E[X])
$$

da cui deriva che la standardizzazione di $\hat \theta_{\text{LIN}}$ è proporzionale alla standardizzazione di $x$

$$
\frac{\hat \theta_\text{LIN}(X) - E[\theta]}{\sigma_\theta} = \rho[X, \theta] \frac{X - E[X]}{\sigma_X}
$$

In questo caso, l'errore quadratico medio associato è pari a $E\left[(\theta - \hat \theta_\text{LIN}(X))^2\right] = (1 - \rho[X, \theta]^2) Var[\theta]$ da cui segue che se $\rho[X, \theta] = 0$ allora l'errore è pari a $Var[\theta]$ mentre se $\rho[X, \theta] = \pm 1$ allora l'errore quadratico medio è pari a $0$.

## Errore di stima

In generale, per controllare la bontà di una stima, esistono alcune metodologie:

- Si controlla qual è la probabilità di cadere in un intervallo fiduciario, similmente a quanto visto per il [problema del sondaggista](#problema-del-sondaggista)
- Si calcolano l'errore quadratico medio e l'errore relativo di stima e lo si impone minore dell'errore target:
  $$
  MSE_n = E\left[ (M_n - E[M_n])^2 \right] = \frac{E[M_n](1 - E[M_n])}{n} \\
  \sqrt{\frac{MSE_n}{E[M_n]^2}} = \sqrt{\frac{1 - E[M_n]}{E[M_n]}\frac{1}{n}} \lt \varepsilon
  $$

# Generatori di Variabili aleatorie

## Generatori di variabili aleatorie uniformi

E' possibile generare numeri $X \sim \text{U}[0, 1]$ generando una sequenza di bit e interpretandoli come parte a destra della virgola in un numero in base 2.

Per distribuzioni $\text{U}[a, b]$, si generano bit come visto appena sopra e si calcola $a + X(b - a)$.

Se non si ha a disposizione un generatore discreto per generare i bit (o per qualsiasi altro scopo che richieda un generatore discreto), è possibile utilizzare un generatore continuo e dividere i possibili valori in classi, in modo che ogni classe abbia la stessa probabilità di essere estratta ed assegnare ad ogni classe un valore discreto.

E' possibile generalizzare la generazione di campioni di qualsiasi funzione deterministica di una variabile aleatoria uniforme tramite il **metodo della cumulata inversa**: sia $U \sim \text{U}[0, 1]$ e $X = g(U)$, allora

$$
F_X(x) = g^{-1}(x) \implies g(u) = F_X^{-1}(u)
$$

L'efficienza dell'algoritmo è del 100% ma vi è difficoltà nel trattare variabili aleatorie congiunte.

## Metodo acceptance-rejection

Sia $f_X$ la pdf della distribuzione da generare e $m$ un valore tale per cui $m \ge \underset{x}{\max} f_X(x)$.
Si generino dei punti distribuiti uniformemente nel rettangolo $[a, b] \times [0, m]$ dove $[a, b]$ è l'intervallo dove la $f_X$ è non-nulla. Per ciascuno di questi punti generati, si tengono solo quelli che cadono sotto la $f_X$. Le ascisse dei punti mantenuti, sono distribuite come $f_X$.

Per avere un efficienza massima, si vuole avere $m$ minimo possibile.

## Generazione distribuzioni gaussiane a partire da un'uniforme e da una esponenziale

Per generare una gaussiana semplicemente, è possibile scomporla in segno e modulo, per poi generare ciascuna parte indipendentemente: $Z = S \cdot |Z|$.

Per generare $S$:

1. Genero $U \sim [0, 1]$.
2. Se $U \lt \frac{1}{2}$ pongo $S = -1$, altrimenti $S = +1$.

Per generare $|Z|$:

1. Genero $Y \sim \text{Exp}(1)$ e $U' \sim \text{U}[0, 1]$ con $Y \perp U$.
2. Se $U' \cdot m \cdot f_Y(Y) \le f_{|Z|}(Y)$ (cioè se $U' \le \frac{f_{|Z|}(Y)}{mf_{Y}(Y)} = \exp\left(-\frac{(Y-1)^2}{2}\right)$) allora accetto e pongo $|Z| = Y$, altrimenti torno al punto 1.

L'efficienza di questo metodo è sempre pari a $\left(\sqrt{\frac{2e}{\pi}}\right)^{-1} \simeq 76\%$

## Generazione di un vettore di gaussiane

Sia $\underline X = (\underline X_1, \underline X_2, \dots, \underline X_n)$ un vettore di variabili aleatorie gaussiane a con media $\underline \mu = E[X]$.

Si indica con $\Sigma$ la matrice delle covarianze:

$$
\Sigma = \left[Cov[X_i, X_j]\right]_{i, j} = E[(\underline X - \underline \mu)(\underline X - \underline \mu)^T]
$$

Per generare un $X$, si comincia generando un vettore $\underline Z = (\underline Z_1, \underline Z_2 \dots, \underline Z_n)$ con $Z_i \sim \mathcal{N}(0, 1)$. E' evidente che $\underline X = A \cdot \underline Z + \underline b$.

Per imporre il $\underline \mu$ desiderato, si pone $\underline b = \underline \mu$, mentre per imporre la covarianza tra elementi desiderata, si pone $Cov[\underline X, \underline X] = A \cdot A^T = \Sigma$ da cui $A = \text{Cholesky}(\Sigma)$.

# Simulazione Montecarlo

Il metodo Montecarlo serve per stimare il valore atteso di una funzione $g(X)$ di una variabile aleatoria $X$.

Prima di tutto si generano $X_i$ con $i = 1, 2, \dots, n$ in maniera indipendente, poi si calcola

$$
\hat G_n = \frac{1}{n} \sum_{i = 1}^n g(X_i)
$$

Risulta che $\hat G_n$ è il valore stimato di $E[g(X)]$.

La varianza dell'errore di stima si calcola in maniera identica ai precedenti (anche qui $E[\hat G_n] = E[X]$):

$$
E \left[ \left( \frac{1}{n} \sum_{i = 1}^n X_i - E[X] \right)^2 \right] = Var\left[\hat G_n\right] = Var \left[ \frac{1}{n} \sum_{i = 1}^n X_i \right] \\
\frac{\sqrt{Var \left[ \frac{1}{n} \sum\limits_{i = 1}^n X_i \right]}}{E[X]} = \frac{1}{\sqrt{n}} \sqrt{\frac{1 - E[X]}{E[X]}}
$$

# Importance sampling

Utile quando si vuole campionare un evento $A$ raro: in pratica si va a trovare un esperimento equivalente nel quale $A$ è molto più probabile che accada e poi si riscala adegauatamente la probabilità stimata.

$$
P(A) = E\left[\mathbf{1}(X \in A) \right] = \int_{-\infty}^{+\infty} \mathbf{1}(x \in A) f_X(x) \, dx = \int_{-\infty}^{+\infty} \mathbf{1}(x \in A) \frac{f_X(x)}{f_Y(x)} f_Y(x) \, dx = E \left[ \mathbf{1}(Y \in A) \underbrace{\frac{f_X(Y)}{f_Y(Y)}}_{\text{Importance weight}} \right]
$$

La descrizione dell'algoritmo è la seguente: prima di tutto si generano $Y_i$ iid, poi si calcola

$$
\hat P_X(A) = \frac{1}{n} \sum_{i = 1}^n \mathbf{1}(Y_i \in A) \frac{f_X(Y_i)}{f_Y(Y_i)}
$$

Logicamente, si deve scegliere $f_Y$ in modo tale da avere $P_Y(A) \gg P_X(A)$.

La varianza dell'errore di stima è 

$$
\frac{1}{n} \left\{ E \left[ \mathbf{1}(X \in A) \frac{f_X(X)}{f_Y(X)} \right] - P_X(A)^2 \right\}
$$

che diventa $0$ se scelgo

$$
f_Y(x) = \frac{\mathbf{1}(x \in A) f_X(x)}{P_X(A)}
$$

Dunque, per avere una stima precisa, devo scegliere un $f_Y$ il più vicino possibile a quello appena visto.

# Calcolo di integrali

Si vuole calcolare il seguente integrale:

$$
I = \int_a^b f(x) dx
$$

L'algoritmo è il seguente:

1. Si estraggono $X_1, X_2, \dots, X_n \sim \text{U}[a, b]$ iid.
2. Si calcola $\hat I_n = \frac{1}{n} \sum\limits_{i=1}^n f(X_i)$

# Entropia e information theory

L'autoinformazione di un evento $A$ è la funzione $i(A) = \log \frac{1}{P(A)}$. A seconda della base del logaritmo, cambia l'unità di misura:

| Base | Unità           |
| ---- | --------------- |
| $e$  | \[nat\] natural |
| $2$  | \[bit\]         |
| $10$ | \[hartley\]     |

Sia $X$ una variabile aleatoria, allora

$$
i(X = x) = i(x) = \log_2 \frac{1}{P_X(x)}
$$

ed è definita la funzione entropia:

$$
H(X) = E[i(X)] = \sum_{j=1}^n \left[P_X(X_j) \cdot \log_2 \frac{1}{P_X(X_j)}\right]
$$

L'entropia è il numero medio di bit di informazione che ci si può aspettare da una singola esecuzione dell'esperimento aleatorio.

Per convenzione $0 \cdot \log_2 \frac{1}{0} = 0$. Non esistono variabili aleatorie con entropia negativa.

Un codice è detto **prefix-free** se ogni codeword non è prefisso di un'altra codeword. Per la disuguaglianza di Kraft-McMillain, esiste un codebook prefix-free formato da codeword di lunghezza $l_j$ se e solo se $\sum\limits_{j = 1}^{m} 2^{-l_j} \le 1$.

Sia $L$ la lunghezza di una codeword, allora vale che

$$
E[L] = \sum_{j=1}^{m} l_j P_X(x_j) \ge H(X)
$$

# Disuguaglianza di Jensen

La disuguaglianza di Jensen afferma che: siano $\lambda_1, \lambda_2, \dots, \lambda_n$ tali per cui $\sum \lambda_i = 1$ e $\lambda_i \ge 0$ $\forall i$, allora

$$
\sum_{i = 1}^n \lambda_i \log x_i \le \log \left[ \sum_{i=1}^n \lambda_i x_i \right]
$$

da cui segue che 

$$
\begin{cases}
    E[f(Y)] \le f(E[Y]) & \forall f \text{ concava} \\
    E[f(Y)] \ge f(E[Y]) & \forall f \text{ convessa}
\end{cases}
$$

# Tabella riassuntiva distribuzioni variabili aleatorie

| Distribuzione | Costruttore                  | Valore atteso       | Varianza                |
| ------------- | ---------------------------- | ------------------- | ----------------------- |
| Geometrica    | $\text{Geom}(p)$             | $\frac{1}{p}$       | $\frac{1-p}{p^2}$       |
| Binomiale     | $\text{Bin}(n,p)$            | $n \cdot p$         | $n \cdot p \cdot (1-p)$ |
| Bernoulli     | $\text{Bern}(p)$             | $p$                 | $p \cdot (1-p)$         |
| Uniforme      | $\text{U}(a,b)$              | $\frac{a+b}{2}$     | $\frac{(b-a)^2}{12}$    |
| Gaussiana     | $\mathcal{N}(\mu, \sigma^2)$ | $\mu$               | $\sigma^2$              |
| Esponenziale  | $\text{Exp}(\lambda)$        | $\frac{1}{\lambda}$ | $\frac{1}{\lambda^2}$   |
| Poisson       | $\text{Pois}(\lambda)$       | $\lambda$           | $\lambda$               |
| Erlang-$k$    | $\text{Erlang-}k(\lambda)$   | $\frac{k}{\lambda}$ | $\frac{k}{\lambda^2}$   |
| Laplace       | $\text{Laplace}(\lambda)$    | $0$                 | $\frac{2}{\lambda^2}$   |
| Pascal-$k$    | $\text{Pascal-}k(p)$         | $k\frac{1-p}{p}$    | $n\frac{1-p}{p^2}$      |

La pdf delle distribuzioni geometriche e binomiali sono già state [riportate qui](#valore-atteso-e-varianza).

## Distribuzione bernoulliana

$$
p_X(x) = \begin{cases}
    p & x = 1 \\
    1 - p & x = 0
\end{cases}
$$

## Distribuzione uniforme

$$
f_X(x) = \begin{cases}
    \frac{1}{b - a} & a \gt x \gt b \\
    0 & \text{Altrimenti}
\end{cases}
$$

## Distribuzione gaussiana

$$
f_X(x) = \frac{1}{\sqrt{2 \pi} \sigma} e^{-\left(\frac{x - \mu}{\sigma}\right)^2\frac{1}{2}}
$$

$$
F_X(x) = \Phi \left( \frac{x - \mu}{\sigma} \right)
$$

## Distribuzione esponenziale

$$
f_X(x) = \begin{cases}
    \lambda e^{-\lambda x} & x \gt 0 \\
    0 & \text{Altrimenti}
\end{cases} \\
F_X(x) = \begin{cases}
    1 - e^{-\lambda x} & x \gt 0 \\
    0 & \text{Altrimenti}
\end{cases}
$$

## Distribuzione laplaciana

$$
f_X(x) = \frac{\lambda}{2} e^{-\lambda |x|} \\
F_X(x) = \frac{\lambda}{2} \left( \frac{-e^{\lambda x \text{sgn}(x)}}{\lambda \text{sgn}(x)} + \frac{\text{sgn}(x)}{\lambda} \right)
$$

## Distribuzione di Poisson

$$
f_X(x) = \frac{\lambda^n}{n!} e^{-\lambda}\\
F_X(x) = e^{-\lambda} \sum\limits_{j=0}^{\lfloor n \rfloor}{\frac{\lambda^j}{j!}}\\
$$

## Distribuzione di Erlang-k

$$
f_X(x) = \begin{cases}
    \frac{(\lambda x)^{k-1}}{(k-1)!} e^{-\lambda x} \lambda & x\ge 0,k>1\\
    0 & \text{Altrimenti}
\end{cases}\\
F_X(x) = \begin{cases}
    1-\sum\limits_{n=0}^{k-1}{\frac{1}{n!} e^{-\lambda x} (\lambda x)^n} & t\ge 0,k>1\\
    0 & \text{Altrimenti}
\end{cases}\\
$$

## Qualcosa di interessante

Sappiamo come comportarci con i processi di Poisson , se dobbiamo calcolare la probabilità che <b>K</b> arrivi avvengano in un intervallo <b>[0,T]</b> facciamo riferimento alla ddp e siamo apposto.

Ma cosa succederebbe se la T fosse una variabile aleatoria?
La legge degli arrivi smetterebbe di essere di tipo Poisson, a quale legge dobbiamo fare riferimento quindi? E soprattutto come la ricaviamo?

L'esercizio 7 dell'esercitazioni dello Scazzoli ( l'ultima ) propone un metodo utile per gestire intervalli di tipo esponenziale tramite discretizzazione.

Qui vi farò vedere un metodo più generale , universale diciamo.

<b>La situazione è la seguente:</b>
$$P \sim Poisson( \lambda)$$
$$T \sim Exp(v)$$

Devo trovare la ddp degli arrivi nell'intervallo <b>[0,T]</b>
Dal [teorema delle probabilità totali](#probabilita_totali) applicato a v.a. continue sappiamo che:

$$ f_X(y)=\int_{-\infty}^\infty f_{X|Y}(x|y) f_Y(y) dy$$

Quindi per trovare la legge dei k arrivi in t tempo scriveremo:

$$ f_P(p)=\int_{0}^\infty \frac{(\lambda t)^k}{k!} e^{-\lambda t}  *ve^{-vt}dt$$

Facile no? Provate a risolverlo.

Se pensate di non riuscire a risolverlo , tranquilli, significa semplicemente che siete ancora sani di mente.

**NESSUN QUANTITATIVO DI APPLICAZIONI DEL METODO DI INTEGRAZIONE PER PARTI O SOSTITUZIONE VI PERMETTERÁ DI RISOLVERE QUESTO INTEGRALE**

Riscriviamo meglio questo integrale : **tiriamo fuori le costanti:**

$$ f_P(p)=C *\int_{0}^\infty t^k e^{-(\lambda+v) t} dt$$

La situzione non sembra essere molto migliorata , ma la struttura di questo integrale ci permette di ricondurci ad una funzione molto particolare:

## Introduciamo la funzione $ \Gamma(K) $ 
La funzione $\Gamma(K)$ è particolarmente utile :
* è una funzione ricorsiva: $ K* \Gamma(K) =\Gamma(K+1)$.
* la sua applicazione è immediata una volta individuata.

La sua struttura è la seguente:

$$\Gamma(K)=\int_{0}^\infty t^{K-1}*e^{-t} dt $$

**OK, AND?** A cosa ci serve questa informazione? Abbiamo semplicemente dato un nome al nostro problema , l'equivalente di dare un nome al proprio mal di pancia.

Se non fosse che la funzione $\Gamma(K)$ è spesso nota con un altra struttura, questa:
$$\Gamma(K)=(K-1)!$$

con un pò di ritocco tramite sostituzione di $(\lambda+v)t=u $  il nostro integrale diventa:

$$ f_P(p)=C *\frac{1}{(\lambda+v)^{k+1}}*\int_{0}^\infty u^k e^{-u} du$$

che è quindi:

$$ f_P(p)=C *\frac{1}{(\lambda+v)^{k+1}}*\Gamma(k+1)$$
$$ f_P(p)=C *\frac{1}{(\lambda+v)^{k+1}}*k!$$

Sostituiamo la $C$ con il suo valore originale:
$$ f_P(p)= \frac{(\lambda )^k}{k!}*v*\frac{1}{(\lambda+v)^{k+1}}*k!$$
Semplifichiamo il tutto:
$$ f_P(p)= \left( \frac{v}{\lambda+v}\right) \left( \frac{\lambda}{\lambda+v}\right)^k$$

Questa è la legge degli arrivi che stavamo cercando.
Non abbiamo discretizzato né fatto nessun ragionamento sui processi , ma risolto un "**semplice**" integrale.


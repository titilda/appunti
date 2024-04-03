---
title: "Riassunto estremamente sintetico di Informazione e Stima"
author:
- "Andrea Oggioni"
- "Niccolò Papini"
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

Se si vogliono contare i modi di estrarre elementi da insiemi di elementi diversi in un certo numero, si può ricorrere alle probabilità ipergeometriche:

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

Continua a valere la regola di bayes ma con delle caratteristiche nuove:

$$
P_{X|Y}(x, y) = \frac{P_{Y|X}(y | x) \cdot P_X(x)}{P_Y(y)}
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

La **disuguaglianza di Chebyshef** (che è derivata da quella di Markov) afferma che $Var[X] \ge a \cdot P((X - E[X])^2 \ge a)\ \forall a \ge 0$.

# Convergenza in probabilità

Sia $\{A_k\}$ una successione di variabili aleatorie ed $a$ un numero. Si dice che $\{A_k\}$ **converge in probabilità** ad $a$ ($A_k \overset{P}{\to} a$) se 

$$
\lim_{k \to \infty} P(|A_k - a| \le \varepsilon) = 0 \qquad \forall \varepsilon \gt 0
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
o \le \lim_{n \to \infty}{P(|M_n - E[X]| \le \varepsilon)} \le \lim_{n \to \infty} \frac{Var[M_n]}{\varepsilon^2} = \lim_{n \to \infty} \frac{Var[X_1]}{n \varepsilon^2} = 0
$$

allora $M_n \overset{P}{\to} E[M_n] = E[X]$. Questo risultato viene detto **legge debole dei grandi numeri** (**WLLN**) e dice che la media campionaria converge in probabilità al proprio valore atteso.

# Tabella riassuntiva distribuzioni variabili aleatorie

| Distribuzione | Costruttore                  | Valore atteso       | Varianza               |
| ------------- | ---------------------------- | ------------------- | ---------------------- |
| Geometrica    | $\text{Geom}(p)$             | $\frac{1}{p}$       | $\frac{1-p}{p^2}$      |
| Binomiale     | $\text{Bin}(n, p)$           | $np$                | $np(1-p)$              |
| Bernoulli     | $\text{Bern}(p)$             | $p$                 | $p(1-p)$               |
| Uniforme      | $U[a, b]$                    | $\frac{b - a}{2}$   | $\frac{(b - a)^2}{12}$ |
| Gaussiana     | $\mathcal{N}(\mu, \sigma^2)$ | $\mu$               | $\sigma^2$             |
| Esponenziale  | $\text{Exp}[\lambda]$        | $\frac{1}{\lambda}$ | $\frac{1}{\lambda^2}$  |


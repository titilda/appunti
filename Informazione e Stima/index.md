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
P(A_i | B) = \frac{P(B \cap A_i)}{P(B)} = \frac{P(B | A_i) \cdot P(A_i)}{P(B)} = \frac{P(B | A_i) P(A_i)}{\sum_{j=1}^n P(B | A_j) \cdot P(A_j)}
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

<!-- Se si vuole calcolare, similmente alle probabilità binomiali, la probabilità di avere esattamente $k$ successi -->

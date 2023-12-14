---
title: "Riassuntino di Logica e Algebra"
author:
- "Andrea Oggioni"
date: "14 Dicembre 2023"
---

# Relazioni

Dati $N$ insiemi $A_1, A_2, \dots, A_N$, si defisce **relazione N-aria** o **di arità N** tra gli insiemi dati un qualunque sottoinsieme del prodotto cartesiano $A_1 \times A_2 \times \dots \times A_N$.

## Relazioni binarie

Una **relazione binaria** è tale se è una relazione tra solo due insiemi.

Può essere rappresentata con il grafo di adiacenza o con la matrice di adiacenza

Dati $R \sube A_1 \times A_2, T \sube A_1 \times A_2, |A_1| = n, |A_2| = m$, se $M_R$ e $M_T$ sono le matrici di adiacenza delle relazioni allora $R \cap T \to M_{R \cap T} = M_R \cdot M_T$ e $R \cup T \to M_{R \cup T} = M_R + M_T$ dove $+$ e $\cdot$ sono, rispettivamente, somma e prodotto elemento per elemento.

## Prodotto di relazioni

Siano $R \sube A_1 \times A_2$ e $T \sube A_2 \times A_3$ allora $RT = \left\{ (a_1, a_3) \in A_1 \times A_3 | \exist a_2 \in A_2 : (a_1, a_2) \in R \cap (a_2, a_3) \in T \right\}$.

$a_2$ viene detto elemento di passaggio.

Siano $M_R$ e $M_T$ le matrici di adiacenza delle due relazioni $R$ e $T$, allora $M_{RT} = M_R \cdot M_T$ con $\cdot$ prodotto tra matrici.

Il prodotto di possiede alcune proprietà:

1. Il prodotto di relazioni gode della proprietà associativa;
2. Il prodotto di relazioni gode della proprietà commutativa;
3. Siano $R,T \sube A_1 \times A_2$, $S,V \sube A_3 \times A_4$, $R \sube T$ e $S \sube V$ allora $RS \sube TV$.

## Inverso di una relazione

Dato $R \sube A_1 \times A_2$ allora $R^{-1} = \{(b, a) \in A_2 \times A_1 | (a, b) \in A_1 \times A_2\}$.

Esiste la relazione identitaria su un insieme $A$: $I_A = \{(a, a) | a \in A\}$.

Se $R \sube A_1 \times A_2$ allora $I_{A_1} R = R$ e $RI_{A_2} = R$ ma in generale $RR^{-1} \ne I_{A_1}$ e $R^{-1}R \ne I_{A_2}$.

La relazione inversa gode di alcune proprietà (che spesso tornano comode negli esercizi):

1. $(R \cap T)^{-1} = R^{-1} \cap T^{-1}$
2. $(R \cup T)^{-1} = R^{-1} \cup T^{-1}$
3. $R \sube T \implies R^{-1} \sube T^{-1}$
4. $(RT)^{-1} = T^{-1} R^{-1}$
5. $R(T \cap S) = RT \cap RS$
6. $R(T \cup S) = RT \cup RS$

## Potenza di una relazione

Sia $R \sube A \times A$ e $n \gt 0$ allora

$$
R^n = \begin{cases}
    \underbrace{A \times A \times \dots \times A}_{\text{n volte}} & n \gt 0 \\
    I_A & n = 0 \\
    \underbrace{R^{-1} \times R^{-1} \times \dots \times R^{-1}}_{\text{n volte}} & n \lt 0
\end{cases}
$$

Se $n,m \gt 0$ vale che (i) $R^n R^m = R^{n+m}$ e che (ii) $(R^n)^m = R^{nm}$;
se $n \lt 0$ o $m \lt 0$ allora non è detto che valga la prima.

## Proprietà delle relazioni binarie

Sia $R \sube A \times A$.

### Serialità

$R$ è seriale se $\forall a \in A \ \exist \overline a : a R \overline a$.

In pratica, una relazione è seriale se per ogni elemento, ce n'è uno che è un relazione con esso.

- Nel grafo di adiacenza, si nota la serialità se da tutti i vertici esce almeno una freccia.
- Nella matrice di adiacenza, si nota la serialità se c'è almeno un "1" in ogni riga.

### Riflessività

$R$ è riflessiva se $\forall a \in A \ a R a$.

In pratica, una relazione è riflessiva se ogni elemento è in relazione con se stesso.

- Nel grafo di adiacenza, si nota la serialità se da tutti i vertici parte un autoanello.
- Nella matrice di adiacenza, si nota la serialità se la diagonale contiene solamente "1" ($I_A \sube R$).

### Simmetricità

$R$ è simmetrica se $\forall (a, b) \in R \ (b, a) \in R$ (alternativamente $a R b \implies b R a$).

In pratica una relazione è simmetrica se contiene la propria inversa.

- Nel grafo di adiacenza, si nota la simmetricità se tutte le frecce sono o doppie frecce o autoanelli (un autoanello conta come doppia freccia).
- Nella matrice di adiacenza, si nota la simmetricità se la matrice è simmetrica ($R \text{ è simmetrica} \iff R \sube R^{-1} \iff R = R^{-1}$).
  
### Antisimmetricità

$R$ è antisimmetrica se $(a R b) \cap (b R a) \implies a = b$.

- Nel grafo di adiacenza, si nota l'antisimmetricità se le uniche doppie frecce che compaiono sono autoanelli.
- Nella matrice di adiacenza, si nota l'antisimmetricità se $R \cap R^{-1} \sube I_A$.
  
### Transitività

$R$ è transitiva se $(a R b) \cap (b R c) \implies a R c$.

- Nel grafo di adiacenza, si nota la transitività se quando due vertici sono collegati da due frecce passando per un terzo vertice, allora compare anche una freccia che collega i due vertici senza passare dal terzo vertice.
- Nella matrice di adiacenza, si nota la transitività se $R^2 \sube R$.

### Proprietà delle proprietà

Siano $R,T \sube A \times A$ e assumo di volta in volta le proprietà in oggetto.

| Proprieta        | $\forall S \supe R$ | $\forall S \sube R$ | $R \cup T$ | $R \cap T$ | $RT$              |
| ---------------- | ------------------- | ------------------- | ---------- | ---------- | ----------------- |
| Serialità        | SI                  | NO                  | SI         | NO         | SI                |
| Riflessività     | SI                  | NO                  | SI         | SI         | SI                |
| Simmetricità     | NO                  | NO                  | SI         | SI         | Solo se $RT = TR$ |
| Antisimmetricità | NO                  | SI                  | NO         | SI         | NO                |
| Transitività     | NO                  | NO                  | NO         | SI         | Solo se $RT = TR$ |

## Chiusure di relazioni rispetto ad un insieme di proprietà

Siano $R \sube A \times A, P=\{proprietà\}$.

Chiudere una relazione rispetto ad un insieme $P$ significa aggiungere il minor numero di elementi ad una relazione per fare in modo che tale relazione goda delle proprietà in $P$.

Se $S$ è la $P$-chiusura di $R$ allora

1. $S \supe R$
2. $S$ gode delle proprietà in $P$
3. se $T \sube A \times A$, $T \supe R$ e $T$ gode delle proprietà in P allora $S \sube T$

La $P$-chiusura di $R$ su $A$, se esiste è unica.

Affiche la $P$-chiusura di una relazione esista, è necessario che le proprietà in $P$ si conservino per intersezione e che $X = \{ x_i \sube A \times A | x_i \supe R, x_i \text{ goda delle proprietà in } P\} \ne \emptyset$

Se ne deduce che esistono solamente le chiusure riflessive, simmetriche e transitive (le stesse proprietà delle relazioni di equivalenza).

Se esistesse la chiusura antisimmetrica di una relazione, vuol dire che tale relazione è già antisimmetrica e quindi il problema non si pone.

Sia $S$ la chiusura di $R \sube A \times A$ rispetto alle proprietà date di volta in volta, allora

- Per ottenere la chiusura riflessiva si deve fare $S = R \cup I_A$
- Per ottenere la chiusura simmetrica si deve fare $S = R \cup R^{-1}$
- Per ottenere la chiusura transitiva si deve fare $S = \underset{i \gt 0}{\bigcup} R^i$ (iterando fino ad arrivare al fix point)
- Per ottenere la chiusura riflessiva e simmetrica si deve fare $S = R \cup I_A \cup R^{-1}$
- Per ottenere la chiusura riflessiva e transitiva si deve fare $S = \underset{i \ge 0}{\bigcup} R^i$ (iterando fino ad arrivare al fix point)
- Per ottenere la chiusura riflessiva, simmetrica e transitiva si deve fare $S = \underset{i \ge 0}{\bigcup} (R \cup I_A \cup R^{-1})$

## Relazioni di equivalenza

Una relazione è detta **relazione di equivalenza** se gode delle proprietà riflessiva, simmetrica e transitiva.

LA chiusura riflessiva, simmetrica e transitiva di $R$ (che, notare, esiste sempre) è detta **relazione di equivalenza generata da $R$**.

Sia $\rho$ una relazione di equivalenza si $A$ e $a \in A$, allora si definisce **classe di equivalenza di $a$ rispetto a $\rho$** l'insieme $\rho_a = \{ b \in A : b \rho a \}$.

Se uno stesso elemento compare in due classi di equivalenza, allora le due classi sono la stessa classe.

## Partizioni

Sia $A$ un insieme e $B_i \in A$ allora $\{ B_i \}$ è detta **partizione di $A$** se (i) $\underset{i \ in I}{\bigcup} B_i = A$ e (ii) $\forall B_i, B_j, i \ne j, B_i \cap B_j = \emptyset$.

In una partizione non ci sono insiemi vuoti.

## Insieme quoziente

Data una relazione di equivalenza $\rho$ su $A$, si definisce **insieme quoziente di $A$ rispetto a $\rho$** il seguente insieme:

$$
\frac{A}{\rho} = \left\{ \rho_a : a \in A \right\}
$$

L'insieme quoziente contiene tutte le classi di equivalenza di un insieme e costituisce una partizione dell'insieme da cui origina.

## Relazioni d'ordine

Una relazione è detta **relazione d'ordine** se gode delle proprietà riflessiva, antisimmetrica e transitiva.

Se la relazione invece che essere riflessiva è antiriflessiva ($\forall a \in A \ a \cancel R a$) allora viene detta **relazione d'ordine stretto**.

Non è detto che la chiusura d'ordine esista: condizione necessaria affichè la chiusura d'ordine di $R$ esista è che $R$ sia antisimmetrica.

### Massimi, minimi e simili

Siano $A$ un insieme e $\le$ una relazione d'ordine, allora

- $m \in A$ è detto **minimo di $A$** se $\forall a \in A \ m \le a$
- $M \in A$ è detto **massimo di $A$** se $\forall a \in A \ a \le M$
- $m \in A$ è detto **minimale di $A$** se $a \le m, a \in A \implies a = m$
- $M \in A$ è detto **massimale di $A$** se $M \le a, a \in A \implies a = M$

Non è detto che il minimo o il massimo esistano ma, se esistono, allora sono unici.
Neanche i minimali e massimali esistono per forza, però possono non essere unici.

In pratica, i minimali e massimali si un insieme sono quelli che sono "più in alto" o "più in basso" degli altri o sono direttamente non confrontabili.

Sia $B \sube A$ allora 

- $m \in A$ è detto **minorante di $B$** se $m \le b \forall b \in B$
- $M \in A$ è detto **maggiorante di $B$** se $b \le M \forall b \in B$
- Il massimo dei minoranti è detto **estremo inferiore**
- Il minimo dei maggioranti è detto **estremo superiore**

Non è detto che il maggiorante o il minorante esistano.

# Funzioni

Una relazione $f \sube A \times B$ che si può scrivere anche $f : A \to B$ è detta **funzione** se $\forall a \in A \exist ! b\in B : (a, b) \in f$; in tal caso vale che:

- $f(a) = b$
- $f(A) = \{ b \in B | \exist a \in A : b = f(a) \}$
- $f^{-1}(b) = \{ a \in A | f(a) = b \}$

Siano $f : A \to B$ e $G : B \to C$ allora $(f \cdot g)(a) = g(f(a))$.

Notare che la notazione algebrica è opposta a quella analitica.

La matrice di adiacenza di una funzione contiene un "1" in ciascuna riga.

Una funzione $f : A \to B$ è iniettiva se (condizioni equivalenti):

- $\forall b \in B$ ha al più una controimmagine in $A$
- $\forall a_1, a_2 \in A \ f(a_1) = f(a_2) \implies a_1 = a_2$
- $\forall a_1, a_2 \in A \ a_1 \ne a_2 \implies f(a_1) \ne f(a_2)$

Nel grafo di adiacenza, si nota l'iniettività se in ogni elemento di $B$ entra al massimo una freccia.

Nella matrice di adiacenza, si nota l'iniettività se in ogni colonna vi è al massimo un "1".

Se $f$ e $g$ sono iniettive allora $f \cdot g$ è a sua volta iniettiva.

Una funzione $f : A \to B$ è suriettiva se (condizioni equivalenti):

- $\forall b \in B$ ha almeno una controimmagine in $A$
- $f(A) = B$

Nel grafo di adiacenza, si nota la suriettività se in ogni elemento di $B$ arriva almeno una freccia.

Nella matrice di adiacenza, si nota la suriettività se in ogni colonna vi è almeno un "1".

Se $f$ è una funzione qualunque (non necessariamente suriettiva) e $g$ è suriettiva allora $f \cdot g$ è suriettiva.

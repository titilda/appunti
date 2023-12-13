---
title: "Riassuntino di Logica e Algebra"
author:
- "Andrea Oggioni"
date: "13 Dicembre 2023"
---

# Relazioni

Dati $N$ insiemi $A_1, A_2, \dots, A_N$, si defisce _relazione N-aria_ o _di arità N_ tra gli insiemi dati un qualunque sottoinsieme del prodotto cartesiano $A_1 \times A_2 \times \dots \times A_N$.

## Relazioni binarie

Una _relazione binaria_ è tale se è una relazione tra solo due insiemi.

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

---
title: "Riassuntino di Logica e Algebra"
description: "Riassunto del corso di Logica e Algebra"
authors:
  - "Andrea Oggioni"
slug: "logica-e-algebra"
---

# Capitolo Uno: Relazioni

Dati $N$ insiemi $A_1, A_2, \dots, A_N$, si defisce **relazione N-aria** o **di arità N** tra gli insiemi dati un qualunque sottoinsieme del prodotto cartesiano $A_1 \times A_2 \times \dots \times A_N$.

## 1.1 Relazioni binarie

Una **relazione binaria** è tale se è una relazione tra solo due insiemi.

Può essere rappresentata con il grafo di adiacenza o con la matrice di adiacenza

Dati $R \sube A_1 \times A_2, T \sube A_1 \times A_2, |A_1| = n, |A_2| = m$, se $M_R$ e $M_T$ sono le matrici di adiacenza delle relazioni allora $R \cap T \to M_{R \cap T} = M_R \cdot M_T$ e $R \cup T \to M_{R \cup T} = M_R + M_T$ dove $+$ e $\cdot$ sono, rispettivamente, somma e prodotto elemento per elemento.

## 1.2 Prodotto di relazioni

Siano $R \sube A_1 \times A_2$ e $T \sube A_2 \times A_3$ allora $RT = \left\{ (a_1, a_3) \in A_1 \times A_3 | \exist a_2 \in A_2 : (a_1, a_2) \in R \cap (a_2, a_3) \in T \right\}$.

$a_2$ viene detto elemento di passaggio.

Siano $M_R$ e $M_T$ le matrici di adiacenza delle due relazioni $R$ e $T$, allora $M_{RT} = M_R \cdot M_T$ con $\cdot$ prodotto tra matrici.

Il prodotto di possiede alcune proprietà:

1. Il prodotto di relazioni gode della proprietà associativa;
2. Il prodotto di relazioni gode della proprietà commutativa;
3. Siano $R,T \sube A_1 \times A_2$, $S,V \sube A_3 \times A_4$, $R \sube T$ e $S \sube V$ allora $RS \sube TV$.

## 1.3 Inverso di una relazione

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

## 1.4 Potenza di una relazione

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

## 1.5 Proprietà delle relazioni binarie

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

## 1.6 Chiusure di relazioni rispetto ad un insieme di proprietà

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

## 1.7 Relazioni di equivalenza

Una relazione è detta **relazione di equivalenza** se gode delle proprietà riflessiva, simmetrica e transitiva.

LA chiusura riflessiva, simmetrica e transitiva di $R$ (che, notare, esiste sempre) è detta **relazione di equivalenza generata da $R$**.

Sia $\rho$ una relazione di equivalenza si $A$ e $a \in A$, allora si definisce **classe di equivalenza di $a$ rispetto a $\rho$** l'insieme $\rho_a = \{ b \in A : b \rho a \}$.

Se uno stesso elemento compare in due classi di equivalenza, allora le due classi sono la stessa classe.

## 1.8 Partizioni

Sia $A$ un insieme e $B_i \in A$ allora $\{ B_i \}$ è detta **partizione di $A$** se (i) $\underset{i \ in I}{\bigcup} B_i = A$ e (ii) $\forall B_i, B_j, i \ne j, B_i \cap B_j = \emptyset$.

In una partizione non ci sono insiemi vuoti.

## 1.9 Insieme quoziente

Data una relazione di equivalenza $\rho$ su $A$, si definisce **insieme quoziente di $A$ rispetto a $\rho$** il seguente insieme:

$$
\frac{A}{\rho} = \left\{ \rho_a : a \in A \right\}
$$

L'insieme quoziente contiene tutte le classi di equivalenza di un insieme e costituisce una partizione dell'insieme da cui origina.

## 1.10 Relazioni d'ordine

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

# Capitolo Due: Funzioni

Una relazione $f \sube A \times B$ che si può scrivere anche $f : A \to B$ è detta **funzione** se $\forall a \in A \exist ! b\in B : (a, b) \in f$; in tal caso vale che:

- $f(a) = b$
- $f(A) = \{ b \in B | \exist a \in A : b = f(a) \}$
- $f^{-1}(b) = \{ a \in A | f(a) = b \}$

Siano $f : A \to B$ e $G : B \to C$ allora $(f \cdot g)(a) = g(f(a))$.

Notare che la notazione algebrica è opposta a quella analitica.

La matrice di adiacenza di una funzione contiene un "1" in ciascuna riga.

Una funzione $f : A \to B$ è **iniettiva** se (condizioni equivalenti):

- $\forall b \in B$ ha al più una controimmagine in $A$
- $\forall a_1, a_2 \in A \ f(a_1) = f(a_2) \implies a_1 = a_2$
- $\forall a_1, a_2 \in A \ a_1 \ne a_2 \implies f(a_1) \ne f(a_2)$

Nel grafo di adiacenza, si nota l'iniettività se in ogni elemento di $B$ entra al massimo una freccia.

Nella matrice di adiacenza, si nota l'iniettività se in ogni colonna vi è al massimo un "1".

Se $f$ e $g$ sono iniettive allora $f \cdot g$ è a sua volta iniettiva.

Una funzione $f : A \to B$ è **suriettiva** se (condizioni equivalenti):

- $\forall b \in B$ ha almeno una controimmagine in $A$
- $f(A) = B$

Nel grafo di adiacenza, si nota la suriettività se in ogni elemento di $B$ arriva almeno una freccia.

Nella matrice di adiacenza, si nota la suriettività se in ogni colonna vi è almeno un "1".

Se $f$ è una funzione qualunque (non necessariamente suriettiva) e $g$ è suriettiva allora $f \cdot g$ è suriettiva.

Se una funzione è sia iniettiva che suriettiva allora è **binuivoca** o **biiettiva**.

Nel grafo di adiacenza, si nota la biunivocità se da ogni

Nella matrice di adiacenza, si nota la biunivocità se in ogni riga e in ogni colonna compare esattamente un "1". (Il singolo "1" sulle righe verifica la biunivocità mentre il singolo "1" sulle colonne verifica il fatto che la relazione è una funzione).

Se $f$ e $g$ sono biunivoche allora $f \cdot g$ è a sua volta binuivoca.

Se $f \cdot g$ è biunivoca allora $f$ è almeno iniettiva e $g$ è almeno suriettiva.

## 2.1 Funzioni inverse

Sia $f : A \to B$ una funzione, allora si definisce **inversa di f** una funzione $g : B \to A$ tale che $g \cdot f = i_B$ (inversa sinistra) e che $f \cdot g i_A$ (inversa destra).

Se $f$ ammette sia inversa destra che inversa sinistra allora coincidono e l'inversa di $f$ è unica.

Una funzione è iniettiva se e solo se ammette inversa destra mentre è suriettiva se e solo se ammette inversa sinistra.

## 2.2 Teorema di fattorizzazione

Sia $f : A \to B$ una funzione, allora si può definire la relazione $\ker(f)$ come segue:

$$
a \ker(f) b \iff f(a) = f(b) \quad \forall a, b \in A
$$

Tale relazione è una relazione di equivalenza su A.

Definiamo anche la funzione $P_\rho : A \to \frac{A}{\rho}$ chiamata **proiezione canonica** che associa ad ogni elemento la sua classe di equivalenza.

Il **teorema di fattorizzazione** afferma che date $f : A \to B$ e $P_{\ker(f)}$ esiste ed è unica la funzione $g : \frac{A}{\ker(f)} \to B$ tale che $P_{\ker(f)} \cdot g = f$. Inoltre $g$ è iniettiva.

## 2.3 Cardinalità di insiemi

Due insiemi $A$ e $B$ hanno la stessa cardinalità ($|A| = |B|$) se e solo se esiste una funzione biunivoca $f : A \to B$.

_Avere la stessa cardinalità_ è una relazione di equivalenza.

Dati due insiemi $A$ è $B$ allora

- $A$ ha **cardinalità inferiore** a quella di $B$ se esiste una funzione almeno iniettiva $f : A \to B$
- $A$ ha **cardinalità strettamente inferiore** a quella di $B$ se esiste una funzione iniettiva e non suriettiva $f : A \to B$

Le due relazioni precedenti sono relazioni d'ordine nell'insieme di tutti gli insiemi.

Un insieme si dice **finito di cardinalità $n$** se può essere messo in corrispondenza biunivoca con l'insieme $\{ 1, 2, \dots, n \}$.

Un insieme è detto numerabile se può essere messo in corrispondenza biunivoca con $\mathbb{N}$.

Un insieme ha la potenza del continuo se può essere messo in corrispondenza biunivoca con $\mathbb{R}$.

## 2.4 Teorema di Cantor

La cardinalità di un insieme è sempre minore di quella del suo insieme delle parti.

# Capitolo Tre: Logica

La sintassi della logica è composta da

- Un infinito al più numerabile di lettere enunciative: $A, B, \dots$
- Connettivi logici: $\neg, \cap, \cup, \implies, \iff$
- Simboli ausiliari: $(,)$
- Altri simboli: $\top, \bot$

Le formule ben formate sono come segue:

- Ogni lettera enunciativa è una f.b.f.
- Se $A$ è una f.b.f. allora che $\neg A$ lo è
- Se $A$ e $B$ sono f.b.f. allora anche $A \cap B, A \cup B, A \implies B, A \iff B$ lo sono
- Le uniche f.b.f. sono quelle definite ai punti precedenti.

La precedenza tra i vari connettivi logici segue l'ordine $\neg \to \cap \to \cup \to \implies \to \iff$; nel caso di più connettivi uguali in serie, si associa da sinistra verso destra.

Data una f.b.f. le sue sottoformule ($\text{Stfm}$) sono definite come

- Se $A$ è una lettera enunciativa allora $\text{Stfm}(A) = \{A\}$
- Se $A \equiv \neg B$ allora $\text{Stfm}(A) = \{A\} \cup \text{Stfm}(B)$
- Se $A$ è equivalente a uno tra $B \cup C$, $B \cap C$, $B \implies C$, $B \iff C$ allora $\text{Stfm}(A) = \{A\} \cup \text{Stfm}(B) \cup \text{Stfm}(B)$

Un' interpretazione $v$ è una funzione $v : \{ \text{f.b.f.} \} \to \{ 0, 1 \}$ tale che

- $v(\neg A) = 1 - v(A)$
- $v(A \cap B) =\min(v(A), v(B))$
- $v(A \cup B) = \max(v(A), v(B))$
- $v(A \implies B) = \max(1 - v(A), v(B))$
- $v(A \iff B) = \max(1 - v(A), v(B)) = \max(v(A), 1 - v(B))$
- $v(\bot) = 0$
- $v(\top) = 1$

Segue carrellata di definizioni per poter categorizzare le f.b.f.

Una f.b.f. $A$ è detta detta **soddisfacibile** se esiste un interpretazione tale che $v(A) = 1$; $v$ è detto modello di $A$.

Una f.b.f. è **insoddisfacibile** o **contraddizione** se non ammette modelli.

Una f.b.f. è detta **tautologia** se ogni interpretazione è modello, quindi se è sempre vera.

$B$ è detta **conseguenza semantica** di $A$ ($A \models B$) se ogni modello di $A$ è anche modello di $B$.

$A$ e $B$ sono dette **semanticamente equivalenti** ($A \equiv B$) se $A \models B$ e $B \models A$.

Sia $\Gamma$ un insieme di f.b.f., $v$ è modello di $\Gamma$ se è modello di ogni f.b.f. di $\Gamma$.

Un insieme $\Gamma$ di f.b.f. è detto **soddisfacibile** se ammette un modello.

Un insieme $\Gamma$ di f.b.f. è detto **insoddisfacibile** se non ammette modelli.

## 3.1 Teorema di deduzione semantica

Sia $\Gamma$ un insieme di f.b.f. e $A$ e $B$ due f.b.f.

Il teorema di deduzione semantica afferma che $\Gamma \cup \{ B \} \models A$ se e solo se $\Gamma \models B \implies A$.

Se ne deduce che $B \models A$ se e solo se $B \implies A$ è una tautologia.

Il teorema contrario afferma che $\Gamma \models A$ se e solo se $\Gamma \cup \{ \neg A \}$ è insoddisfacibile.

## 3.2 Teorema di compattezza

Un insieme $\Gamma$ di f.b.f. è soddisfacibile se e solo se ogni suo sottoinsieme finito lo è.

Questo teorema è più utile se enunciato al contrario.

Un insieme $\Gamma$ di f.b.f. è insoddisfacibile se esiste un suo sottoinsieme finito che è anch'esso insoddisfacibile.

## 3.3 Equivalenze semantiche

Una f.b.f. è detta in **forma normale congiuntiva** se è scritta come congiunzione di disgiunzioni.

Una f.b.f. è detta in **forma normale disgiuntiva** se è scritta come disgiunzione di congiunzioni.

Qualunque f.b.f. può essere scritta utilizzando solo connettivi logici da un **insieme adeguato di connettivi**, ad esempio $\{ \neg, \implies \}$, $\{ \neg, \cap \}$, $\{ \neg, \cup \}$.

- $A \cup A \equiv A$
- $A \cap A \equiv A$
- $A \cup B \equiv B \cup A$
- $A \cap B \equiv B \cap A$
- $A \cap \neg A \equiv \bot$
- $A \cup \neg A \equiv \top$
- $(A \cup B) \cup C \equiv A \cup (B \cup C)$
- $(A \cap B) \cap C \equiv A \cap (B \cap C)$
- $A \cup (B \cap C) \equiv (A \cup B) \cap (A \cap C)$
- $A \cap (B \cup C) \equiv (A \cap B) \cup (A \cap C)$
- $\neg(\neg A) \equiv A$
- $A \cap (A \cup B) \equiv A$
- $A \cup (A \cap B) \equiv A$
- $A \cap \top \equiv A$
- $A \cup \bot \equiv A$
- $\neg(A \cup B) \equiv \neg A \cap \neg B$
- $\neg(A \cap B) \equiv \neg A \cup \neg B$
- $A \iff B \equiv (A \implies B) \cap (B \implies A) \equiv (\neg A \cup B) \cap (\neg B \cup A)$

# Capitolo Quattro: Teorie formali

Una **teoria formale** è definita da

- un alfabeto
- un insieme di f.b.f.
- un insieme di assiomi eventualmente vuoto
- un insieme di regole d'inferenza (o di riscrittura)

Una **dimostrazione** in una teoria formale $\mathcal H$ è una sequenza finita di f.b.f. di $\mathcal H$ che siano o assiomi o ottenute dalle precedenti tramite regole di inferenza.

Un **teorema** di una teoria formale $\mathcal H$ è l'ultima riga di una dimostrazione in $\mathcal H$.

Una formula $\mathscr A$ si deduce sintatticamente da un insieme $\Gamma$ di f.b.f ($\Gamma \vdash_\mathcal H \mathscr A$) se esiste una dimostrazione di $\mathscr A$ da $\Gamma$ cioè esiste una sequenza finita di righe in cui $\mathscr A$ è l'ultima riga nella quale le f.b.f. sono o assiomi di $\mathcal H$ o f.b.f. dedotte dalle precedenti mediante regole di inferenza di $\mathcal H$ oppure f.b.f. di $\Gamma$.

## 4.1 Teoria $\mathcal L$

- Alfabeto: unione di
  - $\{A, B, \dots\}$ insieme al più numerabile di lettere enunciative
  - $\{\neg, \implies\}$
  - $\{(, )\}$
- f.b.f.:
  - ogni lettera enunciativa è una f.b.f.
  - se $\mathscr A$ è una f.b.f. allora anche $\neg \mathscr A$ lo è, se anche $\mathscr B$ è una f.b.f. allora anche $\mathscr A \implies \mathscr B$ lo è
  - non ci sono altre f.b.f..
- Assiomi:
  - A1: $\mathscr A \implies (\mathscr B \implies \mathscr A)$
  - A2: $(\mathscr A \implies (\mathscr B \implies \mathscr C)) \implies ((\mathscr A \implies \mathscr B) \implies (\mathscr A \implies \mathscr C))$
  - A3: $(\neg \mathscr A \implies \neg \mathscr B) \implies ((\neg \mathscr A \implies \mathscr B) \implies \mathscr A)$
- Regole di inferenza:
  - MP - Modus Ponens: se $\mathscr A$ e $\mathscr A \implies \mathscr B$ allora $\mathscr B$

### Esempio: dimostrazione di $\vdash_\mathcal L \mathscr A \implies \mathscr A$

1.  $\mathscr A \implies ((\mathscr A \implies \mathscr A) \implies \mathscr A) \quad A1$
2.  $\mathscr A \implies (\mathscr A \implies \mathscr A) \quad A1$
3.  $(\mathscr A \implies ((\mathscr A \implies \mathscr A) \implies \mathscr A)) \implies ((\mathscr A \implies (\mathscr A \implies \mathscr A)) \implies (\mathscr A \implies \mathscr A)) \quad A2$
4.  $(\mathscr A \implies (\mathscr A \implies \mathscr A)) \implies (\mathscr A \implies \mathscr A) \quad MP1,3$
5.  $\mathscr A \implies \mathscr A \quad MP2,4$

### Teorema di correttezza e completezza di $\mathcal L$

$\Gamma \vdash_\mathcal L \mathscr A$ se e solo se $\Gamma \models \mathscr A$.

Grazie al teorema di completezza e correttezza, si può affermare che i teoremi di $\mathcal L$ sono tutte e sole le tautologie, per sui per dimostrare che una formula $\mathscr A$ è teorema di $\mathcal L$, invece che fare la dimostrazione, si può costruire la tavola di verità che è molto più veloce.

### Teorema di deduzione sintattica in $\mathcal L$

$\Gamma \cup \{\mathscr B\} \vdash_\mathcal L \mathscr A$ se e solo se $\Gamma \vdash_\mathcal L \mathscr B \implies \mathscr A$.

## 4.2 Teoria della risoluzione $\mathscr R$

Un **letterale** è una lettera enunciativa o la sua negazione.

Una **clausola** è la disgiunzione di zero o più letterali (ad esempio $a \cup B \cup C$ diventa $\{A, B, C\}$).

Una clausola priva di letterali è detta **clausola vuota** $\square$.

Una clausola è **soddisfacibile** se almeno uno dei suoi letterali è soddisfacibile ($\square$ non è mai soddisfacibile in quanto priva di letterali).

Scrivere una formula in notazione a clausole significa scriverla in forma normale congiuntiva utilizzando la notazione a clausole ($(\neg A \cup B) \cap (A \cup C)$ diventa ${\{\neg A, B\} \cup \{A, C\}}$).

- Alfabeto: insieme dei letterali unito a $\{,\}$
- f.b.f.: formule scritte in forma a clausole
- Assiomi: $\emptyset$
- Regole di inferenza: "risolvente"

Siano $C_1$ e $C_2$ due clausole, sia $l \in C_1$ tale che $\neg l \in C_2$. Allora la **risolvente** $R$ è $R = (C_1 \backslash \{l\}) \cup (C_2 \backslash \{\neg l\})$.

Se $R$ è la risolvente di $C_1$ e $C_2$ allora $\{C_1, C_2\}\models R$

Sia $S$ un insieme, allora la risolvente di $S$ è definita come

$$
\text{Ris}(S) = S \cup \left\{ C_{ij} : C_{ij} = (\text{Risolvente di $C_i$ e $C_j$}), C_i, C_j \in S \right\} \\
\text{Ris}^2(S) = \text{Ris}(\text{Ris}(S)) \qquad \text{Ris}^n(S) = \text{Ris}(\text{Ris}^{n-1}(S)) \qquad \text{Ris}^*(S) = \underset{n \gt 0}{\bigcup} \text{Ris}^n(S)
$$

Se $S \vdash_\mathcal R \square$ allora esiste una **risoluzione lineare**, cioè ad ogni passo della risoluzione si usa sempre la clausola ottenuta nel passo precedente.

Una risoluzione è **lineare per input** se è lineare e nella risoluzione vengono usate solo clausole di input.

Un **insieme di clausole di Horn** è un insieme di clausole nel quale ciascuna clausola contiene al più un letterale positivo.

Se $S$ è un insieme di clausole di Horn allora $S \vdash_\mathcal R \square$ se e solo se esiste una risoluzione lineare per input.

### Teorema di correttezza

Se $\Gamma \vdash_\mathcal R \mathscr A$ allora $\Gamma \models \mathscr A$ ma non vale il contrario.

### Teorema di correttezza e completezza per refutazione

$S \vdash_\mathcal R \square$ se e solo se $S$ è insoddisfacibile (infatti $S \models \mathscr A$ se e solo se $S \cup \mathscr A$ è insoddisfacibile se e solo se $S^C \cup \{\neg \mathscr A\}^C \vdash_\mathscr R \square$, ove $^C$ significa "scritto in forma a clausole").

Ne segue che S è insoddisfacibile se e solo se $\square \in \text{Ris}^*(S)$.

# Capitolo Cinque: Logica del primo ordine

La sintassi della logica del primo ordine è composta da

- variabili ($x, y, z, \dots$)
- costanti ($a, b, c, \dots$)
- lettere funzionali ($f_i^n,\ i,n \in \mathbb{N}^0$)
- lettere predicative ($\mathscr{A}_j^m, \ j,m \in \mathbb{N}^0$)
- connettivi e quantificatori ($\{ (, ), \neg, \cap, \cup, \implies, \iff, \forall x, \exists x \}$)

I **termini** sono definiti come segue:

- Ogni costante è un termine.
- Ogni variabile è un termine.
- Se $t_1, \dots, t_n$ sono termini $f_i^n(t_1, \dots, t_n)$ è un termine.
- Null'altro è un termine.

Le **formule atomiche** sono definite come segue:

- Se $t_1, \dots, t_n$ sono termini allora $\mathscr{A}_j^m(t_1, \dots, t_n)$ è una f.a..

Le **formule ben formate** sono definite come segue:

- Una f.a. è una f.b.f.
- Se $\mathscr{A,B}$ sono f.b.f. allora anche $\neg \mathscr{A}, (\forall x) \mathscr{A}, (\exists x) \mathscr{A}, \mathscr{A} \cap \mathscr{B}, \mathscr{A} \cup \mathscr{B}, \mathscr{A} \implies \mathscr{B}, \mathscr{A} \iff \mathscr{B}$ lo sono.
- Null'altro è una f.b.f.

La precedenza tra i vari connettivi e quantificatori sono $\neg \rarr \forall \rarr \exists \rarr \cap \rarr \cup \rarr \implies \rarr \iff$; nel caso di più connettivi uguali in serie, si associa da sinistra verso destra.

$\forall$ e $\exists$ vengono detti **quantificatori**, rispettivamente, **universale** ed **esistenziale**.

Il **campo d'azione di un quantificatore** è la sottoformula al quale è applicato.

Una variabile nel campo di azione di un quantificatore è **vincolata** se appare accanto al quantificatore, altrimenti è **libera**.

Una formula è **chiusa** se tutte le occorrenze di tutte le variabili sono vincolate.

Un **termine $t$ si dice libero per una variabile $x$ in una formula $\mathscr A$** se nessuna occorrenza libera di $x$ in $\mathscr A$ cade nel campo di azione di un quantificatore che quantifica una variabile che compare in $t$.

La **chiusura universale** di una formula $\mathscr A$ si ottiene facendo precedere $\mathscr A$ da quantificatori universali che quantificano tutte le variabili libere di $\mathscr A$. Il procedimento è analogo per la **chiusura esistenziale**.

Un'**operazione** in $D$ di arità $n$ è una funzione $D^n \to D$.

Un'**interpretazione** $\lang D, I \rang$ è costituita da un insieme non vuoto $D$ e da $I = \{I_1, I_2, I_3\}$ con

$$
\begin{align*}
    I_1 &: \{\text{costanti}\} \to D \\
    I_2 &: \{f_i^n\} \to \{\text{operazioni n-arie su $D$}\} \\
    I_3 &: \{\mathscr{A}_j^m\} \to \{\text{relazioni m-arie su $D$}\}
\end{align*}
$$

In pratica, si dà un significato ad ogni lettera funzionale, ad ogni costante e ad ogni formula atomica, poi si procede ad analizzare ciò che si è ottenuto.

Una formula si dice **soddisfacibile** se esiste un'interpretazione che la soddisfa.

Una formula si dice **vera** in una sua interpretazione se ogni assegnamento di variabili in quell'interpretazione la soddisfa.

Una formula si dice **falsa** in una sua interpretazione se ogni assegnamento di variabili in quell'interpretazione non la soddisfa.

Una formula si dice **logicamente valida** se è vera in ogni sua interpretazione.

Una formula si dice **logicamente contraddittoria** se è falsa in ogni sua interpretazione.

La chiusura esistenziale di una formula è vera se e solo se la formula è soddisfacibile.

La chiusura universale di una formula è vera se e solo se la formula è vera.

Una formula si dice in **forma normale prenessa** se tutti i quantificatori compaiono all'inizio della formula stessa.

E' possibile portare una qualsiasi f.b.f. in forma normale prenessa attraverso alcuni lemmi.

## 5.1 Equivalenze semantiche

- $(\forall x)(\forall y) \mathscr A \equiv (\forall y)(\forall x) \mathscr A$
- $(\exists x)(\exists y) \mathscr A \equiv (\exists y)(\exists x) \mathscr A$
- $(\exists x)(\forall y) \mathscr A \equiv (\forall y)(\exists x) \mathscr A$
- $(\forall x) \mathscr A \equiv \neg(\exists x) \neg \mathscr A$
- $(\forall x) \mathscr A \cap (\forall x) \mathscr B \equiv (\forall x)(\mathscr A \cap \mathscr B)$
- $(\forall x) \mathscr A \cup (\forall x) \mathscr B \equiv (\forall x)(\mathscr A \cup \mathscr B)$

## 5.2 Lemmi per le forme normai prenesse

- $(\forall x) \mathscr A(x) \implies \mathscr B \equiv (\exists t)(\mathscr A(t) \implies \mathscr B)$ (se $\mathscr B$ non contiene occorrenze libere di $t$)
- $(\exists x) \mathscr A(x) \implies \mathscr B \equiv (\forall t)(\mathscr A(t) \implies \mathscr B)$ (se $\mathscr B$ non contiene occorrenze libere di $t$)
- $\mathscr A \implies (\forall x) \mathscr B(x) \equiv (\forall t)(\mathscr A \implies \mathscr B(t))$ (se $\mathscr A$ non contiene occorrenze libere di $t$)
- $\mathscr A \implies (\exists x) \mathscr B(x) \equiv (\exists t)(\mathscr A \implies \mathscr B(t))$ (se $\mathscr A$ non contiene occorrenze libere di $t$)

Esistono anche molti altri lemmi che però sono derivabili dai quattro riportati.

Se necessario, nel portare in forma normale prenessa è possibile andare a cambiare nome alle variabili, per evitare che si vadano a quantificare variabili che non erano nel campo d'azione del quantificatore che si vuole spostare.

In pratica, se estraendo un quantificatore si va a vincolare occorrenze di variabili che prima non erano vincolate, è necessario, prima di procedere all'estrazione, cambiare nome alla variabile quantificata e a tutte le occorrenze vincolate.

## 5.3 Forma normale di Skolem

Skolemizzare una formula significa trasformarla in un modo tale per cui si ottiene una formula non equivalente ma che conserva la soddisfacibilità.

Per portare una formula in forma normale di Skolem, è prima necessario portarla in forma normale prenessa, successivamente, se comincia con dei quantificatori esistenziali, li tolgo e sostituisco le variabili da essi quantificate con delle costanti; poi per ogni altro quantificatore esistenziale, lo tolgo e sostituisco la variabile quantificata con una lettera funzionale che prende come argomenti tutte le lettere quantificate dai $\forall$ prima del $\exists$ appena rimosso.

## 5.4 Sostituzioni

Una sostituzione $\sigma$ è una scrittura finita del tipo

$$
\sigma = \left\{ \frac{t_1}{x_1}, \frac{t_2}{x_2}, \dots, \frac{t_n}{x_n} \right\} \qquad i \ne j \implies x_i \ne x_j, \forall i = 1, \dots, n \ x_i \ne t_i
$$

ove $t_i$ è un termine e $x_i$ è una variabile.

Applicare una sostituzione ad una formula significa sostituire ogni occorrenza della variabile $x_i$ con il termine $t_i$.

Sia $\sigma = \left\{ \frac{v_1}{x_1}, \frac{v_2}{x_2}, \dots, \frac{v_n}{x_n} \right\}$ e $\theta = \left\{ \frac{u_1}{y_1}, \frac{u_2}{y_2}, \dots, \frac{u_m}{y_m} \right\}$ allora il **prodotto tra le due sostituzioni** si ottiene dalla scrittura

$$
\sigma \theta = \left\{ \frac{v_1}{x_1}\theta, \frac{v_2}{x_2}\theta, \dots, \frac{v_n}{x_n}\theta \right\}
$$

cancellando tutti i $\frac{u_j}{y_j}$ tali che per qualche $i$ si ha che $x_i = y_j$ e tutti i $\frac{t_k}{x_k}\theta$ tali per cui $t_k \theta = x_k$ (in pratica, vanno eliminate tutte le sostituzioni di $\theta$ che vanno ad operare su una variabile su cui già opera una sostituzione di $\sigma$ ed elimino tutte le sostituzioni di $\sigma \theta$ che mandano una variabile in se stessa).

Una sostituzione $\sigma$ è detta **unificatore** di $E_1, E_2, \dots, E_N$ espressioni se $E_1 \sigma = E_2 \sigma = \dots = E_N \sigma$.

Un unificatore $\sigma$ di $E_1, E_2, \dots, E_N$ è detto **unificatore più generale (m.g.u.)** se ogni altro unificatore di tali espressioni si può ottenere componendo $\sigma$ con qualche sostituzione.

Il m.g.u., a parte ordine e nome delle variabili, è unico.

## 5.5 Risolvente per una logica del primo ordine

Siano $C_1, C_2$ due clausole di un linguaggio del primo ordine.

1. Si applicano alle due clausole due sostituzioni ($\sigma_1$ e $\sigma_2$) per fare in modo che non abbiano variabili in comune
2. Se esistono $\{l_1, l_2, \dots, l_n\}$ letterali di $C_1$ e $\{l_{n+1}, l_{n+2}, \dots, l_{n+r}\}$ letterali di $C_2$ tali che $\{l_1, l_2, \dots, l_{n+r}\}$ sia unificabile, detto $\sigma$ il m.g.u. di tale insieme, allora $R = (C_1 \sigma_1 \backslash \{l_1, l_2, \dots, l_n\}) \sigma \cup (C_2 \sigma_2 \backslash \{l_{n+1}, l_{n+2}, \dots, l_{n+r}\})$

In pratica, è come la risoluzione vista [precedentemente](#teoria-della-risoluzione) ma senza tenere conto del nome diverso delle variabili.

## 5.6 Teorema di risoluzione

$S \vdash_\mathcal R \square$ se e solo se $S$ è insoddisfacibil; $S \models \mathscr A$ se e solo se $S \cup \{\neg \mathscr A\}$ è insoddisfacibile.

## 5.7 Teoria formale $\mathcal K$

La teoria formale $\mathcal K$ è l'equivalente della teoria $\mathcal L$ ma per la logica del primo ordine.

- Alfabeto: $\{\text{Formule atomiche}\} \cup \{\neg, \implies, \forall, (, )\}$
- f.b.f.:
  - ogni formula atomica è una f.b.f.
  - se $\mathscr A$ è una f.b.f. allora anche $\neg \mathscr A, (\forall x) \mathscr A$ lo sono
  - se $\mathscr A, \mathscr B$ sono f.b.f. allora anche $\mathscr A \implies \mathscr B$ lo è
- Assiomi logici:
  - A1, A2, A3
  - A4: $(\forall x)(\mathscr A(x)) \implies \mathscr A(t)$, $t$ è un termine libero per $x$ in $\mathscr A(t)$
  - A5: $(\forall x)(\mathscr A \implies \mathscr B) \implies (\mathscr A \implies (\forall x) \mathscr B)$, $\mathscr A$ non contiene occorrenze libere di $x$
- Regole di inferenza
  - Modus Ponens
  - Generalizzazione - Se $\mathscr A$ allora $(\forall x) \mathscr A$

Una teoria del primo ordine priva di assiomi propri è detta **calcolo predicativo del primo ordine**.

Una teoria formale è detta **consistente** se non esiste una f.b.f. $\mathscr A$ tale che $\vdash \mathscr A$ e $\vdash \neg \mathscr A$ (in ogni teoria non consistente, ogni f.b.f. è un teorema).

I teoremi di $\mathcal K$ sono tutte e sole le f.b.f.

Un modello di una teoria del primo ordine $\mathcal K$ è un interpretazione nella quale tutti gli assiomi propri sono veri.

### Teorema di correttezza e completezza per una teoria del primo ordine $\mathcal K$

I teoremi di $\mathcal K$ sono tutte e sole le f.b.f. vere in ogni modello di $\mathcal K$

### Teorema di deduzione sintattica

$\Gamma \mathscr A \vdash \mathscr B$ se e solo se $\Gamma \vdash \mathscr A \implies \mathscr B$ se nessuna applicazione della generalizzazione è stata fatta su f.b.f. che dipendono da $\mathscr A$ quantificando variabili libere in $\mathscr A$.

Se vengono usate solamente formule chiuse, non vi sono variabili libere e quindi il teorema di deduzione sintattica vale sempre.

# Capitolo Sei: Strutture algebriche

Una **struttura algebrica** è una coppia $\lang A, \Omega \rang$ con $A$ un insieme qualsiasi e $\Omega$ un insieme di operazioni interne su $A$ e n-arie

## 6.1 Proprietà delle operazioni

Sia $\lang A, \star \rang$

Le operazioni di $\Omega$ possono avere alcune proprietà

1. P. commutativa: $\forall a,b \in A \quad a \star b = b \star a$
2. P. associativa: $\forall a,b,c \in A \quad (a \star b) \star c = a \star (b \star c)$
3. Elemento neutro:
   $$
   \begin{align*}
    e \in A :& \quad \forall a \in A \quad a \star e = e \star a = a \\
    e_s \in A :& \quad \forall a \in A \quad e_s \star a = a \\
    e_d \in A :& \quad \forall a \in A \quad a \star e_d = a
   \end{align*}
   $$
   Gli inversi destro e sinistro, se esistono entrambi, allora corrispondono.
4. Zero di $A$ rispetto a $\star$:
   $$
   \begin{align*}
    z \in A :& \quad \forall a \in A \quad a \star z = z \star a = z \\
    z_s \in A :& \quad \forall a \in A \quad z_s \star a = z_s \\
    z_d \in A :& \quad \forall a \in A \quad a \star z_d = z_d
   \end{align*}
   $$
5. Inverso di $a \in A$ rispetto a $\star$: $e,a \in A$, $b \in A$ è inverso di $a$ se $a \star b = b \star a = e$
6. Esponenziazione: sia $\star$ associativa, allora
   $$
   a^n = \begin{cases}
    \underbrace{a \star a \star \dots \star a}_{n \text{ volte}} & n \gt 0 \\
    \underbrace{b \star b \star \dots \star b}_{n \text{ volte}} & n \lt 0, b \text{ inverso di } a
   \end{cases}
   $$
7. Sia $\star$ associativa, $e \in A$, $a \in A$ abbia inverso, allora le equazioni $a \star x = c$ e $x \star a = c$ hanno una e una sola soluzione che si ottiene componendo a sinistra e a destra con gli inversi destro e sinistro.
8. Sia $\star$ associativa, $e \in A$, $a \in A$ abbia inverso allora
   $$
   a \star c = a \star b \implies c = b \\
   b \star a = c \star a \implies b = c
   $$
9. Sia $\star$ associativa, $e \in A$, $a,b \in A$ abbiano inversi, rispettivamente, $a^{-1}$ e $b^{-1}$, allora $(a \star b)^{-1} = b^{-1} \star a^{-1}$.

## 6.2 Lista di strutture algebriche

Sia $\lang A, \cdot \rang$ una struttura algebrica, allora

- Un **semigruppo** è un insieme $A$ dotato di un operazione binaria associativa.
- Un **monoide** è un semigruppo nel quale esiste l'elemento neutro.
- Un **gruppo** è un monoide nel quale ogni elemento ammette inverso.
- Un **gruppo abeliano** è un gruppo dotato di operazione commutativa.

Notare come $\{\text{Semigruppi}\} \sub \{\text{Monoidi}\} \sub \{\text{Gruppi}\} \sub \{\text{Gruppi abeliani}\}$.

Sia $\lang A, +, \cdot \rang$ una struttura algebrica, allora

- $\lang A, +, \cdot \rang$ è detto **anello** se
  1. $\lang A, + \rang$ è un gruppo abeliano
  2. $\lang A, \cdot \rang$ è un semigruppo
  3. vale la proprietà distributiva dell'operazione $\cdot$ rispetto all'operazione $+$, ovvero $a \cdot (b + c) = (a \cdot b) + (a \cdot c)$ e $(a + b) \cdot c = (a \cdot c) + (b \cdot c)$
- Un **corpo** è un anello $\lang A, +, \cdot \rang$ nel quale $\lang A \backslash \{0\}, \cdot \rang$ è un gruppo.
- Un **campo** è un corpo in cui vale la proprietà commutativa dell'operazione $\cdot$

$\lang A, \cap, \cup \rang$ si dice **reticolo** se

1. $a \cap b = b \cap a, a \cup b = b \cup a \quad \forall a,b \in A$
2. $a \cap (b \cap c) = (a \cap b) \cap c, a \cup (b \cup c) = (a \cup b) \cup c \quad \forall a,b,c \in A$
3. $a \cup (a \cap b) = a, a \cap (a \cup b) = a \quad a,b \in A$

Notare come $\{\text{Anelli}\} \sub \{\text{Corpi}\} \sub \{\text{Campi}\} \sub \{\text{Reticoli}\}$.

## 6.3 Teorema di risoluzione dei postulati

Sia $\lang A, \cdot \rang$ una struttura algebrica, $\cdot$ associativa, allora sono equivalenti le seguenti affermazioni

1. $\lang A, \cdot \rang$ è un gruppo.
2. $\exists e \in A : \forall a \in A \ e \cdot a = a$ e $\forall a \in A \ \exists b \in A : b \cdot a = e$.
3. Ogni equazione del tipo $a \cdot x = b$ e $x \cdot a = b$ ha una e una sola soluzione.

## 6.4 Proprietà degli anelli

Sia $\lang A, +, \cdot \rang$ un anello e $a - b = a + (-b)$, allora valgono le seguenti

- $\forall a \in A \quad a \cdot 0 = 0 \cdot a = 0$
- $\forall a,b \in A \quad a \cdot (-b) = (-a) \cdot b = -(ab)$
- $\forall a,b \in A, \forall n \in \mathbb{Z} \quad a \cdot (n \cdot b) = (n \cdot a) \cdot b = n \cdot (a \cdot b)$
- $\forall a,b,c \in A \quad (a - b) \cdot c = (a \cdot c) - (b \cdot c), a \cdot (b - c) = (a \cdot b) - (a \cdot c)$

Se $\lang A, +, \cdot \rang$ è un anello in cui $\cdot$ è commutativa, allora $\lang A, +, \cdot \rang$ è detto **anello commutativo**.

Se $\lang A, +, \cdot \rang$ è un anello in cui esiste l'elemento neutro rispetto a $\cdot$, allora $\lang A, +, \cdot \rang$ è detto **anello con unità**.

Sia $\lang A, +, \cdot \rang$ un anello. Due elementi $a,b \in A$ si dicono **divisori dello zero** di $A$ se $a \ne 0, b \ne 0$ ma $a \cdot b = 0$.

I divisori dello zero di $A$ sono tutti gli elementi non invertibili.

Un anello è privo di divisori dello zero se e solo se valgono le leggi di cancellazione.

Un anello commutativo privo di divisori dello zero è detto **dominio d'integrità**.

## 6.5 Sottostrutture algebriche

Sia $\lang A, \Omega \rang$ una struttura algebrica. $\lang H \sube A, \Omega \rang$ è detto **sottostruttura algebrica** di $\lang A, \Omega \rang$ se è una struttura algebrica dello stesso tipo di $\lang A, \Omega \rang$ rispetto alle operazioni di $\Omega$.

Sia $\lang A, \cdot \rang$ un semigruppo. $H \sube A, H \ne \emptyset$ è **sottosemigruppo** se $\forall a,b \in H \ a \cdot b \in H$.

Sia $\lang A, \cdot \rang$ un monoide. $H \sube A, H \ne \emptyset$ è **sottomonoide** se $\forall a,b \in H \ a \cdot b \in H$ e $e \in H$.

Sia $\lang A, \cdot \rang$ un gruppo. $H \sube A, H \ne \emptyset$ è **sottogruppo** se $\forall a,b \in H \ a \cdot b \in H$, $e \in H$ e $\forall a \in H \ a^{-1} \in H$.

Sia $\lang A, +, \cdot \rang$ un anello. $H \sube A, H \ne \emptyset$ è **sottoanello** se $\forall a,b \in H \ a + b \in H, \forall a,b \in H \ a \cdot b \in H, \forall a \in H \ -a \in H$.

Sia $\lang A, +, \cdot \rang$ un corpo. $H \sube A, H \ne \emptyset$ è **sottocorpo** se $\forall a,b \in H \ a + b \in H$, $0 \in H$, $\forall a \in H \ -a \in H$, $\forall a,b \in H_0 \ a + b \in H$, $1 \in H$ e $\forall a \in H_0 \ a^{-1} \in H_0$.

Sia $\lang A, \cap, \cup \rang$ un reticolo. $H \sube A, H \ne \emptyset$ è **sottoreticolo** se $\forall a, b \in H \ a \cap b \in H, \forall a,b \in H \ a \cup b \in H$.

### Criteri

Esistono dei criteri per semplificare la ricerca di alcune sottostrutture algebriche.

Per dimostrare che un insieme è una certa struttura algebrica, a volte, può risultare più semplice dimostrare che è sottostruttura di qualcosa d'altro attracerso i criteri invece che dimostrare direttamente che è una struttura.

$\lang H, \cdot \rang$ è sottogruppo se (condizioni equivalenti):

1. $\forall a,b \in H \ a \cdot b \in H$, $\forall a \in A \ a^{-1} \in H$
2. $\forall a,b \in H \ a \cdot b^{-1} \in H$
3. $\forall a,b \in H \ a \cdot b \in H$

$\lang H, +, \cdot \rang$ è sottoanello se $\forall a,b \in H \ a - b \in H$ e $\forall a,b \in H \ a \cdot b \in H$.

$\lang H, +, \cdot \rang$ è sottocorpo se $\forall a,b \in H \ a - b \in H$, $\forall a,b \in H_0 \ a \cdot b^{-1} \in H$.

## 6.6 Relazioni di equivalenza tra strutture algebriche

Dati un insieme $A$, una relazione di equivalenza $\rho$ su $A$ ed un'operazione $\omega$ $n$-aria su A, si dice che **$\rho$ è compatibile con $\omega$** se $\forall a_1, a_2, \dots, a_n, b_1, b_2, \dots, b_n \in A$ vale che da $\forall i \ a_i \rho b_i$ segue che $\omega(a_1, a_2, \dots, a_n) \rho \omega(b_1, b_2, \dots, b_n)$.

In pratica, $\rho$ è compatibile con $\omega$ se, cambiando gli oggetti con cui si lavora con altri nelle stesse classi di equivalenza, il risultato non cambia.

Sia $\lang A, \Omega \rang$ una struttura algebrica e $\rho$ una relazione di equivalenza su $A$. Si dice che $\rho$ è una **congruenza** su $A$ se è compatibile con tutte le operazioni in $\Omega$.

Sia $A$ un insieme, $\omega$ un'operazione n-aria su $A$ e $\rho$ una relazione di equivalenza su $A$ compatibile con $\omega$. Allora, sull'insieme quoziente $\frac{A}{\rho}$ è possibile definire una nuova **operazione indotta** da $\omega$ detta $\omega'$ nel seguente modo:

$$
\omega' : \frac{A}{\rho} \times \frac{A}{\rho} \times \dots \times \frac{A}{\rho} \to \frac{A}{\rho} \\
\omega' (\rho_{a_1}, \rho_{a_2}, \dots, \rho_{a_n}) = \rho_{\omega(a_1, a_2, \dots, a_n)}
$$

Data una struttura algebrica $\lang A, \Omega \rang$ ed una congruenza $\rho$ su $A$, è possibile definire sull'insieme $\frac{A}{\rho}$ una **struttura quoziente** nella quale l'insieme delle operazioni indotte $\Omega'$ è costituito dalle operazioni indotte dalle $\omega \in \Omega$.

Date due strutture algebriche $\lang A, \Omega \rang$ e $\lang A', \Omega' \rang$, esse si dicono **simili** se esiste una congruenza biunivoca $\tau$ tale che $\omega \in \Omega$ e $\tau(\omega) \in \Omega'$ abbiano la stessa arità.

Date due strutture algebriche $\lang A, \Omega \rang$ e $\lang A', \Omega' \rang$ simili, una funzione $f : A \to A'$ si dice **morfismo** o **omomorfismo** se $\forall \omega \in \Omega \ \forall a_1, a_2, \dots, a_n \in A$ si ha che $f(\Omega(a_1, a_2, \dots, a_n)) = \omega'(f(a_1), f(a_2), \dots, f(a_n))$.

Se un morfismo è iniettivo, è detto **monomorfismo**.

Se un morfismo è suriettivo, è detto **epimorfismo**.

Se un morfismo è biiettivo, è detto **isomorfismo**.

Di seguito alcune specifiche per morfismi tra strutture algebriche:

- Un morfismo tra monoidi deve conservare l'elemento neutro.
- Un morfismo tra un gruppo e se stesso, per essere tale, deve operare su un gruppo abeliano.
- $f : \lang A, +, \cdot \rang \to \lang A, \oplus, \odot \rang$ è isomorfismo di anelli se valgono le condizioni
  - $f(a_1 + a_2) = f(a_1) \oplus f(a_2) \quad \forall a_1,a_2 \in A$
  - $f(a_1 \cdot a_2) = f(a_1) \odot f(a_2) \quad \forall a_1,a_2 \in A$

## 6.7 Laterali e ideali

Sia $\lang A, \cdot \rang$ un gruppo, $H \sube A$ un suo sottogruppo e $a \in A$. Sono detti **laterale destro** di $H$ in $A$ l'insieme $Ha = \{h \cdot a : h \in H\}$ e **laterale sinistro** di $H$ in $A$ l'insieme $aH = \{a \cdot h : h \in H\}$.

$\{Ha\}_{a \in A}$ è una partizione di $A$ (vale anche per i laterali sinistri).

La relazione di equivalenza che ha come partizione $\{Ha\}_{a \in A}$ è la relazione $\rho$ definita come $a \rho b \iff a \cdot b^{-1} \in H$.

Sia $\lang A, \cdot \rang$ un gruppo finito di ordine $n$ ($|A| = n$), se $H$ è sottogruppo di $A$ allora $|H|$ divide $|A|$.

Sia $\lang A, \cdot \rang$ un gruppo, un sottogruppo $H \sube A$ è detto **normale** in $A$ ($H \triangleleft A$) se $\forall a \in A \ \forall h \in H \ a^{-1} h a \in H$. $a^{-1}ha$ si dice trasferimento di $h$ mediante $a$.

Se $\lang A, \cdot \rang$ è gruppo abeliano, tutti i sottogruppi sono normali, infatti $a^{-1}ha = a^{-1}ah = h \in H$.

Sia $\lang A, \cdot \rang$ un gruppo e $H$ un suo sottogruppo, allora $\forall a \in H \ aH = Ha \iff H \triangleleft A$ e se $\frac{|A|}{|H|} = 2$ allora $H \triangleleft A$.

Sia $\lang A, \cdot \rang$ un gruppo e $\rho$ una congruenza su $A$, allora

1. $\rho_e \sube A$ è un sottogruppo
2. $\rho_e \triangleleft A$
3. Sia $\rho_e = N$; le classi di equivalenza di $\rho$ in $A$ sono tutti e soli i laterali di $N$ in $A$.

Gli ideali sono l'analogo dei laterali ma per gli anelli.

Sia $\lang A, +, \cdot \rang$ un anello; un sottoanello $I$ di $A$ è detto **ideale** di $A$ se $\forall a \in A \ \forall i \in I \ a \cdot i \in I, i \cdot a \in I$. Esistono le variazioni destra e sinistra.

Notare che per dimostrare che un sottoinsieme $I$ è un ideale, serve dimostrare anche che è sottoanello.

Sia $\lang A, +, \cdot \rang$ un anello e $\rho$ una congruenza su $A$, allora

1. $\rho_a$ è sottoanello di $A$
2. $\rho_a$ è un ideale di $A$
3. Sia $\rho_a = I$; le classi di equivalenza di $\rho$ in $A$ sono tutti e soli i laterali di $I$ in $A$.

Vale che $I + a = I + b \iff a - b \in I$.

Le operazioni indotte sono

- $(I + a) + (I + b) = I + (a + b)$
- $(I + a) \cdot (I + b) = I + (a \cdot b)$

# Capitolo Sette: Equazioni in $\mathbb{Z}_n$

Sia $\lang \mathbb{Z}_n, \oplus, \cdot \rang$ un anello. Si vogliono risolvere equazioni del tipo $[a]x = [b]$ in $\mathbb{Z}_n$.

Se $\text{MCD}(a, n) = 1$ allora l'equazione ha una e una sola soluzione, in caso contrario ne ha zero oppure molteplici.

Nel primo caso, moltiplico entrambi i lati dell'equazione per $[a]^{-1}$ e ho finito.

Nel secondo caso, se possibile, cerco una soluzione $x_0$ a tentativi (non è detto che esista) e poi so che tutte le altre soluzioni sono della forma $x = x_0 + \frac{n}{\text{MCD}(a, n)} \cdot k$ con $k \in \mathbb{Z}$.s

# Capitolo Otto: Teorie del primo ordine con identità

Le **teorie del primo ordine con identità** sono teorie del primo ordine con una lettera predicativa $\mathscr A_1^2$ a cui vengono aggiunti, oltre ad A1..A5, gli assiomi propri A6 e A7.

A6: $\forall x \ \mathscr A_1^2(x, x)$
A7: $\mathscr A_1^2 (x, y) \implies (\mathscr A_1^2(x, x) \implies \mathscr A_1^2 (x, \frac{y}{x}))$

E' possibile dimostrare che A6 e A7 sono equivalenti ai seguenti:

1. $\mathscr A_1^2(x, x)$
2. $\mathscr A_1^2(x, y) \implies \mathscr A_1^2(y, x)$
3. $\mathscr A_1^2(x, y) \implies (\mathscr A_1^2(y, z) \implies \mathscr A_1^2(x, y))$

Quando si va ad interpretare $\mathscr A_1^2$, questa risulta essere una congruenza.

Con la definizione di $\mathscr A_1^2$ è possibile definire delle teorie basate sulle strutture algebriche viste precedentemente.

| Teoria                      | Alfabeto                                   | Assiomi                   | Note                                                      |
| --------------------------- | ------------------------------------------ | ------------------------- | --------------------------------------------------------- |
| Teoria dei semigruppi       | $x, y, z, f_1^2, \mathscr A_1^2$           | A1..A7, A8                | $F_1^2$ è l'operazione binaria, A8 impone l'associatività |
| Teoria dei monoidi          | $x, y, z, f_1^2, \mathscr A_1^2$           | A1..A7, A8, A9            | L'assioma A9 è da leggere come "esiste l'elemento neutro" |
| Teoria dei monoidi alt.     | $x, y, z, f_1^2, \mathscr A_1^2, e$        | A1..A7, A8, A9'           | $e$ è l'elemento neutro                                   |
| Teoria dei gruppi           | $x, y, z, f_1^2, \mathscr A_1^2$           | A1..A7, A8, A10           |                                                           |
| Teoria dei gruppi alt.      | $x, y, z, f_1^2, \mathscr A_1^2, e$        | A1..A7, A8, A9', A10'     |                                                           |
| Teoria dei gruppi alt. alt. | $x, y, z, f_1^2, \mathscr A_1^2, e, f_1^1$ | A1..A7, A8, A11           | $f_1^1$ è l'inverso                                       |
| Teoria degli anelli         | $x, y, z, f_1^2, \mathscr A_1^2, f_2^2$    | A1..A7, A8, A10, A12, A13 |                                                           |

Partendo da qui, si può arrivare anche ai corpi (aggiungendo l'esistenza dell'elemento neutro rispetto a $f_2^2$ e l'esistenza dell'inverso di ogni elemento diverso da $e$ rispetto alla stessa operazione) e ai campi (aggiungendo, oltre alle caratteristiche dei corpi, anche la commutatività di $f_2^2$).

Per il significato dei vari assiomi, vedere la [tabella riassuntiva degli assiomi](#assiomi).

# Capitolo Extra: Tabelle riassuntive

## Assiomi

| Numero | Assioma                                                                                                                                                                                  | Note                                               |
| ------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------- |
| A1     | $\mathscr A \implies (\mathscr B \implies \mathscr A)$                                                                                                                                   |                                                    |
| A2     | $(\mathscr A \implies (\mathscr B \implies \mathscr C)) \implies ((\mathscr A \implies \mathscr B) \implies (\mathscr A \implies \mathscr C))$                                           |                                                    |
| A3     | $(\neg \implies \mathscr A \implies \neg \mathscr B) \implies ((\neg \mathscr A \implies \mathscr B) \implies \mathscr A)$                                                               |                                                    |
| A4     | $(\forall x)(\mathscr A(x)) \implies \mathscr A(t)$                                                                                                                                      | $t$ è un termine libero per $x$ in $\mathscr A(t)$ |
| A5     | $(\forall x)(\mathscr A \implies \mathscr B) \implies (\mathscr A \implies (\forall x) \mathscr B)$                                                                                      | $\mathscr A$ non contiene occorrenze libere di $x$ |
| A6     | $\forall x \ \mathscr A_1^2(x, x)$                                                                                                                                                       |                                                    |
| A7     | $\mathscr A_1^2 (x, y) \implies (\mathscr A_1^2(x, x) \implies \mathscr A_1^2 (x, \frac{y}{x}))$                                                                                         |                                                    |
| A8     | $\forall x \forall y \forall z \ \mathscr A_1^2(f_1^2(f_1^2(x, y), z), f_1^2(x, f_a^2(y, z)))$                                                                                           |                                                    |
| A9     | $\exists x \forall y (\mathscr A_1^2(f_1^2(x, y), y) \cap \mathscr A_1^2(f_1^2(y, x), y))$                                                                                               |                                                    |
| A9'    | $\forall x(\mathscr A_1^2(f_1^2(e, x), x) \cap \mathscr A_1^2(f_1^2(x, e), x))$                                                                                                          |                                                    |
| A10    | $\exists x(\forall y \mathscr A_1^2(f_1^2(x, y), y) \cap \mathscr A_1^2(f_1^2(y, x), y) \cap \forall y \exists z (\mathscr A_1^2(f_1^2(y, z), x) \cap \mathscr A_1^2 (f_1^2(z, y), x)))$ |                                                    |
| A10'   | $\forall x \exists y (\mathscr A_1^2(f_1^2(x, y), e) \cap \mathscr A_1^2(f_1^2(y, x), e))$                                                                                               |                                                    |
| A11    | $\forall x (\mathscr A_1^2(f_1^2(x, e), x) \cap \mathscr A_1^2(f_1^2(e, x), x) \cap \mathscr A_1^2(f_1^2(x, f_1^1(x)), e) \cap \mathscr A_1^2(f_1^2(f_1^1(x), x), e))$                   |                                                    |
| A12    | $\forall x \forall y (\mathscr A_1^2(f_1^2(x, y), f_1^2(y, x)))$                                                                                                                         |                                                    |
| A13    | $\forall x \forall y \forall z (\mathscr A_1^2(f_2^2(f_2^2(x, y), z), f_2^2(x, f_2^2(y, z))))$                                                                                           |                                                    |

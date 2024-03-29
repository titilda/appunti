---
title: "Algoritmi e Principi dell'informatica (API)"
author: 
- "Niccolò Papini"
- "Andrea Oggioni"
---
# Definizioni iniziali

## Lunghezza di una stringa

La **lunghezza** di una stringa è il numero di simboli contenuti in una stringa (anche detta 
cardinalità e si indica con $|x|$).

La stringa **vuota** è la stringa che ha zero elementi ($|\varepsilon| = 0$).

Due stringhe sono **uguali** se e solo se:

- $|x| = |y|$
- $x_i = y_i \quad \forall 1 \le i \le n$

## Operazioni sui linguaggi

- Unione
- Intersezione
- Complemento
- Differenza
- Concatenazione
- Potenze n-esime
- Chiusura di Kleene

Prima di iniziare delle spiegazioni useremo le stringhe $L_1$ e $L_2$ così composte:

$$
L_1 = \{ \varepsilon, a, b, c, bc, ca \} \\
L_2 = \{ ba, bb, bc, ca, cb, cc \}
$$

Bene partiamo con la carrellata di operazioni.

### Unione ($L_1 \bigcup L_2$)

$L_1 \bigcup L_2 = \{ \varepsilon, a, b, c, ba, bb, bc, ca, cb, cc\}$ tutti gli elementi **SENZA** ripetizioni.

### Intersezione ($L_1 \bigcap L_2$)

$L_1 \bigcap L_2 = \{bc, ca\}$ **SOLO** elementi comuni.

### Differenza ($L_1 \setminus L_2$ o $L_1 - L_2$)

$L_1 \setminus L_2 = \{ba, bb, cb, cc\}$ **SOLO** elementi **NON** comuni.

### Complemento ($L^c = A^* \setminus L$)

$A$ è l'alfabeto su cui $L$ è definito.

$*$ vuol dire tutte le ripetizioni possibili di quell'insieme, $\varepsilon$ compreso.

$L_1^c =$ tutte le stringhe su $\{a, b, c\}*$ tranne stringhe di lunghezza 2 che iniziano con "b", "c".

### Concatenazione ($L_1 \cdot L_2$ o $L_1 L_2$) **NON** è commutativa

$$
L_1 \cdot L_2 = \{ba, bb, bc, ca, cb, cc, aba, abb, abc, aca, acb, acc, bba, bbb, bbc, bca, bcb, bcc, cba, cbb, cbc, cca, ccb, ccc, bcba, bcbb, bcbc, bcca, bccb, bccc, caba, cabb, cabc, caca, cacb, cacc\}
$$

### Potenza $n$-esima ($L^n$) concanetamento con se stesso $n$ volte

$$
L^i = \begin{cases}
    \varepsilon & i = 0 \\
    L ^ {i - 1} \cdot L & i \gt 0
\end{cases}
$$

La potenza **È** associativa.

### Chiusura di Kleene

$$
L^* = \bigcup_{n=0}^\infty L^n \\ \ \\
L^* = L^+ \bigcup L^0 = L^+ \bigcup \{\varepsilon\} \\ \ \\
L^+ = \bigcup_{n=1}^\infty L^n \\ \ \\
L^+ = L \cdot L^* \\ \ \\
\{\varepsilon\} \not = \emptyset \\
$$

# Automi a stati finiti

## FSA (Finite State Automaton)

Un **FSA** ha un insieme finito di stati ovvero un numero limitato di configurazioni.

Es.

- {ON, OFF}
- {Canali TV}

Un FSA è definito su un alfabeto (a, b, c o anche on, off, anche 1, 2, 3, insomma simboli o parole che noi definiamo come "input" anche C o Java possono essere definiti come tale).

Quando si riceve un input, il sistema cambia il suo stato (**transizione**).

![Immagine di transizione](assets/Transizione.jpg)

Gli FSA sono rappresentati tramite una tupla $\lang Q, A, \delta, q_0, F \rang$:

- $Q$ insieme finito di stati
- $A$ alfabeto di ingresso
- $\delta$ funzione di transizione
- $q_0 \in Q$ **stato iniziale**
- $F \subseteq Q$ insieme **stati finali**

Per far sì  che gli FSA riconoscano un linguaggio è necessario:

- Sapere le condizioni iniziali del sistema
- Sapere gli stati finali ammisibili

![Immagine di stati finali e iniziali per dare la nostra convezione grafica](assets/Stati%20finali%20e%20iniziali.jpg)

Una sequenza di mosse è di accettazione se raggiunge uno degli stati finali.

Esempio di un FSA:

![Esempio di FSA preso da un esercizio](assets/Automi%20Deterministici/FSA.jpg)

## Trasduttori a stati finiti

### FST (Finite State Transducer)

Sono FSA con 2 nastri.

È una tupla $ \lang Q, I, \delta, q_0, F, O, \eta \rang$:

- $\lang Q, I, \delta, q_0, F \rang$ come gli accettori (FSA)
- $O$ alfabeto di uscita
- $\eta \colon Q \times I \longrightarrow O^*$

Esempio FST

![Esempio di FST preso da un esercizio](assets/Automi%20Deterministici/FST.jpg)

### Pumping Lemma

Se in un sistema a stati finiti si va da uno stato $q_1$ a $q_1$ (ovvero si attraversa un ciclo) vuol dire che lo si può fare n volte.

Perciò se:

$x \in L$ e $|x| \ge |Q| \implies q \in Q \land w \in I^+$:

- $x = ywz$
- $\delta^*(q, w) = q$

Dunque:

- $\forall n \ge 0 yw^nz \in L$

Conseguenze:

$L = \empty$ se $\exist x \in L \iff \exist y \in L, |y| \lt |Q|$

$|L| = \infty$ se $\exist x \in L |Q| \impliedby |x| \lt 2|Q|$

Problemi:

- Per "contare" un numero $n$ molto grande servirebbe memoria infinita

### Operazioni su FSA

N.B. se un sistema è **chiuso** rispetto a un operazione vuol dire che il risultato è sempre parte dell'insieme.

Operazioni:

- Intersezione ( $\bigcap$ )
- Unione ( $\bigcup$ )
- Complemento ( $^c$ )

Sono operazioni **CHIUSE** negli FSA. 

## PDA (PushDown Automata)

Sono FSA con una pila.

La stringa d'ingresso $x$ è accettata se:

- il PDA la legge tutta
- Quando finisce si trova in uno stato di accettazione

Un PDA è una tupla $\lang Q, I, \Gamma, \delta, q_0, Z_0, F \rang$:

- $Q$ insieme finito di stati
- $I$ alfabeto di ingresso
- $\Gamma$ alfabeto di pila
- $\delta$ funzione di transizione
- $q_o \in Q$ è lo stato iniziale
- $Z_0 \in \Gamma$ simbolo iniziale di pila
- $F \subseteq Q$ insieme stati finiti


Esempio PDA:

![Esempio di PDA preso da un esercizio](assets/Automi%20Deterministici/PDA.jpg)

#### Configurazione

- Mostra lo stato corrente del dispositivo di controllo
- Posizione della stringa d'ingresso
- La pila

È una tripla $\lang q, x, \gamma \rang$

- $q \in Q$ stato corrente dispositivo
- $x \in I^*$ posizione **non** letta della stringa d'ingresso
- $\gamma \in \Gamma^*$ stringa simboli di pila

Le transizioni tra configurazioni ($\vdash$) mostra come commutare tra un PDA e un altro

Esempio

Dato $\delta(q, i, A) =  \lang q', \alpha \rang$ è definita

$c = \lang q, x, \gamma \rang \vdash c'= \lang q', x', \gamma \rang$

- $\gamma = A \beta$
- $x = iy$

allora

- $\gamma' = \alpha \beta$
- $x' = y$

**Condizione di accettazione**

$$
\forall x \in I^* (x \in L \iff \exist q \exist \gamma c_0 = \lang q_0, x, Z_0 \rang \vdash^* c_f = \lang q, \varepsilon, \gamma \rang \land q \in F)
$$

La stringa viene accettata se c'è un cammino coerente che va sallo stato iniziale a uno stato finale.

#### PDA VS FSA

- Gli FSA **NON** riconoscono $a^n b^n$ i PDA si
- Ogni linguaggio regolare (FSA) è riconosciuto da PDA
- I PDA sono più potendi dei FSA

I PDA non si fermano sempre dopo le mosse, ogni PDA si può trasformare in un PDA aciclico (si ferma sempre dopo le mosse).

### PDT (PushDown Trasducer)

I PDT sono una tupla $\lang Q, I, \Gamma, \delta, q_0, Z_0, F, O, \eta \rang$:

- $\lang Q, I, \Gamma, \delta, q_0, Z_0, F \rang$ come nei PDA
- $O$ alfabeto d'uscita
- $\eta \colon Q \times (I \bigcup \{ \varepsilon\} ) \times \Gamma \to O^*$

**Configurazione** <$q, x, \gamma, z$>:

- $\lang q, x, \gamma \rang$ come nei PDA
- $z$ stringa già scritta sul nastro d'uscita

**Condizione di accettazione**:

$$
\forall x \in I^* \forall z \in O^* (x \in L \land z = \tau(x) \iff \exist q \exist \gamma \colon c_0 = \lang q_0, x, Z_0, \varepsilon \rang \vdash^* c_f = \lang q, \varepsilon, \gamma, z \rang \land q \in F )
$$

La traduzione è definita se e solo se $x$ è accettata.

Esempio di PDT

![Esempio di PDT preso da un esrcizio](assets/Automi%20Deterministici/PDT.jpg)

I PDT **NON** sono chiusi rispetto a Unione, Intersezione e Complemento.

## Turing Machine (TM)

I PDA **NON** riconoscono $a^n b^n c^n$ o $a^n b^n \cup a^n b^{2n}$.

La pila è una memoria distruttiva, letto un elemento viene eliminato.

C'è bisogno di **NASTRI DI MEMORIA**.

Le TM usano i nastri come memorie:

- Non distruttivi
- Scorrevoli in entrambi i sensi

I nastri sono sequenze infinite di celle con "Blank" (segnato come "$\cancel{b}$", "_" o "$-$").

##### Mosse

Uguali a prima con aggiunta di:

Spostamento testine dichiarato con 3 movimenti:

- Sposta a destra (R)
- Sposta a sinistra (L)
- Fermo (S)

Si esplicita **SEMPRE**.

Le TM sono una tupla $\lang Q, I, \Gamma, \delta, q_0, Z_0, F \rang$:

- $Q$ insieme finito di stati
- $I$ alfdabeto di ingresso
- $\Gamma$ alfabeto di memoria
- $\delta$ funzione di transizione
- $q_0 \in Q$ stato iniziale
- $Z_0 \in \Gamma$ simbolo iniziale di memoria
- $F \subseteq Q$ insieme stati finali

Esempio di TM

![Esempio di TM preso da un esercizio](assets/Automi%20Deterministici/TM.jpg)

La **Configurazione** di una TM con k nastri è una (k+2)-tupla:

$$
c = \lang q, x \uparrow iy, \alpha_1 \uparrow A_1 \beta_1, \dots, \alpha_k \uparrow A_k \beta_k \rang
$$

- $q \in Q$
- $x, y \in I^*$,  $i \in I$
- $\alpha_r, \beta_r \in \Gamma^*$, $A_r \in \Gamma \forall r \quad 1 \le r \le k$
- $\uparrow \notin I \cup \Gamma$

$\uparrow$ indica la posizione della testina.

**Condizione di accettazione**:

$\exist q \exist x' \exist i \exist y \exist \alpha_1 \exist A_1 \exist \beta_1 \dots \exist \alpha_k \exist A_k \exist \beta_k \colon c_0 = \lang q_0, \uparrow x, \uparrow Z_0, \dots, \uparrow Z_0 \rang \vdash_m^* c_F = \lang q, x'\uparrow iy, \alpha_1 \uparrow A_1 \beta_1, \dots, \alpha_k \uparrow A_k \beta_k \rang$ con $q \in F$ e $x = x'iy$

#### TM VS PDA

- $a^n b^n c^n$ **NON** sono riconosciuti da PDA ma da TM
- Se un linguaggio è riconosciuto da PDA allora è riconosciuto da TM

I linguaggi accettati da TM sono detti **ricorsivamente enumerabili**.

#### TM e Macchine di Von Neumann (VNM)

La differenza sta nell'accesso alla memoria:

- TM : Sequenziale
- VNM : Diretto

Il tipo di accesso **NON** cambia la potenza.

Le TM possono simulare le VNM.

##### Operazioni TM

Le TM sono **chiuse** rispetto a:

- Intersezione $(\bigcap)$
- Unione $(\bigcup)$
- Concatenazione
- Stella di Kleene

**NON** sono chiuse rispetto a:

- Complemento
- Differenza

Se esistessero TM acicliche sarebbero chiuse al complemento, il problema sta dove le computazioni non terminano.

### TM Trasduttrice

Le TM trasuttrici a k nastri sono una tupla di 9 elementi $\lang Q, I, \Gamma, O, \delta, \eta, q_0, Z_0, F \rang$:

- $\lang Q, I, \Gamma, \delta, q_0, Z_0, F \rang$ come le TM
- $O$ alfabeto d'uscita
- $\eta$ funzione d'uscita

N.B. Le testine di uscita si muovono in 2 direzioni:

- Destra (R)
- Ferme (S)

Esempio di TM traduttrice

![Esempio TM traduttrice preso da un esercizio](assets/Automi%20Deterministici/TM%20Trasduttrice.jpg)

**Configurazione**

$c = \lang q, x \uparrow iy, \alpha_1 \uparrow A_1 \beta_1, \dots, \alpha_k \uparrow A_k \beta_k, u \uparrow o \rang$

- $\uparrow \in I \bigcup \Gamma \bigcup o$

Le TM possono:

- Riconoscere linguaggi
- Tradurre linguaggi accettati
- Calcolare funzioni

Le TM sono computer con accesso sequenziale alla memoria (Modello astratto).

Le TM possono avere nastri a n dimensioni.

N.B. Più dimensioni **NON** aggiungono potenza.

# Modelli non deterministici

Di solito un algotimo ha una sequenza deterministica.

Il non deterministmo (ND) è un modello di computazione, utile per algoritmi di ricerca.

![Esempio di ND](assets/Automi%20Non%20Deterministici/Non%20Deterministici.jpg)

### NFSA

Gli NFSA sono gli FSA non deterministici e sono una tupla : $\lang Q, I, \delta, q_0, F \rang$

- $\lang Q, I, q_0, F \rang$ uguali a FSA
- $\delta \colon Q \times I \to \mathcal{P} (Q)$

$$
\delta^* (q, \varepsilon) = \{q\}
\\
\delta^* (q, y, i) = \bigcup \delta\{q', i\}
$$

**Condizione di accettazione**

$x \in L \iff \delta^* (q_0, x) \bigcap F \not ={\emptyset}$

Basta che una vada a buon fine.

ND **esistenziale**

$\delta^* (q_0, x) \subseteq F$

#### DFA VS NFA

- Stesso potere
- Da un NFA si può sintetizzare **automaticamente** un DFA

Se $A_{ND} = \lang Q, I, \delta, q_0, F \rang \implies A_{D} = \lang Q_D, I, \delta_D, q_{0D}, F_D \rang$

- $Q_D = \mathcal{P}(Q)$
- $\delta_D (q_D, i) = \bigcup \delta (q, i)$
- $q_{0D} = \{q_0\}$
- $F_D = \{q_D | q_D \in Q_D \land q_D \bigcap F\not ={\emptyset} \}$

Perchè il ND ? (già è difficile di suo ora ci complichiamo la vita)

Gli NFA hanno pari potenza di DFA, ma non sono inutili (purtroppo):

- A volte gli NFA sono più semplici
- Possono essere esponenzialmente più piccoli (NFA a 5 stati nel peggiore dei casi è un DFA a $2^5$ stati)

Esempio di NFA

![Esempio di NFA preso da un esercizio](assets/Automi%20Non%20Deterministici/NFSA.jpg)

### TM non deterministici (NTM)

Per definire una NTM occorre cambiare la funzione di transizione e la funzione di traduzione.

- Elementi uguali alla DTM
- $\delta \colon (Q-F) \times I \times \Gamma^k \to \mathcal{P}(Q \times \Gamma^k \times \{R, L, S\}^{k+1})$
- $\eta \colon (Q-F) \times I \times \Gamma^k \to \mathcal{P}(O \times \{R, S\})$

Esempio NTM

![Esempio di NTM preso da un esercizio](assets/Automi%20Non%20Deterministici/NTM.jpg)

**Condizione di accettazione**

Una stringa è accettata se e solo se esiste unna computazione con uno stato di accettazione.

Che cosa è la visita dell'albero computazionale ?

Diverse modalità :

- In profondità (Depth-first)
- In ampiezza (Breadth-first)

**Si può** costruire una DTM che visita un albero livello dopolivello ma è un processo molto lungo e noioso.

ND **NON** aggiunge potere alle TM

### PDA non deterministici (NPDA)
L e $\varepsilon$-mosse avevano il seguente vincolo:

$$
\delta(q, \varepsilon, A) \not ={\bot}\implies\delta(q, i, A) = \bot; \forall i \in I
$$

Senza di esso i PDA sarebbero intrinsicamente ND.

Un NPDA è una tupla $\lang Q, I, \Gamma, \delta, q_0, Z_0, F \rang$

- $\lang Q, I, \Gamma, q_0, Z_0, F \rang$ come nel PDA
- $\delta$ è la funzione di transazione definita come:
$$
\delta \colon Q \times (I \bigcup \{\varepsilon\}\times \Gamma \to \mathcal{P_F}(Q \times \Gamma^*))
$$

$\mathcal{P_F}$ indica i sottoinsiemi finiti di $Q \times \Gamma^*$

Esempio di NPDA

![Esempio di NPDA preso da un esercizio](assets/Automi%20Non%20Deterministici/NPDA.jpg)

**Condizione di accettazione**

Dato un NPDA:

$\forall x \in I^* (x \in L(P) \iff \exist q \exist \gamma c_0 = \lang q_0, x, Z_0 \rang \vdash^* c_F = \lang q, \varepsilon, \gamma \rang$ e $q \in F)$

Una stringa è accettata se **c'è un cammino** coerente con $x$ che va dallo stato inizio a uno di fine.

ND noi PDA **aggiunge** potere, riconosce $\{ a^nb^n | n\ge 1\} \bigcup \{a^nb^{2n} | n \ge 1\}$

I linguaggi riconoscibili dai PDA sono detti **context-free**.

DPDA chiusi rispetto al complemento.

**NON** chiusi rispetto unione, intersezione e differenza.

NPDA chiusi rispetto unione.

**NON** chiusi rispetto intersezione, complemento e differenza.

![Schema dei poteri](assets/Poteri.jpg)

| Proprietà chiusura | FSA | DPDA | NPDA | TM  |
| ------------------ | --- | ---- | ---- | --- |
| Unione             | SI  | NO   | SI   | SI  |
| Intersezione       | SI  | NO   | NO   | SI  |
| Complemento        | SI  | SI   | NO   | NO  |
| Concanetazione     | SI  | NO   | SI   | SI  |
| Stella di Klenee   | SI  | NO   | SI   | SI  |


# Grammatiche

Esistono 2 tipi di modelli:

- Modelli operazionali (Automi): Riconoscono/accettano, traducono e calcolano linguaggi.
- Modelli generativi (Grammatiche): Descrivono come generare un linguaggio.

I modelli generativi producono stringhe (grammatica).

Una grammatica è un insieme di parole per costruire le frasi di un linguaggio $\to$ tramite **riscrittura** (insieme di metodi).

## Regole linguistiche

Le regole linguistiche descrivono un "oggetto principale" con una sequenza di componenti.

I componenti vengono dettagliati fino ad andare alla base (vuol dire che es. da staccionata si arriva a Assi, Martello, Chiodi).

Una grammatica è una tupla $\lang V_N, V_T, P, S \rang$

- $V_N$ alfabeto **NON TERMINALE**
- $V_T$ alfabeto **TERMINALE**
- $V = V_N \bigcup V_T$
- $P = \subseteq V_N^+ \times V^*$ insieme finito di **regole di riscrittura**
- $S \in V_N$ è un particolare elemento di $V_N$ chiamato **assioma**

Una grammatica genera un linguaggio sull'alfabeto $V_T$

## Produzioni

Elemento di $V_N^+ \times V^* (\lang \alpha, \beta \rang | \alpha \in V_N^+, \beta \in V^*)$

Si indica con $\alpha \to \beta$

- $\alpha$ sequenza si,boli **NON** terminali.
- $\beta$ sequenza (potenzialmente vuota) di simboli (terminali o non)

### Gerarchia Chomsky

Esistono 4 tipi di grammatiche

| Tipo | Nome                         | Produzioni                                                                                                                                                                                | Macchine                                                                 |
| ---- | ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| 3    | Regolari                     | (Destre) $A \to \alpha, A \to aB (A \to \varepsilon) con A,B \in V_N, a \in V_T$                                                                                                          | FSA                                                                      |
| 2    | Context-free                 | $A \to B, A \in V_N$                                                                                                                                                                      | NPDA                                                                     |
| 1    | Context-sensitive e monotone | $S \to \varepsilon, \alpha A \beta \to \alpha \gamma \beta$ con $A \in V_N, \alpha, \beta, \gamma \in V^+$ (ora monotone) $\alpha \to \beta$ con $\vert \alpha \vert = \vert \beta \vert$ | Automi lineari (NTM usano uno spazio di memoria proporzionale all'input) |
| 0    | Generali o non ristrette     | $P \subseteq V_N^+ \times V^+$                                                                                                                                                            | TM |

### Reazioni di derivazione immediata

$\alpha \implies \beta$ ($\beta$ è ottenuta immediatamente da $\alpha$)

- $\alpha \in V_N^+$ e $\beta \in V^*$

se e solo se

$\alpha = \alpha_1 \alpha_2 \alpha_3, \beta = \alpha_1 \beta_2 \alpha_3$ e $\alpha_2 \to \beta_2 \in P$

$\alpha_2$ riscritta come $\beta_2$ nel contesto $\lang \alpha_1, \alpha_3 \rang$

Es.

$$
V_N = \{S,A, B, C, D\}\\
V_T = \{a, b, c\}\\
S\\
P = \{S \to AB, BA \to cCD, CBS \to ab, A \to \varepsilon\}\\
aaBAS \implies aacCDS \\
bcCBSAdd \implies bcabAdd
$$

#### Linguaggio generato

Data $G = \lang V_N, V_T, P, S \rang$

$\forall x (x \in L(G) \iff x \in V_T^* \land S \implies(+) x)$

Il linguaggio generato da $G$ è l'insieme di tutte le stringhe:

- Solo simboli terminali
- Derivante da $S$ in $n$ numeri di passi con $n \ge 1$

ES.

$$
G_2 = \lang \{S\}, \{a, b\}, \{S \to a Sb | ab\}, S \rang \\
S \to aSb |ab = S \to aSb, S \to ab
$$
Derivazioni
$$
S \implies ab \\
S \implies aSb \implies aabbb \\
S \implies aSb \implies aaSbb \implies aaabbb
$$
Generalizzo $L(G_2) = \{a^nb^n|n> 0\}$

### Regular Grammar (RG) VS FSA

Sia $A$ un FSA, si può costruire un RG $G$ ad esso equivalente.

$G$ genera esattamente lo stesso linguaggio riconosciuto da $A$.

Le RG, FSA e espressioni regolari descrivono lo stesso linguaggio in modelli diversi.

Le context-free grammar (CFG) sono identiche alle Backus-Naus Form (BNF) usate per definire la sintassi dei linguaggi di programmazione.

Le grammatiche regolari sono anche non contestuali

I linguaggi possono essere rappresentati tramite molti modelli tra cui :

- Pattern
- Espressioni Regolari (RE)

## Pattern

È una tripla $\lang A, V, p \rang$

- $A$ è un alfabeto
- $V$ insieme di variabili $| A \bigcap V = \emptyset$
- $p$ è una stringa su $A \bigcup V$ detta pattern

Il pattern genera un linguaggio che ha tutte le stringhe di $A$ ottenute da $p$ sostituendo ogmi variabile in $p$ con una stringa su $A$.

Es.
$\lang \{0, 1\}, \{v_1, v_2\}, v_1v_1 \circ v_2 \rang$

- Stringhe che iniziano con $0 (v_1 = \varepsilon)$
- Stringhe che iniziano con una stringa su A ripetuta due volte, seguita da uno $0$ e da una qualunque stringa (inclusa $\varepsilon$)

## RE: sintassi e semantica

Definizione di un RE su un alfabeto $A \colon$

- $\emptyset$ è una RE
- $\varepsilon$ è una RE
- ogni simbolo di $A$ è una RE

Siano $r$ e $s$ due RE, allora:

- $(r.s)$ è una RE
- $r^*$ è una RE
- Nient'altro è una RE

Es.

$((0.(0|1)^*)|((0|1)^*.0))$ è una RE su $\{0, 1\}$

- stringhe che iniziano con $0$
- stringhe che finiscono con $0$

### RE e Pattern

Le RE hanno la stessa idea dei Pattern ma diverso potere.

$RE \not ={Pattern}$.

$\{xx | x \in \{0, 1\}^*\}$ NO RE

Pattern corrispondente $\lang \{0, 1\}, \{x\}, xx \rang$

Linguaggio denotato da $0^*1^*$ **NON ESPREMIBILE** tramite Pattern.

### RG e RE

$RG = RE$.

### RE POSIX (Linux/Unix)

Caratteri meta: ( ) . [ ] ^ \ $ * + | { }

N.B. "." indica qualunque carattere, **NON** concatenazione.

[$\alpha$] indica un singolo carattere $\in \alpha ([abc]$ indica $\{a, b, c\})$.

[^$\alpha$] negazione: qualunque carattere non in $\alpha$.

^ e $ indicano $\varepsilon$ corrispettivamente all'inizio e alla fine della riga.

*, +, |, (, ) uguali a sempre.

\ "escape" (\ $ indica il simbolo $).

$\alpha ?$ indica $\alpha$ opzionale.

$\alpha \{n\}$ indica $\alpha^n$

$\alpha \{n, m\}$ indica $\alpha^n \bigcup \alpha ^{n+1} \bigcup \dots \bigcup \alpha^m$

# Logica

Esistono vari linguaggi logici :

- Logica Proposizionale
- FOL (First-Order Logic)

La logica è un formalismo "universale" e si applica in molti ambiti :

- Architetture (porte logiche)
- Ingegneria del software
- Linguaggi di programmazione
- Database
- AI

Gli insiemi possono essere visti come abbreviazioni di formule FOL.

Es.

$\{a^nb^n | n \ge 0\} \to$ abbreviata di $\forall x (x \in L \longleftrightarrow \exist n (n \ge 0 \land x = a^n.b^n))$

$x^n ?$

$\forall n \forall x ((n=0 \to x^n = \varepsilon) \land (n > 0 \to x^n = x^{n-1}.x))$

Ora faremo un bell' elenco di operazioni logiche, buon viaggio ; )

$L_1 = a^*b^*$

$L_1$ è il linguaggio delle stringhe su $\{a, b\}$ con tutte le "$a$" all'inizio.

Più precisamente $L_1$ è:

- stringa vuota.
- composta da un prefisso "$a$" e un suffiso $y$ (sempre in $L_1$).
- prefisso $y$ (sempre in $L_1$) e un suffisso "$b$".
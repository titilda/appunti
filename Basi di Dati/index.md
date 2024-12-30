---
title: "Basi di Dati"
author:
- "Andrea Lunghi"
---

# Basi di Dati

## Linguaggi Formali

### Algebra Relazionale

L'algebra relazionale è un *linguaggio procedurale*, ovvero descrive la procedura da attuare per ottenere il risultato desiderato.

#### Operatori Algebra Relazionale

Gli operatori principali dell'algebra relazionale sono:

- **Selezione**: operatore unario che seleziona le righe di $R$ che soddisfano la condizione.
$$\sigma_{\text{predicato}}R$$
- **Proiezione**: operatore unario che seleziona le colonne di $R$ indicate.
$$\pi_{\text{attributi}}R$$
- **Ridenominazione**: operatore unario che cambia i nomi degli attributi.
$$\rho_{\text{[nomi nuovi]←[nomi vecchi]}}R$$
- **Unione**: operatore binario che restituisce l'unione di due relazioni dello stesso schema.
$$R\cup S$$
- **Differenza**: operatore binario che restituisce la differenza tra due relazioni dello stesso schema.
$$R-S$$
- **Prodotto cartesiano**: operatore binario che restituisce il prodotto cartesiano tra due relazioni con tutti gli attributi di R e S.
$$R\times S$$

Da queste operazioni si possono derivare altre operazioni più complesse come:

- **Intersezione**: operatore binario che restituisce l'intersezione tra due relazioni dello stesso schema.
$$R\cap S = R - (R-S)$$
- **Join**: operatore binario che restituisce il join (concatenazione) tra due relazioni.
$$R\bowtie_{\text{predicato}} S$$
  - **Join naturale**: join in cui il predicato è l'uguaglianza tra gli attributi con lo stesso nome.
  $$R\bowtie S$$
  - **Semi-join**: join in cui vengono restituiti solo gli attributi di $R$.
  $$R\ltimes_{\text{predicato}} S$$

#### Ottimizzazione delle Interrogazioni

Per ottimizzare le interrogazioni si possono seguire i seguenti passi:

1. **Eliminazione dei Prodotti Cartesiani**: sostituire i prodotti cartesiani con i join.
$$\sigma_{\text{P}}(R\times S) \Rightarrow (R\bowtie_{\text{P}} S)$$
2. **Push della Selezione**: spostare le selezioni il più in basso possibile per ridurre il numero di tuple.
$$\sigma_{\text{P}}(R\bowtie_{\text{Q}} S) \Rightarrow ((\sigma_{\text{P}}R)\bowtie_{\text{Q}} S)$$
3. **Push della Proiezione**: spostare le proiezioni il più in basso possibile per ridurre il numero di attributi.
$$\pi_{\text{A}}(R\bowtie_{\text{P}} S) \Rightarrow ((\pi_{\text{A}}R)\bowtie_{\text{P}} S)$$
4. **Idempotenza**: è possibile scomporre i predicati delle operazioni di selezione e proiezione.
$$\sigma_{\text{P}}(\sigma_{\text{Q}}R) \Rightarrow \sigma_{\text{P}\land\text{Q}}R$$
$$\pi_{\text{A}}(\pi_{\text{A}\cap\text{B}}R) \Rightarrow \pi_{A}R$$
5. **Distributività**: è possibile distribuire le operazioni di join rispetto alle unioni.
$$R\bowtie_{\text{P}}(S\cup T) \Rightarrow (R\bowtie_{\text{P}}S)\cup(R\bowtie_{\text{P}}T)$$

Uno dei principi base è quello di *minimizzare* la dimensione dei risultati intermedi

### Calcolo Relazionale

Il calcolo relazionale è un *linguaggio dichiarativo*, ovvero descrive il risultato desiderato senza specificare la procedura.

Le interrogazioni sono della forma:

$$\{t\ |\ P(t)\}$$

dove $t$ è una tupla e $P(t)$ è una formula che restituisce vero o falso. Se $P(t)$ è vero, la tupla $t$ viene restituita.

$P(t)$ può essere composta da:

- **Connettivi logici**: $\land$ (and), $\lor$ (or), $\lnot$ (not)
- **Quantificatori**: $\forall$ (per ogni), $\exists$ (esiste)
- **Comparazioni**: $=$, $\neq$, $<$, $>$, $\leq$, $\geq$
- **Appartenenza**: $t \in r$, dove $r$ è una relazione

#### Operatori Calcolo Relazionale

Tramite questi operatori si possono costruire i seguenti operatori:

- **Selezione**: $\{t\ |\ \exist\ t\in r (t[attributo]\ COMP\ valore) \}$
- **Proiezione**: $\{t[attributo]\ |\ \exist\ t_1\in r (t[a, b] = t_1[a,b]) \}$
- **Prodotto Cartesiano**: $\{t\ |\ \exist\ t_1\in r_1\ , \exist\ t_2\in r_2 (t[a] = t_1[a]\ \land\ t[b] = t_2[b]) \}$
- **Join**: $\{t\ |\ \exist\ t_1\in r_1\ , \exist\ t_2\in r_2 (t[a] = t_1[a]\ \land\ t[b] = t_2[b]\ \land\ t_1[c] = t_2[c]) \}$
- **Unione**: $\{t\ |\ \exist\ t_1\in r_1\ , \exist\ t_2\in r_2 (t = t_1\ \land\ t = t_2)\}$
- **Differenza**: $\{t\ |\ (t \in r) \land (t \notin s)\}$

### Datalog

Datalog è un linguaggio dichiarativo basato su *prolog*.
Datalog ha un potere espressivo maggiore rispetto al calcolo relazionale, in quanto permette di definire regole ricorsive.

Si basa sull'idea di creare delle regole che definiscono delle *viste*. Le regole sono composte da un *head* (LHS) e un *body* (RHS) e sono della formula:

$$p\ \text{:-} \ p_1,\ p_2$$

dove ogni $p$ è un *letterale* ed è un istanza di un predicato composto da:

- un nome
- una lista di argomenti
  - variabili: $X$
  - costanti: $a$
  - don't care: $\_$

Le regole sono interpretate come implicazioni logiche.

- LHS è vero se RHS è vero.
- RHS è vero se, per ogni letterale, le variabili sono *unificabili*, ovvero possono essere sostituite con valori tali che la formula sia vera.

Un esempio di regola è:

$$\text{Padre}(X, Y)\ \text{:-}\ \text{Persona}(X, \_, \text{'M'}), \ \text{Genitori}(X,Y)$$

In datalog si definiscono due tipologie di database:

- **Estensionale** (EDB): tabelle presenti nel database
- **Intensionale** (IDB): viste definite tramite regole

Le query possono essere fatti sull'intera base di dati (EDB + IDB). La query termina con i **Goal**, ovvero i predicati che si vogliono verificare.

$$\text{?-}\ \text{Padre}(\text{'Andrea'}, \text{'Marco'})$$

#### Operatori Datalog

Gli operatori principali di Datalog sono:

- **Selezione**:
$$\text{P}(X,\_,\_)\ \text{:-}\ \text{R}(X, \_, \text{'M'})$$
- **Proiezione**: è possibile proiettare solo gli argomenti che si vogliono mantenere
$$\text{P}(X)\ \text{:-}\ \text{R}(X, \_, \_)$$
- **Join**: usa due letterali sulla stessa vista separati da virgola
$$\text{P}(X, Y)\ \text{:-}\ \text{R}(X, \_, \text{'M'}), \ \text{S}(X,Y)$$
- **Unione**: usare due regole separate sulla stessa vista
$$\text{P}(X, Y)\ \text{:-}\ \text{R}(X, Y)$$
$$\text{P}(X, Y)\ \text{:-}\ \text{S}(X, Y)$$
- **Differenza**: da usare assieme ad un predicato positivo per evitare di avere risultati infiniti (unsafe).
$$\text{P}(X, Y)\ \text{:-}\ \text{R}(X, Y), \neg \text{S}(X, Y)$$

In datalog è possibile definire regole ricorsive, ma bisogna definire un'inizio prima del passo induttivo.

$$\text{P}(X, Y)\ \text{:-}\ \text{R}(X, Y)$$
$$\text{P}(X, Y)\ \text{:-}\ \text{R}(X, Z), \ \text{P}(Z, Y)$$

Il passo induttivo continua finché non si raggiunge un punto fisso, ovvero non si hanno più nuovi fatti.

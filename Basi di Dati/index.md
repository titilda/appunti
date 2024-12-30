---
title: "Basi di Dati"
author:
- "Andrea Lunghi"
---

# Basi di Dati

## Linguaggi Formali

### Algebra Relazionale

L'algebra relazionale è un *linguaggio procedurale*, ovvero descrive la procedura da attuare per ottenere il risultato desiderato.

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

Tramite questi operatori si possono costruire i seguenti operatori:

- **Selezione**: $\{t\ |\ \exist\ t\in r (t[attributo]\ COMP\ valore) \}$
- **Proiezione**: $\{t[attributo]\ |\ \exist\ t_1\in r (t[a, b] = t_1[a,b]) \}$
- **Prodotto Cartesiano**: $\{t\ |\ \exist\ t_1\in r_1\ , \exist\ t_2\in r_2 (t[a] = t_1[a]\ \land\ t[b] = t_2[b]) \}$
- **Join**: $\{t\ |\ \exist\ t_1\in r_1\ , \exist\ t_2\in r_2 (t[a] = t_1[a]\ \land\ t[b] = t_2[b]\ \land\ t_1[c] = t_2[c]) \}$
- **Unione**: $\{t\ |\ \exist\ t_1\in r_1\ , \exist\ t_2\in r_2 (t = t_1\ \land\ t = t_2)\}$
- **Differenza**: $\{t\ |\ (t \in r) \land (t \notin s)\}$

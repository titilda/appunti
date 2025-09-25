---
title: "Elenco Teoremi con relative dimostrazioni per esame GAL"
description: "Elenco di teoremi con dimostrazioni per l'esame di Geometria e Algebra Lineare"
authors:
  - "Andrea Oggioni"
slug: "geometria-e-algebra-lineare"
---

# Elenco Teoremi con relative dimostrazioni per esame GAL

## Capitolo Unico: Teoremi

### 1.1 Proposizione di struttura delle soluzioni di un sistema lineare (lez. 3 pag. 8)

Dati

$$
A \in \mathcal{M}_{\mathbb{K}}(m, n) \quad \underline{b} \in \mathbb{K}^m \quad \underline{v}_0 \in \mathbb{K}^n
$$

tali che $A \underline{v}_0 = \underline{b}$, allora, tutte le soluzioni del sistema $A \underline{x} = \underline{b}$ sono del tipo $\underline{v}_0 + \underline{v}_h$, ove $\underline{v}_h$ è soluzione del sistema lineare omogeneo associato $A\underline{x} = \underline{0}$.

#### Dimostrazione

Sappiamo che $\underline{v}_h \in \ker(A)$, $A \cdot \underline{v}_0 = \underline{b}$ e $A \cdot \underline{v}_h = \underline{0}$.
Per proprietà distributiva allora:

$$
A \cdot (\underline{v}_0 + \underline{v}_h) = A \cdot \underline{v}_0 + A \cdot \underline{v}_h = \underline{b} + \underline{0} = \underline{b}
$$

Viceversa se $\tilde{\underline{v}}$ è soluzione di $A \cdot \tilde{\underline{v}} = \underline{b}$, osservo che $\tilde{\underline{v}} = \underline{v}_0 + (\tilde{\underline{v}} - \underline{v}_0)$ e che $A \cdot (\tilde{\underline{v}} - \underline{v}_0) = A\tilde{\underline{v}} - A\underline{v}_0 = \underline{b} - \underline{b} = \underline{0}$.
Di conseguenza $\tilde{\underline{v}} = \underline{v}_0 + \underline{v}_h$ ove $\underline{v}_h = \tilde{\underline{v}} - \underline{v}_0 \in \ker(A)$

### 1.2 Proposizione di unicità della matrice inversa (lez. 5 pag 2)

Data $A \in \mathcal{M}_{\mathbb{K}}(n, n)$ e siano $B, C$ rispettivamente inversa destra e sinistra di $A$, allora $B = C$.

#### Dimostrazione (Da ricontrollare)

Sappiamo che $AB = CA = \mathbb{I}_n$, quindi $B = \mathbb{I}_n \cdot B = (C \cdot A) \cdot B = C \cdot (A \cdot B) = C \cdot \mathbb{I}_n = C$, di conseguenza l'inversa sinistra e l'inversa destra sono uguali.

### 1.3 Condizioni necessarie e sufficienti per l'invertibilità di una matrice (lez. 5 pag. 2)

Sia $A \in \mathcal{M}_{\mathbb{K}}(n, n)$ allora le seguenti affermazioni sono equivalenti:

1.  $\ker(A) = \{\underline{0}\}$
2.  $r(A) = n$
3.  $A$ è invertibile
4.  $A$ ha un'inversa sinistra
5.  $A$ ha un'inversa destra

#### Dimostrazone

Mostriamo $4. \implies 1. \implies 2. \implies 3. \implies 4.$ e poi $4. \iff 5.$.

- $4. \implies 1.$: Poiche $A$ ha un'inversa sinistra, allora $\exists A' : A' \cdot A = \mathbb{I}_n$.
  Cosidero ora il sistema lineare $A \cdot \underline{x} = \underline{0}$ allora $A' \cdot (A \cdot \underline{x}) = A' \cdot \underline{0} = 0$ che implica $(A' \cdot A) \cdot \underline{x} = \underline{0} \implies \underline{x} = \underline{0}$ da cui $\ker(A) = \{\underline{0}\}$
- $1. \implies 2.$: Se $\ker(A) = \{\underline{0}\}$ allora $A \cdot \underline{x} = \underline{0}$ ammette un'unica soluzione e per il teorema di Rouchè-Capelli allora $r(A) = n$
- $2. \implies 3.$: $\underline{e}_i$ l'i-esimo vettore della base canonica di $\mathbb{K}^n$, allora per il teorema di Rouchè-Capelli, il sistema $A\cdot \underline{x} = \underline{e}_i$ ammette un'unica soluzione $\underline{c}_i \in \mathbb{K}^n : A \cdot \underline{c}_i = \underline{e}_i \forall i = 1, \dotsc, n$.
  Posto $C = [\underline{c}_1 | \dotsc | \underline{c}_n]$ allora $A \cdot C = [A \cdot \underline{c}_1 | \dotsc | A \cdot \underline{c}_n] = [\underline{e}_1 | \underline{e}_n] = \mathbb{I}_m$, di conseguenza $C$ è l'inversa destra di $A$ e $C$ è invertibile a sinistra.
  Per quanto già visto, siccome $C$ è invertibile a sinistra, allora ammette anche un'inversa destra che chiamiamo $A'$.
  Allora $C \cdot A' = \mathbb{I}_n \implies A' = A$ che implica che $A$ è invertibile.
- $3. \implies 4.$: è ovvio

- $4. \implies 5.$: Già vista precedentemente nel punto $2. \implies 3.$.
- $5. \implies 4.$ Sia $A'$ l'inversa destra di A allora $A \cdot A' = \mathbb{I}$.
  Di conseguenza, $A'$ ammette un'inversa sinistra e un'inversa destra $A''$, allora $A' \cdot A'' = \mathbb{I}_n \implies A = A''$ quindi $A \cdot A' = A' \cdot A = \mathbb{I}_n$ e, di conseguenza, $A$ è invertibile

#### Note

Nel primo punto, ho dimostrato che l'unica soluzione di $A\underline{x} = 0$ (quindi il kernel di $A$) è $\underline{x} = \underline{0}$.

Nel terzo punto, utilizziamo l'invertibilità di C per dedurre che A è invertibile (a priori non possiamo saperlo).

Nel quarto punto, per definizione se una matrice invertibile allora ammette un'inversa sinistra.

Nel sesto punto, $A'$ è inversa destra di $A$, $A''$ è inversa destra di $A'$.
Se $A \cdot A' = \mathbb{I}_n = A' \cdot A''$ allora $A = A''$ e, di conseguenza $A \cdot A' = A' \cdot A = \mathbb{I}_n$.

### 1.4 Nucleo e immagine di un'applicazione lineare sono sottospazi vettoriali (lez. 6 pag. 7)

Dati $V$ e $W$ spazi vettoriali su $\mathbb{K}$ e $\mathscr{L} : V \to W$ applicazione lineare, allora:

1.  Se $H$ è sottospazio vettoriale di $W$ allora $\mathscr{L}^{-1}(H)$ è sottospazio vettoriale di $V$
2.  Se $U$ è sottospazio vettoriale di $V$ allora $\mathscr{L}(U)$ è sottospazio vettoriale di $W$

#### Dimostrazione

1.  Se $H$ è sottospazio vettoriale di $W$ allora $\underline{0}_W \in H$.
    Poiche $\underline{0}_V \in \mathscr{L}^{-1}(H)$ (infatti $\mathscr{L}(\underline{0}_V) = \underline{0}_W$), si ha che $\mathscr{L}(H) \ne \emptyset$.
    Ora devo verificare che $\forall t_1, t_2 \in \mathbb{K}, \forall \underline{v}_1, \underline{v}_2 \in \mathscr{L}^{-1}(H) \implies t_1 \underline{v}_1 + t_2 \underline{v}_2 \in \mathscr{L}^{-1}(H)$ (quindi che $\mathscr{L}^{-1}(H)$ sia sottospazio vettoriale).
    Per linearità, $\mathscr{L}(t_1 \underline{v}_1 + t_2 \underline{v}_2) = t_1\mathscr{L}(\underline{v_1}) + t_2\mathscr{L}(\underline{v_2})$.
    Siccome $\mathscr{L}(\underline{v}_1), \mathscr{L}(\underline{v}_2) \in H$ allora anche $\mathscr{L}(t_1 \underline{v}_1 + t_2 \underline{v}_2) \in H$, quindi $t \underline{v}_1 + t_2 \underline{v}_2 \in \mathscr{L}^{-1}(H)$, da cui, $\mathscr{L}^{-1}(H)$ è a sua volta sottospazio vettoriale di $V$.
2.  Se $U$ è sottospazio vettoriale di $V$ allora $\underline{0}_V \in U$ e $\mathscr{L}(\underline{0}_V) = \underline{0}_W \in \mathscr{L}(U) \implies \mathscr{L}(U) \ne \emptyset$
    Ora devo verificare che $\forall t_1, t_2 \in \mathbb{K}, \forall \underline{w}_1, \underline{w}_2 \implies t_1 \underline{w}_1 + t_2 \underline{w}_2 \in \mathscr{L}(U)$.
    Siano $t1, t2 \in \mathbb{K}$ e $\underline{w}_1, \underline{w}_2 \in \mathscr{L}(U)$ allora $\exists \underline{v}_1, \underline{v}_2 \in U : \mathscr{L}(\underline{v}_1) = \underline{w}_1, \mathscr{L}(\underline{v}_2) = \underline{w}_2$.
    Quindi si ha che $t_1 \underline{w}_1 + t_2 \underline{w}_2 = t_1 \mathscr{L}(\underline{v}_1) + t_2 \mathscr{L}(\underline{v}_2) = \mathscr{L}(t_1 \underline{v}_1 + t_2 \underline{v_2})$.
    Siccome $t_1 \underline{v}_1 + t_2 \underline{v}_2 \in U$ allora $t_1 \underline{w}_1 + t_2 \underline{w}_2 \in \mathscr{L}(U)$, da cui $\mathscr{L}(U)$ è un sottospazio.

#### Note

So che $H \sube W$ e che $U \sube V$.

Per verificare che, per esempio, $W$ sia sottospazio vettoriale di $V$, basta verificare che $\underline{0}_W \in W$ e che $\forall t_1, t2 \in \mathbb{K}, \forall \underline{v}_1, \underline{v}_2 \in W, t_1 \underline{v}_1 + t_2 \underline{v}_2 \in W$

### 1.5 Proposizione di linearità dell'inversa di un applicazione lineare (lez. 6 pag. 12)

Siano $V$ e $W$ spazi vettoriali su $\mathbb{K}$ e $\mathcal{L}: V \to W$ applicazione lineare.
Se $\mathcal{L}$ è invertibile allora l'inverza $\mathcal{L}^{-1} : W \to V$ è lineare.

#### Dimostrazione

Siano $\underline{w}_1, \underline{w}_2 \in W$ allora $\exists \underline{v}_1, \underline{v}_2 \in V : \mathscr{L}(\underline{v}_1) = \underline{w}_1, \mathscr{L}(\underline{v}_2) = \underline{w}_2$.
Allora $\forall t_1, t_2 \in \mathbb{K}$ si ha che

$$
\begin{align*}
  \mathscr{L}^{-1}(t_1 \underline{w}_1 + t_2 \underline{w}_2) &= \mathscr{L}^{-1}(t_1 \mathscr{L}(\underline{v}_1) + t_2 \mathscr{L}(\underline{v}_2)) \\
  &= \mathscr{L}^{-1}(\mathscr{L}(t_1 \underline{v}_1 + t_2 \underline{v}_2)) \\
  &= t_1 \underline{v}_1 + t_2 \underline{v}_2 \\
  &= t_1 \mathscr{L}^{-1}(\underline{w}_1) + t_2 \mathscr{L}(\underline{w}_2)
\end{align*}
$$

### 1.6 Applicazioni lineari iniettive (lez. 6 pag. 14)

Siano $V$ e $W$ due spazi vettoriali sul campo $\mathbb{K}$ e $\mathscr{L} : V \to W$ applicazione lineare allora $\mathscr{L}$ è iniettiva $\iff \ker (\mathscr{L}) = \{ \underline{0}_V \}$

#### Dimostrazione

$\mathscr{L}$ è iniettiva se $\forall \underline{w} \in W, \mathscr{L}^{-1}(\underline{w})$ o è vuoto o è composto di un unico elemento.

Poiche $\underline{0}_V \in \mathscr{L}^{-1}(\underline{0}_W)$ si ha che $\ker (\mathscr{L}) = \mathscr{L}^{-1}(\underline{0}_W) = \{ \underline{0}_V \}$.
Viceversa, se $\ker (\mathscr{L}) = \{ \underline{0}_V \}$ allora

$$
\begin{align*}
  \mathscr{L}(\underline{v}_1) = \mathscr{L}(\underline{v}_2) & \implies \mathscr{L}(\underline{v}_1) - \mathscr{L}(\underline{v}_2) = \underline{0}_W \\
  & \implies \mathscr{L}(\underline{v}_1 - \underline{v}_2) = \underline{0}_W \\
  & \implies \underline{v}_1 - \underline{v}_2 \in \ker (\mathscr{L}) \\
  & \implies \underline{v}_1 - \underline{v}_2  = \underline{0}_V \\
  & \implies \underline{v}_1 = \underline{v}_2
\end{align*}
$$

#### Note

Ho dimostrato che non possono esserci due valori distinti per cui l'applicazione lineare restituisca lo stesso risultato.

### 1.7 Isomorfismo canonico (lez. 7 pag. 1,2)

Sia $V$ uno spazio vettoriale su $\mathbb{K}$ e $\mathcal{B} = \{ \underline{b}_1, \dots, \underline{b}_n \}$ una sua base.
Allora, per definizione di base, sappiamo che ad ogni $v \in V$ rimane associata un'unica n-upla $[t_1, \dots, t_n]^T \in \mathbb{K}^n$ tale che $\underline{v} = t_1 \underline{b}_1 + \dotsm +t_n \underline{b}_n$.
L'applicazione $X_B : V \to \mathbb{K}^n$ definita da $X_B(\underline{v}) = [t_1, \dots, t_n]^T$ è lineare, iniettiva e suriettiva ed è chiamata isomorfismo canonico di $V$ rispetto a $\mathcal{B}$.

#### Dimostrazione (Da controllare)

Per definizione, $X_B$ è l'inversa della mappa di parametrizzazione, che è lineare, iniettiva e suriettiva, pertanto anche $X_B$ è lineare, iniettiva e suriettiva.

### 1.8 Proposizione senza nome (lez. 9 pag. 2)

Dati $V$ è $W$ spazi vettoriali su $\mathbb{K}$ e $\mathscr{L} : V \to W$ applicazione lineare, allora se $\mathscr{L}$ è iniettiva e $\{ \underline{v}_1, \dots, \underline{v}_d \} \sub V$ è linearmente indipendente allora $\{ \mathscr{L}(\underline{v}_1), \dots, \mathscr{L}(\underline{v}_d) \} \sub W$ è linearmente indipendente.

#### Dimostrazione

Consideriamo una qualsiasi combinazione lineare di $\mathscr{L}(\underline{v}_1), \dots, \mathscr{L}(\underline{v}_d)$ eponiamola uguale al vettore nullo: $t_1 \mathscr{L}(\underline{v}_1) + \dots + \mathscr{L}(\underline{v}_d) = \underline{0}$.
Per linearità si ha che $\mathscr{L}(t_1 \underline{v}_1 + \dots + t_d \underline{v}_d) = \underline{0}$.
Poiche $\mathscr{L}$ è iniettiva allora $t_1 \underline{v}_1 + \dots + t_d \underline{v}_d = \underline{0}$.
Poiche $\underline{v}_1, \dots, \underline{v}_d$ sono linearmente indipendenti, $t_1 = \dots = t_d = 0$.
Dunque $\mathscr{L}(\underline{v}_1), \dots, \mathscr{L}(\underline{v}_d)$ sono linearmente indipendenti.

#### Note

Ho sfruttato il fatto che l'unica combinazione lineare di termini linearmente indipendenti che è ugule al vettore nullo, è quella nella quale tutti i coefficienti sono nulli.

### 1.9 Lemma fondamentale (lez. 9 pag. 5)

Se $V$ è un insieme di generatori su $\mathbb{K}$ che ammette un insimeme di generatori di cardinalità $m$ allora ogni altro insieme di $V$ che ha cardinalità maggiore di $m$ è linearmente dipendente.

#### Dimostrazione

1.  Se $V = \mathbb{K}^m$ ($\{ \underline{e}_1, \dots, \underline{e}_m \}$ è un insieme di generatori) allora l'insieme $\{ \underline{v}_1, \dots, \underline{v}_n \}, n > m, v_1, \dots, v_n \in \mathbb{K}^m$ è linearmente dipendente.
    Infatti esistono $t_1, \dots, t_n \in \mathbb{K}$ non tutti nulli tali che $t_1 \underline{v}_1 + \dots + t_n \underline{v}_n = 0 \iff A \cdot \underline{t} = \underline{0}$ ove $A = [\underline{v}_1 | \dots | \underline{v}_n], \underline{t} = [t_1 | \dots | t_n]$, ammette una soluzione non banale.
    Ma questo è vero per il teorema do Rouchè-Capelli essendo $r([A|\underline{0}]) = r(A) \le m < n$.
2.  Siano $\{ \underline{v}_1, \dots, \underline{v}_m \}$ generatori per $V$. Allora la mappa di parametrizzazione associata a $\underline{v}_1, \dots, \underline{v}_m$ è lineare e suriettiva
    $$
    \begin{align*}
      \mathcal{P} : \mathbb{K}^m & \to V \\
      \begin{bmatrix}
        t_{1} \\
        \vdots \\
        t_{m}
      \end{bmatrix}
      & \mapsto t_1 \underline{v}_1 + \dots t_m \underline{v}_m
    \end{align*}
    $$
    Sia $\{ \underline{w}_1, \dots, \underline{w}_n \} \sube V$ ove $n > m$. Poichè $\mathcal{P}$ è suriettiva $\exists \underline{z}_1, \dots, \underline{z}_n \in \mathbb{K}^m : \mathcal{P}(\underline{z}_i) = \underline{w}_i$.
    Per il primo caso esistono $t_1, \dots, t_n$ non tutti nulli tali che $t_1 \underline{z}_1 + \dots + t \underline{z}_n = \underline{0}$ quindi
    $$
    \begin{align*}
      \underline{0} = \mathcal{P}(t_1 \underline{z}_1 + \dots + t_n \underline{z}_n) &= t_1 \mathcal{P}(\underline{z}_1) + \dots + t_n \mathcal{P}(\underline{z}_n) \\
      &= t_1 \underline{w}_1 + \dots + t_n \underline{w}_n
    \end{align*}
    $$
    Quindi $\underline{w}_1, \dots, \underline{w}_n$ sono linearmente dipendenti.

#### Note

Ho dimostrato che se ho un'insieme di generatori di cardinalità $m$, allora un qualsiasi insieme di cardinalità $n > m$ è linearmente dipendente.

Nella seconda parte, utilizzando il fatto che la mappa di parametrizzazione è suriettiva, l'ho dimostrato per un qualsiasi spazio vettoriale, non solo per $\mathbb{K}^m$

### 1.10 La dimensione è la cardinalità della base (lez. 9 pag. 6)

Sia $V$ uno spazio vettoriale su $\mathbb{K}$, allora valgono le seguenti implicazioni:

1.  Se $V$ ha una base di cardinalità $n$ allora $\dim(V) = n$
2.  Se $\dim(V) = n < +\infty$ allora esiste una base $B \sub V$ che ha cardinalità $n$ ed ogni altra base ha cardinalità $n$

#### Dimostrazione

Sia $\mathcal{B} = \{ \underline{b}_1, \dots, \underline{b}_n \}$ una base per $V$ allora:

1.  $\{ \underline{b}_1, \dots, \underline{b}_n \}$ è inearmente indipendente quindi per la definizione di dimensione: $\dim(V) \ge n$
2.  $\{ \underline{b}_1, \dots, \underline{b}_n \}$ è un insieme di generatori quindi per il lemma fondamentale: $\dim(V) \le n$

Se $\dim(V) \ge n$ e $\dim(V) \le n$ allora $\dim(V) = n$

### 1.11 Un'altra proposizione senza nome (lez. 9 pag. 10)

Sia $V$ spazio vettoriale su $\mathbb{K}$ tale che $\dim(V) = n < + \infty$. Sia $\mathscr{L}: V \to W$ applicazione lineare iniettiva, allora $\dim(\mathscr{L}(H)) = \dim(H)$ per ogni sottospazio $H$ di $V$.
Inoltre, due spazi vettoriali su $\mathbb{K}$ sono isomorfi se e solo se hanno la stessa dimensione.

#### Dimostrazione

Sia $\{ \underline{v}_1, \dots, \underline{v}_d \}$ una base di $H$, allora $H = Span\{ \underline{v}_1, \dots, \underline{v}_d \}$ e $\{ \underline{v}_1, \dots, \underline{v}_d \}$ è linearmente indipendente.

Poiche $\mathscr{L}$ è lineare: $\mathscr{L}(H) = Span\{ \mathscr{L}(\underline{v}_1), \dots, \mathscr{L}(\underline{v}_d) \}$ sono linearmente indipendenti, quindi $\{ \mathscr{L}(\underline{v}_1), \dots, \mathscr{L}(\underline{v}_d) \}$ sono una base di $\mathscr{L}(H)$ da cui $\dim(\mathscr{L}(H)) = d = \dim(H)$

### 1.12 Teorema di rappresentazione (lez. 11 pag. 2)

Sia $\mathscr{L} : V \to W$ un'applicazione lineare, $\mathcal{B} = \{ \underline{b}_1, \dots, \underline{b}_n \}$ base per $V$ e $\mathcal{C} = \{ \underline{c}_1, \dots, \underline{c}_n \}$ base per $W$ allora posto $A := \mathcal{M}_C^B(\mathcal{L})$, si ha che $A$ è una matrice tale per cui $X_C(\mathcal{L}(\underline{v})) = A \cdot X_B(\underline{v}) \quad \forall \underline{v} \in V$

#### Dimostrazione

Siano $\underline{x} = [x_1, \dots, x_n]^T$ le coordinate di $\underline{v} \in V$ rispetto a $\mathcal{B}$ e $\underline{y} = [y_1, \dots, y_n]^T$ le coordinate di $\mathscr{L}(\underline{v}) \in W$ rispetto a $\mathcal{C}$.

Allora $\underline{v} = x_1 \underline{b} + \dots + x_n \underline{b}_n$, inoltre $\mathscr{L}(\underline{b}_i) \in W \quad \forall i=1, \dots, n$ quindi la posso scrivere utilizzando la base $\mathcal{C}$:

$$
\begin{align*}
  \mathscr{L}(\underline{b}_1) &= a_{11} \underline{c}_1 + a_{21} \underline{c}_2 + \dots + a_{m1} \underline{c}_m \\
  \vdots \\
  \mathscr{L}(\underline{b_1}) &= a_{1n} \underline{c}_1 + a_{2n} \underline{c}_2 + \dots + a_{mn} \underline{c}_m
\end{align*}
$$

Allora si ha che

$$
\begin{align*}
  \mathscr{L}(\underline{v}) =& \mathscr{L}(x_1 \underline{b}_1 + \dots + x_n \underline{b}_n) \\
  =& x_1 \mathscr{L}(\underline{b}_1) + \dots + x_n \mathscr{L}(\underline{b}_n) \\
  =& x_1(a_{11} \underline{c}_1 + a_{21} \underline{c}_2 + \dots + a_{m1} \underline{c}_m) + \\
  & x_2(a_{12}\underline{c}_1 + a_{22} \underline{c}_2 + \dots + a_{m2}\underline{c}_m) + \\
  & \vdots \\
  & x_n(a_{1n} \underline{c}_1 + a_{2n} \underline{c}_2 + \dots + a_{mn} \underline{c}_m) \\
  =& (x_1 a_{11} + x_2 a_{12} + \dots + x_n a_{1n}) \underline{c}_1 + \\
  & (x_1 a_{21} + x_2 a_{22} + \dots + x_n a_{2n}) \underline{c}_2 + \\
  & \vdots \\
  & (x_1 a_{m1} + x_2 a_{m2} + \dots + x_n a_{mn}) \underline{c}_m
\end{align*}
$$

Quindi $\mathscr{L}(\underline{v}) = y_1 \underline{c}_1 + y_2 \underline{c}_2 + \dots + y_m \underline{c}_m$ da cui

$$
\begin{cases}
  y_1 &= a_{11} x_1 + a_{12} x_2 + \dots + a_{1n} x_n \\
  y_2 &= a_{21} x_1 + a_{22} x_2 + \dots + a_{2n} x_n \\
  & \vdots \\
  y_m &= a_{m1} x_1 + a_{m2} x_2 + \dots + a_{mn} x_n \\
\end{cases}
\leadsto \underline{y} = A \cdot \underline{x}
$$

ove

$$
A =
\begin{bmatrix}
  a_{11} & a_{12} & \dotsm & a_{1n} \\
  a_{21} & a_{22} & \dotsm & a_{2n} \\
  \vdots & \vdots & \ddots & \vdots \\
  a_{m1} & a_{m2} & \dotsm & a_{mn}
\end{bmatrix} =
\left[ X_C(\mathscr{L}(\underline{{b}_i})) | \dots | X_C(\mathscr{L}(\underline{b}_n)) \right]
$$

### 1.13 Costruzione di applicazioni lineari (lez. 12 bis pag. 1)

Siavo $V$ e $W$ spazi vettoriali su $\mathbb{K}$, $\dim(V) = n$, $\mathcal{B} = \{ \underline{b}_1, \dots, \underline{b}_n \}$ una base di $\mathcal{b}$ e $\{ \underline{w}_1, \dots, \underline{w}_n \} \sube W$ un sottoinsieme di $W$ allor aesiste un'unica applicazione lineare $\mathscr{L} : V \to W$ tale che $\mathscr{L}(\underline{b}_i) = \underline{w}_i \quad \forall i = 1, \dots, n$.
L'applicazione è definita dalla formula $\mathscr{L}(x_1 \underline{b}_1 + \dots + x_n \underline{b}_n) = x_1 \underline{w}_1 + \dots + x_n \underline{w}_n$

#### Dimostrazione

1.  Dimostro l'esistenza
    Osservo che sono definite la mappa di parametrizzazione e l'isomorfismo canonico:
    $$
    \begin{align*}
      \mathcal{P} &: \mathbb{K}^n \to W \\
      & \begin{bmatrix}
        t_1 \\
        \vdots \\
        t_n
      \end{bmatrix} \mapsto t_1 \underline{w}_1 + \dots + t_n \underline{w}_n
    \end{align*} \qquad
    \begin{align*}
      X_B &: V \to \mathbb{K}^n \\
      & \underline{v} \mapsto \begin{bmatrix}
        x_1 \\
        \vdots \\
        x_n
      \end{bmatrix}
    \end{align*}
    $$
    Definiamo $\mathscr{L} := \mathcal{P} \cdot X_B$. Osserviamo che $L$, essendo composizione di due funzioni lineari è a sua volta lineare.
    Inoltre
    $$
    \mathscr{L}(\underline{b}_i) = (\mathcal{P} \cdot X_B)(\underline{b}_i) = \mathcal{P}([0 \dots \underbrace{1}_{\text{i-esima posizione}} \dots 0]) = \underline{w}_i
    $$
    Inoltre $\mathscr{L}(x_1 \underline{b}_1 + \dots x_n \underline{b}_n) = x_1 \mathscr{L}(\underline{b}_1) + \dots + x_n \mathscr{L}(\underline{b}_n) = x1 \underline{w}_1 + \dots + x_n \underline{w}_n$
2.  Dimostro l'unicità
    Supponiamo che esista un'altra applicazione lineare $g : V \to W$ tale che $g(\underline{b}_i) = \underline{w}_i \quad \forall i = 1, \dots, n$.
    Allora per ogni $\underline{v} \in V$ (con $\underline{v} = x_1 \underline{b}_1 + \dots + x_n \underline{b}_n$) si ha che
    $$
    \begin{align*}
      g(\underline{v}) &= g(x_1 \underline{b}_1 + \dots + x_n \underline{b}_n) = x_1 g(\underline{b}_1) + \dots + x_n g(\underline{b}_n) \\
      &= x_1 \underline{w}_1 + \dots + x_n \underline{w}_n = x_1 \mathscr{L}(\underline{b}_1) + \dots + x_n \mathscr{L}(\underline{b}_n) \\
      &= \mathscr{L}(x_1 \underline{b}_1 + \dots x_n \underline{b}_n) = \mathscr{L}(\underline{v})
    \end{align*}
    $$
    Di conseguenza $g(\underline{v}) = \mathscr{L}(\underline{v}) \quad \forall \underline{v} \in V$

### 1.14 Formula di Grassmann (lez. 12 bis pag 5)

Siano $H$ e $K$ sue sottospazi vettoriali di $V$ spazio vettoriale su $\mathbb{K}$.
Se $\dim(H) < + \infty$ e $\dim(K) < è \infty$ allora $\dim(H+K) = \dim(H) + \dim(K) - \dim(H \cap K)$

#### Dimostrazione

Sia $\dim(H) = t$, $\dim(K) = s$ e $\dim(H \cap K) = r$. Sia $\{ \underline{v}_1, \dots, \underline{v}_r \}$ una base di $H \cap K$.

Completiamo ad una base di $H$ l'insieme $\{ \underline{v}_1, \dots, \underline{v}_r \}$: $\{ \underline{v}_1, \dots, \underline{v}_r, \underline{h}_1, \dots, \underline{h}_{t - r} \}$.

Completiamo ad una base di $K$ l'insieme $\{ \underline{v}_1, \dots, \underline{v}_r \}$: $\{ \underline{v}_1, \dots, \underline{v}_r, \underline{k}_1, \dots, \underline{k}_{s - r} \}$.

Si ha che l'insieme $\{ \underline{v}_1, \dots, \underline{v}_r, \underline{h}_1, \dots, \underline{h}_{t - r}, \underline{k}_1, \dots, \underline{k}_{s - r} \}$ è insieme di generatori per $H + K$.

Mostriamo che è linearmente indipendente e in questo modo abbiamo dimostrato la formula di Grassmann.

Consideriamo: $t_1 \underline{v}_1 + \dots + t_r \underline{v}_r + \alpha_1 \underline{h}_1 + \dots + \alpha_{t - r} \underline{h}_{t - r} + \beta_1 \underline{k}_1 + \dots \beta_{s - r} \underline{k}_{s - r} = \underline{0}$.

Si ha che: $t_1 \underline{v}_1 + \dots + t_r \underline{v}_r + \alpha_1 \underline{h}_1 + \dots + \alpha_{t - r} \underline{h}_{t - r} = -\underbrace{(\beta_1 \underline{k}_1 + \dots + \beta_{s - r} \underline{k}_{s - r})}_{= \underline{w}}$, di conseguenza $\underline{w} \in K \cap H$.

Dunque esistono $t'_1, \dots, t'_r \in \mathbb{K} \colon \underline{w} = t'_1 \underline{v}_1 + \dots + t'_r \underline{v}_r$.

Si ha che:

$$
\begin{cases}
  t_1 \underline{v}_1 + \dots + t_r \underline{v}_r + \alpha_1 \underline{h}_1 + \dots + \alpha_{t - r} \underline{h}_{t-r} = t'_1 \underline{v}_1 + \dots + t'_r \underline{v}_r \\
  \beta_1 \underline{k}_1 + \dots + \beta_{s - r} \underline{k}_{s - r} = t'_1 \underline{v}_1 + \dots + t'_r \underline{v}_r
\end{cases}
$$

Ma, essendo $\{ \underline{v}_1, \dots, \underline{v}_r, \underline{h}_1, \dots, \underline{h}_{t - r} \}$ e $\{ \underline{v}_1, \dots, \underline{v}_r, \underline{k}_1, \dots, \underline{k}_{s - r} \}$ linearmente indipendenti, allora si ha che: $t'_1 = \dots = t'_r = t_1 = \dots = t_r = \alpha_1 = \dots = \alpha_{t - r} = \beta_1 = \dots = \beta_{s - r} = 0$

#### Note

All'inizio ho dimostrato che $w$ appartiene sia a $V$ (ovvio) che ad $H$ che a $K$, di conseguenza appartiene a $H \cap K$.

### 1.15 Terza proposizione senza nome (lez. 16 pag. 2)

Siano $\{ \underline{v}_1, \dots, \underline{v}_r \}$ autovettori relativi ad autovalori distinti $\lambda_1, \dots, \lambda_r \in \mathbb{K}$ relativi ad unamatrice $A \in \mathcal{M}_\mathbb{K} (n, n)$ allora ${\underline{v}_1, \dots, \underline{v}_r}$ è linearmente indipendente.

#### Dimostrazione

Caso $r = 1$.

Essendo $\underline{v}_1$ un autovettore allora $\underline{v}_1 = 0$ è quindi $\underline{v}_1$ è linearmente indipendente.

Caso $r = 2$

Siano $\underline{v}_1, \underline{v}_2$ autovettori rlativi a $\lambda_1, \lambda_2$.
Supponiamo per assurdo che esista una loro combinazione lineare uguale al vettore nullo: $\tau_1 \underline{v}_1 + \tau_2 \underline{v}_2 = \underline{0} \quad \tau_1 \ne 0, \tau_2 \ne 0$ allora $\underline{v}_1 = -\frac{\tau_2}{\tau_1} \underline{v}_2$.
Applichiamo $A$ ad entrambi i membri della precedente equazione:

$$
A \cdot \underline{v}_1 = \lambda_1 \underline{v}_1 = -\lambda_1 \frac{\tau_2}{\tau_1}\underline{v}_1 \\
A \cdot \underline{v}_1 = A(-\frac{\tau_2}{\tau_1} \underline{v}_2) = -\lambda_2 \frac{\tau_2}{\tau_1} \underline{v}_2
$$

Sottraendo membro a menbro ottengo $\underline{0} = \underbrace{(\lambda_2 - \lambda_1)}_{\ne 0} \frac{\tau_2}{\tau_1} \underbrace{\underline{v}_2}_{\ne \underline{0}} \implies \frac{\tau_2}{\tau_1} = 0$ assurdo.

Procediamo per induzione

Supponiamo di aver dimostrato la proposizione per $r \ge 2$, voglio dimostrarla per $r + 1$ vettori.

Siano $\underline{v}_1, \dots, \underline{v}_r, \underline{v}_{r+1}$ autovettori relativi a $\lambda_1, \dots, \lambda_r, \lambda_{r + 1}$ autovalori distinti.
Supponiamo pr assurdo che siano linearmente dipendenti.
A meno di riordinare i vettori, esistono $\tau_1, \dots, \tau_r \in \mathbb{K}$ non tutti nulli tali che $\underline{v}_{r + 1} = \tau_1 \underline{v}_1 + \dots + \tau_r \underline{v}_r$.
Applichiamo $A$ ad ambo i membri:

$$
A(\underline{v}_{r + 1}) = \lambda_{r + 1} \underline{v}_{r + 1} = \lambda_{r + 1} \tau_1 \underline{v}_1 + \dots + \lambda_{r + 1} \tau_r \underline{v}_r \\
A(\underline{v}_{r + 1}) = A(\tau_1 \underline{v}_1 + \dots + \tau_r \underline{v}_r) = \lambda_1 \tau_1 \underline{v}_1 + \dots + \lambda_r \tau_r \underline{v}_r
$$

Sottraendo membro a membro otteniamo che $\underline{0} = (\lambda_{r + 1}) \tau_1 \underline{v}_1 + \dots + (\lambda_{r + 1} - \lambda_r) \tau_r \underline{v}_r$ ma, poiche $\underline{v}_1, \dots, \underline{v}_r$ sono linearmente indipendenti per ipotesi allora si ha che $0 = \underbrace{\lambda_{r + 1} - \lambda_1}_{\ne 0} \tau_1 = \dots = \underbrace{(\lambda_{r + 1} - \lambda_r)}_{\ne 0} \tau_r \implies \tau_1 = \dots = \tau_r = 0 \implies underline{v}_{r + 1} = \underline{0}$ assurdo.

Quindi $\underline{v}_1, \dots, \underline{v}_r, \underline{v}_{r + 1}$ non possono essere linearmente dipendenti e dunque sono linearmente indipendenti.

### 1.16 Secondo criterio di diagonalizzabilità (lez. 16 pag. 5)

Sia $A \in \mathcal{M}_\mathbb{K} (n, n)$. $A$ è diagonalizzabile su $\mathbb{K}$ se e solo se il polinomio caratteristico di $A$ ha $n$ radici, contate con la loro molteplicità, in $\mathbb{K}$ ed ogni autovalore di $A$ è regolare.

#### Dimostrazione

Siano $\lambda_1, \dots, \lambda_r$ autovalori distinti di $A$ allora $A$ è diagonalizzabile se e solo se $A$ ammette una base di autovettori quindi se e solo se $M = mg(\lambda_1) + \dots + mg(\lambda_r) = n$.
Osservo che se $M = mg(\lambda_1) + \dots + mg(\lambda_r) = n$ allora $n = mg(\lambda_1) + \dots + mg(\lambda_r) \le ma(\lambda_1) + \dots + ma(\lambda_r) \le n$ da cui

$$
\begin{cases}
  ma(\lambda_1) + \dots + ma(\lambda_r) = n \\
  ma(\lambda_i) = mg(\lambda_i) & \forall i = 1, \dots, r
\end{cases}
$$

Viceversa, se $ma(\lambda_1) + \dots + ma(\lambda_r) = n$ e $ma(\lambda_i) = mg(\lambda_i) \quad \forall i = 1, \dots, r$ allora $M = mg(\lambda_1) + \dots + mg(\lambda_r) = n$

### 1.17 Invarianti per similitudine (lez. 16 pag. 9)

Siano $a, B \in \mathcal{M}_\mathbb{K}(n, n)$ matrici simili allora

1.  le due matric hanno stesso polinomio caratteristico, la stessa traccia, lo stesso determinante e gli stessi autovalori con la stessa molteplicità algebrica
2.  le due matrici hanno lo stesso rango
3.  le due matrici hanno gli stessi autivalori con le stesse molteplicità

#### Dimostrazione

Poiche $A$ è simile a $B$ allora esiste $P \in \mathcal{M}_\mathbb{K}(n, n)$ invertibile tale che $B  =P^{-1}AP$.

Dimostiamo 1.
Si ha che $p_B(\lambda) = \det(B - \lambda \mathbb{I_n}) = \det(P^{-1}AP - \lambda P^{-1}P) = \det(P^{-1} \cdot (A - \lambda \mathbb{I}_n) \cdot p) = \cancel{\det(P^{-1})} \cdot \det(A - \lambda \mathbb{I}_n) \cdot \cancel{\det(P)} = \det(A - \lambda \mathbb{I}_n) = p_A(\lambda)$.

Poichè: $p_A(\lambda) = p_B(\lambda) = (-1)^n \lambda^n + c_1 \lambda^{n - 1} + \dots + c_{n - 1} \lambda + c_n$ si ha che $tr(B) = (-1)^{n - 1} c_1 = tr(A)$, $\det(B) = c_n = \det(A)$ e gli autovalori di $B$ contati con le loro molteplicità algebriche coincidono con glia utovalori di $A$

Dimostriamo 2.

Poichè $A$ e $B$ rappresentano uno stesso endomorfismo $\mathscr{L} \colon \mathbb{K}^n \to \mathbb{K}^n$ rispetto a basi diverse allora $r(\mathscr{L}) = \dim(Im(\mathscr{L})) = r(A) = r(B)$.

Dimostriamo 3.

Dalla relazione $B = P^{-1} A P$ ricaviamo $PB = AP$.
Se $\underline{v}$ è autivalore di $B$ allora $P\underline{v}$ è autovalore di $A$ infatti $A \cdot (P \underline{v}) = (AP) \cdot \underline{v} = (PB) \cdot \underline{v} = P \cdot (B \underline{v}) = \lambda P \underline{v}$.

Viceversa, dalla relazione $B = P^{-1}AP$ ricaviamo $BP^{-1} = P^{-1}A$.
Se $\underline{w}$ è autovettore di $A$ allora $P^{-1} \underline{w}$ è autovettore di $B$ infatti $B \cdot (P^{-1} \underline{w}) = (B P^{-1}) \cdot \underline{w} = (P^{-1} A) \cdot \underline{w} = p^{-1} \cdot (A \underline{w}) = \lambda P^{-1} \underline{w}$.

Dunque se $V_\lambda$ è $V'_\lambda$ sono autospazi di $\lambda$ relativi ad $A$ e $B$ rispettivamente, $\mathscr{L}_P \colon V'_\lambda \to V_\lambda \quad (\underline{v} \mapsto P \cdot \underline{v})$ è isomorfismo.
Quindi $mg^B(\lambda) = \dim(V'_\lambda) = \dim(V_\lambda) = mg^A(\lambda)$

### 1.18 Proiezione ortogonale (lez. 18 pag. 6)

Sia $(V, \lang \cdot, \cdot \rang)$ uno spazio euclideo, sia $\underline{w} \in V$ e sia $H = span(\underline{w})$. Allora si ha che peogni $\underline{v} \in V$

1.  esiste ed è unico $\underline{v}_H \in H$ tale che $\underline{v} - \underline{v}_H \in H^{\perp}$
2.  $\| \underline{v} - \underline{v}_H \| < \| \underline{v} - \underline{w}' \|$ per ogni $\underline{w}' \in H, \underline{w}' \ne \underline{v}_H$
3.  $\underline{v}_H = \frac{\lang \underline{v}, \underline{w} \rang}{\lang \underline{w}, \underline{w} \rang} \underline{w}$

#### Dimostrazione

Mostriamo che esiste $\underline{v}_H \in H$ tale che $\underline{v} - \underline{v}_H \perp \underline{h} \quad \forall \underline{h} \in H$ e che $\underline{v}_H = \frac{\lang \underline{v}, \underline{w} \rang}{\| \underline{w} \| ^2} \underline{w}$.
Poichè $\underline{v}_H \in H$ allora è sufficiente determinare $\hat{x} \in \mathbb{R}$ tali che $\underline{v}_H = \hat{x} \cdot \underline{w}$ e inoltre $\lang \underline{v} - \underline{v}_H, \underline{w} \rang = 0 \implies \lang \underline{v} - \hat{x} \cdot \underline{w}, \underline{w} \rang = 0 \implies \lang \underline{v}, \underline{w} \rang - \hat{x} \lang \underline{w}, \underline{w} \rang = 0 \implies \hat{x} = \frac{\lang \underline{v}, \underline{w} \rang}{\lang \underline{v}, \underline{v} \rang}$.
Quindi $\underline{v}_H$ esiste ed è $\underline{v}_H = \frac{\lang \underline{v}, \underline{w} \rang}{\lang \underline{v}, \underline{v} \rang}$

Osserviamo che $\underline{v} - \underline{v}_H \perp \underline{w} \implies \underline{v} - \underline{v}_H \perp t \cdot \underline{w} \quad \forall t \in \mathbb{R}$.
Mostriamo che $\underline{v}_H$ è unico.
Supponiamo $\underline{v}'_H$ un altro vettore di $H$ tale per cui $\lang \underline{v} - \underline{v}'_H, \underline{h} \rang = 0 \quad \forall \underline{h} \in H$. Allora

$$
\begin{align*}
  \| \underline{v}_H - \underline{v}'_H \| ^2 &= \lang \underline{v}_H - \underline{v}'_H, \underline{v}_H, \underline{v}'_H \rang \\
  &= \lang \underline{v}_H, \underline{v}_H - \underline{v}'_H \rang - \lang \underline{v}'_H, \underline{v}_H - \underline{v}'_H \rang \\
  &= \lang \underbrace{\underline{v}_H}_{\in H}, \underbrace{\underline{v}_H - \underline{v}}_{\perp H} \rang + \lang \underbrace{\underline{v}_H}_{\in H}, \underbrace{\underline{v} - \underline{v}'_H}_{\perp H} \rang - \lang \underbrace{\underline{v}'_H}_{\in H}, \underbrace{\underline{v}_H - \underline{v}}_{\perp H} \rang - \lang \underbrace{\underline{v}'_H}_{\in H}, \underbrace{\underline{v} - \underline{v}'_H}_{\perp H} \rang \\
  &= 0 \implies \| \underline{v}_H - \underline{v}'_H \| ^2 = 0 \implies \underline{v}_H - \underline{v}'_H = \underline{0} \implies \underline{v}_H = \underline{v}'_H
\end{align*}
$$

Infine domostro le proprietà di minima distanza.

$\forall \underline{h} \in H$ si ha che $\| \underline{v} - \underline{h} \| ^2 = \| \underline{v} - \underline{v}_H + \underline{v}_H - \underline{h} \| ^2$ che, per il teorema di Pitagola è pari a $\| \underline{v} - \underline{v}_H \| ^2 + \| \underline{v}_H - \underline{h} \| ^2 \ge \| \underline{v} - \underline{v}_H$

### 1.19 Disuguaglianza di Schwarz (lez. 18 pag. 7)

Sia $(V, \lang \cdot, \cdot \rang)$ uno spazio euclideo, dati $\underline{v}, \underline{w} \in V$ allora $| \lang \underline{v}, \underline{w} \rang | \le \| \underline{v} \| \| \underline{w} \|$.
Inoltre vale l'uguaglianza se e solo se $\underline{v}$ e $\underline{w}$ sono linearmente dipendenti.

#### Dimostrazione

Sia $\underline{v}_H$ la proiezione ortogonale di $\underline{v}$ su $H = Span(\underline{w})$.

$$
\| \underline{v} \| ^2 = \| \underline{v} - \underline{v}_H + \underline{v}_H \| ^2 = \| \underline{v} - \underline{v}_H \| ^2 + \| \underline{v}_H \| ^2 \ge \| \underline{v}_H \| ^2 \implies \| \underline{v}_H \| ^2 = \left \| \frac{\underline{v}, \underline{w} \rang}{\lang \underline{w}, \underline{w} \rang} \cdot \underline{w} \right \| ^2 = \frac{ |\lang \underline{v}, \underline{w} \rang|^2}{\|  \underline{w}\| ^4} \cdot \|\underline{w}\|^2 \le \|\underline{v}\|^2
$$

Da cui $|\lang \underline{v}, \underline{w} \rang | \le \|\underline{v}\|\|\underline{w}\|$

### 1.20 Disuguaglianza triangolare (lez. 18 pag. 7)

$\| \underline{v} + \underline{w} \| \le \| \underline{v} \| + \| \underline{w} \|$

#### Dimostrazione

$\| \underline{v} + \underline{w} \|^2 = \| \underline{v} \|^2 + \| \underline{w} \|^2 + 2 \lang \underline{v}, \underline{w} \rang \le \| \underline{v} \|^2 + \| \underline{w} \|^2 + 2 \| \underline{v} \|\| \underline{w} \| = (\| \underline{v} \| + \| \underline{w} \|)^2$ da cui la tesi.

### 1.21 Matrici ortogonali rappresentano isometrie (lez. 19 pag. 6)

Sia $U \in \mathcal{M}_\mathbb{R}(n, n)$ allora le seguenti condizioni sono equivalenti:

1. $U$ è ortogonale: $U^T \cdot U = \mathbb{I}_n$
2. $U$ preserva la norma euclidea: $\| U \cdot \underline{x} \| = \| \underline{x} \|$
3. $U$ preserva il prodotto scalare standard di $\mathbb{R}^n$: $\lang U \underline{x}, U \underline{y} \rang = \lang \underline{x}, \underline{y} \rang \quad \forall \underline{x}, \underline{y} \in \mathbb{R}^n$

#### Dimostrazione

- $1. \implies 2$.) $\| U \cdot \underline{x} \|^2 = \lang U \cdot \underline{x}, U \underline{x} \rang = (U \cdot \underline{x})^T \cdot U \cdot \underline{x} = \underline{x}^T \cdot \underbrace{U^T \cdot U}_{= \mathbb{I}_n} \cdot \underline{x} = \underline{x}^T \cdot \underline{x} = \lang \underline{x}, \underline{x} \rang$
- $2. \implies 3.$) Utilizziamo la formula di polarizzazione:
  $$
  \begin{align*}
    \lang U \underline{x}, U \underline{y} \rang &= \frac{1}{2} (\| U \underline{x} - U \underline{y} \|^2 - \| U \underline{x} \|^2 - \| U \underline{y} \|^2) \\
    &= \frac{1}{2} (\| U(\underline{x} - \underline{y}) \|^2 - \| \underline{x} \|^2 - \| \underline{y} \|^2) \\
    &= \frac{1}{2}(\| \underline{x} - \underline{y} \|^2 - \| \underline{x} \|^2 - \| \underline{y} \| ^2) \\
    &= \lang \underline{x}, \underline{y} \rang
  \end{align*}
  $$
- $3. \implies 1.$) Sia $\mathcal{E}_n = \{ \underline{e}_1, \dots, \underline{e}_n \}$ la base canonica di $\mathbb{R}^n$.
  Allora
  $$
  \lang \underline{e}_i, \underline{e}_j \rang = \delta_{ij} = \begin{cases}
    1 & i = j \\
    0 & i \ne j
  \end{cases}
  $$
  Inoltre osserviamo che $\lang U \underline{e}_i, U \underline{e}_j \rang = (U \underline{e}_i)^T \cdot (U \underline{e}_j) = \underline{e}_i^T \cdot U^T \cdot U \cdot \underline{e}_j = [U^TU]_{ij}$.
  Quindi, poiche $U$ preserva il prodotto scalare: $\delta_{ij} = \lang \underline{e}_i, \underline{e}_j \rang = \lang U \underline{e}_i, U \underline{e}_j \rang = [U^TU]_{ij} \implies U^TU = \mathbb{I}_n$.

### 1.22 Ancora un'altra proposizione senza nome (lez. 20 pag. 5)

Sia $V$ uno spazio euclideo di dimensione finita e sia $\mathcal{B}$ una base ortonormale. Un endomorfismo $\mathscr{L} \colon V \to V$ è simmetrico se e solo se $\mathcal{M}_\mathcal{B}^\mathcal{B}(\mathscr{L})$ è simmetrica.

#### Dimostrazione

Sia $\mathcal{B} = \{ \underline{q}_1, \dots, \underline{q}_n \}$ una base ortonormale di $V$. Siano $\underline{v}, \underline{w} \in V$ e $\underline{x}, \underline{y} \in \mathbb{R}^n$ le coordinate di $\underline{v}$ e $\underline{w}$ rispetto a $\mathcal{B}$.
Ricordiamo che $\lang \underline{v}, \underline{w} \rang = \lang X_\mathcal{B}(\underline{v}), X_\mathcal{B}(\underline{w}) \rang_{\mathbb{R}^n} = \lang \underline{x}, \underline{y} \rang _{\mathbb{R}^n}$.
Dunque, posto $A = \mathcal{M}_\mathcal{B}^\mathcal{B}(\mathscr{L})$, si ha che:

$$
\begin{align*}
  & \lang \mathscr{L}(\underline{v}), \underline{w} \rang = \underline{v}, \mathscr{L}(\underline{w}) & \forall \underline{v}, \underline{w} \in V \\
  \iff & \lang X_\mathcal{B}(\mathscr{L}(\underline{v})), X_\mathcal{B}(\underline{w}) \rang_{\mathbb{R}^n} = \lang X_\mathcal{B}(\underline{v}), X_\mathcal{B}(\mathscr{L}(\underline{w})) \rang_{\mathbb{R}^n} & \forall \underline{v}, \underline{w} \in V \\
  \iff & \lang A \cdot X_\mathcal{B}(\underline{v}), X_\mathcal{B}(\underline{w}) \rang_{\mathbb{R}^n} = \lang X_\mathcal{B}(\underline{v}), A \cdot X_\mathcal{B}(\underline{w}) \rang_{\mathbb{R}^n} & \forall \underline{v}, \underline{w} \in V \\
  \iff & \lang A \underline{x}, \underline{y}_{\mathbb{R}^n} \rang = \lang \underline{x}, A \cdot \underline{y} \rang _{\mathbb{R}^n} & \forall \underline{x}, \underline{y} \in \mathbb{R}^n \\
  \iff & \text{A è simmetrica}
\end{align*}
$$

### 1.23 Teorema spettrale (lez. 20 pag. 8)

Sia $(V, \lang \cdot, \cdot \rang)$ uno spazio euclideo, $\dim(V) < +\infty$ e $\mathscr{L} \colon V \to V$ un endomorfismo simmetrico. Allora esiste una base ortonormale di $V$ formata da autovettori di $\mathscr{L}$. In particolare una matrice simmetrica reale è ortogonalmente diagonalizzabile.

#### Dimostrazione

Si procede per induzione sulla dimensione di $V$.

Se $\dim(V) = 1$, un versore $\underline{v} \in V$ costituisce una base ortonormale di $V$ formata da autovettori di $\mathscr{L}$ infatti $\mathscr{L}(\underline{v}) = \underline{w} = \lambda \underline{v}$ essendo $V = Span(\underline{v})$.

Supponiamo il teorema vero per ogni spazio euclideo di dimensione $n-1$.

Sia $V$ spazio euclideo di dimensione $n$ e $\mathcal{B} = \{ \underline{b}_1, \dots, \underline{b}_n \}$ una base ortonormale di V.
Sia $A = \mathcal{M}_\mathcal{B}^\mathcal{B}$ allora A è simmetrica.
Sia $\lambda_1$ uno degli autovalori reali di $A$ e $\underline{q}_1$ uno degli autovettori ad esso associati ($\mathscr{L}(\lambda_1) = \lambda_1 \underline{q}_1$).
Sia $H = (Span(\underline{q}_1))^\perp$. Dimostriamo che possiamo restringere $\mathscr{L}$ ad $H$ ed ottenere un endomorfismo $\mathscr{L} \colon H \to H$.
Dobbiamo dimostrare che se $\underline{v} \perp \underline{q}_1$ allora $\mathscr{L}(\underline{v}) \perp \underline{q}_1$.
Si ha che $\lang \mathscr{L}(\underline{v}), \underline{q}_1 \rang = \lang \underline{v}, \mathscr{L}(\underline{q}_1) \rang = \lambda_1 \lang \underline{v}, \underline{q}_1 \rang = 0$.
Quindi, per ipotesi induttiva, essendo $\mathscr{L}$ un endomorfismo di $H$ e $\dim(H) = n - 1$, esiste una base ortonormale di $H$ formata da autovettori di $\mathscr{L}$: $\{ \underline{q}_2, \dots, \underline{q}_n \}$.
Siccome $H = (Span(\underline{q}_1))^\perp$, si ha che $\{ \underline{q}_1, \underline{q}_2, \dots, \underline{q}_n \}$ una base ortonormale di V formata da autovettori di $\mathscr{L}$.

Se $A \in \mathcal{M}_\mathbb{R}(n, n)$ è una matrice simmetrica allora $\mathscr{L}_A \colon \mathbb{R}^n \to \mathbb{R}^n$ ($\underline{x} \mapsto A \cdot \underline{x}$) è un endomorfismo simmetrico (infatti $A$ è la matrice rappresentativa di $\mathscr{L}_A$ rispetto alla base canonica $\mathcal{E}_n = \{ \underline{e}_1, \dots, \underline{e}_n \}$ che è base ortonormale di $\mathbb{R}^n$).
Dunque per quanto abbiamo appena visto, esiste una base ortonormale di $\mathbb{R}^n$ formata da autovettori di $A$: $\{ \underline{q}_1, \dots, \underline{q}_n \}$.
Poniamo $U = [\underline{q}_1 | \dots | \underline{q}_n]$ ($U$ è ortogonale) allora
$A \cdot U = [A \underline{q}_1 | \dots | A \underline{q}_n] = [\lambda_1 \underline{q}_1 | \dots | \lambda_n \underline{q}_n] = U \cdot diag(\lambda_1, \dots, \lambda_n)$.
Dunque $U^TAU = diag(\lambda_1, \dots, \lambda_n)$ e quindi $A$ è ortogonalmente diagonalizzabile

### 1.24 Autovalori e segno do una forma quadratica reale (lez. 21 pag. 9)

Sia $A \in \mathcal{M}_\mathbb{R}(n, n)$ simmetrica e $q(\underline{x}) = \underline{x}^T \cdot A \cdot \underline{x}$ $\forall \underline{x} \in \mathbb{R}^n$. Allora:

1. Se $\underline{v}$ è autovettore di $A$ allora $q(\underline{v}) = \lambda \| \underline{v} \| ^2$
2. Se $\lambda_{min}$ e $\lambda_{max}$ sono gli autovalori minimo e massimo di $A$ allora $\lambda_{min} \| \underline{x} \|^2 \le q(\underline{x}) \le \lambda_{max} \| \underline{x} \|^2$

#### Dimostrazione

1. $q(\underline{v}) = \underline{v}^T \cdot A \cdot \underline{v} = \underline{v}^T \cdot (\lambda \underline{v}) = \lambda \cdot \underline{v}^T \cdot \underline{v} = \lambda \| \underline{v} \|^2$
2. Per il teorema spettrale esiste $U \in \mathcal{M}_\mathbb{R}(n, n)$ ortogonale tale che $U^TAU = diag(\lambda_1, \dots, \lambda_n) = D \implies A = UDU^T$.
   Si ha che: $q(\underline{x}) = \underline{x}^T \cdot A \cdot \underline{x} = \underline{x}^T \cdot UDU^T \cdot \underline{x} = (U^T \underline{x})^T \cdot D \cdot U^T \underline{x}$.
   Posto $\underline{y} = U^T \underline{x}$, si ha che:
   $$
   \lambda_{min} \| \underline{y} \|^2 \le \underline{y}^T D \underline{y} = \lambda_1 y_1^2 + \dots + \lambda_n y_n^2 \le \| \lambda_{max}\underline{y} \|^2 \\
   \Downarrow \underline{y} = U^T \cdot \underline{x} \\
   \lambda_{min} \| U^T \cdot \underline{x} \| ^2 \le q(\underline{x}) = (U^T \cdot \underline{x}) D (U^T \cdot \underline{x}) \le \lambda_{max} \| U^T \cdot \underline{x} \| ^2 \\
   \Downarrow U \text{ è ortogonale} \\
   \lambda_{min} \| \underline{x} \| ^2 \le q(\underline{x}) \le \lambda_{max} \| \underline{x} \|^2
   $$

### 1.25 Forma canonica di un polinomio di secondo grado (lez. 23 pag. 1)

Dato un polinomio di secondo grado $q(\underline{x}) = \underline{x}^T \cdot A \cdot \underline{x} + 2 \cdot \underline{b}^T \cdot \underline{x} + c$ ove $A \in \mathcal{M}_\mathbb{R}(n, n)$ è simmetrica, $\underline{b} \in \mathbb{R}^n$ e $c \in \mathbb{R}$, data una rototransalzione $F(\underline{y}) = Q \cdot \underline{y} + \underline{v}$, ove $Q \in \mathcal{M}_\mathbb{R}(n, n)$ è ortogonale con $\det(Q) = 1$ e $\underline{v} \in \mathbb{R}^n$, posto $\tilde{q}(\underline{y}) = q(F(\underline{y})) = \underline{y}^T \cdot \tilde{A} \cdot \underline{y} + 2 \cdot \tilde{\underline{b}} \cdot \underline{y} + c$ si ha che

1. gli autovalori e il rango di $A$ e $\tilde{A}$ sono uguali
2. il rango e il determinante di $B$ e $\tilde{B}$ sono uguali

Inoltre, sia $r$ il rango di A e siano $\lambda_1, \dots, \lambda_r$ gli autovalori non nulli di $A$, si ha che

1. se $r(B) = r$ allora esiste una rototranslazione $F(\underline{y}) = Q \underline{y} + \underline{v}$ tali che $\tilde{q}(\underline{y}) = q(F(\underline{y})) = \lambda_1 y_1^2 + \dots + \lambda_r y_r^2$
2. se $r(B) = r + 1$ allora esiste una rototranslazione $F(\underline{y}) = Q \underline{y} + \underline{v}$ tale che $\tilde{q}(\underline{y}) = q(F(\underline{y})) = \lambda_1 y_1^2 + \dots + \lambda_r y_r^2 + \tilde{c}$ con $\tilde{c} \ne 0$
3. se $r(B) = r + 2$ allora esiste una rototranslazione $F(\underline{y}) = Q \underline{y} + \underline{v}$ tale che $\tilde{q}(\underline{y}) = q(F(\underline{y})) = \lambda_1 y_1^2 + \dots + \lambda_r y_r^2 + 2py_{r+1}$ con $p \ne 0$

#### Dimostrazione

> Prima parte

Poichè $\tilde{A} = Q^TAQ$ ove $Q$ è una matrice ortogonale allora $\tilde{A}$ è simile ad $A$ e quindi ha lo stesso rango e gli stessi autovalori di $A$.
Poichè $\tilde{B} = F^TBF$ ove

$$
F = \left[\begin{array}{c|c}
  Q & \underline{v} \\
  \hline
  \underline{0}^T & 1
\end{array} \right] \in \mathcal{M}_\mathbb{R}(n+1, n+1)
$$

è invertibile e $\det(F) = \det(Q) = 1$ si ha che $\tilde{B}$ e $B$ hanno lo stesso rango (ma non è detto che abbiano gli stessi autovalori) e $\det(\tilde{B}) = \det(B)$.

> Seconda parte

La dimostrazione si divide in due casi.

Primo caso.

$A \underline{x} = - \underline{b}$ ammette soluzione (infatti $r(A) = r([A | -\underline{b}])$).
Sia $w$ una soluzione di $A\underline{x} = -\underline{b}$.
Posto $\underline{x} = \underline{y}' + \underline{w}$ si ha $q_1(\underline{y}') = q(\underline{y}' + \underline{v}) = (\underline{y}')^T \cdot A \cdot \underline{y}' + 2\underbrace{(A \underline{w} + \underline{b})}_{= \underline{0}} \cdot \underline{y}' + \underbrace{\underline{w}^T \cdot A \cdot \underline{w}}_{= -\underline{w}^T \cdot \underline{b}} + 2 \underline{b}^t \cdot \underline{w} + c = (\underline{y}')^T \cdot A \cdot \underline{w} + \underline{b}^T \cdot \underline{w} + c$.
Per il teorema spettrale esiste $Q \in \mathcal{M}_\mathbb{R}(n, n)$ ortogonale tale che $Q^TAQ = diag(\lambda_1, \dots, \lambda_r, 0, \dots, 0)$.
Posto $\tilde{q}(\underline{y}) = q_1(Q \cdot \underline{y}) = \underline{y}^T Q^TAQ \underline{y} + \underline{b}^T \cdot \underline{w} + c = \lambda_1 y_1^2 + \dots + \lambda_r y_r^2 + \underbrace{\underline{b}^T \cdot \underline{w} + c}_{= \tilde{c}}$
La rototranslaione per trasformare $q(\underline{x})$ in forma canonica è $F(\underline{y}) = Q \underline{y} + \underline{w}$ ove $\underline{w}$ è una soluzione di $A \underline{x} = - \underline{b}$.
Osserviamo che

$$
\tilde{B} =
\left[
  \begin{array}{c|c}
    \begin{array}{cccccc}
      \lambda_1 &         &           &   &         &   \\
                & \ddots  &           &   &         &   \\
                &         & \lambda_r &   &         &   \\
                &         &           & 0 &         &   \\
                &         &           &   & \ddots  &   \\
                &         &           &   &         & 0 \\
    \end{array}
    & \underline{0} \\
    \hline
    \underline{0}^T & \tilde{c}
  \end{array}
\right]
$$

e quindi $r(\tilde{B}) = r(B) = r$ se e solo se $\tilde{c} = 0$ e $r(\tilde{B}) = r(B) = r + 1$ se e solo se $\tilde{c} \ne 0$.

Secondo caso.

$A \underline{x} = - \underline{b}$ non ammette soluzioni (infatti $r(A) < r([A | -\underline{b}])$).
Sia $H = Col(A)$ e $\underline{b} = \underline{b}_H + \underline{B}_{H^\perp}$.
Poichè $\underline{b}_H \in H = Col(A)$ il sistema $A \underline{x} = -\underline{b}_H$ ammette una soluzione $\underline{w}$.
Consideriamo la translazione $\underline{x} = \underline{y}' + \underline{w}$.
Allora si ha che $q_1(\underline{y}') = q(\underline{y}' + \underline{w}) = (\underline{y}')^T \cdot A \cdot \underline{y}' + 2 (\underbrace{\overbrace{A \cdot \underline{w}}^{= -\underline{b}_H}+ \underline{b}}_{= \underline{b}_{H ^ \perp}})^T \cdot \underline{y}' + \underbrace{\underline{w}^TA\underline{w} + 2 \underline{b}^T \cdot \underline{w} + c}_{= \tilde{c}} = (\underline{y}')^T \cdot A \cdot \underline{y}' + 2 \underline{b}_{H^\perp}^T \cdot \underline{y}' + \tilde{c}$.
Essendo $A$ simmetrica $H^\perp = (Col(A))^\perp = \ker(A^T) = \ker(A)$ quindi $\underline{b}_{H^\perp}$ è la proiezione ortogonale di $\underline{b}$ su $\ker(A)$.
Essendo $\underline{b}_{H^\perp} \in \ker(A)$, $\underline{b}_{H^\perp}$ è un autovettore riferito all'autovalore nullo.
Posto

$$
\underline{b}_1 = \frac{\underline{b}_{h^\perp}}{\| \underline{b}_{h^\perp} \| }
$$

completiamo $\underline{b_1}$ ad una base ortonormale di autovettori di $A$: $\{ \underbrace{\underline{q}_1, \dots, \underline{q}_r}_{\text{Autovettori riferiti a } \lambda_1, \dots, \lambda_r}, \underbrace{\underline{b}_1, \dots, \underline{b}_{n-r}}_{\text{Autovettori riferiti a } 0} \}$ e poniamo $Q = \left[ \begin{array}{c|c|c|c|c|c} \underline{q}_1 & \dots & \underline{q}_r & \underline{b}_1 & \dots & \underline{b}_{n - r}\end{array} \right]$.
Allora $Q^TAQ = diag(\lambda_1, \dots, \lambda_r, 0, \dots, 0)$.consideriamo la rototranslazione $\underline{y}' = Q \underline{z}$, si ha che $q_2(\underline{z}) = q_1(Q \cdot \underline{z}) = \underline{z}^T Q^T A Q \underline{z} + 2 \underline{b}_{H^\perp}^T \cdot Q \cdot \underline{z} + \tilde{c} = \lambda_1 z_1^2 + \dots + \lambda_r z_r^2 + 2 \| \underline{b}_{H^\perp} \| \cdot z_{r+1} + \tilde{c}$.
Infine, ponendo $z_1 = y_1, \dots, z_r = y_r, z_{r+1} = y_{r+1}-\frac{\tilde{c}}{2 \| \underline{b}_{H^\perp} \| }, z_{r+2} = y_{r+2}, \dots, z_n = y_n$, otteniamo $\tilde{q}(y) = q_2(y_1, \dots, y_r, y_{r+1} - \frac{\tilde{c}}{2 \| \underline{b}_{H^\perp} \| }, y_{r+2}, \dots, y_n) = \lambda_1 y_1^2 + \dots + \lambda_r y_r^2 + 2 \| \underline{b}_{h^\perp} \| y_{r+1}$.
La rototranslazione è data da

$$
F(\underline{y}) = Q \cdot \underline{y} - \frac{\tilde{c}}{2 \| \underline{b}_{H^\perp} \|^2}\underline{b}_{H^\perp} + \underline{w}
$$

ove $w$ è una soluzione di $A \underline{x} = - \underline{b}_H$ e $\tilde{c} = \underline{w}^T A \underline{w} + 2 \underline{b}^T \cdot \underline{w} + c$.
In questo caso

$$
\left[
  \begin{array}{ccccccc|c}
    \lambda_1 &         &           &         &         &         &         & 0       \\
              & \ddots  &           &         &         &         &         & \vdots  \\
              &         & \lambda_2 &         &         &         &         & 0       \\
              &         &           & 0       & \dotsm  & \dotsm  & \dotsm  & p       \\
              &         &           & \vdots  & 0       &         &         & 0       \\
              &         &           & \vdots  &         & \ddots  &         & \vdots  \\
              &         &           & \vdots  &         &         & 0       & 0       \\
    \hline
    0         & \dots   & 0         & p       & 0       & \dots   & 0       & 0       \\
  \end{array}
\right]
$$

e quindi $r(\tilde{B}) = r(B) = r + 2$

---
title: "Riassunto di Analisi I"
author: "Andrea Oggioni"
---

# Riassunto di Analisi I

## Insiemi numerici

### Insieme N

- N contiene tutti i numeri naturali (interi senza segno)
  - $\mathbb{N} = \{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, \dots\}$
- In N è definita una **relazione d'ordine**:
  - $\exists j \in \mathbb{N} : m + j = n \implies n > m$
  - Proprietà:
    - Riflessiva
    - Antisimmetrica
      - $N \ge M, M \ge N \implies N = M$
    - _proprietà senza nome_:
      - $n \ge m \implies n + j \ge m + j$
- Operazioni definite sull'insieme:
  - Somma:
    - Proprietà:
      - Associativa
      - Commutativa
      - Ammette elemento neutro: 0
  - Prodotto
    - Proprietà:
      - Commutativa
      - Associativa
      - Distributiva rispetto alla somma
      - Ammette elemento neutro: 1
- N è anche definito da una **definisione assiomatica** (di Peano), composta dalle seguenti parti:
  - Un oggetto che si chama Zero (Z) è un numero
  - Ogni numero (N) ha un successivo (N')
  - Z non è il successivo di nessun numero
  - Se due numeri hanno lo stesso successivo allora sono lo stesso numero
  - Il successivo di una somma di due numeri è uguale alla somma di uno dei due numeri con il successivo dell'altro
  - **Postulato d'induzione completa**:
    - $\begin{cases} \text{M è una classe di numeri} \\ \Z \in M \\ n \in M \implies n' \in M \end{cases} \implies \text{M contiene tutti i numeri}$

### Principio d'induzione

Serve per dimostrare proposizioni _per induzione_ e si attua in due step [data una preposizione P]:

1. Dimostro che P(0) è vera
2. Supponendo che P(n) sia vera, dimostro che P(n+1) è vera

Per esempio, voglio dimostrare che: $\sum_{k=0}^n k = \frac{n(n+1)}{2}$
Prima dimostro che la preposizione è vera con n = 0: $P(0): \sum_{k=0}^0 k = \frac{0(0+1)}{2} = 0$
Poi, supponendo che sia vera per n, la dimostro per n+1: $P(n+1): \sum_{k=0}^{n+1} k = \sum_{k=0}^n k + (n+1) = \frac{n(n+1)}{2} + (n+1) = \frac{n(n+1) + 2(n+1)}{2} = \frac{(n+1)(n+2)}{2} = \frac{(n+1)((n+1)+1)}{2}$

### Insieme Z

- Contiene tutti i numeri interi con segno:
  - $\mathbb{Z} = \{0, \pm 1, \pm 2, \dots \}$
- **Relazione di equivalenza**:
  - Due numeri relativi sono uguali se il segno è uguale e il valore assoluto (numero senza segno) dei numeri è uguale
- **Relazione d'ordine**:
  - Se il segno è dierso, il numero con segno positivo è maggiore, se il segno è uguale, allora se il segno è negativo, il numero con valore assoluto maggiore è il minore, il contrario se i due numeri hanno segno positivo.
- Le operazioni definite sull'insieme Q sono le stesse definite sull'insieme N ma si tiene conto del segno

### Insieme Q

- Contiene tutte le frazioni
  - $Q = \left\{ \frac{n}{m} : n \in \Z, m \in \N^+ \right\}$
- **Relazione d'ordine**:
  - $\frac{n}{m} \gt \frac{n'}{m'} \iff n \times m' \gt n' \times m$
  - Idem per &lt;
- **Relazione di equivalenza**:
  - $\frac{n}{m} = \frac{n'}{m'} \iff n \times m' = n' \times m$
- Operazioni definite sull'insieme:
  - Sia somma che prodotto hanno le stesse proprietà definite per N e Z
  - $\frac{n}{m} + \frac{h}{k} = \frac{n \times k + h \times m}{m \times k}$
  - $\frac{n}{m} \times \frac{h}{k} = \frac{n \times h}{m \times k}$
  - In più, ogni elemento ha un proprio inverso:
    - $\frac{n}{m} \times \frac{m}{n} = 1 \qquad (con\,n \ne 0)$

### Irrazionalità della radice di 2

Per dimostrare che $sqrt(2)$ è irrazionale, bisogna procedere per assurdo, ma prima serve conoscere una definizione di numeri pari e dispari:
$$n \text{ è pari} \iff n = 2k \\ n \text{ è dispari} \iff n = 2k + 1$$
Ora, pre assurdo, supponiamo che $sqrt(2)$ sia un numero razionale, quindi
$$\sqrt{2} = \frac{n}{m} \qquad \text{(con n e m primi tra loro)}$$
ma allora
$$2 = \frac{n^2}{m^2} \implies m^2=2n^2$$
e quindi vuol dire che m è pari e, siccome m ed n sono primi tra loro, ciò vuol dire che n è dispari. Ma allora, riscrivendo m in funzione di k abbiamo che
$$m = 2k$$
e, di conseguenza,
$$\begin{align*} m^2 &= 2n^2 \\ (2k)^2 &= 2n^2 \\ 4k^2 &= 2n^2 \\ 2k^2 &= n^2 \end{align*}$$
il che significherebbe che n è pari, ma questo va contro l'ipotesi, per cui la radice di 2 è irrazionale.

### Insieme R

Un numero reale è un qualsiasi numero razionale o meno
$$R = \left \{ \frac{n}{m} : n \in \Z, m \in \N^+ \} \cup \{ \sqrt{2}, \pi, e, \dots \right \}$$
Le operazioni, le loro proprietà e le varie relazioni sono le stesse di Q e Z.
Dato un insieme A tale che
$$ A \subset R $$
allora A ammette una serie di numeri interessanti:
$$ a^* \in A \text{ si dice massimo di A se } a^* \ge a \,\forall a\in A $$
$$ a^* \in A \text{ si dice minimo di A se } a^* \le a \,\forall a\in A $$
Se A è superiormente limitato, si dice **Estremo superiore di A** il minimo dei suoi maggioranti, e, se A è inferiormente limitato, si dice **Estremo inferiore di A** il massimo dei suoi minoranti (a patto che A abbia maggioranti/minoranti).
$$
\text{Se A è superiormente limitato, un maggiorante di A è ogni numero } k \in \R : k \ge a \forall a \in A \\
\text{Se A è inferiorimente limitato, un minorante di A è ogni numero } k \in \R : k \le a \forall a \in A
$$
**Teorema di completezza di R**: in R, l'estremo superiore di un insieme superiormente limitato esiste sempre. Idem per l'estremo inferiore.

### Topologia in R

Se $A \subset R$ allora

- $x_0 \text{ è un punto interno di A se } \exist \delta : (x_0 - \delta, x_0 + \delta) \subset A$
- $x_0 \text{ è un punto esterno di A se } \exist \delta : (x_0-\delta, x_0+\delta) \cap A = \emptyset$
- $x_0 \text{ viene detto di frontiera se } \forall \delta \in (x_0 - \delta, x_0 + \delta) \text{ cadono sia punti interni che punti esterni di A}$
- $x_0 \text{ viene detto isolato se } \exist \delta : (x_0 - \delta, x_0 + \delta) \cap A = \{x_0\}$

Questo significa che se abbiamo un insieme $A = [1, 3) \cup {5}$ allora 

|  X =  |             caratteristica              |
| :---: | :-------------------------------------: |
|   2   |                è interno                |
|   1   |             è di frontiera              |
|   3   |             è di frontiera              |
|   4   |                è esterno                |
|   5   | è isolato (e quindi anche di frontiera) |
| (1,3) |                è interno                |

L'insieme A si dice aperto se ogni punto è interno (non ha punti di frontiera), quindi quando è espresso nella forma $A = (a, b)$ mentre si dice chiuso se contiene tutti i suoi punti di frontiera (l'insieme dell esempio precedente non contiene ne a ne b che sono i suoi punti di frontiera)

- $\text{Se A è chiuso, allora } A^c \text{ è aperto e viceversa}$
- Esistono insiemi che non sono ne aperti ne chiusi: $A = [a, b)$
- Esistono insiemi sia aperti che chiusi: $A = \R \\ A = \emptyset$
- Dato un insieme A, la **frontiera di A** si indica con $ \partial A$
- Si chiama **chiusura di A** $A \cup \partial A$
- Si chiama **parte interna di A** $\bar A = \left \{\text{l'insieme dei punti interni di A} \right \}$

Di seguito alcuni esempi per chiarire:
$$
A = (a, b) \\
A \cup \partial A = [a, b] \\
\overline {A \cup \partial A} = (a, b)
$$
$$
A = (0, 1) \cup (1, 3) \\
A \cup \partial A = [0, 3] \\
\overline {A \cup \partial A} = (0, 3)
$$
$$
A = [0, 2] \cup {3} \\
\bar A = (0, 2) \\
\bar A\cup \partial \bar A = [0, 2]
$$
Se chiudo e riapro un insieme, potre non arrivare all'insieme di partenza (è più grande)
Se Apro e richiudo un insieme, potre non arrivare all'insieme di partenza (è più piccolo, perde i punti isolati)
| Insieme       | Apertura/chiusura |
| ------------- | ----------------- |
|               |                   |
| (3, +&infin;) | aperto            |
| [3, +&infin;) | chiuso            |
Definiamo "intorno di un punto" come ogni intervallo aperto a cui appartiene il punto stesso.
$$\char"1D4B0 (x_0) = \big \{ \text{Ogni intervallo aperto che contiene } x_0 \big \}$$
Ad esempio
$$\char"1D4B0 (27) = \R, (26.8, 27.1), (-1, 31)$$
Definiamo **chiusura di R** come
$$ \R \cup \partial \R = \R \cup \{\infin, -\infin\} $$
Si dice **intorno di &infin;** qualsiasi intervallo del tipo
$$ \char"1D4B0 (\infin) = (a, 0) \qquad \forall a \in \R $$
**Punto di accumulazione**:
$$x_0 \text{ è un punto di accumulazione per A se in ogni } \char"1D4B0 (x_0) \text{ cadono infiniti punti di A}$$
Ad esempio, se
$$ A = (0, 1] $$
i punti di accumulazione possono essere
$$ 0, 1, \frac{1}{3}, \frac{1}{2}, \dots $$
Esempio finale:
$$ A = \bigg\{\frac{1}{n} : n \in \N, n \gt 0 \bigg \} \subset \R $$

- A non ha punti interni
- A ha soltanto punti isolati
- A ha 0 come unico punto di accumulazione

### Cardinalità di insiemi finiti, numerabilità di Q e non numerabilità di R

La cardinalità di un insieme è il numero di elementi che contiene.
Se un insieme è infinito, però non è sempre facile dire quanti elementi contiene: come posso dire se N ha più o meno elementi di R?
Entrambi hanno infiniti elementi, ma il tipo di infinito è diverso: esistono infiniti numerabili e infiniti non numerabili.
Se posso associare a ogni elemento di un insieme un numero naturale, allora l'insieme è numerabile, altimenti non lo è.
N, Z, e Q sono numerabili, infatti posso associare ad ogni loro un numero naturale:
$$
N: \{(0, 0), (1, 1), (2, 2), (3, 3), \dots\} \\
Z: \{(0, 0), (1, 1), (2, -1), (3, 2), (4, -2), \dots\} \\
$$
Per Q, la faccenda è un po' più complicata, ma basta seguire il concetto illustrato nella seguente immagine:
![](https://www.competenzamatematica.it/wp-content/uploads/2019/12/Schermata-2019-12-28-alle-11.00.25-300x270.png)
In questo modo, per ogni elemento di Q, posso associare un numero naturale.

#### Dimostrazione della non numerabilità di R

Dimostriamo prima che in R ci sono tanti elementi quanti ce ne sono in (0, 1) e successivamente che (0, 1) non è numerabile (di conseguenza, visto che contengono lo stesso numero di elementi, R non è numerabile).
Troviamo una funzione biiettiva che associa ad ogni elemento di R un solo numero di (0, 1).
$$
f : R \to (0, 1) \\
\text{La funzione ritorna la coordinata X del punto di intersezione tra una semicorconferenza di raggio } \frac{1}{2} \text{ e di centro } \bigg (\frac{1}{2}, \frac{1}{2} \bigg) \text{ con un segmento che parte dallo stesso punto e ha come punto finale il punto di coordinate } (r, 0) \text{ con r parametro della funzione}
$$
[Qui esempio pratico](https://www.geogebra.org/calculator/uj7fgxqm)
Ora dimostriamo che (0, 1) non è numerabile.
La dimostrazione avviene per assurdo: supponiamo che sia possibile numerare tutti gli elementi di (0, 1) e iniziamo a scriverne qualcuno:
$$
0.1324 \dots \\
0.5000 \dots \\
0.1298 \dots \\
0.3328 \dots \\
$$
Ignorando lo zero prima del punto decimale, del primo numero prendiamo la prima cifra, del secondo la seconda e così via, poi vi sommiamo uno (se il risultato fosse 10, allora consideriamo solo lo 0)
$$
0.1324 \dots \to 1+1 = 2\\
0.5000 \dots \to 0+1 = 1\\
0.1298 \dots \to 9+1 = 0\\
0.3328 \dots \to 8+1 = 9\\
$$
Ora alla lista aggiungiamo un nuovo numero composto da zero virgola tutte le nuove cifre che abbiamo trovato $(0,2109)$ e notiamo che non è presente nella lista (infatti è impossibile che sia presente nella lista, c'è sempre almeno una cifra differente da tutti i numeri già presenti).
Se ripetiamo il processo infinite volte, non finiremo mai, quindi $(0, 1)$ non è numerabile.
Siccome $(0, 1)$ non è numerabile ed è possibile associare ad ogni elemento di $(0, 1)$ un elemento di R e civecersa, allora anche R non è numerabile.

### Numeri complessi
I numeri complessi numeri basati sull'esistenza del numero $i$ chiamato anche **Unità Immaginaria**.
$$ 
i^2 = -1 \\
i = sqrt(-1) \\
$$
$$
Z \in \char"2102 \iff Z = a + ib \qquad \text{ con } a, b \in \R \\
Re(Z) = a \\
Im(Z) = b \\
$$
Esistono varie forme di numeri complessi:

- Forma esponenziale: $Z = r e^{i \theta}$
- Forma trigonometrica: $Z = r (\cos \theta + i \sin \theta)$
  - $r$ è detto modulo di $Z$
  - $\theta$ è detto argomento di $Z$
- Forma algebrica: $Z = a + ib$

Ogni numro ha anche un coniugato, che è il numero complesso ottenuto invertendo il segno della parte immaginaria:
$$
\text{Se } Z = a + ib \implies \overline{Z} = a - ib \\
$$

Si può trasformare un numero in ciascuna delle forme di cui sopra in ogni altra forma:

1. Da algebrica a trigonometrica:
$$
r = sqrt(a^2 + b^2) \\
\theta = \begin{cases}
  arctan ( \frac{b}{a} ) & x > 0 & \text{I e IV quadrante} \\
  arctan ( \frac{b}{a}) + \pi & x < 0 & \text{II e III quadrante} \\
  \frac{\pi}{2} & x = 0, y > 0 & \text{Asse Y tra il I e il II quadrate} \\
  -\frac{\pi}{2} & x = 0, y < 0 & \text{Asse Y tra il III e il IV quadrante} \\
  \text{Indeterminato} & x = 0, y = 0 & \text{Origine} \\
\end{cases}
$$
  - Nota: si può fare ad'occhio , senza memorizzare tutte le condizioni di $\theta$: se sta tra il III e IV quadrante, basta callcolare $arctan(\frac{b}{a})$ e aggiungere $\pi$. Per i casi particolari (assi e origine) è ovvio.
2. Da trigonometrica/esponenziale a algebrica:
  - Basta calcolare $cos(\theta)$ e $sin(\theta)$ e moltiplicare per $r$.
3. Da algebrica/trigonometrica a esponenziale:
  - Basta calcolare $r$ e $\theta$ come nella forma trigonometrica e scriverli nella forma $r \times e^{i\theta}$
Le varie forme servono per gestire meglio i calcoli. Di seguito formule per le varie operazioni con le varie forme (e, dove utile, anche la descrizione di cosa succede al grafico)
$$
\text{Forma algebrica (Comoda per addizioni, sottrazioni, moltiplicazioni e divisioni)} \\
(a + ib) + (c + id) = (a + c) + i(b + d) \\
(a + ib) \times (c + id) = (ac - bd) + i(ad + bc) \\
\frac{a + ib}{c + id} = \frac{(a + ib)(c - id)}{(c + id)(c - id)} = \frac{(ac + bd) + i(bc - ad)}{c^2 + d^2} \qquad \text{Notare che la divisione è impossibile se $c = d = 0$} \\
$$
$$
\text{Forma trigonometrica (Comoda per moltiplicazioni, divisioni, radici e potenze)} \\
\rho(\cos \theta + i \sin \theta) \times r(\cos \alpha + i \sin \alpha) = \rho r (\cos (\theta + \alpha) + i \sin (\theta + \alpha)) \\
\left[ \rho(\cos \theta + i \sin \theta) \right]^n = \rho^n (\cos n \theta + i \sin n \theta) \\
$$

#### Dimostrazione del prodotto di due numeri complessi

Dati due numeri complessi
$$
Z = \rho(\cos \theta + i \sin \theta) \\
W = r(\cos \alpha + i \sin \alpha) \\
$$
allora
$$
\begin{align*}
  Z \times W = \rho(\cos \theta + i \sin \theta) \times r(\cos \alpha + i \sin \alpha) &= \rho r \times (\cos \theta + i \sin \theta) \times (\cos \alpha + i \sin \alpha) \\
  &= \rho r \times(\cos \theta \cos \alpha + i \cos \theta \sin \alpha + i \sin \theta \cos \alpha - \sin \theta \sin \alpha) \\
  \text{Riordinando i vari seni e coseni:} \qquad &= \rho r \times \left[ (\cos \theta \cos \alpha - \sin \theta \sin \alpha) + (\sin \theta \cos \alpha + \cos \theta \sin \alpha) \right] \\
  &= \rho r \times (\cos (\theta + \alpha) + i \sin (\theta + \alpha)) \\
\end{align*}
$$

#### Radici di un numero complesso

Dato un numero complesso $Z = \rho(\cos \theta + i \sin \theta)$ e $W = r(\cos \alpha + i \sin \alpha)$ allora se $W^n = Z$ allora
$$r^n(\cos n \alpha + i \sin n \alpha) = \rho(\cos \theta + i \sin \theta) $$
e quindi
$$
\begin{cases}
r^n &= \rho \\
n \alpha &= \theta + 2k \pi \qquad \text{con } k \in \mathbb{N} \\
\end{cases} \implies
\begin{cases}
r &= \sqrt[n]{\rho} \\
\alpha &= \frac{\theta}{n} + \frac{2k \pi}{n} \qquad \text{con } 0 \le k \le n-1, k \in \mathbb{N} \\
\end{cases}
$$
Per esempio
$$
\begin{align*}
  W = \sqrt[4]{-4} &= \sqrt[4]{4(\cos \pi + i \sin \pi)} \\
  &= \begin{cases}
    r &= \sqrt[4]{4} = \sqrt{2}\\
    \alpha &= \frac{\pi}{4} + \frac{2k \pi}{4} \qquad \text{con } k = 0, \dots, 3 \\
  \end{cases}
\end{align*}
$$
Dopo aver calcolato i vari $r$ (ce ne possono essere più di uno) e i vari $\theta$, prendo tutte le combinazioni valide dei vari $r$ e $\theta$ e le scrivo:
$$
\begin{align*}
  k = 0, \quad & \alpha = \frac{\pi}{4} & \implies W_0 &= \sqrt{2}(\cos \frac{\pi}{4} + i \sin \frac{\pi}{4}) \\
  k = 1, \quad & \alpha = \frac{3 \pi}{4} & \implies W_1 &= \sqrt{2}(\cos \frac{3 \pi}{4} + i \sin \frac{3 \pi}{4}) \\
  k = 2, \quad & \alpha = \frac{5 \pi}{4} & \implies W_2 &= \sqrt{2}(\cos \frac{5 \pi}{4} + i \sin \frac{5 \pi}{4}) \\
  k = 3, \quad & \alpha = \frac{7 \pi}{4} & \implies W_3 &= \sqrt{2}(\cos \frac{7 \pi}{4} + i \sin \frac{7 \pi}{4}) \\
\end{align*}
$$
Di seguito il disegno delle radici di $-4$: [Geogebra](https://www.geogebra.org/calculator/uxmcb4dr)

Note:
  - Se $r = 0$ allora è solo l'origine, non serve combinarlo con i vari theta
  - Se $r < 0$ allora lo scarto

#### Elevazione ad un numero complesso

Se $x \in \R$ allora
$$
e^x = \lim_{n \to \infty} \left( 1 + \frac{x}{n} \right)^n
$$
Il teorema di monotonia assicura che il limite esiste infatti la funzione è limitata e ammette limite superiore.
Cosa succede se $x \in \mathbb{C}$? [Nel seguente procedimento, al posto di $x$ verrà utilizzato $Z$ per evitare confusione]
$$
Z = x + iy \in \mathbb{C}\\
e^Z = \lim_{n \to \infty} \left( 1 + \frac{Z}{n} \right)^n = \lim_{n \to \infty} \left( 1 + \frac{x + iy}{n} \right) \\
\text{Chiamo } W_n = \frac{x + iy}{n} = \left( 1 + \frac{x}{n} \right) + i \left( \frac{y}{n} \right) \\
|w_n|^2 = \left( 1 + \frac{x}{n} \right)^2 + \left( \frac{y}{n} \right)^2 = 1 + \frac{2x}{n} + \frac{x^2}{n^2} + \frac{y^2}{n^2} \\
\implies |W_n| = \sqrt{1 + \frac{2x}{n} + \frac{x^2 + y^2}{n^2}} \\
$$
Con $n \to \infty$ allora $W \to 1$
$$
\arg{W_n} = \arctan{\frac{\frac{y}{n}}{1 + \frac{x}{n}}} = \arctan{\frac{y}{n + x}} \\
$$
Ora riscrivo il tutto in forma trigonometrica
$$
W_n^n = \left( \sqrt{1 + \frac{2x}{n} + \frac{x^2 + y^2}{n^2}} \right)^n \left( \cos \left( n \arctan{\frac{y}{n + x}} \right) + i \sin \left( n \arctan{\frac{y}{n + x}} \right) \right) \\
$$
Quindi 
$$
\begin{align*}
  e^Z = \lim_{n \to \infty} W_n^n &= \lim_{n \to \infty} \left[ \left( \sqrt{1 + \frac{2x}{n} + \frac{x^2 + y^2}{n^2}} \right)^n \left( \cos \left( n \arctan{\frac{y}{n + x}} \right) + i \sin \left( n \arctan{\frac{y}{n + x}} \right) \right) \right] \\
  &\sim \lim_{n \to \infty} \left[ \left( 1 + \frac{2x}{n} + \frac{x^2 + y^2}{n^2} \right) ^ \frac{n}{2} \left( \cos y + i \sin y \right) \right] \\
  &\sim \lim_{n \to \infty} \left[ e^{\frac{n}{2} \ln \left( 1 + \frac{2x}{n} + \frac{x^2 + y^2}{n^2} \right) } \left( \cos y + i \sin y \right) \right] \\
  &\sim \lim_{n \to \infty} \left[ e ^ {\frac{n}{2} \times \frac{2x}{n}} \left( \cos y + i \sin y \right) \right] \\
  &= e^x \left( \cos y + i \sin y \right) \\
\end{align*}
$$

Di seguito alcuni esempi:
- $e^{i \theta} = \cos \theta + i \sin \theta$
- $e^{3 + 2i} = e^3 \left( \cos 2 + i \sin 2 \right) = e^3 \cos 2 + e^3 i \sin 2$

#### Teorema fondamentale dell'algebra

In $\mathbb{C}$, ogni equazione algebrica di grado $n$ ha esattamente $n$ soluzioni. (In altre parole, ogni polinomio di grado $n$ ha esattamente $n$ radici.)

#### Ruffini (sia reale che complesso)

## Successioni

Una successione è una funzione definita in $\N$ con valori in $\R$.
$$
\begin{align*}
f: \N &\to \R \\
n &\mapsto f(n) = a_n
\end{align*}
$$
La scrittura precisa e pignola è la seguente: $\left\{ a_n \right\}_0^{+\infty}$
Esempi:
- $a_n = \frac{1}{n} \implies 1, \frac{1}{2}, \frac{1}{3}, \dots$
- $b_n = s^n \implies 1, 2, 4, \dots$
- $c_n = \left( -1 \right)^n \implies 1, -1, 1, \dots$

### Successioni monotone

Una successione è monotona crescente se $a_n \leq a_{n+1}$ per ogni $n \in \N$.
Per le monotone decrescenti, strettamente crescenti e strettamente decrescenti vale la stessa regola ma cambiando il '$\leq$' con '$\geq$', '$<$' e '$>$'.

### Successioni limitate

Legati alle successioni ci sono alcuni numeri notevoli:
- $\sup \left\{ a_n \right\}$: il minimo dei maggioranti della successione
- $\inf \left\{ a_n \right\}$: il massimo dei minoranti della successione
- $\max \left\{ a_n \right\}$: il massimo della successione
- $\min \left\{ a_n \right\}$: il minimo della successione

Si dice che $a_n$ tende al limite $l$ (quindo $\lim_{n \to \infty} a_n = l$) se
$$
\forall \varepsilon > 0 \exists n_\varepsilon : n \geq n_\varepsilon, \left| a_n - l \right| < \varepsilon
$$
In italiano: esite un numero arbitrariamente piccolo ($\varepsilon$) tale per cui, dopo un certo numero di elementi della successione, la differenza tra il limite e gli elementi della successione è minore di quel numero.

### Definizione topologica di successione

Oltre alla definizione metrica (quella espressa qui sopra) esiste anche una definizione topologica equivalente:
$$
\lim_{n \to \infty} a_n = L, \quad L \in \R  \iff \forall \char"1D4B0 \left( L \right) \text{ definitivamente } a_n \in \char"1D4B0 \left( L \right)
$$
In italiano: esite un intorno di $L$  che contiene infiniti valori di $a_n$.

### Regolarità di una successione

Una successione è regolare se ammette un limite finito o infinito.
Una successione è irregolare se non ammette un limite.
Ad esempio, la successione $a_n = \frac{1}{n}$ è regolare mentre $b_n = \sin n$ non lo è.

### Teorema di monotonia

Se $a_n$ è una successione monotona crescente e superiormente limitata allora ammette limite.
Se $a_n$ è una successione monotona decrescente e inferiormente limitata allora ammette limite.

#### Dimostrazione del primo punto (la dimostrazione del secondo è analoga)

Sia $a_n$ una successione monotona crescente e superiormente limitata.
$$
S = Sup \left \{ a_n \right \} \\
\forall \varepsilon > 0, \, a_{n^*} \in \left( S - \varepsilon, S \right] \\ \text{(Per ogni valore $\varepsilon$ arbitrariamente piccolo, ci sarà un certo $a_{n^*}$ che appartiene all'insieme che va da $S - \varepsilon$ a $S$)} \\
\forall n \ge n^* \quad S \ge a_n \ge a_{n^*} \implies \left( S - \varepsilon, S \right] \implies \left| S - a_n \right| < \varepsilon \\
$$

### Teorema di unicità del limite

Se $a_n$ è una successione regolare allora ammette un solo limite.

#### Dimostrazione

Sia $a_n$ una successione regolare.
$$
a_n \to l_1 \quad \text{e} \quad a_n \to l_2 \\
\forall \varepsilon > 0, \, def. \left| a_n - l_1 \right| < \varepsilon \quad \text{e} \quad def. \left| a_n - l_2 \right| < \varepsilon \\
$$
Allora
$$
\left| l_2 - l_1 \right| > 0 \\
\left| l_1 - a_n + a_n - l_2 \right| \le \left| l_1 - a_n \right| + \left| a_n - l_2 | \right| < 2 \varepsilon \\
$$
Di conseguenza $l_1$ e $l_2$ sono uguali.

### Teorema di permaneza del segno

Data una successione $a_n \to l$: allora

- $l > 0 \implies def. \, a_n > 0$
- $l < 0 \implies def. \, a_n < 0$
- Idem con $\ge$ e $\le$

#### Dimostrazione

$$
\forall \varepsilon > 0, def. \, \left| a_n - l \right| < \varepsilon \implies l - \varepsilon < a_n < l +\varepsilon 
\implies a_n > l - \varepsilon > 0 \implies \text {Se $l < 0$ allora $a_n < 0$} \\
$$

### Teorema del confronto

Date tre successioni $a_n$, $b_n$ e $c_n$ tali che $a_n < b_n < c_n$ allora se $a_n \to l$ e $c_n \to l$ allora anche $b_n \to l$.

#### Dimostrazione

$$
\begin{align*}
\forall \varepsilon > 0 \qquad & l - \varepsilon < a_n < l + \varepsilon \\
& l - \varepsilon < c_n < l + \varepsilon \\
\end{align*}
$$
Siccome $a_n < b_n < c_n$ allora anche $l - \varepsilon < b_n < l + \varepsilon$.

### Corollario

1.  Se $\left| b_n \right| \to 0$ allora $b_n \to 0$.
2.  Se $b_n$ è limitata e $c_n \to 0$ allora $(b_n \times c_n) \to 0$

### Algebra dei limiti

Se $a_n \to a$ e $b_n \to b$ allora
- $a_n \pm b_n \to a \pm b$
- $(a_n \times b_n) \to (a \times b)$
- $\frac{a_n}{b_n} \to \frac{a}{b} \qquad \left( \text{Con } b \ne 0, b_n \ne 0\right)$
- $a_n^{b_n} \to a^b \qquad \left( \text{Con } a > 0, a_n > 0\right)$

#### Dimostrazione (limitatamente alla somma e al prodotto)

##### Somma

$$
\left| \left( a_n + b_n \right) - \left( a + b \right) \right| = \left| a_n -a + b_n - b \right| \le \left|a_n - a \right| + \left| b_n - b \right| < 2 \varepsilon \qquad \text{La differenza tra la somma delle due successioni e la somma dei loro limiti è miserrima}
$$

##### Prodotto

Devo dimostrare che la differenza tra $ a_n \times \b_n$ e $a \times b$ è miserrima.
Lo faccio aggiungendo e togliendo $a \times b_n$.
Notare le priprietà del modulo.

$$
\left| a_n \times b_n - a \times b \right| = \left| a_n \times b_n + a \times b_n - a \times b - a \times b_n \right| = \left| \left(a_n - a \right) \times b_n \right| + \left| a \times \left( b_n - b \right) \right| = \left| a_n - a \right| \times \left| b_n \right| + \left| a \right| \times \left| b_n - b \right| \le \varepsilon
$$

### Forme d'indecisione

A seconda della forma d'indecisione trovata, si può procedere in vari modi (il più adeguato dei quali varia da esercizio ad esercizio)
Le forme d'indecisione sono: $\infty - \infty$, $0 \times \infty$ $\frac{0}{0}$, $\frac{\infty}{\infty}$, $\infty^0$, $\infty^{\infty}$, $1^\infty$.
Nel caso di queste ultime trè e sempre comoda la formula:
$$
a^b = e ^ {\ln {a^b}} = e ^ {b \ln a}
$$

Tutte le forme di indecisione sono riconducibili a $0 \times \infty$.

### Confrontare infiniti e infinitesimi

#### Confronto tra infiniti

$$
\text{Se } x \to \infty: \log x < x ^ n < k^x < x! < x^x
$$

#### Ordini di infinito

$$
\text{Se } a_n \to \infty, b_n \to \infty \\
\lim \frac{a_n}{b_n} =
\begin{cases}
  0 & \text{Se $a_n$ è un infinito di ordine inferiore di $b_n$} \\
  \ne 0 & \text{Se $a_n$ e $b_n$ sono infiniti dello stesso ordine} \\
  \infty & \text{Se $a_n$ è un infinito di ordine superiore di $b_n$} \\
  \nexists & \text{Se $a_n$ e $b_n$ non sono comparabili}
\end{cases}
$$

Un infinito di orine superiore va all'infinito di un infinito di ordine inferiore.

#### Ordini di infinitesimi

$$
\text{Se } a_n \to 0, b_n \to 0 \\
\lim \frac{a_n}{b_n} =
\begin{cases}
  0 & \text{Se $a_n$ è un infinitesimo di ordine superiore a $b_n$} \\
  \ne 0 & \text{Se $a_n$ e $b_n$ sono infiniti dello stesso ordine} \\
  \infty & \text{Se $a_n$ è un infinito di ordine inferore a $b_n$} \\
  \nexists & \text{Se $a_n$ e $b_n$ non sono comparabili}
\end{cases}
$$

Un infinitesimo di orduine superiore va a zero più velocemente di un infinitesimo di ordine inferiore.

#### Calcolare l'ordine di un infinito/infinitesimo

Per calcolare l'ordine di un infinito/infinitesimo è necessario calcolarne il limite del rapporto con un infinito/infinitesimo campione e ricondurre il risultato ad una forma simile a $\frac{k}{n^\alpha}$.

$\alpha$ è l'ordine di infinito.

Di seguito una lista di infiniti e infinitesimi campioni:

|         &nbsp;         |   $x \to 0$   | $x \to \pm \infty$ |    $x \to x_0$     |
| :--------------------: | :-----------: | :----------------: | :----------------: |
| Infinitesimo campione: |      $x$      |   $\frac{1}{x}$    |     $x - x_0$      |
|   Infinito campione:   | $\frac{1}{x}$ |        $x$         | $\frac{1}{x - x_0}$ |

Da notare che tra infinitesimo campione e infinito campione, sono uno il reciproco dell'altro.
Gli infinitesimi campione tendono a 0 mentre gli infiniti campione tendono a infinito.

$f(x)$ è un infinito di ordine $\alpha$ se 
$$ \exist \alpha : \lim_{x \to x^*} \frac{f(x)}{\left[ C(x) \right] ^ \alpha} = l \ne 0 $$
dove $C(x)$ è un infinito campione.

Questo èequivalente a dire che
$$ f(x) \sim \left[ C(x) \right] ^ \alpha $$

#### Asintotici notevoli

Sia $\varepsilon_n$ una successione, allora
$$
\begin{align*}
  \lim_{\varepsilon_n \to 0} \frac{\sin \varepsilon_n}{\varepsilon_n} = 1 &\implies \sin \varepsilon_n \sim \varepsilon_n \\
  \lim_{\varepsilon_n \to 0} \frac{e^{\varepsilon_n} -1}{\varepsilon_n} = 1 &\implies e^{\varepsilon_n} -1 \sim \varepsilon_n \\
  \lim_{\varepsilon_n \to 0} \frac{\left( 1  +\varepsilon_n \right)^\alpha - 1}{\varepsilon_n} = 1 &\implies \left( 1 + \varepsilon_n \right) - 1 \sim \alpha \varepsilon_n \\
  \lim_{\varepsilon_n \to 0} \frac{1 - \cos \varepsilon_n}{\varepsilon_n^2} = \frac{1}{2} &\implies 1 - \cos \varepsilon_n \sim \frac{\varepsilon_n^2}{2} \\
  \lim_{\varepsilon_n \to 0} \frac{\ln(1  +\varepsilon_n)}{\varepsilon_n} = 1 &\implies \ln(1 + \varepsilon_n) \sim \varepsilon_n \\
  \lim_{\varepsilon_n \to 0} \frac{\tg \varepsilon_n}{\varepsilon_n} = 1 &\implies \tg \varepsilon_n \sim \varepsilon_n \\
  \lim_{\varepsilon_n \to 0} \frac{\arctan \varepsilon_n}{\varepsilon_n} = 1 &\implies \arctan \varepsilon_n \sim \varepsilon_n \\
  \lim_{\varepsilon_n \to 0} \frac{\sh \varepsilon_n}{\varepsilon_n} = 1 &\implies \sh \varepsilon_n \sim \varepsilon_n \\
  \lim_{\varepsilon_n \to 0} \frac{\ch \varepsilon_n - 1}{\varepsilon_n^2} = 1 &\implies \ch \varepsilon_n - 1 \sim \frac{\varepsilon_n ^ 2}{2} \\
\end{align*}
$$
$$
\lim_{n \to \infty} \left( 1 + \frac{\alpha}{n} \right)^n = e^\alpha \\
n \to \infty \implies n! \sim n^n \times e^{-n} \times \sqrt{2 \pi n} \\
$$

## Serie numeriche

Una serie numerica è una sommatoria dei termini di una successione:
$$ S = \sum_{n=1}^\infty a_n $$
Il metodo più comune per trovarne un valore è quello di calcolare il limite di una sommatoria parziale:
$$
S_k = \sum_{n=1}^k a_n \\
S = \lim_{k \to \infty} S_k
$$
Le serie numeriche possono convergere (il loro valore è un numero finito), divergere (il loro valore è un numero infinito) è essere irregolari (Il loro valore oscilla).
Questa loro caratteristica viene detta "carattere della serie".
La convergenza/divergenza di una serie può essere assoluta o semplice.

Le serie possono essere a termini positivi, a termini negativi o a termini alternati.
Se una serie è a termini negativi, si raccoglie il meno e diventa una serie a termini positivi.

La prima cosa da fare, data una serie, è capire da quali tipi di termini è composta e successivamente capirne il carattere e dopo, se possibile capirne il valore.

Ci sono alcune serie più famose e comuni di altre.

### Serie geometriche

Una serie geometrica è una serie della forma
$$ S = \sum_{n = k}^\infty q^n $$
e il loro valore di convergenza/divergenza si calcola con la seguente formula:
$$ S = \begin{cases}
  \frac{q^k}{1 - q} & \text{se } |q| < 1 & \text{(converge)} \\
  + \infty & \text{se } q \ge 1 & \text{(diverge)} \\
  \nexists & \text{se } q \le 1 & \text{(irregolare)} \\
\end{cases}
$$
$q$ si dice "ragione" della serie geometrica.

#### Dimostrazione del caso in cui converge

Nel caso in cui $|q| < 1$, si può dimostrare che la serie converge con la seguente formula:
$$
\sum_{n = M}^\infty q^n = q^M + q^{M+1} + q^{M+2} + \dots = q^M \times \left( 1 + q^1 + q^2 + \dots \right) = q^M \times \sum_{n = 0}^{\infty} q^n = q^M \times \frac{1}{1 - q} = \frac{q^M}{1 - q}
$$
L'equivalenza 
$$
\sum_{n = 0}^\infty q^n = \frac{q}{1 - q}
$$
si può dimostrare nel seguente modo (e anche verificare per induzione):
$$
\begin{align*}
  \sum_{k = 0}^n q^k = 1 + q + q^2 + q^3 + \dots + q^n &= \frac{\left( 1 + q + q^2 + q^3 + \dots + q^n \\ \right) \times \left( 1 - q \right)}{\left( 1 - q \right)} \\
  &= \frac{\left( 1 - q + q - q^2 + q^2 - q^3 + q^3 - q^4 + \dots + q^n - q^{n+1} \right)}{1 - q} \\
  &= \frac{1 - q^{n+1}}{1 - q}
\end{align*}
$$
e, impostando $n = \infty$,
$$
\sum_{k = 0}^\infty q^k = \frac{1 - q^{\infty + 1}}{1 - q} = \frac{1}{1 - q}
$$

### Serie di Mengoli

La serie di Mengoli fa parte delle cosiddette serie telescopiche, che possono essere compresse in una differenza tra due termini (ma si vedrà dopo).

$$
\sum_{n = 0}^\infty \frac{1}{n(n+1)} = \frac{1}{2} + \frac{1}{6} + \frac{1}{12} + \dots
$$

La serie converge a 1, infatti:
$$
S_k = \sum_{n = 1}^k \frac{1}{n(n+1)} = \sum_{n = 1}^k \left( \frac{1}{n} - \frac{1}{n+1} \right) = \left(1 - \frac{1}{2} \right) + \left(\frac{1}{2} - \frac{1}{3}\right) + \left(\frac{1}{3} - \frac{1}{4}\right) + \dots + \left(\frac{1}{k} - \frac{1}{k+1}\right) = 1 - \frac{1}{k+1} \\
S = \lim_{k \to \infty} S_k = 1 - \frac{1}{\infty} = 1 - 0 = 1
$$

### Serie armonica

Le serie armoniche sono le serie nella forma
$$
S = \sum_{n = 1}^\infty \frac{1}{n^\alpha} = 
\begin{cases} 
  \text{converge} & \text{se } \alpha > 1 \\
  \text{diverge} & \text{se } \alpha \le 1 \\
\end{cases}
$$

Nota: se $\alpha = 1$ allora è un caso particolare della serie geometrica.

### Teorema di Cauchy (Teorema della condizione necessaria)

Se $\sum a_n$ converge allora $a_n \to 0$

(Da cui possiamo ricavare che se $a_n \not \to 0$ allora la serie non converge)

#### Dimostrazione

Supponiamo esista finito il $\lim S_k = S$ (quindi che la serie converga ad $S$).
Allora 
$$
S_{k+1} - S_k = \sum_{n = 0}^{k + 1} a_n - \sum_{n = 0}^k a_n = a_{k+1} \\
$$
quindi
$$
\lim_{k \to \infty} a_{k+1} = \lim_{k \to \infty} \left( S_{k+1} - S_k \right) = S - S = 0
$$

#### Perchè il viceversa non è vero???

Si pensi a $a_n = \frac{1}{n}$: $a_n \to 0$ ma $\sum a_n$ diverge.

### Come risolvere le serie a termini positivi (e, di conseguenza, anche quelle a termini negativi)

Ci sono vari criteri a cui affidarsi per capire se una serie converge o diverge.

#### Criterio del confronto asintotico

Data una serie $\sum a_n$, se si riesce a ricondurre $a_n$ alla forma
$$ a_n \sim d\frac{k}{n^\alpha \times [\ln n]^\beta} $$
allora
$$
\begin{cases}
  \text{converge} & \text{se } \alpha > 1 \\
  \text{converge} & \text{se } \alpha = 1 \text{ e } \beta > 1 \\
  \text{diverge} & \text{altrimenti}
\end{cases}
$$

#### Criterio del confronto

Data una serie $\sum b_n$, e altre due serie $a_n$ e $c_n$ tali che $a_n < b_n < c_n$ allora:
1.  Se $c_n$ converge anche $b_n$ converge
2.  Se $a_n$ diverge anche $b_n$ diverge

#### Criterio del rapporto

Data una serie $\sum a_n$ allora si calcola
$$
\lim_{n \to \infty} \sqrt[n]{a_n} = 
\begin{cases}
  > 1 & \text{diverge} \\
  < 1 & \text{converge} \\
  = 1 & \text{non si sa}
\end{cases}
$$

##### Dimostrazione

Se $\sqrt[n]{a_n} \to l < 1$ allora
$$
\left( a_n \right) ^ {\frac{1}{n}} \to l < 1 \\
a_n \to l^n \\
a_n \to \left( \frac{1}{l^{-1}} \right)^n \le \frac{1}{n^2} \text{ (che converge)} \\
\implies \sum a_n < \sum \frac{1}{n^2} \text{ e quindi converge}
$$

#### Criterio del rapporto

Data una serie $\sum a_n$ allora si calcola
$$
\lim_{n \to \infty} \frac{a_{n+1}{a_n}} = 
\begin{cases}
  > 1 & \text{diverge} \\
  < 1 & \text{converge} \\
  = 1 & \text{non si sa}
\end{cases}
$$

##### Dimostrazione

Ricapitolando, se \frac{a_{n+1}}{a_n} \to l < 1$ allora la serie diverge, altrimenti se $\frac{a_{n+1}}{a_n} \to l < 1$ converge.
In questo secondo caso,
$$
\exists \bar n : \forall n > \bar n \quad a_{n+1} < (l - \varepsilon) \\
\text{Chiamo } k = (l - \varepsilon) < 1 \text{ quindi} \\
a_{\bar n + 1} < ka_{\bar n} \text{ e } a_{\bar n + 2} < k^2a_{\bar n} \text{ e così via} \dots \\
$$
Di conseguenza posso riscrivere la sommatoria:
$$
\sum a_n = \sum_{n = 0}^{\bar n - 1} a_n + \sum_{n = \bar n}^{\infty} a_n
$$
Notare che la prima sommatoria è finita, pertanto è trascurabile, abbiamo quindi che
$$
\sum a_n = \sum_{n = \bar n}{\infty} a_n = \sum_{m = 0}^{\infty} k^ma_{\bar n} = a_{\bar n} \times \sum_{m = 0}^{\infty} k^m
$$
Siccome $m > 1$, la serie converge per il criterio del confronto.

## Funzioni

Una funzione è una relazione tra gli elementi di due insiemi, chiamati dominio e codominio.
Una funzione defe essere definita per ciascun elemento del dominio.

### Iniettività e suriettività

Una funzione è iniettiva se a diversi elementi del codominio corrispondono diversi elementi del dominio:

$$
f: D \to C \\
f(x) \ne f(y) \implies x \ne y
$$

Una funzione è suriettiva se a ciascuno degli elementi del codominio corrisponde un elemento del dominio:

$$
\exists x \in D : f(x) = y \qquad \forall y \in C
$$

### simmetria

Uan funzione è pari se $f(x) = f(-x)$.
Una funzione è dispari se $f(x) = -f(-x)$.
Requisito necessario perchè una funzione sia pai o dispari è che il dominio debba essere simmetrico.
Ciascuna funzione polinomio può essere scritto come somma di una funzione pari e di una dispari.
Somma, prodotto, differenza e rapporto tra funzioni pari daranno funzioni pari.
Somma e differenza tra funzioni dispari daranno funzioni dispari.
Prodotto e rapporto tra funzioni dispari daranno funzioni pari.
Prodotto e rapporto tra funzioni pari e dispari daranno funzioni dispari.

### Limitatezza e monotonia

Come le successioni, anche le funzioni possono essere monotone e limitate. [Monotonia di successioni](#successioni-monotone) e [limitatezza di successioni](#successioni-limitate).

### Invertibilità di funzioni

Per essere invertibile, una funzione deve essere sia suriettiva che iniettiva (quindi monotona).
In particolare, ad ogni $x$ deve corrispondere un solo $y$ e viceversa; in caso questo non sia possibile, si attua una restrizione del dominio.
Ad esempio, dato $f : \R \to \R$ tale che $f(x) = x^2$, $f$ non sarebbe invertibile perchè $f(1) = f(-1)$ ma se restringiamo il dominio ai soli numero positivi allora si.
Di seguito esempio di inversione della funzione $\sh$:

$$
\sh x = \frac{e^x - e^{-x}}{2} \\
$$
$$
\begin{align*}
  y = \frac{e^x - e^{-x}}{2} &\implies x = \frac{e^y - e^{-y}}{2} \\
  \end{align*} \\
$$
$$
\begin{align*}  
  &\implies 2x = \frac{e^{2y}-1}{e^y} \\
  \end{align*} \\
$$
$$
\begin{align*}
  &\implies 2xe^y = e^{2y}-1 \\
  \end{align*} \\
$$
$$
\begin{align*}
  &\implies (e^y)^2 - 2x \times e^y - 1 = 0 \\
\end{align*} \\
$$
$$
\begin{align*}
  e^y = \frac{-2x \pm \sqrt{4x^2 + 4}}{2} &\implies e^y = \frac{-2x \pm 2\sqrt{x^2 + 1}}{2} \\
  \end{align*} \\
$$
$$
\begin{align*}
  &\implies e^y = -x \pm \sqrt{x^2 + 1} \\
\end{align*} \\
$$
$$
\begin{align*}
  &\implies y  = \ln \left( -x + \sqrt{x^2 + 1} \right)
\end{align*} \\
$$
$$
\text{Il $-$ non lo si può utilizzare perchè $e^x$ è sempre positiva ma col $-$ si arriverebbe ad un risultato negativo} \\
$$

[Rappresentazione grafica](https://www.geogebra.org/calculator/ykamawqq)

Di seguito le principali funzioni e le loro inverse:

|       Funzione       |                                    Inversa                                    |
| :------------------: | :---------------------------------------------------------------------------: |
|     $f(x) = x^2$     |                            $f^{-1}(x) = \sqrt{x}$                             |
|     $f(x) = x^3$     |                           $f^{-1}(x) = \sqrt[3]{x}$                           |
|  $f(x) = \sqrt{x}$   |                               $f^{-1}(x) = x^2$                               |
| $f(x) = \sqrt[3]{x}$ |                               $f^{-1}(x) = x^3$                               |
|    $f(x) = \ln x$    |                               $f^{-1}(x) = e^x$                               |
| $f(x) = \log_{10} x$ |                              $f^{-1}(x) = 10^x$                               |
| $f(x) = \log_{a} x$  |                               $f^{-1}(x) = a^x$                               |
|   $f(x) = \sin x$    |                            $f^{-1}(x) = \arcsin x$                            |
|   $f(x) = \cos x$    |                            $f^{-1}(x) = \arccos x$                            |
|   $f(x) = \tan x$    |                            $f^{-1}(x) = \arctan x$                            |
|    $f(x) = \sh x$    |              $f^{-1}(x) = \ln \left( x + \sqrt{x^2 + 1} \right)$              |
|    $f(x) = \ch x$    | $f^{-1}(x) = \ln \left( x + \sqrt{x^2 - 1} \right)$ (con restrizione dominio) |

#### Teorema di invertibilità di funzioni

Se $f$ è strettamente monotona allora $f$ è invertibile e la funzione inversa è monotona.

### Funzioni periodiche

Una funzione $f$ è periodica con periodo $t$ se $f(x) = f(x + t)$ per ogni $x$.

### Operazioni sul grafico di funzioni

$$
\begin{align*}
  y = f(x) + k
  & \qquad \begin{cases}
    k = 0 & \qquad \text{Non succede niente} \\
    k > 0 & \qquad \text{Il grafico si sposta verso l'alto} \\
    k < 0 & \qquad \text{Il grafico si sposta verso il basso}
  \end{cases} \\

  y = f(x + k)
  & \qquad \begin{cases}
    k = 0 & \qquad \text{Non succede niente} \\
    k > 0 & \qquad \text{Il grafico si sposta verso destra} \\
    k < 0 & \qquad \text{Il grafico si sposta verso sinistra} \\
  \end{cases} \\

  y = \alpha f(x)
  & \qquad \begin{cases}
    \alpha = 0 & \qquad \text {Il grafico diventa $y=0$} \\
    \alpha = 1 & \qquad \text {Non succede niente} \\
    \alpha = -1 & \qquad \text{Il grafico si ribalta rispetto all'asse x} \\
    \alpha > 1 & \qquad \text{Il grafico si dilata verticalmente} \\
    0 < \alpha < 1 & \qquad \text{Il grafico si stringe verticalmente} \\
    -1 < \alpha < 0 & \qquad \text{Il grafico si stringe verticalmente e si ribalta rispetto all'asse x} \\
    \alpha < -1 & \qquad \text{Il grafico si dilata verticalmente e si ribalta rispetto all'asse x} \\
  \end{cases} \\

  y = f(\beta x)
  & \qquad \begin{cases}
    \beta = 0 & \qquad \text {Il grafico diventa $y = f(0)$} \\
    \beta = 1 & \qquad \text {Non succede niente} \\
    \beta = -1 & \qquad \text{Il grafico si ribalta rispetto all'asse y} \\
    \beta > 1 & \qquad \text{Il grafico si restringe orizzontalmente} \\
    0 < \beta < 1 & \qquad \text{Il grafico si dilata orizzontalmente} \\
    -1 < \beta < 0 & \qquad \text{Il grafico si dilata orizzontalmente e si ribalta rispetto all'asse y} \\
    \beta < -1 & \qquad \text{Il grafico si restringe orizzontalmente e si ribalta rispetto all'asse y} \\
  \end{cases} \\

  y = \left| f(x) \right|
  & \qquad \text{Il grafico nel III° e IV° quadrante viene ribaltato e sorvapposto a quanto presente nel II° e I° quadrante; III° e IV° sono vuoti} \\

  y = f\left( \left| x \right| \right)
  & \qquad \text{Il grafico di I° e IV° viene riflesso e sostituisce quanto presente nel II° e III° quadrante; la funzione diventa pari}
\end{align*}
$$

### Limiti di funzioni
I limiti delle funzioni sono molto simili ai lititi per le successioni: asintotici, teoremi e proprietà varie che valgono sia per le successioni valgono anche per le funzioni.
Di seguito le definizioni di limite di una funzione.

#### Definizione metrica

$$
\lim_{x \to \infty} f(x) = l \iff \forall \varepsilon > 0 \quad def. \left|  l - f(x) \right| < \varepsilon
$$

#### Definizione topologica

Dati $x^* \in \overline \R$ e $L \in \overline \R$, se $f(x)$ è definita in $\char"1D4B0 (x^*)$ salvo al più in $x^*$ allora
$$
\lim_{x \to x^*} f(x) = L
$$
se
$$
\forall \char"1D4B0 (L) \quad \exists \char"1D4B0(x^*) : x \in \char"1D4B0(x^*) \implies f(x) \in \char"1D4B0(L)
$$
In italiano, il limite a $x^*$ di una funzione è $L$ se per ogni $x$ intorno ad $x^*$, $f(x)$ è intorno a $L$.

#### Definizione successionale

$$
\lim_{x \to x^*} = L \text{ se } \forall \left\{ x_n \right\}, x_n \to x^* \text{ e } x_n \ne x^* \, \forall n
$$
quindi se ho $\left\{ f(x_n) \right\}$ allora $f(x_n) \to L$.
Inoltre
$$
\begin{align*}
  \lim f(x) = l^+ & \implies f(x) \to l \text{ e } f(x) \ge l \\
  \lim f(x) = l^- & \implies f(x) \to l \text{ e } f(x) \le l \\
  \lim_{x \to x_0^-} f(x) = l & \implies \text{Se } x < x_0 \text{ allora } f(x) \to l \\
  \lim_{x \to x_0^+} f(x) = l & \implies \text{Se } x > x_0 \text{ allora } f(x) \to l \\
\end{align*}
$$

#### Altri teoriemi riciclabili dalle successioni

##### Teorema di permanenza del segno

$$
\lim_{x \to x^*} f(x) \implies
\begin{cases}
  \text{Se } L > 0 \implies \exists \char"1D4B0(x^*) \text{ dove } f(x) > 0 \\
  \text{Se } L < 0 \implies \exists \char"1D4B0(x^*) \text{ dove } f(x) < 0
\end{cases}
$$
Valido anche per $\ge$ e $\le$

##### Teorema del confronto

Se
$$
\exists \char"1D4B0(x^*) : f(x) \le g(x) \le h(x) e \lim_{x \to x^*} f(x) = \lim_{x \to x^*} h(x) = L
$$
allora
$$
\lim_{x \to x^*} g(x) = L
$$

##### Corollario

Dato $h(x) \to 0$ con $x \to x^*$ allora
- $\exists \char"1D4B0(x^*) : \left| g(x) \right| \le h(x) \implies g(x) \to 0$
- $\exists \char"1D4B0(x^*) : g(x) \text{ è limitata } \implies g(x) \times h(x) \to 0$

### Continuità di funzioni

Dato un intervallo $(a, b)$ con $x_0 \in (a, b)$ allora
$$
f : (a, b) \to \R \quad \text{ è continua in $x_0$ se } \lim_{x \to x_0} f(x) = f(x_0)
$$
Se $f \in \mathcal{C}(D)$ si dice che $f$ appartiene all'insieme delle funzioni continue in ogni punto del dominio $D$.
Tutte le funzioni elementari, trigonometriche e iperboliche sono continue nel loro dominio (anche $\sqrt{x}$ e $\frac{1}{x}$ infatti non sono definite per tutto $\R$ ma dove lo sono, sono continue).

#### Teorema

Se $f$ è continua in $(a, b)$ ed è monotona allora

$$
\forall c \in (a, b) \exists \lim_{x \to c^+} f(x) = \lim_{x \to c^-} f(x)
$$

#### Teorema (quello che permette di fare il cambio di base)

Se $f$ è continua in $x_0$ e $f(x_0) > 0$ allora $\exists \char"1D4B0(x_0) : f > 0$.

#### Teorema

Se
$$
\lim_{x \to x_0} f(x) = t_0 \in \overline \R \\
\lim_{t \to t_0} g(t) = l \in \overline \R \\
f(x) \ne t_0 \quad \forall x \in \char"1D4B0(x_0) \\
$$
allora
$$
\lim_{x \to x_0} g(f(x)) = l
$$

In altre parole, se $t_n = f(x_n)$ e $\forall t_n, \, g(t_n) \to l$ allora $\forall x_n, \, g(f(x_n)) \to l$

#### Teorema

Se
$$
\lim_{x \to x_0} f(x) = f(x_0) \\
\lim_{x \to t_0} g(t) = g(t_0) \\
t_0 = f(x_0)
$$
allora
$$
\lim_{x \to x_0} g(f(x)) = g(f(x_0))
$$
quindi $g \cdot f$ è continua in $x_0$.

#### Teorema degli zeri (importante)

Se
$$
f \in \mathcal{C}[a, b] \\
f(a) \times f(b) < 0
$$
allora
$$
\exists x^* \in [a, b] : f(x^*) = 0
$$

##### Dimostrazione costruttiva

Se $a_0 = a$ e $b_0 = b$ allora

1.  Calclolo $c_n = \frac{a_n + b_n}{2}$
2.  Se $f(c_n) = 0$ allora $c_n$ è la soluzione, altrimenti
    1.  Se $f(a_n) \times f(c_n) > 0$ e $f(b_n) \times f(c_n) < 0$ allora $a_{n+1} = c_n, b_{n+1} = b_n$
    2.  Altrimenti se $f(a_n) \times f(c_n) < 0$ e $f(b_n) \times f(c_n)> 0$ allora $a_{n+1} = a_n, b_{n+1} = c_n$
3.  Riparto dal punto 1.

Dopo infiniti passaggi, si arriverà alla soluzione.
$a_n$ è una successione che si avvicina da sinistra al punto $x^*$ e $b_n$ è una successione che si avvicina da destra al punto $x^*$ (sono successioni monotone e limitate).
Da questo si può verificare che $x^*$ è unico: $a = b$ quindi $a - b \to l = 0$

Se $x_n \to x$ allora $f(x_n) \to f(x)$, infatti $f(a_n) \times f(b_n) < 0 \implies f(a_n) \times f(b_n) \to f(\lim a_n) \times f(\lim b_n) = [f(l)]^2$.
Per il teorema di permanenza del segno, $[f(l)]^2 \le 0$ ma $[f(l)]^2$ è un quadrato per cui $[f(l)]^2 \ge 0$, di conseguenza $[f(l)]^2 = 0$ e quindi $f(l) = 0$.

#### Teorema di Weiersstrass

Se $f \in \mathcal{C}(D)$ con $D$ chiuso e limitato (quindi senza infiniti) allora $\exists x_1, x_2 \in D : f(x_1) \ge f(x) \ge f(x_2) \quad \forall x \in D$.

In italiano, la funzione ammette un massimo e un minimo: l'idea è come quella del teorema degli zeri, continuando a dividere a metà, si arriverà per forza ad un massimo e ad un minimo.

Il teorema di Weierstrass è una condizione sufficiente ma non necessaria: se le ipotesi sono verificate allora è vera la tesi ma non è detto che sia vero che se la tesi è vera allora le ipotesi sono verificate.
Per esempio, nella funzione $f(x) = xe^{-x^2}$, se $x \to 0$ allora $f(x) \sim x$ e se $x \to \pm \infty$ allora $f(x) \to 0$.
I massimi e i minimi esistono anche se il dominio ($\R$) non è ne chiuso ne limitato.

##### Perchè le condizioni nell'ipotesi sono necessarie?

1.  $f : [0, 1] \to \R \qquad f(x) = \begin{cases} x & 0 < x < 1 \\ \frac{1}{2} & x = 0, x = 1 \end{cases}$ Non è continua.
    $f$ non può avere un massimo (1) perchè non esiste una $x$ tale che $f(x) = 1$.
2.  $f : (-\frac{\pi}{2}, \frac{\pi}{2}) \to \R \qquad f(x) = \tg x$ è continua in $D$ ma $D$ è aperto pertanto $f$ non è definita nei punti nei quali avrebbe valore massimo: $\pm \frac{\pi}{2} \not \in (-\frac{\pi}{2}, \frac{\pi}{2})$
3.  $f : [0, +\infty) \to \R \qquad f(x) = \sqrt{x}$ è continua in $D$ ma $D$ è chiuso ma non limitato: $\min(f) = 0$ ma $\max(f) = \nexists$ (dovrebbe essere $\infty$)

#### Teorema di Darboux (teorema dei valori intermedi)

Se $f \in \mathcal{C}[a, b]$ e $\exist m = \underset{[a, b]}{\min}(f), \, M = \underset{[a, b]}{\max}(f)$ (quindi se il teorema di Weierstrass è verificato) allora
$$
\forall \lambda \in (m, M) \exist x^* \in [a, b] : f(x^*) = \lambda
$$
In italiano, se una funzione è continua in un dato intervallo, allora assumerà tutti i possibili valori compresi tra il suo massimo e minimo in quell'intervallo.

##### Dimostrazione

Se $x_1, x_2$ sono i punti in cui la funzione $f$ assume i valori di minimo e massimo (per il contrario, la dimostrazione è analoga) allora poniamo
$$
g(x) = f(x) - \lambda \\
m = f(x_1) \\
M = f(x_2) \\
I = [x_1, x_2] \subseteq [a, b]
$$
da cui concludiamo che:
1.  $g \in \mathcal{C}[a, b] \implies g \in \mathcal{C}(I)$
2.  $g(x_1) \times g(x_2) < 0$ infatti, se $f(x_1) > \lambda$ allora $f(x_2) < \lambda$ e viceversa, per cui $g(x_1)$ e $g(x_2)$ sono di segni opposti.
    In questo caso, $f(x_1) = m < \lambda \implies m - \lambda < 0 \implies g(x_1) < 0$ e quindi $g(x_2) > 0$
3.  1. e 2. soddisfano le ipotesi del teorema degli zeri, quindi $\exists x^* \in I : g(x^*) = 0 \implies f(x) = \lambda$

#### Teorema

Se $I$ è un intervallo qualsiasi e $f \in \mathcal{C}(I)$ allora $f$ è invertibile  $\iff$ $f$ è strettamente monotona.

##### Dimostrazione

- $\lArr$: Se $f$ è strettamente monotona allora è invertibile per precedente (insert link here) dimostrazione che verrà di seguito riportata.
  $\forall x_1, x_2 \in D : x_1 > x_2 \implies \begin{cases} f(x_1) > f(x_2) & \quad \text{Se strettamente crescente} \\ f(x_1) < f(x_2) & \quad \text{Se strettamente decrescente} \end{cases}$
  Da qui deduciamo che $\forall x_1, x_2 \in D : x_1 \ne x_2 \implies f(x_1) \ne f(x_2)$ per cui la funzione è iiettiva e di conseguenza invertibile se si presta attenzione al fatto che dominio e codominio si scambiano.
- $\rArr$: $A \rArr B$ allora $\overline B \rArr \overline A$ quindi _"Se $f$ è invertibile allora è monotona"_ diventa _"Se $f$ non è monotona allora non è invertibile"_
  Quindi se $f$ non è monotona e $x_1 < x_2 < x_3$ allora $f(x_1) < f(x_2) > f(x_3)$ (oppure $f(x_1) > f(x_2) < f(x_3)$, la dimostrazione è analoga).
  Suppongo che $f(x_1) < f(x_3)$ (il seguente ragionamento è analogo per il contrario): per il teorema di Darboux, $\exists x^* \in [x_1, x_2] : f(x^*) = f(x_3) \implies f$ non è iniettiva e pertanto neanche invertibile.

##### Corollario

- Se $f \in \mathcal{C}(I)$ è invertibile allora $g = f^{-1} \in \mathcal{C}(\text{Im} f)$
- Se $g$ non fosse continua, non lo sarebbe neanche $f$

### Infiniti e infinitesimi

Funzionano esattamente come con le successioni

### Asintotici

#### Asintoto verticale
Se
$$
\lim_{x \to x_0} f(x) = \pm \infty \text{ oppure } \lim_{x \to x_0^\pm} f(x) = \pm \infty
$$
allora ad $x = x_0$ c'è un asintoto verticale.

#### Asintoto orizzontale

Se
$$
\lim_{x \to \pm \infty} f(x) = l
$$
allora ad $y = l$ c'è un asintoto orizzontale

#### Asintoto obliquo

Se
$$
\lim_{x \to \pm \infty} f(x) = \pm \infty \\
\lim_{x \to \pm \infty} \frac{f(x)}{x} = l \ne 0, \ne \pm \infty \\
\lim_{x \to \pm \infty} f(x) - mx = q \ne \pm \infty
$$
allora l'asintoto obliquo esiste ed è $y = mx + q$.
Con $x \to \infty$, $f(x) \sim mx$

### o piccoli e algebra degli stessi
$o(\square) =$ qualcosa di trascurabile rispetto a $\square$.
Per $x \to x^*$, $f(x) \sim g(x) \iff f(x) = g(x) + o(g(x))$
Per $x \to 0$ se $f(x) = o(x^\alpha)$ allora
$$
\lim_{x \to 0} \frac{f(x)}{x^\alpha} = 0
$$
Se $f(x)$ è un infinito allora $f(x) = o(f(x^2))$
Se $f(x)$ è un infinitesimo allora $f(x^2) = o(f(x))$

#### Algebra degli o piccoli

$$
o(x^\alpha) = o(kx^\alpha) = ho(x^\alpha) \\
o(x^\alpha) \text{ va a zero più velocemente di } x^\alpha \\
o(x^\alpha) \pm o(x^\alpha) = o(x^\alpha) \\
x^\beta \times o(x^\alpha) = o(x^\alpha) \times o(x^\beta) = o(x^{\alpha + \beta})
$$

Se $\beta > \alpha$ allora
$$
o(x^\alpha) \pm o(x^\beta) = o(x^\alpha) \\
o(x^\alpha) \pm ax^\beta = o(x^\alpha) \\
$$

### Parte principale di un infinito/esimo

La parte principale di un infinito/esimo si calcola al limite indicato ed è nella forma $k(x - x_0)^\alpha$

#### Come calcolarla

Data una $f(x)$ e $x \to x_0$, calcolo
$$
\lim_{x \to x_0} \frac{f(x)}{(x - x_0)^\alpha} = \lim_{x \to x_0} \frac{k(x - x_0)^n}{(x - x_0)^\alpha} = k(x - x_0)^{n - \alpha}
$$
Pongo $\alpha = n$ (e ho trovato $\alpha$) poi sostituisco
$$
k(x - x_0)^{n - \alpha} = k(x - x_0)^0 = k
$$
e ho trovato anche $k$.
Ora scrivo il tutto nella forma $K(x - x_0)^\alpha$ e ho finito

#### Esercizio d'esempio

$f(x) = \ln x - \ln 2$ con $x \to 2$

$$
\begin{align*}
  \lim_{x \to 2} \frac{\ln x - \ln 2}{(x - 2)^\alpha} &= \lim_{x \to 2} \frac{\ln \frac{x}{2}} {(x - 2)^\alpha} \\
  &= \lim_{x \to 2} \frac{\ln \left( 1 + \frac{x}{2} - 1\right)}{(x - 2)^\alpha} \\
  &\sim \lim_{x \to 2} \frac{\frac{x}{2} - 1}{(x - 2)^\alpha} \\
  &= \lim_{x \to 2} \frac{1}{2} \times (x - 2)^{1 - \alpha} \implies \alpha = 1,\, k = \frac{1}{2}
\end{align*}
$$

## Glossario formule trigonometriche

### Seno, coseno, tangente
$$
\sin x = -\sin -x \\
\cos x = \cos -x \\
\tan x = -\tan -x = \frac{\sin x}{\cos x} \\
\left[ \tan x \right]^{-1} = \arctan x \\
$$

### Funzioni iperboliche
$$
\sh x = \frac{e^x - e^{-x}}{2} \\
\ch x = \frac{e^x + e^{-x}}{2} \\
$$

### Seno e coseno di x mezzi

Il segno va deciso in base al quadrante di partenza e di arrivo.
$$
\cos \frac{x}{2} = \pm \sqrt{\frac{1 + \cos x}{2}} \\
\sin \frac{x}{2} = \pm \sqrt{\frac{1 - \cos x}{2}} \\
$$

### Seno e coseno di somme di angoli

Conoscendo la prima di ogni coppia di formule, ci si può ricavare la seconda.
$$
\sin (a + b) = \sin a \cos b + \cos a \sin b \\
\sin (a - b) = \sin a \cos b - \cos a \sin b \\
\cos (a + b) = \cos a \cos b - \sin a \sin b \\
\cos (a - b) = \cos a \cos b + \sin a \sin b \\
\tan (a + b) = \frac{\tan a + \tan b}{1 - \tan a \tan b} \\
$$

### Angoli famosi

| Angolo (rad) | Angolo (°) |    Seno    |   Coseno   |  Tangente  |
| :----------: | :--------: | :--------: | :--------: | :--------: |
|      0       |     0      |     0      |     1      |     0      |
|    &pi;/6    |     30     |    1/2     | &radic;3/2 | 1/&radic;3 |
|    &pi;/4    |     45     | &radic;2/2 | &radic;2/2 |     1      |
|    &pi;/3    |     60     | &radic;3/2 |    1/2     |  &radic;3  |
|    &pi;/2    |     90     |     1      |     0      |   &infin   |

### Cambio dei parametri

$$
\sin (\alpha + \pi) = -\sin \alpha \\
\cos (\alpha + \pi) = -\cos \alpha \\
\tan (\alpha + \pi) = \tan \alpha \\
\sin (\alpha + \frac{\pi}{2}) = \cos \alpha \\
$$

## Calcolo Differenziale

### Definizione di derivata

Data $f : (a, b) \to \mathbb{R}$ e $x_0 \in (a, b)$, $f$ si dice derivabile in $x_0$ se
$$
 \exists \text{ finito } f'(x_0) = \lim_{h \to 0} {\frac{f(x_0+h) - f(x_0)}{h}}
$$

$f'(x)$ associa ad ogni $x$ la derivata di $f$ nel punto $x$ ovvero il coefficente angolare della retta tangente a $f(x)$ nel punto x.

$\frac{f(x_0 + h) - f(x_0)}{h}$ è detto **rapporto incrementale**

### Caratterizzazione dei punti di non derivabilità

Ci sono 3 tipi di punti di non derivabilità: il **Punto angoloso**, il **Flesso a tangente verticale** e la **Cuspide**.

Per trovarli, è necessario calcolare i seguenti rapporti incrementali:
$$
f'_+(x_0) = \lim_{x \to 0^+} {\frac{f(x_0 + h) - f(x_0)}{h}} \\
f'_-(x_0) = \lim_{x \to 0_-} {\frac{f(x_0 + h) - f(x_0)}{h}}
$$

Se $f'_+(x_0) = f'_-(x_0)$ allora la funzione è derivabile in $x_0$ altrimenti $x_0$ è detto punto di non derivabilità.

1. **Punto angoloso**: $f'_+(x_0) \ne f'_-(x_0)$ ed entrambi esistono finiti.
   Ad esempio, se $f(x) = |x|$ e $x_0 = 0$ allora $f'_+(x_0) = 1 \ne f'_-(x_0) = -1$
2. **Flesso a tangente verticale**: $f'(x_0) = \pm \infty$
   Ad esempio, se $f(x) = \sqrt[3]{x}$ e $x_0 = 0$ allora $f'(x_0) = +\infty$
3. **Cuspide**: $f'_+(x_0) = \pm \infty \ne f'_-(x_0) = \pm \infty$
   Ad esempio, se $f(x) = \sqrt[3]{x^2}$ e $x_0 = 0$ allora $f'_+(x_0) = +\infty \ne f'_-(x_0) = -\infty$

### Relazione tra derivabilità e continuità

Se $f : (a, b) \to \mathbb{R}$ e $x_0 \in (a, b)$, se $f$ è derivabile in $x_0$ allora $f$ è anche continua in $x_0$.

#### Dimostrazione

Se $f$ è continua in $x_0$ allora:

$$
\lim_{x \to x_0} {f(x)} = f(x_0) = \lim_{x \to x_0} \left[f(x) - f(x_0) \right] \implies \lim_{h \to 0} \left[ f(x_0 + h) - f(x_0) \right] = 0
$$

Se $f$ è derivabile allora:

$$
\lim_{h \to 0} {\frac{f(x_0 + h) - f(x_0)}{h}} = f'(x_0)
$$

Riscrivendo, $f(x_0 + h) - f(x_0) = f'(x_0) \cdot h + \small{o}(h)$ e

$$
\lim_{h \to 0} [f(x_0 + h) - f(x_0)] = \lim_{h \to 0} [f'(x_0) \cdot h + \small{o}(h)] = 0
$$

### Regole di derivazione

Per evitare di calcolare sempre il rapporto incrementale, esistono delle forme generiche che possono essere riciclate per derivare velocemente quasi tutto.

| Funzione   | Derivata                      |
| ---------- | ----------------------------- |
| $k$        | $0$ (*1)                      |
| $x^\alpha$ | $\alpha x ^{\alpha - 1}$ (*2) |
| $e^x$      | $e^x$ (*3)                    |
| $\sin x$   | $\cos x$ (*4)                 |

Seguono dimostrazioni e note.

(*1): 

$$
\lim_{h \to 0} \frac{f(x_0 + h) - f(x_0)}{h} = \lim_{h \to 0} \frac{k - k}{h} = 0
$$

_(La forma di indecisione non c'è in questo caso perchè la frazione è "sicuramente zero"/"molto vicino a zero")_

(*2): _Se $x_0 = 0$ e $\alpha > 0$ allora il limite del rapporto incrementale è pari al limite di $h^{\alpha - 1}$ che diverge per $\alpha < 1$_.

$$
\lim_{h \to 0} \frac{f(x_0 + h) - f(x_0)}{h} = \lim_{h \to 0} \frac{(x_0 + h) ^ \alpha - x_0^\alpha}{h} = x_0^\alpha  \cdot \lim_{h \to 0} \frac{(1 + \frac{h}{x_0}) ^ \alpha - 1}{h} = x_0^\alpha \cdot \lim_{h \to 0} \frac{\alpha \cdot \frac{\cancel{h}}{x_0}}{\cancel{h}} = \alpha \cdot \frac{x_0^\alpha}{x_0} = \alpha \cdot x_0^{\alpha - 1}
$$

(3*): _$e$ è quel numero che sappiamo esistere solo grazie al teorema di monotonia_

$$
\lim_{h \to 0} \frac{f(x_0 + h) - f(x_0)}{h} = \lim_{h \to 0} \frac{e^{x_0} \cdot e^h - e^{x_0}}{h} = e^{x_0} \cdot \lim_{h \to 0} \frac{e^h - 1}{h} = e^{x_0}
$$

(4*):

$$
\begin{align*}
  \lim_{h \to 0} \frac{\sin(x_0 + h) - \sin(x_0)}{h} &= \lim_{h \to 0} \frac{\sin x_0 \cdot \cos h + \cos x_0 \cdot \sin h - \sin x_0}{h} \\
  &= \lim_{h \to 0} \left[ \sin x_0 \cdot \left( \frac{\cos h - 1}{h} \right) + cos x_0 \cdot  \left( \frac{\sin h}{h} \right) \right] \\
  &= \lim_{h \to 0} \left[ \sin x_0 \cdot \left( -\frac{h^2}{2} \cdot \frac{1}{h} \right) + \cos x_0 \cdot \left( \frac{h}{h} \right) \right] \\
  &= \sin x_0 \cdot 0 + \cos x_0 \cdot 1 \\
  &= \cos x_0
\end{align*}
$$

_Il fatto che $\lim_{x \to 0} \frac{\sin x}{x} = 1$ si può dimostrare con l'Hopital che però richiede che sia dimostrato che $[\sin x]' = \cos x$, ma quest'ultimo fatto si può dimostrare con il fatto che $\lim_{x \to 0} \frac{\sin x}{x} = 1$ quindi serve la dimostrazione senza derivate (quella della "torta") per evitare di cadere nella definizione circolare e autoreferenziale ("l'Hopital ha ragione perchè $[\sin x]' = \cos x$ ma $[\sin x]' = \cos x$ è vero perchè l'Hopital ha ragione")._

### Derivate elementari ma più in particolare

| Funzione          | Derivata                                         |
| ----------------- | ------------------------------------------------ |
| $k$               | $0$                                              |
| $\square^\alpha$  | $\alpha \square^{\alpha - 1} \cdot \square'$     |
| $e^\square$       | $e^\square \cdot \square'$                       |
| $a^\square$       | $a^\square \cdot \ln a \cdot \square'$           |
| $\ln \square$     | $\frac{1}{\square} \cdot \square'$               |
| $\log_n \square$  | $\frac{1}{\square \cdot \ln n} \times \square'$  |
| $\sin \square$    | $\cos \square \cdot \square'$                    |
| $\cos \square$    | $-\sin \square \cdot \square'$                   |
| $\tan \square$    | $\frac{1}{\cos^2 \square} \cdot \square'$        |
| $\arcsin \square$ | $\frac{1}{\sqrt{1 - \square^2}} \cdot \square'$  |
| $\arccos \square$ | $\frac{-1}{\sqrt(1 - \square^2)} \cdot \square'$ |
| $\arctan \square$ | $\frac{1}{1 + \square^2} \cdot \square'$         |
| $\sinh \square$   | $\cosh \square \cdot \square'$                   |
| $\cosh \square$   | $\sinh \square \cdot \square'$                   |
| $\tanh \square$   | $\frac{1}{\cosh \square} \cdot \square'$         |

### Algebra delle derivate

Siano $f, g : (a, b) \to \mathbb{R}$, $x_0 \in (a, b)$ e $f, g$ derivabili in $x_0$, allora:

1.  $[f \pm g]'(x_0) = f'(x_0) \pm g'(x_0)$
2.  $[f \times g]'(x_0) = [f' \times g](x_0) + [f \times g'](x_0)$
3.  $\left[ \frac{f}{g} \right]'(x_0) = \frac{[f' \times g](x_0) - [f \times g'](x_0)}{[g(x_0)]^2}$

#### Dimostrazioni

1.  Somma

$$
\begin{align*}
  \lim_{h \to 0} \frac{[f + g](x_0 + h) - [f + g](x_0)}{h} 
  &= \lim_{h \to 0} \left[ \frac{1}{h} \cdot [f(x_0+h) + g(x_0 + h) - f(x_0) - g(x_0)] \right] \\
  &= \lim_{h \to 0} \left[ \frac{1}{h} \cdot [f(x_0 + h) - f(x_0) + g(x_0 + h) - g(x_0)] \right] \\
  &= \lim_{h \to 0} \left[ \frac{f(x_0 + h) - f(x_0)}{h} + \frac{g(x_0 + h) - g(x_0)}{h} \right] \\
  &= \lim_{h \to 0} \frac{f(x_0 + h) - f(x_0)}{h} + \lim_{h \to 0} \frac{g(x_0 + h) - g(x_0)}{h} \\
  &= f'(x_0) + g'(x_0)
  \end{align*}
$$

_La dimostrazione è analoga per la differenza._

2.  Prodotto

$$
\begin{align*}
  \lim_{h \to 0} \frac{f(x_0 + h) \cdot g(x_0 + h) - f(x_0) \cdot g(x_0)}{h}
  &= \lim_{h \to 0} \frac{f(x_0 + h) \cdot g(x_0 + h) - f(x_0) \cdot g(x_0 + h) + f(x_0) \cdot g(x_0 + h) - f(x_0) \cdot g(x_0)}{h} \\
  &= \lim_{h \to 0} \left[ \frac{f(x_0 + h) - f(x_0)}{h} \cdot g(x_0 + h) \right] + \lim_{h \to 0} \left[ f(x_0) \cdot \frac{g(x_0 + h) - g(x_0)}{h} \right] \\
  &= f'(x_0) \cdot g(x_0) + f(x_0) \cdot g'(x_0)
\end{align*}
$$

Nota: $[f \times g \times h]' = f' \times g \times h + f \times g' \times h + f \times g \times h'$

3. Rapporto

_Prima trovo $\left[\frac{1}{g} \right]', poi \left[ f \times \frac{1}{g} \right]'$_

_Ricordo che $\left[\frac{1}{g} \right] (x) = \frac{1}{g(x)}$_

$$
\begin{align*}
  \lim_{h \to 0} \frac{\left[ \frac{1}{g} \right] (x_0 + h) - \left[ \frac{1}{g} \right] (x_0)}{h}
  &= \lim_{h \to 0} \frac{g(x_0) - g(x_0 + h)}{g(x_0 + h) \cdot g(x_0) \cdot h} \\
  &= \lim_{x \to 0} \left[ -g'(x_0) \cdot \frac{1}{g(x_0 + h) \cdot g(x_0)} \right] \\
  &= - \frac{g'(x_0)}{[g(x_0)]^2}
\end{align*}
$$

$$
\begin{align*}
  \left[ \frac{f}{g} \right]' = \left[ f \times \frac{1}{g} \right]'
  &= \left[ f' \times \frac{1}{g} + f \times \frac{-g'}{g^2} \right] \\
  &= \left[ \frac{f'}{g} - \frac{f \times g'}{g^2} \right] \\
  &= \left[ \frac{f' \times g - f \times g'}{g^2} \right]
\end{align*}
$$

### Derivata della funzione composta

Se $f \colon (a, b) \to \mathbb{R}$, $x_0 \in (a, b)$, $f$ derivabile in $x_0$, $g \colon (c, d) \to \mathbb{R}$, $y_0 = f(x_0) \in (c, d)$, $g$ derivabile in $y_0$ allora $[g \cdot f]$ è derivabile in $x_0$ e $[f \cdot g](x_0)' = g'(f(x_0)) \cdot f'(x_0)$

### Derivata della funzione inversa

Se $f \colon (a, b) \to \mathbb{R}$ è derivabile in $x_0 \in (a, b)$ ed è invertibile allora se $g = f^{-1}$ e $f(x_0) \ne 0$ allora $g$ è derivabile in $y_0 = f(x_0)$ e $g'(y_0) = \frac{1}{f'(x_0)}$

### Teorema di Fermat

Se $f \colon (a, b) \to \mathbb{R}$, $x_0 \in (a, b)$ è punto di massimo o minimo relativo e $f$ è derivabile in $x_0$ allora $f'(x_0) = 0$.

#### Dimostrazione

Se $x_0$ è minimo, con $h > 0$, $\frac{f(x_0 + h) - f(x_0)}{h} > 0$ quindi, per il teorema di permanenza del segno

$$
\lim_{h \to 0^+} \frac{f(x_0 + h) - f(x_0)}{h} \ge 0
$$

viceversa, se $h < 0$ allora

$$
\lim_{h \to 0^-} \frac{f(x_0 + h) - f(x_0)}{h} \le 0
$$

Se $f$ è derivabile allora è continua e i limiti destro e sinistro sono uguali quindi

$$
\lim_{h \to 0} \frac{f(x_0 + h) - f(x_0)}{h} = 0
$$

#### Quindi

Per trovare i massimi e miimi, bisogna considerare tulle le $x \colon f'(x) = 0$, gli estremi del dominio e i punti dove $f$ non è derivabile.
Non vale il viceversa.

### Teorema di Lagrange

Se $f \colon [a, b] \to \mathbb{R}$, $f$ è continua in $[a, b]$ e derivabile in $(a, b)$ allora $\exists c \in (a, b) \colon f'(c) = \frac{f(b) - f(a)}{b - a}$

_In altre parole, esiste un punto la cui derivata è parallela al segmento che va tra $(a, f(a))$ e $(b, f(b))$_

#### Dimostrazione

Definiamo $g(x) = f(x) - SEGMENTO$ ove $SEGMENTO = \text{Segmento che va da }(a, f(a)) \text{ a } (b, f(b))$ ([Disegno qui](https://www.geogebra.org/calculator/n2pkpzqp)) quindi

$$
g(x) = f(x) - \left[ f(a) + \frac{f(b) - f(a)}{b - a} (x - a) \right]
$$

e, di conseguenza $g(a) = g(b) = 0$

- $g \in \mathcal{C}[a, b]$
- $g$ è derivabile in $(a, b)$ in quanto differenza di funzioni derivabili

Per il teorema di Weierstrass, esistono punti di massimo e minimo assoluti:

- Se MAX e MIN si trovano agli estremi, $g(x) = 0$ è f è un segmento tra $a$ e $b$
- Altrimenti esiste almeno un punto $x_0 \in (a, b)$ di massimo o minimo all'interno

Nel secondo caso, vengono verificate le ipotesi del teorema di Fermat:

- $g$ ha in $x_0$ un punto di massimo o minimo
- $\exists g(x_0)$

Di conseguenza $g'(x_0) = 0$, ma $g'(x_0) = f'(x_0) - \frac{f(b) - f(a)}{b - a} = 0$ da cui $f'(x_0) = \frac{f(b) - f(a)}{b - a}$

#### Note

Ho preso la differenza tra f e il segmento, in modo da poter rilevare un eventuale minimo o massimo con facilità.

Per Fermat, esiste per forza un punto di massimo o minimo (quindi la cui derivata è zero ed è parallelo al nuovo segmento (parallelo asse y = 0)).

Siccome conosco $g$ (e quindi anche $g'$), la pongo uguale a zero e vedo che c'è effettivamente un punto nel quale la derivata di $f$ è effettivamente parallela al segmento.

### Test di monotonia

Se $I$ è un intervallo qualsiasi, $I \sube \mathbb{R}$ e $f \colon I \to \mathbb{R}$ è continua e derivabile allora

- $f$ è crescente $\iff f'(x) \ge 0 \quad \forall x \in I$
- $f$ è decrescente $\iff f'(x) \le 0 \quad \forall x \in I$

#### Dimostrazione

_Verrà dimostrata solo la crescenza, la decrescenza è analoga._

- $\implies$) $f$ crescente $\iff x_1 < x_2 \implies f(x_1) \le f(x_2) \quad x_1, x_2 \in I$
  
  $$
  f'_+(x_0) = \lim_{h \to 0^+} \frac{f(x_0 + h) - f(x_0)}{h} = \frac ++ = + \\
  f'_-(x_0) = \lim_{h \to 0^-} \frac{f(x_0 + h) - f(x_0)}{h} = \frac ++ = +
  $$

  Per il teorema di permanenza del segno allora $f'(x_0) > 0$
- $\impliedby$) $f'(x_0) > 0 \quad \forall x \in I$ allora $\forall x_1, x_2 \in I \quad x_1 < x_2$,
  
  $$
  \frac{f(x_2) - f(x_1)}{x_2 - x_1} \implies f(x_2) - f(x_1) = f'(c)(x_2 - x_1)
  $$

  Questo perchè $f$ è contnua da $x_1$ a $x_2$ (infatti $(x_1, x_2) \sube I$) ed è derivabile per lo stesso motivo.
  Per il teorema di Lagrange, $\exists c \in [x_1, x_2] \colon \frac{f(x_2) - f(x_1)}{x_2 - x_1} = f'(c)$ quindi

  - $f'(c) > 0$ per ipotesi (infatti $f'(x) > 0$)
  - $x_2 - x_1 > 0$
  
  da cui $f(x_2) - f(x_1) \ge 0 \implies f$ è crescente

### Teorema del tappabuchi

Se $f \colon (a, b) \to \mathbb{R}$ è continua in $(a, b)$ e derivabile in $(a, b)$ salvo al più in $x_0 \in (a, b)$ (quindi potrebbe esserlo o meno) allora, se $\exists \lim_{x \to x_0} f'(x_0)$, allora $f$ è derivabile in $x_0$ è $f'(x_0) = \lim_{x \to x_0} f'(x)$

#### Dimostrazione

_In questa dimostrazione si considera solo la derivata da destra, il procedimento per la derivata da sinistra è identico_

$$
\lim_{h \to 0^+} \frac{f(x_0 + h) - f(x_0)}{h} = \lim_{h \to 0^+} f'(x_0 + \alpha h) \quad 0 < \alpha < 1 = \lim_{x \to x_0} f'(x)
$$

### Concavità e convessità

Data $f \colon D \to \mathbb{R}$e $I \sube D$, $f$ è convessa in $I$ se $\forall x_1, x_2 \in I, \, f(x_1 + (1-\alpha)x_2) \le \alpha f(x_1) + (1-\alpha) f(x_2) \quad \forall \alpha \in [0, 1]$.

$$
\begin{cases}
  \alpha = 1 & f(x_1) \le f(x_1) \\
  \alpha = 0 & f(x_2) \le f(x_2) \\
  \alpha = \frac 12 & f(\frac{x_1 + x_2}2 \le \frac{f(x_1) + f(x_2)}2)
\end{cases}
$$

_Il segmento è il secondo membro della disequazione, la curva è il primo_

Un segmento è convesso perchè c'è il $=$ nel $\le$, se voglio un qualcosa di strettamente convesso utilizzo solo $<$ e metto $\alpha \in (a, b)$

_In generale, una curva è convessa se sta sotto il segmento ed è concava se sta sopra il segmento_

Se $f$ è concava o convessa in $I$ allora si può dimostrare che $f$ è continua in $I$ salvo al più agli estremi.

- $f$ convessa $\iff f'$ crescente $\iff f'' > 0$ (se derivabile 2 volte)
- $f$ concava $\iff f'$ decrescente $\iff f'' < 0$ (se derivabile 2 volte) 

- Se $f'' > 0$ e $f' = 0$ ho un massimo
- Se $f'' < 0$ e $f' = 0$ ho un minimo
- Se $f'' = 0$ per ora non lo so fare

### Differenziabilità e derivabilità

Se $f \colon (a, b) \to \mathbb{R}$, $x_0 \in (a, b)$ allora $f$ si dice differenziale in $x_0$ se $\exists m \colon \lim_{h \to 0} \frac{f(x_0 + h) - f(x_0) - mh}{h} = 0$.

_In altre parole, $f$ è differenziabile in $x_0$ se esiste una retta che l'approssima in un punto meglio di tutte le altre_

Notare che se $f$ non è differenziabile allora non è neanche derivabile.

Dalla definizione si può vedere come $f$ differenziabile in $x_0$ $\iff$ $f$ è derivabile in $x_0$ con $m = f'(x_0).

Ciò è vero solo per le funzioni a una variabile, nelle funzioni a 2+ variabili si guarda solo la differenziabilità perchè ci sono funzioni non continue ma comunque derivabili

- Differenziabilità $\implies$ derivabilità: sempre vero
- Derivabilità $\implies$ differenziabilità: valido solo nelle funzioni a 1 variabile

Si chiama **Differenziale di $f$ nel punto $x_0$** l'espressione $df(x_0) = \underbrace{f'(x_0)}_m \cdot \underbrace{dx}_h$

### Teorema di Cauchy

Se $f, g \in \mathcal{C}[a, b]$ e sono derivabili in $(a, b)$ con $g'(x) \ne 0 \quad \forall x \in (a, b)$ allora $\exists c \in (a, b) \colon \frac{f(b) - f(a)}{g(b) - g(a)} = \frac{f'(c)}{g'(c)}$

#### Dimostrazione (non richiesta ma non l'avevo notato)

Definisco la funzione $w(x)$

$$
w(x) = f(x) - \frac{f(b) - f(a)}{g(b) - g(a)} (g(x) - g(a)) \\
w(a) = f(a) - \frac{f(b) - f(a)}{g(b) - g(a)} (g(a) - g(a)) = f(a) \\
w(b) = f(b) - \frac{f(b) - f(a)}{g(b) - g(a)} (g(b) - g(a)) = g(a)
$$

Per il teorema di Rolle, $\exists c \in (a, b) \colon w'(c) = 0$

$$
w'(c) = f'(c) - \frac{f(b) - f(a)}{g(b) - g(a)} \cdot g'(c) \\
\begin{align*}
  w'(c) = 0 &\iff f'(c) = \frac{f(b) - f(a)}{g(b) - g(a)} \cdot g'(c) \\
  &\iff \frac{f'(c)}{g'(c)} = \frac{f(b) - f(a)}{g(b) - g(a)}
\end{align*}
$$

Ho ottenuto la tesi.

### Teorema di l'Hopital

Se $f, g \colon (a, b) \to \mathbb{R}$, $f, g \underset{x \to a^+}{\longrightarrow} 0 \text{ oppure } \pm \infty$, allora se $g' \ne 0$ in $(a, b)$ e se $\exists \lim_{x \to a^+} \frac{f'(x)}{g'(x)} = L$ allora $\exists \lim_{x \to a^+} \frac{f(x)}{g(x)} = L$

Questo teorema si utilizza quando ci si trova in una forma d'indecisione del tipo $\frac 00$ oppure $\frac \infty \infty$.
Solitamente è possibile arrivare alla soluzione anche senza utilizzare l'Hopital, l'unico caso in cui è ammesso utilizzarlo è quando si deve calcolare la $m$ dell'asintoto obliquo di una funzione integrale.

#### Dimostrazione (non richiesta ma non l'avevo notato)

Se $x \to a \implies f, g \to 0$ allora $f(a) = 0$ e $f(b) = 0$ e, per Cauchy,

$$
\frac{f(x)}{g(x)} = \frac{f(x) - f(a)}{g(x) - g(a)} = \frac{f'(c)}{g'(c)} \qquad a < c < x
$$

quindi

$$
\lim_{x \to a^+} \frac{f(x)}{g(x)} = \lim_{x \to a^+} \frac{f'(c)}{g'(c)} = \lim_{c \to a^+} \frac{f'(c)}{g'(c)}
$$

## Polinomi di Taylor e di MacLurin

I polinomi di Taylor e MacLaurin servono ad approssimare una funzione nell'intorno di un punto in maniera molto precisa.

Data una $f \in \mathcal{C}^n(a, b)$ e $x_0 \in (a, b)$ , allora esiste un unico polinomio che ha in comune con $f$ il valore di tutte le derivate fino all'ordine $n$:

$$
T_{n, x_0} = f(x_0) + f'(x_0)(x - x_0) + f''(x_0) \frac{(x - x_0)^2} 2 + f'''(x_0) \frac{(x - x_0)^3}{3!} + \dots + f^{(n)}(x_0) \frac{(x - x_0)^n}{n!}
$$

Questo polinomio vine chiamato **Polinomio di Taylor** di grado $n$ centrato in $x_0$.

Con $x \to x_0$, $f(x) \sim T_{n, x_0}$, da cui $f(x) = T_{n, x_0} + resto$.

Con $x_0 = 0$ il polinomio prende il nome di **Polinomio di MacLaurin**

### Formula di Taylor con resto secondo Peano

Se $f \in \mathcal{C}^n(a, b)$ allora $f(x) = T_{1, x_0}(x) + \small o((x - x_0)^n)$

In questo caso il resto è solo qualitativo, non abbiamo indicazioni oltre al fatto che è di un ordine di infinitesimo superiore rispetto al polinomio.

### Formula di Taylor con resto secondo Lagrange

Se $f \in \mathcal{C}^{n + 1}(a, b)$ allora $f(x) = T_{n, x_0}(x) + f^{(n+1)}(c) \frac{(x - x_0)^{n+1}}{(n + 1)!}$ con $c \in (x, x_0)$

#### Esempio chiarificatore

Se $\sin x = x - \frac{x^3}{3!} + \frac{x^5}{5!} - (\sin c)\frac{x^6}{6!}$, $x_0 = 0$, $x = \frac 1 2$ e quindi $0 < x < \frac 1 2$, quanto vale l'errore?

$$
|err| = (\sin c) \frac{\left(\frac 1 2 \right)^6}{6!} \implies 0 < \sin c < \sin \frac 1 2 < \frac 1 2 \implies (\sin c) \frac{\left( \frac 1 2 \right)^6}{6!} < \frac{\left( \frac 1 2 \right)^7}{6!} = \frac{1}{720 \cdot 128} \simeq \frac{1}{75000}
$$

### Sviluppi noti da sapere a memoria

| Funzione         | Sviluppo di MacLaurin                                                                                       |
| ---------------- | ----------------------------------------------------------------------------------------------------------- |
| $e^x$            | $1 + x + \frac{x^2}{2} + \frac{x^3}{3!} + \frac{x^4}{4!} + \dots + \frac{x^n}{n!} + \small o(x^n)$          |
| $\sinh x$        | $x + \frac{x^3}{3!} + \frac{x^5}{5!} + \dots + \frac{x^{2n+1}}{(2n+1)!} + \small o(x^{2n + 1})$             |
| $\cosh x$        | $1 + \frac{x^2}{2!} + \frac{x^4}{4!} + \dots + \frac{x^{2n}}{(2n)!} + \small o(x^{2n})$                     |
| $\cos x$         | $1 - \frac{x^2}{2!} + \frac{x^4}{4!} + \dots + (-1)^n \frac{x^{2n}}{(2n)!} + \small o(x^{2n})$              |
| $\sin x$         | $x - \frac{x^3}{3!} + \frac{x^5}{5!} + \dots + (-1)^n \frac{x^{2n+1}}{(2n+1)!} + \small o(x^{2n+1})$        |
| $\ln (1+x)$      | $x - \frac{x^2}{2} + \frac{x^3}{3} - \frac{x^4}{4} + \dots + (-1)^{n+1} \frac{x^n}{n} + \small o(x^n)$      |
| $(1 + x)^\alpha$ | $1 + \alpha x + \binom{\alpha}{2}x^2 + \binom{\alpha}{3}x^3 + \dots + \binom{\alpha}{n}x^n + \small o(x^n)$ |

_Per ricordare seno e coseno, sia iprbolico che non, è necessario sapere soltanto lo sviluppo di $e^x$: siccome $\sinh x + \cosh x = e^x$ allora, siccome $\sinh$ è dispari, prendo i termini dispari dello sviluppo di $e^x$ e faccio lo spesso con i termini pari per $\cosh$._

_Per trovare $\sin$ e $\cos$ basta prendere gli sviluppi di $\sinh$ e $\cosh$ ma con segno alterno davanti ad ogni termine della somma._

## Calcolo Integrale

Dato un intervallo $[a, b]$ e $f \colon [a, b] \to \mathbb{R}$ limitata

$$
S_n = \sum_{i = 1}^n f(t_i)(x_i - x_{i-1}) \quad t_i \in [x_{i-1}, x_i] \\
S_n = \frac{b - a}{n} \sum_{i=1}^n f(t_i).
$$

Se $\exists \lim_{n \to \infty} S_n$ e non dipende dalla scelta dei $t$ allora $f$ è integrabile su $[a, b]$:

$$
\int_a^b f(x) \, dx = \lim_{n \to \infty} S_n
$$

Se $f$ è integrabile su $[a, b]$ allora si dice che $f \in \mathcal{R}[a, b]$

### Criteri di integrabilità

1.  Se $f \in \mathcal{C}[a, b]$ allora $f \in \mathcal{R}[a, b]$
2.  Se $f \in \mathcal{C}(a, b]$ ed esiste finito $\lim_{x \to a^+} f(x)$ allora $f \in \mathcal{R}[a, b]$
3.  Se $f$ monotona su $[a, b]$ allora $f \in \mathcal{R}[a, b]$
4.  Se $f$ ha discontinuità in $x_0 \in (a, b)$ allora $f \in \mathcal{R}[a, b]$
5.  Se $f \in \mathcal{C}[a, b]$ e $g \in \mathcal{C}[b, c]$ e 
    $$
    h(x) = \begin{cases}
      f(x) \quad & a \le x < b \\
      \dots \quad & x = b \\
      g(x) \quad & b < x \le c
    \end{cases}
    $$
    allora $h \in \mathcal{R}[a, c]$

### Interpretazione geometrica

Con l'integrale si può calcolare l'area che va dalla curva all'asse x ed è limitata da $y = a$ e $y = b$: se $dA = f(x) \, dx$ allora $A = \int_a^b dA = \int_a^b f(x) \, dx$ (l'area totale è la somma delle varie aree infinitesime).

Funziona allo stesso modo col volume del solido ottenuto ruotando la curva attorno all'asse x: $dV = \pi (f(x))^2 \, dx$ e $V = \int_a^b \, dV = \int_a^b \pi(f(x))^2 \, dx$ (il volume totale è la somma dei vari volumi infinitesimi)

### Proprietà delle integrali

#### Linearità

Se $f, g \in \mathcal{R}[a, b]$ allora $(\alpha f + \beta g) \in \mathcal{R}[a, b]$ e 

$$
\int_a^b \left[ \alpha f(x) + \beta g(x) \right] \, dx = \alpha \int_a^b f(x) \, dx + \beta \int_a^b g(x) \, dx
$$

infatti 

$$
\frac{b - a}{n} \sum [\alpha f(t_i) + \beta g(t_i)]
$$

converge.

#### Additività

Se $f \in \mathcal{R}[a, b]$ e $f \in \mathcal{R}[b, c]$ allora $f \in \mathcal{R}[a, c]$ e 

$$
\int_a^b f(x) \, dx + \int_b^c f(x)\, dx = \int_a^c f(x) \, dx \\
\int_p^q f(x) \, dx = - \int_q^p f(x) \, dx
$$

#### Monotonia

Se $f, g \in \mathcal{R}[a, b]$ e $f(x) \le g(x) \quad \forall x$ allora

$$
\int_a^b f(x) \, dx \le \int_a^b g(x) \, dx
$$

#### Corollario

$$
-|f(x)| \le f(x) \le |f(x)| \\
-\left|\int_a^b f(x) \, dx \right| \le \int_a^b f(x) \, dx \le \left|\int_a^b f(x) \, dx \right| \\
\left| \int_a^b f(x) \, dx \right| \le \int_a^b |f(x)| \, dx
$$

### Media integrale

Se $f \in \mathcal{R}[a, b]$ la media integrale di $f$ su $[a, b]$ è 

$$
M_f = \frac{1}{b - a} \int_a^b f(x) \, dx
$$

### Teorema della media integrale

$$
f \in \mathcal{C}[a, b] \implies \exists c \in [a, b] \colon f(c) = \frac{1}{b - a} \int_a^b f(x) \, dx
$$

#### Dimostrazione

Dato $m \le f(x) \le M$ allora

$$
\int_a^b m \, dx \le \int_a^b f(x) \, dx \le \int_a^b M \, dx \\
m(b - a) \le \int_a^b f(x) \, dx \le M(b - a) \\
m \le \frac{1}{b - a} \int_a^b f(x) \, dx \le M
$$

e, di conseguenza,

$$
\exists c \in [a, b] \colon f(c) = \frac{1}{b - a} \int_a^b f(x) \, dx
$$

### Primitiva

Se $f \colon [a, b]$ e $f(x) = F'(x) \quad \forall x \in [a, b]$, $F$ si dice primitiva di $f$.

Se $f \in \mathcal{C}[a, b]$ allora $F$ ammette primitiva su $[a, b]$.

Se $f$ ha in $x_0$ una discontinuità a salto, $f$ non ammette primitiva.

### Teorema fondamentale del calcolo integrale - 1

Se $f \in \mathcal{C}[a, b]$ e $G$ è la sua primitiva allora

$$
\int_a^b f(x) \, dx = G(b) - G(a) = [G(x)]_a^b
$$

#### Dimostrazione

$$
\begin{align*}
  G(b) - G(a) = G(x_n) - G(x_0) 
  &= (G(x_n) - G(x_{n - 1})) + (G(x_{n - 1}) - G(x_{n - 2})) + \dots + (G(x_1) - G(x_0)) \\
  &= \sum_{i = 1}^n [G(x_i) + G(x_{i - 1})] = \sum_{i = 1}^{n} G(t_i)(x_i - x_{i - 1}) \underset{n \to \infty}{\longrightarrow} \int_a^b f(x) \, dx
\end{align*}
$$

### Integrazione per parti

Se $F, G : [a, b] \to \mathbb{R}$ derivabili e $f = F', g = G'$ allora

$$
\int_a^b F(x)g(x) \, dx = [F(x)G(x)]_a^b - \int_a^b f(x)g(x) \, dx \implies \int F(x)g(x) \, dx = [F(x)G(x)] - \int f(x)g(x) \, dx
$$

#### Dimostrazione

$$
\frac{d}{dx} (F(x)G(x)) = F(x)g(x) + f(x)G(x) \implies \int_a^b [F(x)g(x) + f(x)G(x)] \, dx = [F(x)G(x)]_a^b
$$

### Integrazione per sostituzione

Sia $\varphi \colon [a, b] \to \mathbb{R}$ invertibile e derivabile: $\varphi(a) = \alpha$, $\varphi(b) = \beta$, $f \in \mathcal{C}[a, b]$ allora

$$
\int_a^b f(\varphi(t))\varphi'(t) \, dt = \int_\alpha^\beta f(\tau) \, d\tau \qquad \tau = \varphi(t)
$$

_Ricordo che $y = f(x) \implies dy = f'(x) \, dx$_

#### Dimostrazione

$$
\int_\alpha^\beta f(\tau) \, d\tau = [F(\tau)]_\alpha^\beta = F(\beta) - F(\alpha) \\

\int_a^b f(\varphi(t))\varphi'(t) \, dt = [F(\varphi(t))]_a^b = F(\varphi(b)) - F(\varphi(a)) = F(\beta) - F(\alpha)
$$

### Integrazione di funzioni razionali

Sia $\int \frac{P_n(x)}{Q_m(x)} \, dx$, $n < m$ e $m = 1,2$ allora

- $m = 1$: $\int \frac{k}{ax + b} \, dx = \frac{k}{a} \int \frac{a}{ax + b} \, dx$
- $m = 2$: $\int \frac{3x + 5}{x^2 + x - 2} \, dx = \int \frac{\frac{8}{3}}{x - 1} \, dx + \int \frac{\frac{1}{3}}{x + 2} \, dx = \frac{8}{3} \ln |x - 1| + \frac{1}{3} \ln |x + 2| + c$

### Funzioni integrali

Se $f \colon [a, b] \to \mathbb{R}$ è integrabile in tutti l'intervallo $[a, b]$ e $x_0 \in [a, b]$ allora $\int_{x_0}^x f(t) \, dt$ è una funzione che dipende da $x$.

$G(x) = \int_{x_0}^x f(t) \, dt$ è la funzione integrale della funzione $f$ centrata in $x_0$.

### Teorema fondamentale del calcolo - 2

_Ricordo che il rpimo era $\int_a^b f'(x) \, dx = f(b) - f(a)$_

Se $f \colon [a, b] \to \mathbb{R}$ è integrabile (propriamente o meno) nell'intervallo $[a, b]$ e converge e $x_0 \in [a, b]$ allora $G(x) = \int_{x_0}^x f(t) \, dt$ e $G$ è continua su $[a, b]$.

Se $f$ è continua su $[a, b]$ allora $G$ è derivabile su $[a, b]$ e $G'(x) = f(x) \quad \forall x \in [a, b]$.

#### Dimostrazione

Se $f$ è invertibile in senso proprio (quindi $f$ è limitata su $[a, b]$ e, di conseguenza, $\forall x \in [a, b] \implies |f(x)| < k$), $G$ è continua in $x^*$, allora

- $G$ è continua in $x^*$ se $\lim_{x \to x^*} G(x) = G(x^*)$
- $\lim_{h \to 0} [G(x^* + h)] = G(x^*)$
- $\lim_{h \to 0} [G(x^* + h) - g(x^*)] = 0$

Utilizzo la terza per la dimostrazione.

$$
\lim_{h \to 0} [G(x^* + h) - G(x^*)] = \lim_{h \to 0} \left[ \int_{x_0}^{x_0 + 1} f(t) \, dt \right] = \lim_{h \to 0} \left[ \int_{x_0}^{x^* + h} f(t) \, dt - \int_{x_0}^{x^*} f(t) \, dt \right] = \lim_{h \to 0} \left[ \int_{x^*}^{x^* + h} f(t) \, dt \right] = 0 \\
\left| \int_x^{x_0 + h} f(t) \, dt \right| \le |h| \cdot k \text{ ma } h \to 0 \text{ quindi } |h| \cdot k = 0
$$

Per far veder che è derivabile, faccio il limite del rapporto incrementale:

$$
\lim_{h \to 0} \left[ \frac{G(x^* + h) - G(x^*)}{h} \right] = \lim_{h \to 0} \underbrace{\frac{1}{h} \int_{x^+}^{x^+ + h} f(t) \, dt}_{\text{Media integrale}}
$$

per il teorema della media integrale

$$
\begin{align*}
&= \lim_{h \to 0} f(x^* + \alpha h) \qquad (\text{con } o \le \alpha \le 1) \\
&= \lim_{h \to 0} (f^* + \alpha h) = f(x^*)
\end{align*}
$$

Da cui segue che la derivata di $G$ è $f$.

#### Dimostrazione alternativa

Se $G$ è continua in $x^*$, allora

$$
\begin{align*}
  &\lim_{x \to x^*} G(x) = G(x^*) \\
  \implies &\lim_{h \to 0} G(x^* + h) = G(x^*) \\
  \implies &\lim_{h \to 0} [G(x^* + h) - G(x^*)] \\
    &=\lim_{h \to 0} \left[ \int_{x_0}^{x^* + h} f(t) \, dt - \int_{x_0}^{x^*} f(t) \, dt \right] \\
    &= \lim_{h \to 0} \int_{x^*}^{x^* + h} f(t) \, dt
    &= \left| \int_{x^*}^{x^* + h} f(t) \, dt \right| \le |h| \cdot k \to 0
\end{align*}
$$

$$
\begin{align*}
  \lim_{h \to 0} \left[ \frac{G(x^* + h) - G(x^*)}{h} \right] &= \lim_{h \to 0} \underbrace{ \frac{1}{h} \int_{x^*}^{x^* + h} f(t) \, dt }_{\text{Media integrale}}\\
  &= \lim_{h \to 0} f(x^* + \alpha h) \qquad (0 \le \alpha \le 1) \\
  &= f(x^*)
\end{align*}
$$

#### Note

Nel primo passaggio ho trovato che il limite del numeratore del rapporto incrementale è pari all'integrale (che poi ho visto essere limitata).

Nella seconda parte noto che il rapporto incrementale è scrivibile sotto forma 

### E se la $x$ nell'integrale fosse $f(x)$?

$$
\int_{x_0}^{i(x)} f(t) \, dt = G(i(x)) \qquad \left( \text{con } G(x) = \int_{x_0}^x f(t) \, dt\right)
$$

di conseguenza è una funzione composta e si deriva come tutte le funzioni composte

$$
\left[ \int_{x_0}^{i(x)} f(t) \, dt \right]' = f(x) \cdot i'(x)
$$

### E se invece l'integrale avesse $a(x)$ e $b(x)$ al posto di $x_0$ e $x$?

Sia $F(x) = \int_{a(x)}^{b(x)} g(t) \, dt$ e $G$ primitiva di $g$ allora

$$
\begin{align*}
  F(x) &= \int_{a(x)}^{b(x)} g(t) \, dt 
  = [G(x)]_{a(x)}^{b(x)}
  = G(b(x)) - G(a(x)) \\
  \implies F'(x) &= G'(b(x)) \cdot b'(x) - G'(a(x)) \cdot a'(x) \\
  &= g(b(x)) \cdot b'(x) - g'(a(x)) \cdot a'(x)
\end{align*}
$$

### Integrali generalizzati

Dato $\int_a^b f(x) \, dx$, cosa succede se $b = \infty$ o se $f(x_0 \in (a, b)) = \pm \infty$? (quindi se la $f$ o l'intervallo non sono limitati)

Nel primo caso, si porta $f(x)$ nella forma $\frac{1}{x^\alpha} \quad (x \to \infty)$: se $\alpha > 1$ converge, altrimenti diverge.

Se converge, per calcolarne il valore, è sufficiente il seguente calcolo:

$$
\int_a^{+\infty} f(x)\, dx = \lim_{R \to +\infty} [F(x)]_a^R = \lim_{R \to +\infty} F(R) - F(a)
$$

Nel secondo caso, si porta $f(x)$ nella forma $\frac{1}{x^\beta} \quad (x \to x_0)$: se $\beta < 1$ converge, altrimenti diverge.

Se converge, per calcolarne il valore, è sufficiente il seguente calcolo:

con $f(b) = +\infty$

$$
\int_a^b f(x) \, dx = \lim_{\varepsilon \to 0}\int_a^{b - \varepsilon} f(x) \, dx = \lim_{\varepsilon \to 0} [F(x)]_a^{b - \varepsilon}
$$

_I due calcoli inglobano anche il controllo di convergenza: se il risultato è $\pm \infty$ allora divergono mentre se il limite non esiste allora non sono proprio integrabili._

Se un integrale risulta essere improprio in più punti, lo so spezza in quei punti e poi si va a verificare la convergenza di tutte le varie parti: se almeno una di esse diverge allora l'integrale originale diverge.

### Criteri di convergenza per integrali generalizzati

I criteri di convergenza per integrali generalizzati funzionano in maniera simile a quelli per le serie (con l'eccezione che il criterio del rapporto e della radice qui non esistono).

Entrambi i criteri applicabili (quello del **Confronto** e quello del **Confronto asintotico**) si applicano a funzioni definite positive sull'intervallo di integrazione (se sono definite negative, raccolgo il meno).

#### Criterio del confronto

Siano $f(x), g(x) > 0$ e $f(x) \le g(x) \quad \forall x \in [a, b]$ allora

- se $\int f(x)$ diverge allora anche $\int g(x)$ diverge
- se $\int g(x)$ converge allora anche $\int f(x)$ converge
  
#### Criterio del confronoto asintotico

Siano $f(x), g(x) > 0$ e $f(x) \underset{x \to \infty}{\sim} g(x)$ allora entrambi gli integrali hanno lo stesso carattere (entrambi convergono o entrambi divergono)

# Equazioni Differenziali

## Equazioni Differenziali Ordinarie (EDO) di primo ordine

* **Forma normale:** $y'(t) = f(t, y(t))$

* **Integrale generale:** è l'insieme di tutte le soluzioni ($\infty^1$ soluzioni per le EDO di primo ordine, $\infty^2$ per le EDO di secondo ordine, ecc.); le soluzioni sono scritte in funzione di una costante arbitraria $c$

* **Soluzione particolare:** soluzione che soddisfa una data condizione iniziale

* **Soluzione costante:** soluzione particolare che soddisfa la condizione $y(t_0) = y_0\, \forall t$  
    Per determinare le eventuali soluzione costanti, si sostituiscono:
    * $y(t)$ con $y_0$
    * $y'$ con $0$ (derivata di una costante)


### Equazioni a variabili separabili

* **Forma normale:** $y'(t) = h(t) \cdot g(y(t))$  

    Con $h: J_1 \subseteq \mathbb{R} \to \mathbb{R}$ e $g: J_2 \subseteq \mathbb{R} \to \mathbb{R}$ funzioni continue

* **Eventuali soluzioni costanti:** $y(t) = c \, \forall t \in \mathbb{R}$, con $g(c) = 0$

Per risolvere le equazioni a variabili separabili:

1. trovo il dominio della funzione $f(t,y(t))$

2. trovo eventuali soluzioni costanti (se $g(c) = 0$)

3. trovo l'integrale generale nel caso in cui $g(y(t)) \neq 0$:
    
    1. divido per $g(y(t))$ e moltiplico per $dt$ ambo i membri:
        $$\frac{y'(t)}{g(y(t))}\, dt =  h(t)\, dt$$

    2. integro entrambi i membri:
        $$\int \frac{y'(t)}{g(y(t))}\, dt = \int h(t)\, dt$$

    3. ricavo $y$ (forma esplicita)

### Equazioni lineari

* **Forma normale:** $y'(t) = a(t) \cdot y(t) + b(t)$

    Con $a, b: J \subseteq \mathbb{R} \to \mathbb{R}$ funzioni continue su $J$

* **Eventuali soluzioni costanti:** se $\displaystyle y_0 = -\frac{b(t)}{a(t)}$ è costante $\forall t \in J$, allora $y(t) = y_0$ è soluzione costante

Per risolvere le equazioni lineari:

### ***Teorema I:*** Formula risolutiva EDO lineari di primo ordine

Date $a,b: J \subseteq \mathbb{R} \to \mathbb{R}$ funzioni continue su $J$, data l'equazione differenziale lineare di primo ordine:

$$y'(t) = a(t) \cdot y(t) + b(t)$$

Considero la funzione $A(t) = \displaystyle \int{a(t)}\, dt$ una qualsiasi primitiva di $a(t)$, ho che:

$$y(t) = e^{A(t)} \cdot \left( \int{e^{-A(t)} \cdot b(t)\, dt} \textcolor{lime}{+ c}\right)$$

è l'integrale generale dell'equazione differenziale lineare di primo ordine.

#### Dimostrazione:

1. Moltiplico l'equazione differenziale per $e^{-A}$, dove $A$ è una primitiva di $a$:  
    $$e^{-A}  y' = e^{-A} a y + e^{-A} b$$
2. Porto il termine in $y$ a sx:
    $$e^{-A} y' - e^{-A} a y = e^{-A} b$$
3. Riconosco la derivata di un prodotto:
    $$\left( e^{-A} y \right)' = e^{-A} b$$
4. Integro ambo i membri tra $t_0$ e $t$, con $t_0 < t$, entrambi appartenenti a $J$:
    $$\int_{t_0}^t \left( e^{-A(x)} y(x) \right)' \, dx = \int_{t_0}^t e^{-A(x)} b(x) \, dx$$
5. Applico il primo teorema fondamentale del calcolo integrale:
    $$e^{-A(t)} y(t) - \textcolor{lime}{e^{-A(t_0)} y(t_0)} = \int_{t_0}^t e^{-A(x)} b(x) \, dx$$  
    Chiamo $c = e^{-A(t_0)} y(t_0)$:  
    $$e^{-A(t)} y(t) - \textcolor{lime}{c} = \int_{t_0}^t e^{-A(x)} b(x) \, dx$$  
6. Sposto il c a destra e divido per il coefficiente di $y(t)$:
    $$y(t) = e^{A(t)}\left( \int_{t_0}^t e^{-A(x)} b(x) \, dx + \textcolor{lime}{c} \right)$$  
$\square$

### Equazioni di Bernoulli

* **Forma normale:** $y'(t) = k(t) \cdot y(t) + h(t) \cdot y(t)^\alpha$  
    (con $\alpha \in \mathbb{R},\, \alpha \ne 0,\, \alpha \ne 1$)  
    (con $k, h$ funzioni continue)

Premesse:

1. Se $\alpha$ è irrazionale o razionale a denominatore pari, $y^\alpha$ ha senso solo per $y \ge 0$ **(noi consideriamo sempre $y \ge 0$)**

2. Per $\alpha \in (0, 1)$, non vale l'unicità della soluzione del problema di Cauchy

3. Per $\alpha = 0$, non ha significato $y^\alpha$ per $y = 0$

Per risolvere le equazioni di Bernoulli:

1. Cerco eventuali soluzioni costanti:  
    $$k(t)y + h(t) y^\alpha = 0$$  
    È sempre soluzione costante $y(t) = 0\, \forall t$; se $k$ e $h$ sono costanti, allora trovo l'altra soluzione

2. Per cercare soluzioni non costanti, divido per $y^\alpha$:
    $$\frac{y'}{y^\alpha} = k(t)\textcolor{lime}{y^{1-\alpha}} + h(t)$$
    
3. Pongo $z(t) = y^{1- \alpha}$ e determino l'equazione soddisfatta da $z$:
    $$\begin{align*}
        z'(t) &= (1 - \alpha)\, y^{1-\alpha -1} y' \\
        &= (1 - \alpha)\, \frac{y'}{y^\alpha} \\
        &= (1 - \alpha) \left[ k(t) z(t) + h(t) \right] \\
        &= (1 - \alpha) \, k(t) z(t) + (1 - \alpha) \, h(t)
    \end{align*}$$  
    che è lineare in $z$

4. Risolvo la EDO lineare in $z$ e trovo $z(t)$:

    $$z'(t)\, \underbrace{- (1 - \alpha) \, k(t)}_{a(t)} \, z(t) = \underbrace{(1-\alpha)\, h(t)}_{b(t)}$$

5. Ritorno alla variabile $y$:
    $$y(t) = z(t)^{\frac{1}{1-\alpha}}$$

## Equazioni Differenziali Ordinarie (EDO) di secondo ordine

Vediamo inizialmente il caso delle omogenee:

$$a(t)y''(t) + b(t)y'(t) + c(t)y(t) = 0$$

con $a, b, c: J \subseteq \mathbb{R} \to \mathbb{R}$ funzioni continue su $J$ e $a \ne 0$ in $J$.

Consideriamo lo scenario più semplice, cioè con $a, b, c$ costanti reali.

### EDO di secondo ordine lineari omogenee a coefficienti costanti
* **Forma normale:** $ay''(t) + by'(t) + cy(t) = 0$  
    (con $a, b, c \in \mathbb{R}$)

Per risolvere le EDO di secondo ordine lineari omogenee a coefficienti costanti, si considera l'equazione caratteristica:

$$a \lambda^2 + b \lambda + c = 0$$

e si risolve per $\lambda_1, \lambda_2$:

$$\lambda_{1,2} = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$$

* Se $\Delta = b^2 - 4ac > 0$, allora $\lambda_1, \lambda_2 \in \mathbb{R}$ e l'integrale generale è una combinazione lineare di esponenziali reali:
    $$y(t) = c_1 e^{\lambda_1 t} + c_2 e^{\lambda_2 t}$$

* Se $\Delta = 0$, allora $\lambda_1 = \lambda_2 = \frac{-b}{2a} \in \mathbb{R}$ e l'integrale generale è una combinazione lineare di esponenziali reali, uno dei quali moltiplicato per $t$:
    $$y(t) = c_1 e^{\lambda_1 t} + c_2 t e^{\lambda_2 t}$$

* Se $\Delta < 0$, allora $\lambda_1, \lambda_2 \in \mathbb{C}$ e posso scrivere $\lambda$ come:  
    $$\lambda = m \pm ui$$  
    L'integrale generale diventa:  
    $$y(t) = e^{mt} \left[ c_1 \cos u + c_2 \sin {ut} \right]$$

### ***Teorema II:*** Teorema di struttura per le EDO di secondo ordine omogenee

Siano $a, b, c : J \subseteq \mathbb{R} \to \mathbb{R}$ continue, con $a \ne 0$ in $J$, l'integrale generale dell'equazione omogenea:

$$a(t) y''(t) + b(t) y'(t) + c(t) y(t) = 0$$

è uno spazio vettoriale di dimensione 2, cioè le soluzioni sono tutte e sole della forma:

$$y_O (t) = c_1 y_{O_1}(t) + c_2 y_{O_2}(t)$$

con $c_1, c_2 \in \mathbb{R}$, dove $y_{O_1}, y_{O_2}$ sono soluzioni linearmente indipendenti.

#### Dimostrazione:
Sia $V$ lo spazio vettoriale delle funzioni $y \in C^2(J)$, cioè:

$$C^2 (J) = \{y \in C^1(J) \mid \text{$y''$ derivabile due volte e $y''$ continua su $J$}\}$$

L'integrale generale dell'omogenea è il seguente sottoinsieme di $V$:

$$W=\{y \in V \mid ay'' + by'' + cy'' = 0\} = \ker \mathcal{L}$$

dove $\mathcal{L}$ è l'operatore definito nel *principio di sovrapposizione*. $W$, in quanto nucleo di un'applicazione lineare, è un sottospazio vettoriale.

Per dimostrare che $W$ ha dimensione 2 devo:

1. **esibire due soluzioni linearmente indipendenti dell'equazione:**  
    $$\begin{cases}
    ay_{O_1}'' + by_{O_1}' + cy_{O_1} = 0 \\
    y_{O_1}(t_0) = 1 \\
    y_{O_1}'(t_0) = 0
    \end{cases}
    \qquad
    \begin{cases}
    ay_{O_2}'' + by_{O_2}' + cy_{O_2} = 0 \\
    y_{O_2}(t_0) = 0 \\
    y_{O_2}'(t_0) = 1
    \end{cases}$$
    verifico che $y_{O_1}, y_{O_2}$ sono linearmente indipendenti:

    se per assurdo fossero una multiplo dell'altra:

    $$y_{I_1} (t) = k y_{O_2} (t)\, \forall t \in J$$

    in particolare, per $t = t_0$:

    $$y_{O_1} (t_0) = k y_{O_2} (t_0) \implies 1 = 0$$

    che è assurdo, quindi $y_{O_1}, y_{O_2}$ sono linearmente indipendenti.
2. **dimostrare che ogni altra soluzione dell'equazione si scrive come combinazione lineare di $y_{O_1}, y_{O_2}$:**  
    Data una qualunque soluzione $y_O$ dell'equazione, pongo:

    $$\begin{cases}
    k_1 = y_O(t_0) \\
    k_2 = y_O'(t_0)
    \end{cases}
    $$

    e

    $$z(t) k_1 y_{O_1}(t) + k_2 y_{O_2}(t)$$

    e affermo che $z(t) = y_O(t)\, \forall t$.

    Infatti $z(t)$ è soluzione della EDO e soddisfa il medesimo problema di Cauchy:

    $$z(t_0) = k_1 \underbrace{y_{O_1}(t_0)}_1 + k_2 \underbrace{y_{O_2}(t_0)}_0 = k_1 = y_O(t_0)$$

    $$z'(t_0) = k_1 y_{O_1}'(t) + k_2 y_{O_2}'(t) = k_1 = y_O(t_0)$$

    quindi, per il teorema di esistenza e unicità delle soluzioni del problema di Cauchy, $z(t) = y_O(t)\, \forall t \in J$.

$\square$
## Problema di Cauchy

Problema che consiste nel trovare la soluzione particolare che soddisfa una data condizione iniziale:  
    $$\begin{cases}
        y' = f(t, y(t)) \\
        y(t_0) = y_0
    \end{cases}$$

**NB:**

* il problema di Cauchy ha **una e una sola soluzione** (Teorema di esistenza e unicità delle soluzioni, che NON verrà dimostrato)

* per determinare la soluzione del problema di Cauchy, è utile ripartire il dominio di $f$ in regioni su $t$ ed $y(t)$ ove $f$ è continua, e considerare solo l'integrale generale relativo all'intervallo o regione che contiene $(t_0, y_0)$ (perché la soluzione è unica, quindi non può cambiare in intervalli diversi); questo può semplificare i conti, specialmente quando integrando si ottengono moduli

Per risolvere il problema di Cauchy:  

1. determino l'integrale generale (con eventuali soluzioni costanti)

2. impongo la condizione $y(t_0) = y_0$ e determino la costante $c$

3. sostituisco la costante $c$ nell'integrale generale e ottengo la soluzione particolare
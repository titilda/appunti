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
    (con $\alpha \in \mathbb{R},\ \alpha \ne 0,\ \alpha \ne 1$)  
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


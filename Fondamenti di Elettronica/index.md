---
title: "Fondamenti di Elettronica (FdE)"
author: 
- "Niccolò Papini"
---
# Introduzione

Salve lettori, questa pagina è stata creata per riassumere o ampliare gli appunti che prendiamo a lezione, questi appunti possono servire per capire meglio l'argomento e non possono essere capiti completamente se non si seguono le lezioni. Se trovate errori o parti poco chiare vi prego di segnalarlo così provederemo a corregere. Buona lettura -NP

# Capitolo Uno: Il Diodo

Il diodo non è altro che un componente NON-lineare che fa passare la corrente in un verso e ne impedisce il movimento nel verso opposto.

Ha un comportamento NON-lineare, per essere precisi di tipo semi-esponenziale, avendo polirizzazione diretta, inversa e avendo un possibile evento di Breackdown.

## 1.1 Comportamento fisico-chimico

Per capire ciò bisogna fare un leggero passo indietro, dobbiamo partire dall'atomo di Silicio, ora tanti atomi di silicio vicini a temperature superiori allo $0K$ fanno ciò che gli atomi fanno, dunque rottura dei legami e successiva riformazione di questi legami con altri atomi con posti liberi. Ora se levassi un atomo di silicio con uno di boro (1 elettrone in meno) faccio una procedura di **DROGAGGIO** di tipo **ACCETTORE** visto che il boro ha uno spazio libero tende a chiedere un elettrone per andare in stabilità, se lo sostituissi con un atomo di fosforo (un elettrone in più) il drogaggio è di tipo **DONORE**, avendo un elettrone in più cede per andare in stabilità, quindi qui si spostano più elettroni.

Detto questo cosa succede ? Dobbiamo immaginarci il diodo come un rettagolo pieno di atomi di silicio, dentro questo rettangolo ci solo cariche positive e cariche negative, ora queste cariche si bilanciano in due zone: catodo $(-)$ e anodo $(+)$.

Le cariche positive nello spostarsi dal catodo all'anodo si perdono qualche carica positiva, idem gli elettroni, creando una zona non bilanciata, che crea un campo elettrico che impedisce ulteriori diffusioni.

## 1.2 Polarizzazione Inversa e Diretta

Detto ciò, in **polarizzazione inversa** questa zona aumenta e ciò impedisce alla corrente di passare. In **polarizzazione diretta** la zona si restringe e la corrente aumenta, la tensione rimane costante a 0,7 V.

## 1.3 Evento di Breakdown

In polarizzazione inversa può avvenire il Breakdown, sostanzialmente la tensione diventa costante a un valore $V_{BD}$ e la corrente aumenta (diodo bruciato).

## 1.4 Formule

La corrente del diodo $I_D$ è legata esponenzialmente alla tensione del diodo $V_D$.

$I_D=I_{SO}(e^\frac{V_D}{V_{TH}}-1)$

$I_{SO}=qA^2n_i(\frac{D_p}{N_DL_P}+\frac{D_n}{N_AL_n})$

Parametri:

$(n_i,D_P,D_n)$ fisici.

$(A)$ geometrici.

$(N_A,N_D)$ drogaggio di giunzioni.

$(L_P,L_n)$ profili.

A P.I. la corrente vale $-I_{SO}$.

## 1.5 Tipi di diodo

- Diodo Zener $V_{PD}=5V$, $v_{PI}=-20V$. Particolarità: non si può bruciare. ![Diodo Zener](assets/Capitolo_Diodo/Diodo_Zener.jpg)
- Diodo Schottky $V_{PD}=0.2V$, $V_{BD}<50V$. ![Diodo Schottky](assets/Capitolo_Diodo/Diodo_Schottky.jpg)
- Diodo LED Emettono luce in P.D. $V_{VD}$ varia a seconda del colore. ![Diodo LED](assets/Capitolo_Diodo/Diodo_LED.jpg)
- Fotodiodo riceve luce e lo trasforma in segnale elettrico. ![Fotodiodo](assets/Capitolo_Diodo/Fotodiodo.jpg)
- Diodo Varicap possiede una capacità di giunzione $C_j$. ![Diodo Varicap](assets/Capitolo_Diodo/Diodo_Varicap.jpg)

## 1.6 Potenza Dissipata

Vista la caratteristica dei diodi di avere 3 tipi di funzinamento diversi, calcolare la potenza dissipata dal diodo non è passeggiata.

In teoria la potenza dissipata in questi 3 periodi $(T)$ è:

$P_D=\frac{E_D}{T}=\frac{1}{T}*\int_{0}^{T} V_D(t)i_D(t)dt$

Per facilitare tale impresa la dividiamo secondo i 3 periodi e poi facciamo la somma (un pò come nella sovrapposizione degli effetti).

- In diretta, abbiamo $V$ costante:

$E_{D,F} = V_F \int_{t \in T_F} i_D(t) dt$

- In inversa, $i_D(t)=I_S$ (costante), lo si approssima a $0$:

$E_{D,R}=I_S \int_{t \in T_R} v_d(t)dt \to 0$

- In Breakdown (se non mi polverizza il diodo) ha $V_{BD}$ costante:

$E_{D,BD}= V_{BD} \int_{t \in T_{BD}} i_D(t) dt$

F=forward, R=reverse.

Quindi $P_D= \frac{1}{T}(V_F \int_{t \in T_F} i_D(t)dt + I_S \int_{t \in T_R} v_D(t) dt + V_{BD} \int_{t \in T_{BD}} i_D(t)dt)$

Con $I_S \to 0$

$P_D=V_F(\frac{1}{T} \int_{t \in T_F} i_D(t)dt) + V_{BD}(\frac{1}{T} \int_{t \in T_{BD}} i_D(t)dt)$

# Capitolo Due: Circuiti con diodo

Allora cerchiamo di capire perchè il modello linerare è comodo per "cookare" (prof la amo) i circuiti con i diodi.

## 2.1 Metodo analitico

![Circuito in esame](assets/Capitolo_Analisi_Circuitale/Analisi_Circuitale.jpg)

$KCL, KVL 
\begin{cases}
    I_R=I_D\\
    V_G=V_R+V_D
\end{cases}$

Legge di Ohm 
$\begin{cases}
    V_R=RI_R\\
    I_D=I_S(e^{\frac{V_D}{V_{TH}}}-1)
\end{cases}$

Semplificando 
$\begin{cases}
    V_D=V_G-RI_D\\
    I_D=I_S(e^{\frac{V_G-RI_D}{V_{TH}}}-1)
\end{cases}$

Conti brutti da gestire.

## 2.2 Metodo Grafico

![Grafico](assets/Capitolo_Analisi_Circuitale/Metodo_Grafico.jpg)

Molto bello ma molto poco pratico (guarda quanti bei colori (3)).

## 2.3 Modello Lineare (questo "cooka" bene)

Se $I_D > 0\\
V_D=0,7 V\\
I_D= \frac{V_G-0,7V}{R}\\ 
\iff V_G>0,7V$

Se $V_D<0,7V\\
I_D=0A\\
V_D=V_G-0V \iff V_G=V_D$

![Grafico Modello Lineare](assets/Capitolo_Analisi_Circuitale/Modello_Lineare.jpg)

![Grafico Corrente-Tensione](assets/Capitolo_Analisi_Circuitale/Grafico_Corrente_Tensione.jpg)

## 2.4 Work in progress

# Capitolo Tre: MOSFET

## 3.1 Struttura

Dopo i diodi introduciamo un nuovo componente elettronico **FONDAMENTALE**, il **MOSFET** (che qualuno non riesce a pronunciare, ciao Andre ti voglio bene $:)$ ).

Allora cerchiamo di capire cos'è e come funziona.

![MOSFET Struttura](assets/Capitolo_MOSFET/Struttura_MOSFET.jpg)

Allora questo è un MOSFET (bello vero?), partiamo dalla composizione: è un panino di metallo (parte superiore) metallo-ossido (parte inferiore) e silicio. L'ossido è il mio dielettrico $(SiO_2)$.

È un condensatore-semiconduttore, il suo scopo è quello di creare carica libera.

È formato da tre terminali $\textcolor{red}{source}$, $\textcolor{pink}{drain}$, $\textcolor{green}{gate}$, e la corrente che scorre tra drain e gate dipende dalla tensione che viene applicata tra source e gate.

MOSFET sta per:

M.O.S. $\to$ Metal Oxide Semiconductor, per la struttura.

F.E.T. $\to$ Field Effect Transistor, per il funzionamento.

Ma come funziona ?

## 3.2 Funzionamento

Il funzionamento è quello di applicare una tensione agli elettrodi e induco una carica $Q=CV_1$, ora applico una tensione $V_2$ al semiconduttore.

$>$ carica indotta, $>$ è la corrente.

A seconda della carica indotta ho 2 tipi di MOS:

- NMOS (elettroni)
- PMOS (lacune)

![NMOS](assets/Capitolo_MOSFET/NMOS.jpg)

![PMOS](assets/Capitolo_MOSFET/PMOS.jpg)

Per i ragazzi che come me si trovano disorientati quando vi è un cambio di rappresentazione ecco una breve spiegazione sul NMOS (idem al PMOS).

![Spiegazione NMOS](assets/Capitolo_MOSFET/NMOS_2.jpg)

Dunque sul gate troviamo il metallo, poi ossido (spessore $t_{ox}=1-10 nm$) e il semiconduttore che nel PMOS è N come di vede che ha il compito di modulare la carica nel canale (sopra semiconduttore e sotto ossido).

Se si è capito il funzionamento del diodo, adorerai MOSFET Live, ehm... dicevo, se si è capito il diodo capire il FET sarà più semplice. Se applichiamo una tensione positiva creiamo cariche negative libere che diventano fisse (quindi NO corrente) il MOS è in **interdizione**, se aumento la tensione creo **elettroni di conduzione**, la tensione che crea elettroni di conduzione è settata ed è detta **di soglia** (threshold voltage, $V_T$) e questa condizione viene chiamata **inversione del canale** ($V_T=0,5V - 2V$).

In parole povere più tensione applico più induco corrente (nella realtà prima o poi qualcosa fa "boom").

Introduciamo ora le due regioni nel quale si può trovare un MOSFET: **Ohmica** e **di Saturazione**.

## 3.3 Regione Ohmica

Ci troviamo in questa regione quando la tensione applicata è molto piccola quindi abbiamo degli elettroni fissi ma non liberi ($V_{DS} \to 0V$).

![Modello NMOS](assets/Capitolo_MOSFET/NMOS_Dettaglio.jpg)

La carica libera presente (poca) nel canale dipende dalla tensione che sfora la $V_T$ duqnue $(V_{GS}-V_T)$ e dalla capacità dell'ossido $C_{ox}(= \frac{\varepsilon_{ox}}{t_{ox}}WL)$ con $W=$ larghezza e $L=$ lunghezza:

$Q_{CH}=C_{ox}(V_{GS}-V_T)$

la velocità con il quale gli elettroni da source a drain dipende dal campo elettrico $\implies V_{GS}$:

$v=\mu_n E=\mu_n \frac{V_{DS}}{L}$ con $\mu_n =$ conduttività materiale.

La corrente è la carica spostata lungo $L$ in un tempo secondo $v:$

$I_{DS}=\frac{Q_{CH}}{\varDelta t}=\frac{Q_{CH}}{L/v}=\frac{C_{ox}(V_{GS}-V_T)}{\frac{L}{\mu_n \frac{V_{DS}}{L}}}=\frac{C_{ox}(V_{GS}-V_T)\mu_n V_{DS}}{L^2}$

Per $V_{DS}$ piccole il transistore (MOSFET) si comporta come una resistenza:

$R_{CH}=\frac{V_{DS}}{I_{DS}}=\frac{L^2}{\mu_n C_{ox} (V_{GS} - V_T)}$

Termini tecnici noiosi:

$V_{GS}-V_T=$ Tensione di overdrive $(V_{ov})$.

$\frac{W}{L}=$ fattore di forma.

Calcoli più precisi:

La densità superficiale di carica dipende non solo dalla tensione di Overdrive ma anche dalla tensione nel punto $x$ del canale.

$($se $V_{DS} > 0$$)$ $\implies Q'_{CH}=\frac{\varepsilon_{ox}}{t_{ox}}(V_{GS}-V_T-V(x))$

La corrente $(I_{DS})$ è la carica superficiale $(Q'_{CH})$ integrata in $W$ e $x$ spostata in $\varDelta t$ lungo $L$ a $v$.

$Q_{CH}=\int_{y=0}^{W} \int_{x=0}^{L} Q'_{CH}dxdy = \frac{WL}{V_{DS}}\int_{V(0)=0}^{V(L)=V_{DS}}Q'_{CH}dV(x) = \frac{WL}{V_{DS}} \frac{\varepsilon_{ox}}{t_{ox}}[(V_{GS}-V_T)V_{DS}-\frac{V_{DS}^2}{2}]$

$I_{DS} = \frac{Q_{CH}}{\varDelta t} = \mu_n \frac{\varepsilon_{ox}}{t_{ox}} \frac{W}{L} (V_{GS}- V_T-\frac{V_{DS}}{2})V_{DS} = \mu_n C_{ox} (V_{GS}-V_T-\frac{V_{DS}}{2})V_{DS}$

$R_{CH}(V_{DS}) = \frac{\partial V_{DS}}{\partial I_{DS}} = \frac{L^2}{\mu_n C_{ox}(V_{GS}-V_T-V_{DS})}$

![Grafico NMOS regioni](assets/Capitolo_MOSFET/Grafico_Corrente_Tensione_MOSFET.jpg)

## 3.4 Regione di Staurazione
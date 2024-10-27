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

Quando $V_{DS}$ raggiunge la tensione Overdrive l'aumento di tensione non apporta alcuna modifica questo stato si chiama **Regione di Saturazione**, il MOS si comporta come un generatore di corrente.

$I_{DS}^{SAT}=I_{DS}V_{DS} \implies I_{DS}^{SAT}= \frac{1}{2} \mu_n C_{ox} (V_{GS}-V_T)^2$

Nella realtà l'aumento di tensione aumenta la corrente di un fattore $\lambda$, non più generatore ideale:

$I_{DS}^{SAT}= \frac{1}{2} \mu_n C_{ox} (V_{GS}-V_T)^2 (1+ \lambda V_{DS})$

![Grafico regioni e livelli](assets/Capitolo_MOSFET/Grafico_Corrente_Tensione_MOSFET_2.jpg)

## 3.5 Formule

- NMOS

![NMOS](assets/Capitolo_MOSFET/Circuito_NMOS.jpg)

  -   SATURAZIONE

        $V_{GS}>V_T>0$
        
        $V_{DS}>V_{ov}$

        $I_{DS}^{SAT}= \frac{1}{2} \mu_n C_{ox} (V_{GS}-V_T)^2>0$
  
  -   OHMICA
        
        $V_{GS} > V_T$

        $V_{DS} < V_{ov}$

        $I_{DS} = \mu_n C_{ox} (V_{GS} - V_T - \frac{V_{DS}}{2}) V_{DS} > 0$
  
  -    INTERDIZIONE

        $V_{GS} < V_T$

        $I_{DS} = 0$

- PMOS

![NMOS](assets/Capitolo_MOSFET/Circuito_PMOS.jpg)


  - SATURAZIONE

    $V_{GS} < V_T < 0$

    $V_{DS} < V_{ov}$

    $I_{DS}^{SAT}= - \frac{1}{2} \mu_p C_{ox} (V_{GS} - V_T)^2 < 0$

  - OHMICA 

    $V_{GS}<V_T$

    $V_{DS} > V_{ov}$

    $I_{DS} = - \mu_p C_{ox} (V_{GS} - V_T - \frac{V_{DS}}{2}) V_{DS} < 0$

  - INTERDIZIONE

    $V_{GS} < V_T$

    $I_{DS} = 0$

## 3.6 Curiosità finali

I MOS sono ottimi interrutori visto che si comportano da circuiti aperti quando $V_{GS} < V_T$ $( V_{GS} < |V_T|$ per PMOS $)$ e da resistenza quando $V_{GS} > V_T ( V_{GS} > |V_T|$ per PMOS $)$.

Se ho MOS in serie la $K_{eq} = \frac{1}{K_{eq}}= \Sigma_1^n \frac{1}{K_i}$, in parallelo $K_{eq}= \Sigma_1^n K_i$.

# Capitolo Quattro: Analisi Circuitale pt.2

## 4.1 Caratteristiche circuitali logiche

D'ora in avanti ci troveremo davanti svariate parole che indicano determinate caratteristiche dei circuiti logici, qui potevo decidere di spiegarli la prima volta che si usavano ma ho deciso che è meglio avere un elenco dove ci sono tutte, ecco cosa vogliono dire;

- **Caratteristica (statica) Ingresso/Uscita:** Uscita in funzione degli ingressi.
- **Soglia logica:** Tensione che divide $0$ e $1$ logico.
- **Livelli logici:** Livelli di tensione che determinano $0$ e $1$ logico.
- **Noise Margin:** Massima fluttuazione tollerabile sul livello logico.
- **Tempo di propagazione:** Tempo che ci mettono i segnali ad arrivare nelle varie parti del circuito.
- **Potenza dissipata:**

  - **Statica:** Devo davvero spiegarlo ?
  - **Dinamica:** Idem con patate.

- **Area:** Spazio occupato.
- **Fan-Out:** Numero massimo di device pilotabili senza perdita di informazioni.

## 4.2 Work in progress

# Capitolo Cinque: Logica CMOS

Come abbiamo detto nel capitolo tre i MOS sono ottimi interruttori che hanno due funzioniamenti distinti: pull-up (PU) e pull-down (PD) con PD forziamo un livello "low" (L) in uscita mentre con PU un livello "high" (H) in uscita.

I PMOS sono ottimi interruttori con PU mentre gli NMOS sono ottimi interruttori con PD.

## 5.1 PDN e PUN

Cambio formule:

Per $K_n$ si intende $K_n = \frac{1}{2} \mu_n C_{ox}$ mentre con $K_p = \frac{1}{2} \mu_p C_{ox}$

Per i PMOS in Ohmica si usa anche  $I_{SD} = 2 K_p (V_{SG} - |V_T| - \frac{V_{SD}}{2})V_{SD}$ e la satuta $I_{SD}^{SAT} = K_p (V_{SG} - |V_T|)^2$.

Cambio rappresentazione:

![""](assets/Capitolo_CMOS/Rappresentazione_PMOS_e_NMOS.jpg)

- NMOS PDN

  - Portano corrente tra $D$ ed $S$

!["Porta PDN"](assets/Capitolo_CMOS/Porta_NMOS.jpg)

Per digitalizzarlo bisogna pilotare l'NMOS:

- $G$ dell'NMOS a $V_{DD}$.
- Tengo i $S$ più vicini a $GROUND$.

Una rete di NMOS con PD si chiama PDN (Pull-down Network).

- PMOS PUN

  - Spingono una corrente da $S$ a $D$

![""](assets/Capitolo_CMOS/Porta_PMOS.jpg)

Per digitalizzarlo bisogna pilotare il PMOS:

- $G$ a $0$.
- $S$ vicino a $V_{DD}$

Una rete di PMOS con PU si parla di PUN (Pull-up Network).

## 5.2 F-CMOS

La logica fully complementary MOS (F-CMOS) serve per realizzare parte con PUN e PDN che lavorano in mutua esclusione.

Proprietà:

- Potenza statica nulla.
- $V_{OH} = V_{DD}$ e $V_{OL} = GND$
- Fronte di salita $(t_R)$ e discesa $(t_F)$ dei segnali d'ingresso, creando cross-conduzione $(P_{CROSS} \propto t_{R/F})$.

## 5.3 Porte logiche

In questo paragrafo parleremo di due porte logiche $NAND-2$ e $NOR-2$ analizzandone in/out e caratteristiche elettroniche.

### NAND-2

$Y=\neg(AB)$

|$A$|$B$|$Y$|
|---|---|---|
|0|0|1|
|0|1|1|
|1|0|1|
|1|1|0|

!["Porta NAND-2"](assets/Capitolo_CMOS/Porta_NAND.jpg)

Abbiamo 3 effetti IN/OUT dipende da come commutano $A$ e $B$, con 3 inverter si ha:

- $AB(00) \to AB(11)$
- $AB(10) \to AB(11)$
- $AB(01) \to AB(11)$

Ogni "caratteristica" avrà punti caratteristici diversi, ma sempre $V_{OH} = V_{DD}$ e $V_{OL} = GND$.

In tutti i casi useremo il seguente circuito:

![""](assets/Capitolo_CMOS/Porta_NAND2.jpg)

- $AB(00) \to AB(11)$

$AB(00): \\M_3$ ed $M_4$ ON.

$M_1, M_2$ OFF.

$AB(11) : \\ M_3, M_4$ OFF.

$M_1, M_2$ ON.

$K_{M_p} = K_3 + K_4$

$K_{M_n} = \frac{K_1 K_2}{K_1 + K_2}$

Tempi di propagazione:

$t_{p,LH}^{11,00} = C \frac{V_M - V_{OL}}{K_{M_n}(V_{DD} - V_T)^2} = C \frac{V_M}{(K_3 + K_4)(V_{DD} - V_T)^2}$

$t_{p,HL}^{00,11} = C \frac{V_{OH} - V_M}{ K_{M_p}(V_{DD} - V_T)^2} = C \frac{V_{DD} - V_M}{\frac{K_1 K_2}{K_1+ K_2} (V_{DD} - V_T)^2}$

- $AB(01) \to AB(11)$

$AB(01): \\M_3$ ON ed $M_4$ OFF.

$M_1$ OFF $M_2$ ON.

$AB(11) : \\ M_3, M_4$ OFF.

$M_1, M_2$ ON.

$K_{M_p} = K_3$

$K_{M_n} = \frac{K_1 K_2}{K_1 + K_2}$

Tempi di propagazione:

$t_{p,LH}^{11,01} = C \frac{V_M - V_{OL}}{K_{M_n}(V_{DD} - V_T)^2} = C \frac{V_M}{K_3(V_{DD} - V_T)^2}$

$t_{p,HL}^{01,11} = C \frac{V_{OH} - V_M}{ K_{M_p}(V_{DD} - V_T)^2} = C \frac{V_{DD} - V_M}{\frac{K_1 K_2}{K_1+ K_2} (V_{DD} - V_T)^2}$

- $AB(10) \to AB(11)$

$AB(10): \\M_3$ OFF ed $M_4$ ON.

$M_1$ ON $M_2$ OFF.

$AB(11) : \\ M_3, M_4$ OFF.

$M_1, M_2$ ON.

$K_{M_p} = K_4$

$K_{M_n} = \frac{K_1 K_2}{K_1 + K_2}$

Tempi di propagazione:

$t_{p,LH}^{11,10} = C \frac{V_M}{K_4(V_{DD} - V_T)^2}$

$t_{p,HL}^{10,11} = C \frac{V_{DD} - V_M}{\frac{K_1 K_2}{K_1+ K_2} (V_{DD} - V_T)^2}$

### NOR-2

$Y = \neg (A+B)$

|$A$|$B$|$Y$|
|---|---|---|
|0|0|1|
|0|1|0|
|1|0|0|
|1|1|0|

!["Porta NOR-2"](assets/Capitolo_CMOS/Porta_NOR.jpg)

3 Caratteristiche IN/OUT:

- $AB(00) \to AB(11)$
- $AB(00) \to AB(01)$
- $AB(00) \to AB(10)$

con sempre $V_{OH} = V_{DD}$ e $V_{OL} = GND$.

Useremo come prima il seguente circuito:

![""](assets/Capitolo_CMOS/Porta_NOR2.jpg)

- $AB(00) \to AB(11)$

$AB(00): \\M_3$ ed $M_4$ ON.

$M_1$ ed $M_2$ OFF.

$AB(11) : \\ M_3, M_4$ OFF.

$M_1, M_2$ ON.

$K_{M_p} = \frac{K_3 K_4}{K_3 + K_4}$

$K_{M_n} = K_1 + K_2$

Tempi di propagazione:

$t_{p,LH}^{11,00} = C \frac{V_M}{\frac{K_3 K_4}{K_3 + K_4}(V_{DD} - V_T)^2}$

$t_{p,HL}^{00,11} = C \frac{V_{DD} - V_M}{(K_1 + K_2)(V_{DD} - V_T)^2}$

- $AB(00) \to AB(01)$

$AB(00): \\M_3$ ed $M_4$ ON.

$M_1$ ed $M_2$ OFF.

$AB(01) : \\ M_3$ ON M_4$ OFF.

$M_1$ OFF M_2$ ON.

$K_{M_p} = \frac{K_3 K_4}{K_3 + K_4}$

$K_{M_n} = K_2$

Tempi di propagazione:

$t_{p,LH}^{01,00} = C \frac{V_M}{\frac{K_3 K_4}{K_3 + K_4}(V_{DD} - V_T)^2}$

$t_{p,HL}^{00,01} = C \frac{V_{DD} - V_M}{K_2(V_{DD} - V_T)^2}$

- $AB(00) \to AB(10)$

$AB(00): \\M_3$ ed $M_4$ ON.

$M_1$ ed $M_2$ OFF.

$AB(10) : \\ M_3$ OFF M_4$ ON.

$M_1$ ON M_2$ OFF.

$K_{M_p} = \frac{K_3 K_4}{K_3 + K_4}$

$K_{M_n} = K_1$

Tempi di propagazione:

$t_{p,LH}^{00,10} = C \frac{V_M}{\frac{K_3 K_4}{K_3 + K_4}(V_{DD} - V_T)^2}$

$t_{p,HL}^{10,00} = C \frac{V_{DD} - V_M}{K_1(V_{DD} - V_T)^2}$

Caratteristiche Elettroniche sinstesi (Reti logiche ma a FdE, anche loro presenti su appunti).

CMOS offre:

!["Tabelle di efficienza di alcune porte logiche"](assets/Capitolo_CMOS/Tabella_Carina.jpg)

Il resto sono tecniche di espansione delle formule logiche, consigliamo la visione di "Reti Logiche" presente su appunti.

## 5.4 Porte NMOS e PMOS

Problematiche CMOS:

- Perdita di capacità di corrente dovuta ai MOS in serie.
- Aumento Area per bilanciare questo.
- Aumento di MOS = aumento capacità di carica $C_{ox}' (= \frac{\varepsilon_{ox}}{t_{ox}})$ ed area totale della porta.
- Un grande $C_L$ = aumento potenza dinamica.
- Grandi capacità parassite riducono frontesalita e discesa aumento di cross-conduzione.

### Porte NMOS

Sostituisco (con dei costi che vedremo) le PUN con resistori o PMOS accesi.

!["Porta NMOS e Pseudo-CMOS(NMOS)"](assets/Capitolo_CMOS/Pseudo_CMOS_NMOS.jpg)

Costi:

- Potenza dissipata statica non nulla.
- $V_{OH} = V_{DD}$ ma $V_{OL} > GND$
- $t_{p, HL}^{INs}$ dipende dal $K$ dell'NMOS eq., se trascuriamo $R$ e PMOS.
- $t_{p,LH}^{INs}$ dipende solo dal $K$ del PMOS o dal $RC$.

### Porte PMOS

!["Porta PMOS e Pseudo-CMOS"](assets/Capitolo_CMOS/Pseudo_CMOS_PMOS.jpg)

Costi:

- Potenza dissipata non nulla.
- $V_{OL} = GND$ ma $V_{OH} < V_{DD}$.
- $t_{p,HL}^{INs}$ dipendono solo da $K$ del NMOS o $RC$.
- $t_{p,LH}^{INs}$ dipendono da $K_{eq}$ PUN, se trascuriamo $R$ e NMOS.

## 5.5 Porte (C)MOSTri-State

In questo tipo di porte oltre a $1$ e $0$ abbiamo l'alta impedenza (HZ), in questo stato i dispositivi incaricati del PU e PD vengono spenti (o sconnessi) lasciando l'uscita flottante.

L'ingresso se acceso fa tutto normalmente altrimenti lascia l'output a HZ.

!["CMOS Tri-State"](assets/Capitolo_CMOS/Porta_Tristate.jpg)

Svantaggi:

- Aumento Area.
- Poichè bisogna raddoppiare la dimensione dei MOS per stesso $t_p$.
- MOS $E_n$ grossi almeno quanto $K_{eq}$ PUN e PDN.
- $\neg E_n$ si genera con $NOT$ su $E_n$.
- Più parassiti = meno $t_p$ e più potenza dinamica.
- Viene usata solo raramente.
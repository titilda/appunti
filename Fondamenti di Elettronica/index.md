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

Allora cerchiamo di capire perchè il
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

## Comportamento fisico-chimico

Per capire ciò bisogna fare un leggero passo indietro, dobbiamo partire dall'atomo di Silicio, ora tanti atomi di silicio vicini a temperature superiori allo $0K$ fanno ciò che gli atomi fanno, dunque rottura dei legami e successiva riformazione di questi legami con altri atomi con posti liberi. Ora se levassi un atomo di silicio con uno di boro (1 elettrone in meno) faccio una procedura di **DROGAGGIO** di tipo **ACCETTORE** visto che il boro ha uno spazio libero tende a chiedere un elettrone per andare in stabilità, se lo sostituissi con un atomo di fosforo (un elettrone in più) il drogaggio è di tipo **DONORE**, avendo un elettrone in più cede per andare in stabilità, quindi qui si spostano più elettroni.

Detto questo cosa succede ? Dobbiamo immaginarci il diodo come un rettagolo pieno di atomi di silicio, dentro questo rettangolo ci solo cariche positive e cariche negative, ora queste cariche si bilanciano in due zone: catodo $(-)$ e anodo $(+)$.

Le cariche positive nello spostarsi dal catodo all'anodo si perdono qualche carica positiva, idem gli elettroni, creando una zona non bilanciata, che crea un campo elettrico che impedisce ulteriori diffusioni.

## Polarizzazione Inversa e Diretta

Detto ciò, in **polarizzazione inversa** questa zona aumenta e ciò impedisce alla corrente di passare. In **polarizzazione diretta** la zona si restringe e la corrente aumenta, la tensione rimane costante a 0,7 V.

## Evento di Breakdown

In polarizzazione inversa può avvenire il Breakdown, sostanzialmente la tensione diventa costante a un valore $V_{BD}$ e la corrente aumenta (diodo bruciato).

## Formule

La corrente del diodo $I_D$ è legata esponenzialmente alla tensione del diodo $V_D$.

$I_D=I_{SO}(e^\frac{V_D}{V_{TH}}-1)$

$I_{SO}=qA^2n_i(\frac{D_p}{N_DL_P}+\frac{D_n}{N_AL_n})$

Parametri:

$(n_i,D_P,D_n)$ fisici.

$(A)$ geometrici.

$(N_A,N_D)$ drogaggio di giunzioni.

$(L_P,L_n)$ profili.

A P.I. la corrente vale $-I_{SO}$.

## Tipi di diodo

- Diodo Zener $V_{PD}=5V$, $v_{PI}=-20V$. Particolarità: non si può bruciare.
- Diodo Schottky $V_{PD}=0.2V$, $V_{BD}<50V$.
- Diodo LED Emettono luce in P.D. $V_{VD}$ varia a seconda del colore.
- Fotodiodo riceve luce e lo trasforma in segnale elettrico.
- Diodo Varicap possiede una capacità di giunzione $C_j$.

# Capitolo Due: Circuiti con diodo
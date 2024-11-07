---
title: "Sistemi Informativi"
author:
- "Andrea Lunghi"
---

# Sistemi Informativi

## Dati

I dati sono sempre più importati all'interno del sistema informativo aziendale.
Secondo la **Piramide DIKW** è possibile dividere i dati e le informazioni in

- **Data**: Il *dato* rappresenta la base della piramide. Esso rappresenta un elemento che descrive la realtà. Associato al dato c'è un tipo (numerico, stringa, etc) o un'unità di misura. Singolarmente il dato è poco utile (es. ci sono 32 gradi).
- **Information**: L'*informazione* è l'interpretazione di un singolo (o un insieme) di dati. Unendo diversi dati è possibile definire un contesto (es. a dicembre ci sono 32 gradi in aula).
- **Knowledge**: La *conoscenza* è ottenibile integrando l'informazione con l'esperienza. (es. se a dicembre ci sono 32 gradi in aula, c'è qualcosa che non va)
- **Wisdom**: La *saggezza* è applicare l'esperienza alla conoscenza per guidare all'azione più adatta al momento. (es. se a dicembre ci sono 32 gradi in aula, spegnere il riscaldamento).

## Organizzazioni

### Risorse

Le risorse sono ciò con cui un'organizzazione opera, sia materiale che immateriale, e sono divise in:

- **Risorse Esterne**:
  - Ambiente sociale ed economico
  - Mercato
  - Clienti
- **Risorse Interne**:
  - Risorse di scambio: Prodotti, beni e servizi
  - Risorse di struttura: strumenti finanziari, persone, infrastrutture
  - Risorse di gestione: norme, organigrammi, piani, informazioni

Le risorse hanno un ciclo di vita:

- Pianificazione
- Acquisizione
- Gestione
- Manutenzione

### Processi

I processi sono un'insieme di attività che l'organizzazione svolge per gestire il ciclo di vita di una risorsa.

#### Piramide di Anthony

Fornisce una classificazione dei processi e della applicazioni.

La tabella si basa su tre livelli:

- **Livello Operativo**: attività di tipo operative.
- **Livello di Programmazione e Controllo**: si considerano le attività tattiche come programmazione delle risorse e il controllo degli obiettivi.
- **Livello di Pianificazione Strategica**: Attività legate alla scelta degli obiettivi e politiche aziendali.

#### Modello di Porter

Questo modello considera esclusivamente i processi di livello operativo.
Le attività sono divise in:

- **Attività Primarie**: Attività verticali orientate agli obiettivi aziendali.
  - Logistica interna
  - Operations
  - Logistica Esterna
  - Marketing e Vendita
  - Servizi
- **Attività di Supporto**: Attività orizzontali che assicurano il corretto funzionamento dei processi primari.
  - Infrastruttura dell'Impresa
  - Gestione delle risorse umane
  - Sviluppo della Tecnologia
  - Approvvigionamenti

## Sistemi informativi

I sistemi informativi sono un insieme di procedure, metodi e strumenti dedicati allo svolgimento di alcune funzioni per raggiungere un risultato.
Il sistema di alimenta di eventi basati da dati, li trasforma in informazioni tramite dei processi.

Un sistema informatico è un componente del sistema informativo. Il sistema IT per mette di elaborare, archiviare e gestire le informazioni secondo le Business Rules.

I sistemi informativi sono divisi in:

- **Sistemi Operazionali**: I quali svolgono operazioni di base, quotidiane. Le parti fondamentali sono la base dati operazionale e funzioni operative.
- **Sistemi Decisionali**: Sono sistemi a supporto delle attività decisionali e strategiche che sfruttano i dati per identificare informazioni utili.

I dati sono classificabili in base al livello (operativo/controllo/strategico).
I dati *operativi* sono ben strutturati, e in alti volume e provenienti dall'interno.
Andando verso il livello *strategico* i dati diventano sempre più aggregati, il formato potrebbe essere meno strutturato e potrebbero provenire anche da fonti esterne.

### Sistemi OLTP e OLAP

I sistemi informativi sono applicazioni che interagiscono con basi di dati.
Le interazioni solitamente avvengono con le *transazioni*.

A seconda delle operazioni i sistemi si identificano come:

- **OLTP** (OnLine Transaction Processing): sono sistemi che trattano operazioni basate su un numero elevato di transazioni brevi e online. Questi sistemi sono molto rapidi e sono idonei alla gestione di processi di livello operativo e di controllo.
- **OLAP** (OnLine Analytical Processing): sono sistemi che trattano grandi quantità di dati storici che si basano su poche transazioni complesse che aggregano diversi dati e hanno bisogno di molto tempo per essere processate. Questi sistemi sono utilizzati per l'elaborazioni di dati a livello di pianificazione e strategico.

### Organizzazione e IT

Le scelte all'interno dell'azienda possono avere impatto sulle scelte tecnologiche (*Requirements Pull*) e derivano spesso dall'esigenza di nuove funzionalità o migliorare le funzionalità attuali.
In altri casi i cambiamenti delle nuove tecnologie possono portare a scelte nuove organizzative (*Technology Push*).

## Enterprise Architecture (EA)

L'*Enterprise Architecture* è usato per analizzare e descrivere lo stato attuale e futuro di un'azienda.
L'EA fornisce una panoramica dei processi, dei sistemi, delle tecnologie e delle capacità dell'azienda.

### Framework di Zachman

Questa architettura viene descritta dal **Framework di Zachman** che utilizza una matrice per descrivere l'EA.
Zachman utilizza le colonne per definire gli *Aspetti* da analizzare:

- **Dati** (Cosa): rappresentano i dati che l'organizzazione ha bisogno per operare.
- **Funzioni** (Come): analizza le funzioni che l'azienda esegue per condurre il business.
- **Rete** (Chi): le persone che sono coinvolte nell'esecuzione delle funzioni aziendali.
- **Tempo** (Quando): indica gli elementi significativi del business.
- **Motivazione** (Perché): gli obiettivi dell'azienda.

Le righe invece indicano i *Punti di Vista* che possono interessare gli stakeholder. Ogni riga introduce quindi dei vincoli sul sistema.

- **Scopo** (Contestuale): interessa chi si occupa di pianificazione e rappresenta ad alto livello il sistema in base alla dimensione, forma, relazione e obiettivi.
- **Modello dell'Azienda** (Modello Contestuale): interessa il proprietario del sistema e offre una prospettiva orientata al business.
- **Modello del Sistema** (Modello Logico): interessa il progettista e offre una specifica dettagliata del modello del sistema.
- **Modello Tecnologico** (Modello Fisico): interessa chi deve realizzare il sistema e deve tradurre il progetto logico in fisico.
- **Rappresentazione Dettagliata**: raccoglie le specifiche per i programmatori per la realizzazione effettiva del sistema.

L'intersezione tra le righe e le colonne sono definite *Viste* e forniscono le informazioni su un particolare aspetto in base ad un punto di vista.

## Elementi Tecnologici

Le tecnologie si possono dividere in tre livelli:

### Livello Applicativo

A livello applicativo le tecnologie supportano i processi operazionali e informazionali. 
L'insieme delle tecnologie a livello applicativo viene detto *portafoglio applicativo* ed è costituito da:

- **Data Warehouse** (DW): archivio che contiene i dati di un'organizzazione in un modello multidimensionale.
- **Business Intelligence** (BI): insieme di moduli per la raccolta e l'analisi delle informazioni della situazione aziendale.
- **E-Business** (EB): strumenti digitali per la comunicazione, collaborazione tra imprese ed esecuzioni di transazioni.
- **Customer Relationship Management** (CRM): moduli per l'interazione con i clienti potenziali ed esistenti.
- **Enterprise Resource Planning** (ERP): suite di software formata da diversi moduli per supportare le attività aziendali.
- **Advanced Planning and Scheduling** (APS): Applicazioni usate in ambito manifatturiero per gestire le materie prime e la capacità produttive.
- **Manufacturing Execution System** (MES): Sistemi usati in ambito manifatturiero per tracciare e documentare il processo produttivo.

### Livello di Piattaforma

L'implementazione delle applicazioni richiede l'utilizzo di componenti di supporto divise in quattro categorie:

- **Tecnologie Orientate alle Funzionalità**: Alcuni esempi sono:
  - **DataBase Management System** (DBMS): per gestire l'accesso e manipolazione dei dati.
  - **Business Process Management System** (BPMS): per modellare, automatizzare, eseguire, controllare e ottimizzare i flussi di attività.
  - **User Interface System** (UIS): servizi per l'interazione con gli utenti.
  - **Rule Engine**: sistema per definire, testare, eseguire regole di business.
- **Tecnologie Orientate agli Aspetti Funzionali**: tecnologie per garantire caratteristiche non funzionali (efficienza, sicurezza e la correttezza)
- **Tecnologie di Base**: tecnologie che permettono la comunicazione tra le applicazioni e i componenti.
- **Advanced Platform Technology**: sistemi per messaggistica, gestione degli eventi e delle transazioni.

### Livello di Architettura Fisica

Questo livello indica l'architettura fisica che hosta l'applicazione.

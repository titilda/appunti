---
title: "Reti Logiche"
author: "Niccolò Papini"
---

# Introduzione

Salve lettori, questa pagina è stata creata per riassumere o ampliare gli appunti che prendiamo a lezione, questi appunti possono servire per capire meglio l'argomento e non possono essere capiti completamente se non si seguono le lezioni. Se trovate errori o parti poco chiare vi prego di segnalarlo così provederemo a corregere. Buona lettura -NP

# Capitolo Uno: Proprietà dell'algebra

Le 10 proprietà dell'algebra sono fondamentali per riiuscire a capire come siamo arrivati alle tecniche di semplificazioni dei giorni d'oggi.

1. Elemento neutro

   - $a + 1 = 1$
   - $a + 0 = a$
   - $a * 0 = 0$
   - $a * 1 = a$
2. Idempotenza

    - $a + a = a$
    - $a * a = a$
3. Inverso o Complemento

    - $a + \neg a = 1$
    - $a * \neg a = 0$
4. Commutativa

    - $a + b = b + a$
    - $a * b = b * a$
5. Associativa

    - $a + (b + c) = (a + b) + c$
    - $a * (b * c) = (a * b) * c$
6. Distributiva

    - $a * (b + c) = a * b + a * c$
    - $a + (b * c) = (a + b) * (a + c)$
7. Assorbimento

    - $a + (a * b) = a$
    - $a * (b + a) = a$
    - $a + \neg a * b = a + b$
    - $a * (\neg a +b) = a * b$
8. Leggi di De Morgan

    - $\neg (a + b) = \neg a * \neg b$
    - $\neg (a * b) = \neg a + \neg b$
9. Consenso

    - $a * b + \neg a * c + b * c = a * b + \neg a * c$
    - $(a + b) * (\neg a + c) * (b + c) = (a + b) * (\neg a + c)$
10. Principio di Dualità

    Ogni formula logica può essere trasformata in un'altra se:

    - ogni $+$ diventa $*$ e viceversa.
    - ogni $0$ diventa $1$ e viceversa.

# Capitolo Due: Teorema di Shannon e mappe di Karnaugh

## Teorema di espansione di Shannon

**Teorema di Espansione di Shannon**: Data $f(a,b)$ posso scriverla come $a f(1,b) + \neg a f(0,b)$.

Dimostrazione

$a = 1 \implies f(1,b) = 1f(1,b) + 0f(0,b)$ visto che $a = 1 \implies \nexists 0 \to = f(1,b)$ si potrebbe espandere anche per $b$ quindi $a(bf(1,1) + \neg b f(1,0)) + \neg a (bf(0,1) + \neg b f(0,0))$.

## Mappa di Karnaugh

**Logica che sta dietro Karnaugh**
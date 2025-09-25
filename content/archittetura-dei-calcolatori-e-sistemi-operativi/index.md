---
title: "Formulario Cache"
description: "Formulario riassuntivo sui concetti di cache nella architettura dei calcolatori"
authors:
  - "Nadav Moscovici"
slug: "archittetura-dei-calcolatori-e-sistemi-operativi"
---

# Formulario Cache

## <span style="color:red">Nozioni Generali</span>

> 1.  La dimensione di un blocco di Cache deve essere multiplo della dimensione della parola.
>     Ad esempio: 128 bit = 4 parole x 32 bit
> 2.  Cache Hit -> Se il dato compare in uno dei livelli superiori della Cache
> 3.  Cache Miss -> Se il dato non compare in uno dei livelli superiori della Cache
> 4.  Write Back -> un valore che viene modificato diventa _dirty_, ovvero che ha un valore diverso nella Cache rispetto a quello presente nella memoria principale, la riscrittura del valore corretto dalla Cache alla memoria avverà solo dopo un Miss di lettura su un bit _dirty_. Il Write-Back che è l'operazione di scrittura nella Cache (**non nella memoria principale**) va effettuta sempre, tranne nel caso di _Read-Hit_

## <span style="color: red">Formule</span>

$$
\text{Dimensione Blocco} = \text{Numero Parole} \times \text{Numero di bit per parola} \\[5mm]
\text{Numero Blocchi} = \frac{\text{Dimensione Cache}}{\text{Dimensione Blocco}}\\[5mm]
\text{Indice Blocco}_{cache} = \text{Indice Blocco}_{memoria} \times \textcolor{cyan}{\text{Numero Blocchi in Cache}}\\[5mm]
\text{Numero di Set} = \frac{\text{Dimesione Cache}}{\text{Dimensione blocco}\times n} \text{ \underline{dove n è il numero di vie}}\\[5mm]
\text{Indice Set}_{cache} = \text{Indice Blocco}_{memoria} \times \textcolor{cyan}{\text{Numero Set in Cache}}\\[5mm]
\text{Hit Rate} = \frac{\text{Numero di Hit}}{\text{Numero di Righe}}\\[5mm]
\text{Miss Rate} = \frac{\text{Numero di Miss}}{\text{Numero di Righe}}\\[5mm]
\text{Miss Time} = \text{Hit Time } + \text{ Miss Penalty}\\[5mm]
\text{Tempo Medio} = \text{Hit Time } + \text{ Miss Rate} \times \text{Miss Penalty}\\[5mm]
\text{}_{\textcolor{cyan}{\text{i valori scritti in azzurino indicano che va preso il modulo, ovvero il resto della divisione}}}
$$

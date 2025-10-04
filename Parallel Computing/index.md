---
title: "Parallel Computing"
author: 
- "Andrea Oggioni"
---

# Introduction

The topic of **Parallel Computing** explores how parallel machines work and how algorithms can be parallelized, analysing all the gains and drawbacks of such a choice.

Measuring the performance of a parallel algorithm is not straightforward: multiple metrics are involved and, depending on the context, different assumptions must be made.

Starting from the abstract representation of a parallel machine, different architectures will be presented through this document.

# PRAM Machine and Performance Evaluation

The **PRAM** (Parallel Random Access Memory) machine is an extension of the conventional RAM machine. It is used to model a theoretical parallel machine able to run parallel algorithms.

PRAM is easy and intuitive and eliminates a great amount of synchronization issues, allowing us to focus on understanding the algorithm. Variations of the PRAM exists in the real world (e.g. CUDA enabled GPUs) and abtract PRAM algorithms can be easiliy adapted to other parallel architectures.

The PRAM model has the following characteristics:

- infinite number of processors each one knowing its own index;
- infinite number of registers per processor;
- infinite number of shared memory cells;
- infinite number of input memory cells;
- infinite number of output memory cells;
- each cell can store an integer of an arbitrary length;
- all processors can access any memory cell at the same time;
- processors communicate over shared memory.

In a PRAM, all the processors executes the same instructions at the same time. For this reason cocurrent writes and/or reads to/from the same memory cell may happen.

PRAM machines can be classified with respect to their ability to perform concurrent reads/writes the memory: reads/writes can be **exclusive** (all processors can concurrently read/write to/from _distinct_ memory cells) or **concurrent** (all processors can concurrently read/write to any memory cellm, even the same one).

While concurrent reads are trivial and are not an issue, what happens when two or more processors try to write something to the same memory cell? Three possible solutions:

- **Priority CW**: the value that is written to the memory is the one coming from the processor priority;
- **Common CW**: the value gets written if and only if _all_ the processors that are trying to perform the write are writing the same value (if this does not happen, the state of the cell becomes undefined);
- **Random CW**: only one randomly chosen processor is allowed to perform the write.

::: {.callout .callout-example title="Common CW machine"}
While the _priority_ and _random_ CW machines are trivial to understand, _common_ CW machines may be not so a clarificatory example is provided.

Assume you have a machine that has to evaluate the disjunction of a large number of formulae. The way the formulae are read from the input is not of our interest.

The machines start initializing a chosen output cell (e.g. the first) with a value representing "false". This may happen by making only a chosen processor write to the cell or making all the processors writing to the same cell at the same time (that works because the same value is written).

After that, each processor starts evaluating their formula and, if the formula was evaluated to be "true", writes it to the output cell, otherwise does nothing.

This algorithm works because a disjunction of formulae is true if and only if at least one of the formulae is true. If no formulae were evaluated to be true, the initial "false" value will not be modified while if one or more formulae were avaluated to be true, the corresponding processors will write a value representing "true" to the cell.

This is a valid algorithm because if multiple processors write to the output cell, they are writing the same value so there are no undefined behaviors.
:::

There are other types of PRAM machines (mostly obtained constraining the amount of resources available): machines with a bounded shared memory, machines with a bounded number of processors, machines with a bounded word size and machines with other constraints on simultaneous memory access. Those kinds of machines will not be considered for the time being.

Let $A, B \in \{\text{EREW}, \text{CREW}, \text{Priority}, \text{Common}, \text{Random}\}$. We define an order relation $A \le B \iff \text{any algorithm written for } A \text{ will also work unchanged on } B$. If $A \le B$ then $B$ is computationally stronger than $A$ but it is also less realistic.

::: {.callout .callout-note title="Note"}
The more powerful is a PRAM, the least realistic it is.
:::

With respect to the aforedescribed order relation, all the PRAMs can be sorted as follows:

$$
\text{EREW} \le \text{CREW} \le \text{Priority} \le \text{Common} \le \text{Random}
$$

As said before, measuring the performance of an algorithm running on a PRAM machine is not trivial: the existence of multiple processors running in parallel makes everything harder. Moreover, not all processors may be running at the same time (this is not in contrast with the fact that each processors run the same instructions at the same time, this will be explained later in this document) so only a fractions of them are working while the others are just idling.

While explaining all the performance metrics involved in the process of measuring, "to solve a problem of input size $n$ will be used as a synonim to "running an algorithm whose input can be expressed as a function of $n$".

Let $T^*(n)$ be the time it takes to solve a problem of input size $n$ on a single processor using the best sequential algorithm currently available, $T_p(n)$ the time it takes to solve the same problem using $p$ processors and $T_\infty(n)$ the time it takes to solve the same problem with any number of processors (read that as "as many processors that can be used").

The **speedup** is defined as _how faster can we run the algorithm using $p$ processors_ and it is calculated as

$$
SU_p(n) = \frac{T^*(n)}{T_p(n)}
$$

The **efficiency** is defined as _how good we are using each single processor_ and it is calculated as

$$
E_p(n) = \frac{T_1(n)}{p T_p(n)}
$$

An efficiency close to $1$ means that the processors are almost never idling while an efficiency close to $0$ means that the processors are almost always idling.

For a finite-size problem, it is useless to make $p$ greater than the $SU$: at some point, there will be not enough data to occupy all the processors and the efficiency will decrease.

If $T^* \simeq T_1$ (that is a realistic assumption) then

$$
E_p(n) = \frac{T_1(n)}{p T_p(n)} \simeq \frac{T^*(n)}{p T_p(n)} = \frac{SU_p(n)}{p}
$$

Let $C(n)$ be the cost (in a currency of choice, AWS credits or any similar unit) of using a PRAM to solve an $n$-sized problem, $P(n)$ the number of processors used and $T(n)$ the total time for which the processors were used, then $C(n) = P(n) \cdot T(n)$.

Let $W(n)$ the total work (i.e. the total number of operations) performed by a PRAM machine.

As processors may be idling, it is true that $W \le C$.

The energy consumption of the PRAM machine is proportional to $\frac{W}{T_p}$.

We mentioned before that we may apply constraints to the _unbounded_ PRAM machine: in this way we get a more realistic version that can be used to analyze more in detail its practical application (as an example, CUDA enabled GPUs are really similar to a constrained PRAM machine).

Two lemmas are given to simplify the computation of performance metrics in the case of specific constraints applied to PRAM machines.

::: {.callout .callout-theorem title="Lemma"}
Any problem that can be solved in $T$ steps using $P$ processors can also be solved using $P' \lt P$ processors in $O(TP/P')$ steps.

It is possible to obtain this assigning multiple _tasks_ (defining _task_ as what is done on a single processor of the bigger PRAM) so a single processor of the smaller PRAM.
:::

::: {.callout .callout-theorem title="Lemma"}
Any problem that can be solved in $T$ steps on a PRAM with $p$ processors and $m$ memory cells can be solved on a machine with $\max(p, m')$ processors and $m'$ memory cells (with $m' \lt m$) on $O(Tm/m')$ steps.
:::

## Scaling: Amdahl's law vs. Gustafson's law

**Amdahl's law** and **Gustafson's law** are two laws used to determine how the performance of a parallel machine will scale. Both laws are more or less realistic but they use different assumptions so the context in which the laws is really important.

According to Amdahl's law, any program consists of an interleaving of non-parallelizable (serial) segments with parallelizable segments.

In this case, $T_p \gt \frac{T_1}{p}$ because the serial segments will take always the same time to complete (they run on a single processor and cannot be parallelized). This means that $SU \lt p$.

Let $f$ be the fraction of the program that is paarallelizable (so that $1-f$ is the fraction that is _not_ parallelizable), then the speedup according to Amdahl's law is computed as

$$
SU(p, f) = \frac{T_1}{T_1 \cdot (1-f) + \frac{T_1 \cdot f}{p}} = \frac{1}{1 - f + \frac{f}{p}}
$$

With an infinite number of processor, the parallelizable segments of the program will take no time to execute:

$$
\lim_{p \to \infty} SU(p, f) = \frac{1}{1 - f}
$$

According to the formulae, Amdahl is pessimistic: the performance increase obtained by adding new processors decreases with the number of processors. This means that there will be a point where the performance increase cannot justify the cost of a new processor.

While Amdahl assumes that the portion of the algorithm that is parallelizable is fixed, Gustafson drops this assumption.

According to Gustafson, it is possible to increase the size of the problem to make use of more processors: let $s$ be the fraction of the time used to execute the serial part of the algorithm (so that $1 - s$ is the time to complete the parallelizable part of the algorithm), then

$$
SU(p) = \frac{T_1}{T_p} = \frac{s + p \cdot (1 - s)}{s + (1 - s)} = s + p \cdot (1 - s)
$$

Note that, according to Gustafson, the speedup depends only on the number of processors so it is always a good idea to increase processors when the size of the problem increses (linear speedup, Gustafson is optimistic).

::: {.callout .callout-definition title="Strong scaling"}
**Strong scaling** is the ability of a system to improve performance of a program increasing the number of processors while keeping the problem constant.

This type of scaling is often analyzed using Amdahl's law.
:::

::: {.callout .callout-definition title="Weak scaling"}
**Weak scaling** is the ability of a system to maintain the efficiency when the number of processor is modified proportionally to how the workload size changes.

This type of scaling is often analyzed using Gustafson's law.
:::

Gustafson's law assumptions better fit the reality because, currently, there is an almost infinite amount of data that can be processed in countless ways just to produce and extract even more data. As computing power, nowdays, is relatively cheap, it is almost always possible to add more processors to get faster results. 

_To be continued_

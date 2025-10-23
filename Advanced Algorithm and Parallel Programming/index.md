---
title: Advanced Algorithm and Parallel Programming (AAPP)
author:
  - "Andrea Lunghi"
---

## Random Access Machine (RAM)

The _random access memory_ is a theoretical model of computation that is used to analyze sequential algorithms.

- **Infinity Memory**: the machine has an infinite amount of memory cells;
- **Arbitrary Integer Size**: each memory cell can store an integer of arbitrary size;
- **Uniform Cost Model**: each basic operation (arithmetic, memory access, etc.) takes exactly one time unit.

### RAM Complexity Analysis

The complexity of an algorithm is measured by:

- **Time complexity** ($T(n)$): number of steps to complete the algorithm with input size $n$.
- **Space complexity** ($S(n)$): number of memory cells used during the execution of the algorithm with input size $n$.

## Parallel Random Access Machine (PRAM)

The _parallel random access machine_ is a theoretical model of computation that is used to analyze parallel algorithms.

$$M' = \langle M, X, Y, A \rangle$$

- $M$: is a set of RAM machines (processors) with the ability to recognize its own index;
- $X$: is a set of input cells shared by all the processors;
- $Y$: is a set of output cells shared by all the processors;
- $A$: is a set of shared memory cells.

Each time step, each processor can simultaneously perform:

- Read from shared memory ($A, X$);
- Perform an internal computation;
- Write to shared memory ($A, Y$).

### PRAM Classification

PRAMs are classified on how they handle concurrent access to the shared memory.

Read from the shared memory can happen in two ways:

- **Exclusive Read** (ER): All processors can read _distinct_ memory cells at the same time.
- **Concurrent Read** (CR): Two or more processors can access the _same_ memory cell at the same time.

Write in the shared memory can happen in two ways:

- **Exclusive Write** (EW): All processors can write in _distinct_ memory cells at the same time.
- **Concurrent Write** (CW): Two or more processors can access the _same_ memory cell at the same time.
  - **Priority**: each processor has a priority level. The highest is the one allow to write.
  - **Common**: all processors must write the same value.
  - **Random**: one of the processors is chosen randomly to write.

The classification is done by combining the two types of access.

### PRAM Complexity

The complexity of a PRAM algorithm is measured by:

| Metric              | Symbol          | Formula                                             | Context / Meaning                                                                                                                          |
| :------------------ | :-------------- | :-------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------- |
| **Elapsed Time**    | $T_p(n)$        | -                                                   | The number of steps (time) to complete the algorithm of size $n$ using $p$ processors.                                                     |
| **Sequential Time** | $T^*(n)$        | -                                                   | The time complexity of the **best known sequential algorithm** for the same problem. $\mathbf{T^*(n) \neq T_1(n)}$.                        |
| **Speedup**         | $SU_p(n)$       | $\frac{T^*(n)}{T_p(n)}$                             | Measures how much **faster** the parallel algorithm is compared to the best sequential one. Ideally, $SU_p(n) \approx p$ (linear speedup). |
| **Efficiency**      | $E_p(n)$        | $\frac{SU_p(n)}{p} = \frac{T^*(n)}{p \cdot T_p(n)}$ | Measures the **average utilization** of the $p$ processors. $0 \leq E_p(n) \leq 1$. An efficiency close to 1 is considered **optimal**.    |
| **Cost / Work**     | $C_p(n) = W(n)$ | $p \cdot T_p(n)$                                    | The total number of operations performed by **all** $p$ processors during the execution.                                                   |

### Scaling

The scaling is the ability of an algorithm to efficiently use an increasing number of processors.

The scaling is the relationship between three parameters:

- Problem Size ($n$): the size of the input data;
- Number of Processors ($p$): the number of processors used in the computation;
- Time ($T$): the time taken to complete the computation.

The scaling is classified in two types:

#### Strong Scaling

In strong scaling the problem size ($n$) is kept constant while the number of processors ($p$) is increased. The goal is to reduce the time ($T$) taken to complete the computation.

To analyze strong scaling, we use **Amdahl's Law**:

$$SU_p(n) = \frac{1}{(1 - f) + \frac{f}{p}}$$

- $f$: fraction of the algorithm that can be parallelized;
- $1 - f$: fraction of the algorithm that is sequential (cannot be parallelized).

Due to the sequential part, there is a limit to the speedup that can be achieved by adding more processors.

$$\lim_{p \to \infty} SU_p(n) = \frac{1}{1 - f}$$

This means that even with an infinite number of processors, the speedup is limited by the sequential portion of the algorithm.

Ideally, for strong scaling, we want:

$$T \propto \frac{1}{p}$$

#### Weak Scaling

In weak scaling, the problem size ($n$) is increased proportionally with the number of processors ($p$), keeping the workload per processor constant. The goal is to maintain a constant time ($T$) as both $n$ and $p$ increase.

To analyze weak scaling, we use **Gustafson's Law**:

$$SU_p(n) = s + P \cdot (1 - s)$$

- $s$: fraction of the algorithm that is sequential (cannot be parallelized);
- $1 - s$: fraction of the algorithm that can be parallelized.

This law suggests that as we increase the number of processors, the speedup can grow linearly with $p$, assuming the parallelizable portion is significant.

Ideally, for weak scaling, we want:

$$T \propto P$$

## Randomized Algorithm

A **Randomized Algorithm** is an algorithm that incorporates random choices into its logic. This introduces an element of chance into the execution path, even for a fixed input.

While the performance of a _deterministic algorithm_ can vary based on the input (e.g., Insertion Sort is $O(n)$ on sorted input, $O(n^2)$ worst-case), a randomized algorithm uses randomness to work with a high probability on an average-case performance level, rather than being vulnerable to a worst-case scenario.

Some randomization techniques are:

- _Randomizing Input Order_: Pre-processing the input randomly to prevent worst-case arrangements (e.g., randomly shuffling an array before processing).
- _Random Selection (Pivots/Candidates)_: Randomly selecting key elements (e.g., selecting a random pivot in Quicksort). This ensures that the pivot is likely to be good, minimizing the probability of a consistently bad partition.
- _Random Number Generation_: Using random numbers to make decisions during the algorithm's execution (e.g., randomized algorithms for primality testing).
- _Randomized Deterministic Algorithm_: Randomly choosing among several deterministic algorithms to solve the same problem, which can help avoid worst-case scenarios associated with any single algorithm.

### Types of Randomized Algorithms

Randomized algorithms are generally divided into two main categories based on what they guarantee: the **correctness of the output** or the **running time**.

### Las Vegas

**Las Vegas algorithms** always provide the **correct solution**, but the running _time is random_.

They keep iterating until a solution is reached and are efficient if the average case is polynomial.

### Monte Carlo

**Monte Carlo algorithms** have a _fixed_, or _bound_, running time but may produce **incorrect results** with a certain probability.

They are efficient if the complexity is polynomial in the worst case and the probability of error is low.

These algorithms are often used for decision problems, where the output is either "yes" or "no".

The Monte Carlo algorithms can be further classified based on the nature of their errors:

- **One-sided error**: The algorithm may produce incorrect results in only one direction (e.g., false positives or false negatives).
- **Two-sided error**: The algorithm may produce incorrect results in both directions (e.g., both false positives and false negatives).

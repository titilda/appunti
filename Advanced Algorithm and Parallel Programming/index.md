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

## Mincut

Given an undirected graph $G = (V, E)$ with $n = |V|$ and $m = |E|$, a cut is a partition of the vertices $V$ into two non-empty sets $(S, V \setminus S)$. The size of the cut is the number of edges crossing the partition (edges between $S$ and $V \setminus S$). The Mincut problem is to find a cut of the minimum size.

### Karger's Contraction Algorithm

The Karger's algorithm is a randomized approach to find the Mincut.

The main idea is to randomly select two nodes and contract them into a single node, merging their edges. Edge between the two nodes are removed.

This process is repeated until only two nodes remain. The edges between these two nodes represent a cut in the original graph.

The solution might not be the best, but is feasible, the probability to find the best solution is ${Pr}\ge \frac{1}{\binom n2}$ with a complexity of $O(n^2)$.

It's possible to repeat $l$ times the algorithm, keeping all the results and choosing the best one, increasing the probability to find the best solution to $1 - (1 - \frac{1}{\binom n2})^{l\binom n2}$.

Choosing $l = c \log n$ the probability become $\le \frac{1}{n^c}$ with a complexity of $O(n^2 \cdot l \cdot \log n)$.

### Karger-Stein Algorithm

Stein used a telescopic approach to improve the Karger algorithm as contracting edges from the mincut is less probable when the graph is big.

If the graph has less than 6 nodes a deterministic algorithm is used to find the mincut as the constants are high for small graphs.

If the graph has more than 6 nodes, the algorithm contract the graph to two graphs with number of vertices equal to $\frac{n}{\sqrt{2}}$ (with a probability of not having contracted the min cut of $\frac{1}{2}$).

Then it duplicate the graph and contract both of them.

Then it recurses on both graphs and returns the best result.

This have a total complexity of $O(n^2 \log n)$, this algorithm can be run $l$ times with a complexity of $O(n^2 \log n \cdot l)$.

## Sorting

### Quicksort

The quicksort is an efficient _in-place_ algorithm, based on the _divide and conquer_ paradigm.

- _divide_ (partition): Select a pivot element and rearrange the array such that all elements less than the pivot are to its left, and all elements greater than the pivot are to its right. The pivot is then in its final sorted position;
- _conquer_: Recursively sort the two sub-arrays (left and right of the pivot);
- _combine_: The combination step is trivial (zero time), as the sub-arrays are sorted in place.

```plaintext
quicksort(array A, int low, int high):
    if low < high:
        pivotIndex = partition(A, low, high)
        quicksort(A, low, pivotIndex - 1)
        quicksort(A, pivotIndex + 1, high)

partition(array A, int low, int high):
    pivot = A[low]              // Choose the first element as pivot
    i = low + 1                 // i is the boundary of the 'less than pivot' region

    for j from low + 1 to high:
        if A[j] < pivot:        // Element belongs to 'less than pivot' region
            swap A[i] and A[j]
            i = i + 1

    swap A[low] and A[i]        // Place pivot in its final position

    return i
```

The partition completes in $O(n)$ time, as it requires a single pass through the array to rearrange the elements around the pivot.

The worst-case scenario occurs when the pivot selection consistently results in unbalanced partitions, leading to a time complexity of $O(n^2)$. This can happen if the array is already sorted or reverse sorted and the first or last element is always chosen as the pivot.

The best-case and average-case time complexity of quicksort is $O(n \log n)$, which occurs when the pivot divides the array into two roughly equal halves at each recursive step.

#### Randomized Quicksort

The choice of the pivot is crucial for the performance of the algorithm. A poor choice can lead to unbalanced partitions, resulting in suboptimal performance.

To mitigate the probability to find the worst scenario, it's possible to randomize the pivot, instead of taking always the first one. This make the probability to get any configuration of partition $\frac{1}{n}$.

### Decision Tree

The Decision Tree model is used to prove the theoretical lower bound for any comparison-based sorting algorithm.

- Each internal node represents a comparison between two elements;
- Each branch represents the outcome of the comparison (less than, greater than);
- Each leaf node represents a possible permutation ($n!$) of the input array.

The minimum number of comparisons required to sort $n$ elements is at least the height $h$ of the decision tree. A binary tree of height $h$ has at most $2^h$ leaves ($n! \leq 2^h$).

$$h \geq \log_2(n!) \approx n \log_2(n)$$

This establishes that any comparison-based sorting algorithm must make at least $\Omega(n \log n)$ comparisons in the worst case.

### Counting Sort

The **Counting Sort** is a non-comparison based sorting algorithm that is _stable_ (preserves the relative order of equal elements).

The algorithm is based on 3 main steps:

1. Count the occurrences of each unique element of the input array in an auxiliary array $C$ of size $k$ (where $k$ is the range of the input values);
2. Modify the auxiliary array $C$ such that $C[x]$ stores the final position of the element $x$ in the sorted output array, this is done by summing the counts of all previous elements (`C[x] = C[x] + C[x - 1]`);
3. Iterate backwards through the input array $A$, placing each element in its correct position in the output array based on the values in $C[A[i]]$, and decrementing the count in $C[A[i]]$ for each placed element.

The complexity is $O(n + k)$, where:

- $n$ is the number of elements in the input array;
- $k$ is the range of the input values.

### Radix Sort

Radix Sort is an algorithm that sorts numbers by processing individual digits (or bits) from least significant to most significant, using the counting sort to sort the digits.

Chosen the amount of bits to consider $r$, the algorithm processes the input array $A$ in multiple passes, each time sorting the array based on a specific digit (or group of bits).

The number of passes required is $\lceil \frac{b}{r} \rceil$, where $b$ is the number of bits in the largest number.

The size of the auxiliary array is fixed to $2^r$.

A bigger base decreases the amount of iteration, but increases the size of the auxiliary array.

The complexity is $O(\frac{b}{r} (n + 2^r))$.

An ideal r is $\ln(n)$ that gives a complexity of $\theta(\frac{b \cdot n}{ \log n})$.

During the last phase there is low caching as the accessed positions are almost random.

## Order Statistics

The **Order Statistics** problem is to find the $i$-th smallest element in an unsorted array of $n$ elements.

### Randomized divide and conquer

This algorithm is based on the quicksort partitioning method.

A random pivot is selected and the array is partitioned in two sub-arrays.

If the index of the pivot is equal to the searched one, the pivot is returned, otherwise if the index is greater the algorithm recurses on the left sub-array, otherwise on the right one.

The average complexity is $\Theta(n)$, but the worst case is $O(n^2)$ when the pivot is always the smallest or the biggest element.

### Deterministic divide and conquer

This algorithm use a more complex method to select the pivot and guarantee a worst case of $O(n)$.

1. Divide the $n$ elements into $\lfloor \frac{n}{5} \rfloor$ groups of 5, with at most 4 elements remaining;
2. Find the median of each group (by sorting the group and selecting the middle element) with a constant complexity of $O(1)$ per group, as the size is fixed;
3. Take all the medians and choose the median $x$ of these medians;
4. Partition the original array around $x$;
5. if the index of $x$ is equal to the searched one, return $x$, otherwise if the index is greater recurse on the left sub-array, otherwise on the right one.

Dividing in groups of 5 ensure that at least half of the medians are greater than $x$ and at least half are smaller than $x$, this guarantee that at least $30\%$ of the elements are greater than $x$ and at least $30\%$ are smaller than $x$.

## Primality testing

The **Primality Testing** problem is to determine if a number $n$ is prime or composite.

### Trial Division

The classical deterministic method is **Trial Division**: check if $n$ is divisible by any integer $d$ from $2$ up to $\sqrt{n}$.

The complexity is $O(\sqrt{n})$.

On a modern computer this is allow to compute in a couple of minute a number with 50 bit, but for cryptography this is too slow.

### Monte Carlo Primality Test

Using _monte carlo_ it's possible to improve the time by performing an heuristics based on the _Fermat's little theorem_:

> If $p$ is a prime number and $a$ is an integer such that $1 < a < p$, then:
> $$a^{p-1} \mod p \equiv 1$$

It's possible for a composite number to satisfy this property for some base $a$, these numbers are called **pseudo-primes**.

If a number is pseudo-prime for all the bases $a$ it's called a **Carmichael number**.

It is possible to create an algorithm that:

1. Select a random base $a$ in the range $[2, n - 2]$;
2. Compute $a^{n-1} \mod n$;
3. If the result is not equal to 1, return _"composite"_, otherwise return _"probably prime"_.

The algorithm can be repeated $k$ times to increase the probability to find a prime number.

#### Efficient Exponentiation

The exponentiation can be computed efficiently with the method of _fast exponentiation_ that reduces the number of multiplications needed by using the properties of exponents:

1. If $b$ is even: $a^b = (a^2)^{b/2}$
2. If $b$ is odd: $a^b = a \cdot (a^2)^{(b-1)/2}$

```plaintext
function fastExponentiation(a, b):
    if b == 0:
        return 1
    elif b is even:
        return fastExponentiation(a * a, b / 2)
    else:
        return a * fastExponentiation(a * a, (b - 1) / 2)
```

The complexity of this algorithm is $O(\log b)$ multiplications.

During the computation it's possible to perform the **non trivial square root** $a^2 \mod p = 1$, where a is the value calculated at each step.

This makes the complexity of the primality test $O(k \cdot \log^2 n \cdot \log p)$, where $k$ is the number of iterations, $n$ is the number to test, and $p$ is the exponent.

### Cryptography

This algorithm is highly used in asymmetric encryption.

The core idea is based on the computational difficulty of factoring the product of two large primes.

The keys are generated by:

1. Select two _prime_ numbers $p$ and $q$ with 500+ bits, the primality test is used here;
2. Calculate the modulus $n = p \cdot q$;
3. Calculate the totient $\phi(n) = (p - 1) \cdot (q - 1)$;
4. Choose the **public exponent** $e$ such that $1 < e < \phi(n)$ and is relative prime to $\phi(n)$ ($\gcd(e, \phi(n)) = 1$);
5. Choose the **private exponent** $d$ such that $d \cdot e \mod \phi(n) = 1$ (the modular multiplicative inverse of $e$ modulo $\phi(n)$);
6. Public key is $(e, n)$, this will be used to encrypt messages;
7. Private key is $(d, n)$, this will be used to decrypt messages.

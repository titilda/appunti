---
title: Applied Statistics
author:
  - "Andrea Oggioni"
banners:
  wip: true
---

# Introduction

Statistics is concerned with trying to understand the relations between variables in a set of records. Machine Learning, on the other hand, is concerned with trying to perform the correct decision given the same set of records. With that said, there is a lot of overlapping between the two subjects.

A set of record is called a **dataset**. Each record is composed by values of some **variables**.

This document assumes basic statistic knowledge like **expected value**, **variance**, etc.

From the _applied_ standpoint, data is always wrong and comprise some amount of error. The same holds for the prediction on the behaviour of a variable $y = f(X)$ given the value of the other variables $X$. This is called a **rediction problem**.

The error can be decomposed in **bias**, **variance** and **noise**.

- Bias is the difference between the average prediction and the average real value;
- Variance represents how much the prediction can vary around the expected value;
- Noise is just noise and it is always there to make your job more difficult.

There is always a tradeoff between bias and variance: there is no way to get zero bias with perfect variance at the same time.

A good solution $f$ of a prediction model is a function that is not **overfit** (works only with the available data and gives wrong results with new data) nor **underfit** (does not work at all) and that is as simple as possible.

::: {.callout .callout-note title="Note"}
Throughout the rest of this document _row_ and _record_ will be used as synonims. The same holds for _variable_, _column_ and for _table_ and _dataset_.
:::

# Dimensionality reduction

All the records in a dataset live in a mathematical space there each variable correspont to a dimension and can assume all the values in the domain of that dimension. Intuitively, the volume of this space grows exponentially w.r.t. the number of variables in the dataset. From this it follows that, keeping the number of records constant, the data became exponentially more sparse if considering more and more columns.

In order to build a good prediction model, the number of dimensions should be adequate, not too big and not too small.

Usually, it never happens to have too few dimensions available (and, it that case, it is usually a matter of performing more measurements). We will now discuss the problem of having _too many_ dimensions.

Assume we have more variables than records: this means that one of the variable _must_ be a combination of the others. This phenomenon is called **multicollinearity**. Multicollinearity has a deep impact on the interpretability of a model since we cannot understand how a single variable influences the final prediction.

Another phenomena that can be observed is that the distribution of distances between points in a record gets progressively higher and more concentrated with the number of domensions. This means that it becomes harder and harder to group records by similarity.

There are many ways to perform a dimensionality reduction. We will talk about them now.

## Principal Component Analysis

**Principal Component Analysis** (PCA) is a technique that aims at finding a new base where each component is a linear combination of one or more variables. Those components are ranked by decreasing variability. Once PCA is performed, we can only consider the most variable directions, discarding the lower ranked ones.

This is based on the principle that lower variability directions does not help much in giving information abut records, as they are all quite close to one each other.

Before performing PCA, it is a good idea to normalize the data: we first center it to zero by subtracting from each column the average of the column itself and then we normalize by dividing each column by its standard deviation.

::: {.collapsible title="How to manually derive the new dimensions"}
Assume we have $n$ records $x^{(1)}, \dots, x^{(n)} \in \mathbb{R}^p$ such that they are centered:

$$
\frac{1}{n} \sum_{i=1}^n x^{(i)} = 0
$$

Let $u \in \mathbb{R}^b : \|u\|_2 = 1$.

We define the **projection score** of a record as 

$$
z_i = u^T x^{(i)}
$$

Since data is centered, the average projection score is also zero. This implies that the variance can be simplified to

$$
Var(z) = \frac{1}{n} \sum_{i=1}^n z_i^2 = \frac{1}{n} \sum_{i=1}^n (u^T x^{(i)})^2
$$

The objective is to find the direction $u$ such that $Var(z)$ is maximized. This will be the first principal component analysis. Then we can find the second by adding the constraint that it must be orthogonal to the first. The third must be orthogonal to both the first and the second and so on.

Let

$$
\Sigma = \frac{1}{n} \sum_{i=1}^n x^{(i)} x^{(i)T}
$$

then we can rewrite variance as

$$
Var(z) = u^T \Sigma u
$$

_To be continued_

<!-- TODO: 01:33-45 -->

:::

It can be proven that the variance of a component is the corresponding eigenvalue of the $\Sigma$ matrix.

We define the **explained variance ratio of the $k$-th component** as 

$$
\frac{\lambda_k}{Trace(\Sigma)}
$$

and the **cumulative variance ratio of the first $K$ components** as

$$
\frac{\sum_{k=1}^K{\lambda_k}}{Trace(\Sigma)}
$$

The plot of the cumulative variance ration of the first $K$ components can help visually understanding how each new principal component is contributing variance. When the addition of a principal component is not increasing variance too much, it means that the components already added are likely enough to produce good results without wastng much computing power and losing only small amount of information.

A **biplot** over the first two principal components can be used to understand the correlations between PCs and variablens and between variables themselves.




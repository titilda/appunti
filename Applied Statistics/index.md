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

## Nonlinear reduction

Complex data usually lies on nonlinear spaces called **manifolds** that can be described with few variables. A manifold is shaped like $\phi : \mathbb{R}^n \to \mathbb{R}^m$ with $n < m$. If we can find such a manifold, we can then work within the manifold space, which is lower dimensional.

There are three main approaches to try to work with manifolds. Those approaches will be described in the following subsections.

### Kernel PCA

**Kernel PCA** follows the same approach as standard PCA but it works on a nonlinear mapping of the data instead of working on the data itself. The idea is to apply a nonlinear transofrmation to the nonlinear data in order to make it _more linear_ and then work on it.

Kernel PCA has both advantages (can, for example, untangle circles and complex shapes) and disadvantages (it is usually impossible to go back from mapped data to the original values).

### Local similarity

Local similariti methods are used to map data into another space, preserving proximity between data points: points that are originally close to one each other will be also positioned close in the new space while _hic sunt leones_ holds for distant points and global geometry.

An example of a local similarity method is the **t-distributed Stochastic Neighbour Embedding** (t-SNE). This method is particularly suited for visualization as it can produce nice plots grouping datapoints by similarity.

t-SNE can be parametrized by **perplexity**: it is a parameter determining the amount of "local" a point should be to another in order to be considered in the same neighbourhood. A lower perplexity works with dense clusters while an higher one works with sparser ones.

Another local similarity method is **Uniform Manifold Approximation and Projection** (UMAP) which is a graph based algorithm. First, for each data point, a radius based on its $n$-th nearest neighbour is decided, then two points are considered similar if both point are within each other's radia.

<!-- This may not be correct, TODO: better understand UMAP -->

### Nonlinear mapping learning

The idea behind this is to learn an **autoencoder**, i.e. the composition of an encoder and a decoder.

<!-- TODO: autoencoders -->

# Clustering

A **clustering** procedure aims at grouping records from a dataset without having the labels provided. It is therefore an **unsupervised learning** method.

Clustering procedures triy to identify clusters by minimizing intra-cluster distance while maximizing the inter-cluster one.

Obviously, there may be many different possible _correct_ clustering in a dataset so the results must be analyzed to understand which cluster represent what.

In order to start a clustering procedure, two things must be defned before-hand: the firs is a **distance function** (which is used to determine the distance between two arbitrary records) and the second one is the **clustering algorithm** used to actually derive clusters.

The distance function is very important: in the case of columns vith very different variances, with the wrong distance function, it is possible to make columns with small variance almost completely neglected (this can be usually solved by normalization). Additionally, when talking about non-real data types (e.g. booleans or strings), the distance between values is not obvious and should be considered carefully.

Given a dataset of only numeric variables, a sample of the most used distance functions is given in the list below:

- **Euclidean distance**: $d_E(x, q): \sqrt{\sum_{i=1}^n(x_i - q_i)^2}$.
- **Squared euclidean distance**: $d_{E^2}(x, q) = d_E(x, q)^2$. This function is more sensitive to outliers.
- **Standardized euclidean distance**: $d_{SE}(x, q) = \sqrt{\sum{i=1}^n \frac{1}{s_i^2}(x_i - q_i)^2}$, where $s_i^2$ is the variability on the $i$-th dimension. The idea is to weight each contribution by something that is inversely proportional to the variance of the corresponding columns.
- Čebyšëv distance: $d_{max}(x, q) = \max_i |x_i - q_i|$. This function is really sensitive to outliers but also robust to noise.
- **Manhattan distance**: $d_M(x, q) = \sum_{i=1}^n |x_i - q_i|$. This function is useful when working with sparse count and things where the differences must add up.
- **Correlation based distance**: $d_R(x, q) = 1 - r_{xq}$ where
  $$
  r_{xq} = \frac{S_{x, q}}{\sqrt{S_x} \sqrt{S_q}} = \frac{\sum_{i=1}^n (x_i - \bar x)(q_i - \bar q)}{\sum_{i=1}^n (x_i - \bar x)^2 \sum_{i=1}^n (q_i - \bar q)^2}
  $$

Clustering algorithms may be classified according to multiple parameters: it can be **hard** (all records belong to one and only single cluster) vs **soft** (one record belong to one or more clusters) and **flat** (all clusters are on the same level) vs **hierarchical** (clusters are organized in a tree where the father encompasses al the children).

The four most used clustering techniques are **partition-based clustering**, **hierarchical clustering**, **density-based clustering** and **probabilistic clustering**.

## Partition-based clustering

PArtition-based clustering algorithms try to assign to each record one specific label. They work by initially assigning each point to a cluster (the number of clusters is an input parameter) and then, through an iterative algorithm, refine the assignment many times.

The most used algorithm in this category is **k-means**. This algorithm is randomic, therefore the result may change every time. This algorithm is also very sensitive to outliers (we can attenuate this by using the $k-medoids$ algorithm).

k-means works well with ellipsoidal clusters but cannot untangle complex shapes.

## Hierarchical clustering

Hierarchical clustering does _not_ require the target number of clusters to be defined. This is an itrative procedure.

Algorithm of this kind start by assigning each record to a different cluster (one cluster per record) then, iteratively, the two most close clusters get merged into a single one.

The distance between clusters is a function that must be chosen before hand. Examples are

- **single link**: minimum of the pairwise distances distance between points belonging to the first cluster to points belonging to the second cluster.
- **average link**: average of the pairwise distances distance between points belonging to the first cluster to points belonging to the second cluster.
- **complete link**: maximum of the pairwise distances distance between points belonging to the first cluster to points belonging to the second cluster.

**Dendrograms** are used to depict at which distance clusters get merged. Usually, the correct clustering emerges when no merges happens for a long range of distances.

## Density-based clustering

The idea behind density based clustering is that a cluster is usually identified by an area with an higher density of data points. Algorithms of this kind can untangle complex shapes.

<!-- TODO: 03:28 -->

## Probabilistic clustering



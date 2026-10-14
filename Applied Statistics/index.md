---
title: Applied Statistics
author:
  - "Andrea Oggioni"
banners:
  wip: true
---

# Introduction

Statistics is concerned with trying to understand the relations between variables in a set of records. Machine Learning, on the other hand, is concerned with trying to perform the correct decision given the same set of records. With that said, there is a lot of overlapping between the two subjects.

[Here](practice.html) you can find another document, which contains practical information about how to apply what it is described here on real datasets.

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

One of the density-based clustering algorithms is **Density-Based Spatial Clustering of Applications with Noise** (DBSCAN). This algorithm is able to exclude noisy points from the clustering.

The main idea behind this algorithm is that a point belongs to a cluster if it is close to many points of this cluster. The algorithm needs two parameters to start: **eps** is the maximum thistance two points should be spaced apart for them to be considered neighbors and the **min_points** is the minimum numbers of points used to define a cluster.

Points in clusters with less than **min_points** are considered noise.

DBSCAN is very powerful as it can cluster very complex shaped data while being resilient to outliers and noise but fails when the clusters are really different in term of density. Points near the border of a cluster are also more likely to be assigned to the neighbor cluster, producing incorrect results.

## Probabilistic clustering

Probabilistic clustering is a technique that assigns to each record the probability for it to be part of each cluster: if a record is likely not part of a cluster, it will get a small probability and the opposite is true for clusters it likely belongs to.

Since this is not an hard assignment, it is possible to have datapoints with high probability of belonging to more than one clusters, so that it is possible to have single data points belonging to multiple clusters.

<!-- TODO: gaussian mixture models 03:38 -->

## Evaluation of clustering results

We will now analyse the metrics most commonly used in the evaluation of clustering results.

The **Within Cluster Sum of Squares** (WSS) is a metric used to determine the intra-cluster variance. The smaller the better. It is computed as

$$
WSS = \sum_{C \in \text{Clusters}} \sum_{p \in C} \operatorname{Distance}(p, \bar C)^2
$$

where $\bar C$ is the centroid of cluster $C$.

The WSS metric is useful with algorithms which require the number of clusters to be set. If we plot the WSS over the number of cluster, we can visually see from which point increasing the number of cluster does not improve the WSS much: that is the number of clusters we should choose for our analysis.

Another metricis the silhouette score, which tells us how much clusters are isolated.

Let $a$ be the average distance between a point $i$ in a cluster and all the other points in the same cluster and let $b$ be the average distance between the same point $i$ and all the points in the nearest cluster, then the solhouette score of that chosen point $i$ is computed as

$$
\operatorname{Sil}(i) = \frac{b - a}{\max(a, b)}
$$

The silhouette score of an entire cluster is the average silhouette score of its points. The silhouette score of the clustering results is the average of the scores of the clusters.

Silhouette score alone does not mean anything. It is meaningful only when comparing different clustering results: the higher the better.

<!-- TODO: AIC e BIC 03:55 -->

# Linear models

A **Linear Model** wants to learn how a target variables vary w.r.t. many other variables in a linear combination.

The simples linear model is the **linear regression**.

The linear regression model can be expressed as $Y = \beta_0 + \beta_1 X + \varepsilon$ where $\beta_i$ are the coefficient to be learned, $X$ is the predictor (used to predict the value of $Y$), $Y$ is the target and $\varepsilon$ is a random variable modelling the error.

With a fixed $X$, $Y$ is a random variable.

We take the following assumprions for granted when talking about the error: the error is **homoscedastic** (i.e. has the same variance everywhere) and it is gaussian-shaped.

It can be proven that, with the **least-square error** method, the optimal model coefficients can be computed with

$$
\beta_1 = \frac{\sum_{i=1}^n (x_i - \bar x)(y_i - \bar i)}{\sum_{i=1}^n (x_i -  \bar x)^2} \\
\beta_0 = \bar y - \beta_1 \bar x
$$

Since we have the measurement error, the $\beta_i$ parameters are only an estimation of the real one. We define the **confidence interval** as the range which contains the real value of $\beta_i$ a given percentage of the time.

<!-- 04_1:22 watch recording or appendix textbook -->

The simple linear regression works with only two variables. If we want to analyze how many variables interact together, we need to switch to the **multiple linear regression model**. This model is almost equivalent to the simpler one, but with more coefficients. here, each coefficient tells us how each variable contributes to the whole estimation of the target variable, keeping all the others fixed.

<!-- TODO: how is this a vector??? 04_2:6 The multiple linear regression model can be expressed in a compact shape. Let $n$ be the number of observations, $p$ the number of parameters and $r = p - 1$ the number of variables, then, we can define $Y \in \mathbb{R}^n$ -->

The estimation of the parameters is similar to the previous case: the method is called **ordinary least squares** and aims at the minimization of the residual sum of squares.

<!-- TODO: giorgio aiutami tu -->

# Logistic regression

Assume we want to predics a binary outcome. This is equivalent to predicting the probability that some event occurs or not. Linear scale is not a good fit for this task because they can go from $-\infty$ to $+\infty$. We want to have a scale that goes from 0 to 1 that somehow resembles a step function centered in 0.

Let $p$ be the probability that some event occurs, then we define the **odds** of this events as the ration between the probability of it happening vs. the probability of it _not_ happening:

$$
\operatorname{odds} = \frac{p}{1-p}
$$

Then we define the **log-odds** (or, better, **logit**) of the event as $\log(\operatorname{odds})$.

The log-odds of the vent will be the target of a linear regression model:

$$
\log\left(\frac{p}{1-p}\right) = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + \dots
$$

We now define the **sigmoid function**

$$
\sigma(z) = \frac{1}{1+\exp{(-z)}}
$$

It holds that

$$
p = \sigma(\beta_0 + \beta_1 x_1 + \beta_2 x_2 + \dots)
$$

This is a logistic regression model: it is a linear model which prediction is piped into a sigmoid function.

Let $\beta_j$ be one of the predictors. Keeping all the others fixed, the odds are multiplied by $e^{\beta_j}$ for each unit increase in the predictor.

::: {.callout .callout-note title="Note}
Remeber. It is not the odds that are predicted by the model. It is the log-odds.

Each unit increase of a predictor, multiplie the odds by the corresponding weight.
:::

<!-- TODO: categorical predictors? + continuous predictors? -->

It is possible to convert a logistic model into a prediction one by choosing a treshold: the label will be given according to the comparison of the predicted log-odds with said treshold.

## Interactions

It is possible to add interactions between predictors to see how their combination affects the results. As an example

$$
\log\left(\frac{p}{1-p}\right) = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + \beta_3 x_1 x_2
$$

To better visualize how one predictor influences the other, it is a good idea to split the model into submodels defined by cases and plot each one on the top of the other. Each case can be a different value or a different range of values a predictor can assume.

## Maximum Likelyhood

As always, the model parameters are estimated using maximum-likelyhood.

We define the **likelyhood function** $L$ as

$$
L(\beta) = \prod_{i=1}^n p_i^{y_i} (1 - p_i)^{y_i}
$$

This comes from the fact that the outcome of a logistic regression is distributed like a Bernoulli distribution.

We define the **log-likelyhood fnction** as the logarithm of the likelyhood:

$$
\ell(\beta) = \log L(\beta) = \sum_{i=1}^n \left[ y_i \log p_i + (1 - y_i) \log(1 - p_i) \right]
$$

The maximization of the log-likelyhood function is equivalent to finding the parameters $\beta$ of the model that make the classification as plausible as possible.

## Model evaluation

In order to understand if the whole model is somewhat useful, we define $\ell_{full}$ as the log-likelyhood of the whole model with all the predictors and $\ell_{null}$ as the log-likelyhood of the model containing only the intercept. We compute the **likelyhood ratio** as $2(\ell_{full} - \ell_{null})$: the larger the value, the better is the job done by the predictors in making a better model w.r.t. the intercept-only model.

Another test we can do is to compute the **McFadden's pseudo R^2**:

$$
R^2_{\text{McFadden}} = 1 - \frac{\ell_{full}}{\ell_{null}}
$$

<!-- TODO: confidence intervals 05:34 -->

Other useful metrics are summarized in the **confusion matrix**:

<a href="https://www.researchgate.net/figure/Calculation-of-specificity-sensitivity-and-accuracy_fig4_378386094"><img src="https://www.researchgate.net/profile/Binson-V-A/publication/378386094/figure/fig4/AS:11431281266236121@1722787340735/Calculation-of-specificity-sensitivity-and-accuracy.png" alt="Calculation of specificity, sensitivity and accuracy"/></a>

Another source of information is the **ROC curve**: we put the **false positives rate** (i.e. $1 - \text{specificity}$) on the x-axis and the **true positives rate** (i.e. the sensitivity) on the y-axis and then draw a curve varying the treshold. The larger the area under the curve, the better the model. Models with less tha 0.5 times relative AUC are worse than the random models. Models with 1.0 AUC are perfect models.

## Perfect separation

As the coefficient $\beta_1$ determines the steepness of the logit, when perfect separation (or almost perfect, with a point _reeeeeeeally_ close to the treshold on each side), the fitting may fail to converge or produce humongous values for the coefficients.

# Classification

Classification is a supervised learning task that eats data shaped like $(x_i, y_i)$ in order to predict $\bar y$ when given $\bar x$. Classification can be **binary** or **multiclass** (actually, it is probabilities that one input belongs to a given class that are predicted, not directly classes).

Geometrically speaking, a classifier learns how to partition an hyperspace into several distinc regions: one for each label.

A simple algorithm to implement classification is **k-nearest neighbors**: it tries to predict the label of an element using the $k$ nearest ones (probability of a class is proportional to the number of neighbors with same class).

The problem of KNN is that $k$ must be chosen manually: a small $k$ makes the model really sensitive to noise and subject to really high variance, the opposite is true for bigger $k$s. Moreover, KNN is really expensive in terms of computing resources during inference.

Other kinds of classification algorithms includes **logistic regression**, **decision trees**, **random forests**, **boosting**, **neural networks**.

In order to train a classifier, the target is the minimization of the **loss function** (i.e. something similar to the error function). The classifier must also learn how to generalize so that it does not work only with training data but also with new unseen data.

The best model is the one wich has the smaller **generalization loss**. While **training loss** becomes smaller and smaller with each training pass, the generalization error will initially decrease and then start to increase again as the model is unlearning to generalize (**overfitting**).

The training loss is usually always more optimistic and smaller than the generalized one.

In order to have the model learn to generalize with only the training data, one of a few possible techniques must be applied.

The first technique is **cross validation**: it consists in splitting the entire dataset in many small folds ($K$) and, for each fold, train the model uing all the others and validate on that one (pay attention to [data leakage](#data-leakage)).

To _measure_ the performance of the model, **cross validation estimate** is used:

$$
CV_K = \frac{1}{K} \sum_{k=1}^K \left[ \frac{1}{|\mathcal{D}_k|} \sum_{i \in \mathcal{D}_k} L(y_i \hat f^{(-k)}(x_i)) \right]
$$

In order to choose the number $K$ of folds in which partition the dataset, hyperparameter tuning is used (i.e. try them all and choose the best). In general, a small number of folds results in a lower training error and in an higher variance (the opposite holds with a big $K$). <!-- TODO: in chse senso varianza? -->

Another technique is called **bootstrapping**. <!-- TODO: non l'ho capito  06:28-33 -->

As said before, having to deal with too many variables can cause serious problems: it is a good idea to try to reduce the number of dimensions, preferring simple models.

There are many many ways to do that. All those methods can be classified in **feature selection** (**filter methods**, **wrappers** and **embeddings**) and **feature extraction**.

Filter methods assings to each dimensions a score and then selects the highest ones. The score does not depend on the final model.

Wrapper methods consists in training models with subsets of features (either starting with one and adding one at a time or starting with all and removing one at a time) and then choosing the best one.

Embedded methods consists in a special model training such that the model itself can learn what the most important features are. The typical mechanisms which can make this magic happen are **coefficient shrinkage** (e.g. **lasso**) and **feature importance** (e.g. **tree-based methods**).  Out of all the rechniques, this is also the one that reduces overfitting the most because it usually can noticeably improve generalization capabilities.

Another source of instability for a model is the ize of it's learned coefficients: the higher the coefficients, the higher the risk of instability. It is a good idea to also penalize a model which coefficients are too big. This is called **regularization**.

With regularization, the loss function acquires the following shape:

$$
\operatorname{Loss}(\beta) + \lambda \cdot \operatorname{Penalty}(\beta)
$$

Since regularization penalizes big weights, it already performs a _kind of_ feature selection, setting parameters to zero.

**Ridge regression** is a kind of penalty that is proportional to the $L^2$ norm of the weights. It does not perform feature selection _per se_ because it tends to make weights really small, but not null.

The **LASSO** (_Least Absolure Shrinkage and Selection Operator_) regression, instead uses an $L^1$ penalty rule. With those rule, coefficients are more likely to became exactly zero. LASSO can therefore be used to achieve both regularization and feature selection at the same time.

<!-- sparse model something something 06:48-51 -->

## Data leakage

**Data leakage** is the phenomenon that happens when the validation set influences the training of the model, making its performance look better than what it actually is.

In order to prevent leakage, the training set must actually be split in three (training, validation and test) and not only in two. The new set (the **test** one), must only be used at the very end, after every trial, every tuning and every everything, to unbiasedly evaluate model performance.

<!-- 06: appendix -->

# Decision trees

Decision trees are an alternative way to perform classification. The partitioning of the hyperspace happens by choosing each time one dimension and putting a treshold. The partitions are therefore hyper-rectangles.

The result is a tree with a condition on each node: for each input, by walking down the tree and taking the correct branches, you end up in the leaf with the correct cathegorization.

In order to decide which treshold is the best one for each node, the RSS is computed for each treshold and the one with the lower RSS is chosen.

Assume we split on variable $x_i$ on a node, the two child nodes may split on completely different variables with respect to each other: this is the **interaction** mechanism, arising when a variable has some influence on the result only if some other variable respect some condition.

How do we decide how much to grow the tree? Me make a BIG one and then we prune it back until an optimal expected performance on new data.

<!-- TODO: pruning 07_1:20-23 -->

Trees work best when the classification can be described as a piecewise function of the search space (in fact, the model they produce is exactly a piecewise function of the search space).

The problem is that a single, well placed, change in the data may cause an entirely different tree: this algorithm is higlhy unstable. The solution is to use **ensembles** of methods. This means to have a number of different variations of various algorithms to make a prediction and use a combination of all those predictions to make the final one.

One can choose to build the algorithm ensemble choosing one or more of the following strategies:

- use of different algorithms;
- use of multiple instances of the same algorithm with different hyperparameters;
- use of multiple instances of the same algorithm trained on different samples of the data;
- use of multiple instances of the same algorith trained on different subsets of predictors.

The idea is that the error of many models, on average, cancels out. The usage of $B$ different algorithm intances multiplies the variance by $1 / B$.

This strategy can also help to speedup the training of the model (also providing best results) if the dataset is too big for one machine: the dataset is split across machines, each one training a different model with a different algorithm.

To create ensembles, there are four main methods:

- bagging;
- random forests;
- boosting;
- stacking.

## Bootstrapping and AGGregatING (Bagging)

**Bagging** is the simple application of bootstrapping: one different tree for each different sample, predictions are averaged (majority wins in case of classification).

<!-- TODO: out of bag cose -->

Trees produced by bagging should _not_ be pruned as the stabilization is already performed by averaging them.

Assume one predictor has a really strong influence on the result: many trees would probably succesfully identify it, making many trees very correlated, defeating the beneficial effect on the variance. To selve this, we use random forests.

## Random forests

Random forests are used in the case of very strong predictors: it is just bagging on random predictor subsets. This way, the trees are forced to consider different predictors.

Assume we use $m$ predictors. As $m$ gets smaller, the tree gains in diversity but the splits may be weaker. Let $p$ be the number of predictors, then common choices are $m \simeq \sqrt p$ for classification and $m \simeq p/3$ for regression.

Regarding the number of trees, it is just a matter of progressively increasing the number of tree until the OOB <!-- TODO: che caspita è? --> stabilizes.

<!-- 08_2:22 -->

## Boosting

Boosting is the technique that consists in fitting a model, then fitting another one on it's residual, then fitting another one of this last one's residual and so on.

_Trust me, boosting works really well. Maybe a bit too well._

Let $\hat f_0(x)$ be the first tree, $\hat h_b(x)$ be the three fitted at step $b$  and $\eta$ be the learning rate, then the complete model can be expressed as

$$
\hat f_B(x) = \hat f_0(x) + \sum_{b=1}^B \eta \hat h_b(x)
$$

Any tree that is not the first one, is called **weak learner**, mainly due to the fact that they should contribute only a small amount to the whole model. They only capture small corrections and reduce the risk of overfitting, until the model gets too complex: in that case, boosting, can make the effect of overfitting much worse.

The three hyperparameters for this algorithm are the number of trees $B$ (the higher, the more corrections can be applied), the learning rate $\eta$ (the bigger, the more influential each weak learner is) and the tree depth $B$ (the bigger, the more complex each correction is).

## XGBoost

EXtreme Gradient Boosting (XGBoost) is an optimized version of normal boosting.

The whole model is similar to the one used with normal boosting:

$$
\hat f_M(x) = \sum_{m=1}^M \eta \hat h_m(x)
$$

The loss function is

$$
\mathcal{L} = \sum_{i=1}^n \ell(y_i, \hat y_i) + \sum_{m=1}^M \Omega(h_m)
$$

where

$$
\Omega(h) = \gamma T + \frac{1}{2} \lambda \sum_{j=1}^T w_j^2
$$

where $T$ is the number of leaves in the tree, $w_j$ is the prediction score in the $j$-th leaf, $\gamma$ penalizes adding leaves and $\lambda$ penalizes large leaf weights.

<!-- What was w_j and ell? -->

In addition, each tree should be fitted with both row and column subsampling.

<!-- 08_2:34 utile -->

## Stacking

It is possible to combine ensemble methods and all the other methods above, with a procedure called **stacking**: many models are trained with different algorithms and a **meta learner** learns how to combine their outputs. Obviously, to prevent data leakage, the meta learner should _not_ be trained on the same observations that were used to train the other models.

<!-- 09 -->


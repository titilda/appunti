---
title: Applied Statistics (practice)
author:
  - "Andrea Oggioni"
banners:
  wip: true
---

# Introduction

In this document we will make heavy use of [Python](https://www.python.org/), together with the [Pandas](https://pandas.pydata.org/), [Numpy](https://numpy.org/), [Matplotlib](https://matplotlib.org/) and [Seaborn](https://seaborn.pydata.org/) libraries. Please check that all of the required software is installed. Everything described here, expecially when experimenting, works best on a notebook: my suggestion is a local instance of [Jupyter](https://jupyter.org/), but also [Google Colab](https://colab.research.google.com/) is fine. Remember: when you are working with Python, the use of virtual environments is strongly encouraged (otherwise you risk breaking your system, am I right, Ubuntu?)

All the following code boxes may or may not be context-sensitive. Integration of a python notebook in this site is a really nice but out-of-reach idea, so the reader must deduce it by themself. Sorry.

Usually, all the code blocks, requires some libraries, here we will put all the necessary ones for the entire file, just to prevent repeating them everywhere.

```py
import pandas as pd
pd.options.display.float_format = '{:.2f}'.format

import matplotlib.pyplot as plt
plt.rcParams["figure.figsize"] = (8,8)

import seaborn as sns

import numpy as np
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
from mpl_toolkits.mplot3d import Axes3D
import time
from tqdm.notebook import tqdm
```

# Heating up

To start working with datasets, we need to load one up:

```py
df = pd.read_csv("path/to/dataset.csv")
```

One can query various properties of the dataset (stored into a `DataFrame` object):

```py
df                        # try to print the entire dataset
df.shape                  # shape of the dataset
df.columns                # array of dataset dimensions
df.info()                 # low level info about the dataset
df[['col1', 'col2', ...]] # like `df` but for a subset of the columns (this returns a new dataframe)
df['col1'].value_counts()
df['col1'].unique()       # array of all the unique values in the column
df['col1'].nunique()      # counts the number of unique values in the column
```

One can exclude the `NaN` from the `value_counts` (with `dropna=True`) and/or normalize (with `normalize=True`).

# Basic plots

We use the seaborn library for this because matplotlib is terrible to work with and somebody else (the seaborn authors) already did the dirty job for us.

In order to plot the amount of times each unique value appears in a column, we use `sns.countplot`.

```py
ax = sns.countplot(data=df, x='colN', palette=['#color1', '#color2', ...], hue='colM', legend=False)
ax.set_title('Chart title')
```

if `colN` is the same as `colM`, the plot will look like this

![https://seaborn.pydata.org/_images/countplot_1_0.png](Image from seaborn documentation)

otherwise, a comparative plot between different classes will be obtained

![https://seaborn.pydata.org/_images/countplot_3_0.png](Image from seaborn documentation)

One can plot many plots in the same image like this

```py
fig, ax = plt.subplots(nrows=2, ncols=3, figsize=(13, 10))

ax = ax.flatten()

for i in range(len(cat_vars)):
    sns.countplot(..., ax=ax[i])
    ax[i].set_title('...')

plt.tight_layout()
plt.show()
```

For cathegorical variables, pue charts are also a good option:

```py
c = df['col1'].value_counts()

plt.pie(
    c,
    labels = c.index,
    startangle = 90,
    colors = colors
)
```

# Basic statistics

We can extract basic statistical information about columns from a dataset:

```py
cols = ['col1', 'col2', ...]

df[cols].describe()

df['col1'].quantile(0.25)
df['col1'].quantile(0.50)
df['col1'].quantile(0.75)
df['col1'].mean()
df['col1'].std()
df['col1'].var()
df['col1'].min()
df['col1'].max()

iqr = df['col1'].quantile(0.75) - df['col1'].quantile(0.25)
```

An histplot is used to plot the count of rows that fall into specific discrete bins:

```py
sns.histplot(
    data = df,
    x = 'varN',
    kde = True,
    alpha = 0.4,
    edgecolor = (1, 1, 1)
)
```

We define **skeweness** as a measure of the asymmetricity of a given distribution:

$$
\operatorname{skeweness} = 3 \cdot \frac{\operatorname{mean} - \operatorname{mdian}}{\operatorname{std}}
$$

When skeweness is greater than zero, we have a right skew, the opposite holds for the opposite skeweness.

We define **kurtosis** as the tailedness of a distribution (the higher it is, the fatter the tails are).

```py
k = scipy.kurtosis(df['col1'])
s = scipy.skew(df['col1'])
```

Another good way to display a good resume of variables distribution is the boxplot:

```py
sns.boxplot(data=df, y='col1')
```

The covariance of two varibles tells us if variables vary in the same or opposite way. The problem of covariance is that it is influenced by the scale of the variables.

```py
c = df[['col1', 'col2', ...]].cov()
sns.heatmap(c, annot=True, cmap='Blues')
```

The correlation matrix solves the problem of the covariance but it is limited to linear relationships:

```py
c = df[num_vars].corr()
sns.heatmap(c, cmap="Blues",annot=True);
```

Scatterplots & co.:

```py
sns.scatterplot(data=df, x='col1', y='col2', hue='col3')
```

A pairplot is a group of scatterplots between many variables. On the diagola, the distributions of the variables are shown:

```py
vars = ['col1', 'col2', ...]
sns.pairplot(df, x_vars=vars, y_vars=vars, hue="DEATH_EVENT", height=5).add_legend()
```

# Dimensionality reduction

## Principal Component Analysis

Assume we want to perform PCA on some dataset. We can either do it by hand or use pre-baked code.





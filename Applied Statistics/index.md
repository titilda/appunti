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


---
title: Software Engineering for HPC
author:
  - "Andrea Oggioni"
---

# Introduction

This documents contains a brief overview of all the techniques and procedures used to support robust software creation and such. [Related document](/Software%20Engineering%202/index.html).

A **software engineer** job is to architecture and organize work on a complex program in order to have it completed efficiently while preventing errors. This is impossible in reality so software engineers also needs to know how to handle problems, regressions and such.

Doftware engineers are made a different kind of engineers: they do not work on tangible things, they cannot (usually) touch their products. Also, software is very malleable compared to tangible stuff, it can evolve much quickly.

There are multiple ways to organize software development: **code & fix**, **waterfall**, **iterative**, **agile**, **devops** and much more. All those methods usually comprise multiple phases: **feasibility study/project estimation**, **requirement analysis and specification**, **design**, **coding/unit testing**, **integration/system testing** and **maintenance**. Those phases can be partially overlapping and or repeated ina loop until the final product is finished.

## High Performance Computing (HPC)

HPC is the name of the field that encompasses the study of the usage of big _megachonk_ computers in order to, usually, solve large problems or simulations, process large amount of data and such. HPC computations are usually split over a big cluster of computers, therefore efficient communication between them is essential (and, also, very sensitive because it can cause lots of errors).

A _good_ HPC software must satisfy a few requirements:

- **correctness**: the software must correctly reflect the model of the reality we want to work on;
- **performance**: the software must make efficient use of the resources it has access to;
- **portability**: the software can be moved from a machine to another without much trouble;
- **maintainability**: the software must follow the following principles:
  - **operabiity**: the software must be easy to use;
  - **simplicity**: the software must be easy to understand;
  - **evolvability**: the software must be easy to modify as new requirements emerge;
- **reliability**: the software must have a low probability of experiencing **failures** (termination of the ability of the program to do what it is supposed to do), **faults** (defects found in production, can be hardware, software or configuration errors) or **defects** (non-compliance with the specification);
- **scalability**: the software must handle well load increases.

# Requirement engineering

**Requirement engineering** is the "science" that study how to specify the requirements a software product must follow. In order to plan the development of a bit software product, one must define the **goals** (what are the final objectives), the **domain assumptions** (what we assume as true about the context and on how the users will interact with the product) and the **requirements** (what will the software do and how, in response to what).

::: {.callout .callout-definition title="Requirement completeness"}
Let $R$, $G$ and $D$ be the set of requirements, the set of goals and the set of domain assumtions respectively.

$R$ is said to be **complete** iff $R \cap D \models G$.
:::

Obviously, if the domain requirements are incorrect, the software may not work correctly (in the context it has been deployed).

The requirements be **functional** (what whe software must do) **unfeasible functional** (what is not under control of the software but it is necessary for the entire system to work), **non functional** (how the software should do its job) and **technical/constraints** (how the software must be built).





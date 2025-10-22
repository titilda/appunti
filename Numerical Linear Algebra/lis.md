---
title: "LIS cheatsheet"
author: 
- "Andrea Oggioni"
---

# Introduction

In this cheatsheet, a rudimentary overview of a couple executables that can be compiled from the test folder will be provided.

The instructions included in this document targets LIS version 2.1.10. The sources for LIS can be downloaded [here](https://www.ssisc.org/lis/dl/).

All the executables used in this document are obtained from the compilation of the sources in the test folder in the sources.

The following table lists all the executable and their respective sources:

| Executable | Source |
| ---------- | ------ |
| `test1`    | `test1.c` |

To compile the sources, the following command was used:

    mpicc -DUSE_MPI -I</path/to/LIS/includes> -L</path/to/LIS/library> -llis </path/to/source.c> -o </path/to/output/executable>

The compiled executables can be both run standalone or with `mpirun`.

# Linear system solution

The `test1` executable is used to solve linear systems. It includes multiple iterative solvers that can be tuned with various parameters.

Syntax: `./test1 INPUT_FILE RHS_SETTINGS SOLUTION_FILE HISTORY_FILE [OPTIONS]` where

- `INPUT_FILE` is the $A$ matrix as in $Ax = b$ stored in **matrix market format** (mtx);
- `RHS_SETTINGS` is the $b$ of the system: it can be a number (to set all the entries in the vectors to that number) or a file containing the vector in mtx format;
- `SOLUTION_FILE` is the file to which the solution is written;
- `HISTORY_FILE` is the file to which all the intermediate solution approximation are written.

Multiple optional options can be passed to the executable:

| Option                                | Description                                                    |
| ------------------------------------- | -------------------------------------------------------------- |
| `-i jacobi|gs|cg|bicgstab|gmres|bicg` | Selects the method used to solve the system                    |
| `-tol <tolerance>`                    | Sets the tolerance for the iterative method                    |
| `-maxiter`                            | Sets the maximum number of iterations for the iterative method |
| `-restart`                            |                                                                |
| `-p jacobi|sainv|ilu`                                  | Selects a preconditioner |

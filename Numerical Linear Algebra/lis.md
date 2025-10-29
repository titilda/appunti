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

| Executable | Source     |
| ---------- | ---------- |
| `test1`    | `test1.c`  |
| `etest1`   | `etest1.c` |

To compile the sources, the following command was used:

    mpicc -DUSE_MPI -I</path/to/LIS/includes> -L</path/to/LIS/library> -llis </path/to/source.c> -o </path/to/output/executable>

The compiled executables can be both run standalone or with `mpirun`.

# Linear system solution

The `test1` executable is used to solve linear systems. It includes multiple iterative solvers that can be tuned with various parameters.

Syntax: `./test1 INPUT_FILE RHS_SETTINGS SOLUTION_FILE HISTORY_FILE [OPTIONS]` where

- `INPUT_FILE` is the $A$ matrix as in $Ax = b$ stored in **matrix market format** (mtx);
- `RHS_SETTINGS` is the $b$ of the system: it can be a number ($1$ to set $b = (1, 1, \dots, 1)$, $2$ to set $b = A \times (1, 1, \dots, 1)$) or a file containing the vector in mtx format;
- `SOLUTION_FILE` is the file to which the solution is written;
- `HISTORY_FILE` is the file to which all the intermediate solution approximation are written.

Multiple optional options can be passed to the executable:

| Option                                | Description                                                    |
| ------------------------------------- | -------------------------------------------------------------- |
| `-i jacobi|gs|cg|bicgstab|gmres|bicg` | Selects the method used to solve the system                    |
| `-tol <tolerance>`                    | Sets the tolerance for the iterative method                    |
| `-maxiter <number>`                   | Sets the maximum number of iterations for the iterative method |
| `-restart <number>`                   | Restart parameter for GMRES                                    |
| `-p jacobi|sainv|ilu|ssor|ilut`       | Selects a preconditioner                                       |
| `-ilu_fill <number>`                  |                                                                |
| `-ssor_omega <number>`                |                                                                |

## Examples

```sh

```

# Eigenproblem solution

The `etest1` executable is used to solve eigenproblems. It can be tuned with various parameters.

Syntax: `./etest1 INPUT_FILE EVEC_FILE HISTORY_FILE [OPTIONS]` where

- `INPUT_FILE` is the input matrix in MTX format;
- `EVEC_FILE` is the file in which the eigenvectors are written;
- `HISTORY_FILE` is the file to which all the intermediate solution approximation are written.

Multiple options can be passed to the executable:

| Option                     | Description                                                                                            |
| -------------------------- | ------------------------------------------------------------------------------------------------------ |
| `-e pi|ii|cr|rqi|si|li|ai` | Selects the eigensolver                                                                                |
| `-i`, `-p`                 | Options for the linear solver used by the method. Same options as here [here](#linear-system-solution) |
| `-emaxiter <number>`       |                                                                                                        |
| `-etol <number>`           |                                                                                                        |
| `-shift <number>`          |                                                                                                        |
| `-ie cg|ii|rqi`            |                                                                                                        |
| `-ss <number>`             |                                                                                                        |

_Work in progress_
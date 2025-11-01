---
title: "Eigen by examples"
author: 
- "Andrea Oggioni"
---

# Introduction

`Eigen` is a library that is used to perform matrix calculation in C++. Multiple examples are given in the following sections.

## Basic data types

```c++
using namespace Eigen;

size_t n = 10;
size_t m = 20;

// Vectors
VectorXd all_zeros = VectorXd::Zeros(n);
VectorXd all_ones = VectorXd::Ones(n);
VectorXd all_given = VectorXd::Constant(n, 3.14f);
VectorXd random = VectorXd::Random(n);

// Dense matrices
MatrixXd all_zeros = MatrixXd::Zeros(m, n);
MatrixXd identity = MatrixXd::Identity(m, n);
MatrixXd all_given = MatrixXd::Constant(m, n, 2.71f);
MatrixXd random = MatrixXd::Random(m, n);

// Diagonals

// Sparse matrices
SparseMatrix<double> sparse(m, n);

using T = Triplet<double>;

std::vector<T> triplets;
triplets.reserve(n);
for (...) {
    triplets.emplace_back(triplet(i, j, 1.61f));
}
sparse.setFromTriplets(triplets.begin(), triplets.end());
```

## Access vector/matrix members

```c++
VectorXd first_n_elements = vector.head(n);
VectorXd last_n_elements = vector.tail(n);
VectorXd segment_of_n_elements = vector.segment(3, n);

MatrixXd top_left_block = matrix.topLeftCorner(m, n);
MatrixXd top_right_block = matrix.topRightCorner(m, n);
MatrixXd bottom_left_block = matrix.bottomLeftCorner(m, n);
MatrixXd bottom_right_block = matrix.bottomRightCorner(m, n);

VectorXd ith_row = matrix.row(i);
VectorXd ith_col = matrix.col(i);
```

## Basic operations

Apart from addition, subtraction et similia, various operations are supported by Eigen.

Matrices and vectors are 0-indexed. Entries can be accessed uting the `operator()`.

```c++
double vector_norm = vector.norm();
double matrix_norm = matrix.norm();

// Check if matrix is symmetric
bool symmetric = (A - A.transpose()).norm() < tolerance;
bool symmetric = A.isApprox(A.transpose());

size_t number_of_rows = matrix.rows();
size_t number_of_columns = matrix.columns();

size_t number_of_nonzeros = sparse_matrix.nonZeros();  // Breaks with non sparse.
```

## Matrix Market

```c++
SparseMatrix<double> matrix;
VectorXd vector;

loadMarket(matrix, "filename.mtx");
saveMarket(matrix, "filename.mtx");

loadMarketVector(vector, "filename.mtx");
saveMarketVector(vector, "filename.mtx");

// Used to export and load mtx vectors in a format compatible with LIS.
// If you have any questions, first read xkcd.com/927
void saveLISVector(const VectorXd& v, const std::string& filename) {
    std::ofstream os(filename);

    os << "%%MatrixMarket vector array real general\n";
    os << v.size() << "\n";

    for (int i = 0; i < v.size(); i++) {
        os << std::format("{} {}\n", i + 1, v(i));
    }

    os.close();
}

VectorXd loadLISVector(const std::string& filename) {
    std::ifstream is(filename);

    char c = '\0';

    while(c != '\n') {
        is.get(c);
    }

    int size, temp;
    double val;
    is >> size;

    VectorXd data = VectorXd::Zero(size);
    
    for(int i = 0; i < size; i++) {
        is >> temp >> val;
        data(i) = val;
    }

    return data;
}
```

## Linear systems

```c++
MatrixXd A = MatrixXd::Random(n, n);
VectorXd b = VectorXd::Random(n);

// Fully preconditioned LU
VectorXd x = A.fullPivLu().solve(b);

// Preconditioned complex conjugate with diagonal preconditioner
Eigen::ConjugateGradient<SpMat, Eigen::Lower|Eigen::Upper, DiagonalPreconditioner<double>> cg;
cg.setMaxIterations(max_iterations);
cg.setTolerance(tolerance);
cg.compute(A);
x = cg.solve(b);

//BiConjugate Gradient Stabilized




```

## Eigenproblems

_Work in progress_
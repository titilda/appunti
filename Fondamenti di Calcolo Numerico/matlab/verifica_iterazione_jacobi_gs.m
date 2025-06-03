N = 10;

A = generate_converging_matrix(N);
b = rand(N, 1);

x0 = zeros(N, 1);

[x_j, n_j] = solve_jacobi(A, b, x0, 1e-12);
[x_gs, n_gs] = solve_gauss_seidel(A, b, x0, 1e-12);

fprintf('Iterazioni Jacobi: %d\n', n_j);
fprintf('Iterazioni Gauss-Seidel: %d\n', n_gs);

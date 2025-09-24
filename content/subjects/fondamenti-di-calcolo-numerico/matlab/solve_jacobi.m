function [x, n_iters] = solve_jacobi(A, b, x0, tolerance)
    N = length(b);

    D = diag(diag(A));

    B = eye(N) - D\A;

    f = (eye(N) - B) / A * b;

    n_iters = 0;
    normalized_residue = tolerance + 1;
    x = x0;

    while normalized_residue > tolerance
        n_iters = n_iters + 1;
        x = B * x + f;
        residue = b - A*x;
        normalized_residue = norm(residue)/norm(b);
    end
end

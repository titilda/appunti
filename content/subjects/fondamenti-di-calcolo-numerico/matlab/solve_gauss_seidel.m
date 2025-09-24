function [x, n_iters] = solve_gauss_seidel(A, b, x0, tolerance)
    N = length(b);

    D = diag(diag(A));
    E = -tril(A, -1);
    F = -triu(A, +1);

    B = (D - E) \ F;
    f = (eye(N) - B) / A * b;

    increment = tolerance + 1;
    n_iters = 0;

    x = x0;

    while increment > tolerance
        n_iters = n_iters + 1;
        x_new = B * x + f;
        increment = norm(x_new - x);
        x = x_new;
    end
end


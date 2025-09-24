function [x, n_iters] = solve_richardson(A, b, alpha, x0, tolerance)
    N = length(b);
    
    B = eye(N) - alpha * A;

    f = (eye(N) - B) / A * b;

    n_iters = 0;
    normalized_residue = tolerance + 1;
    x = x0;

    while normalized_residue > tolerance
        n_iters = n_iters + 1;
        x = B*x + f;
        residue = b - A*x;
        normalized_residue = norm(residue)/norm(b);
    end
end

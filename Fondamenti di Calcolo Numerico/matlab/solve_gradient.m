function [x, n_iters] = solve_gradient(A, b, x0, tolerance)
    x = x0;
    n_iters = 0;
    residue = b - A*x;
    normalized_residue = tolerance + 1;

    while normalized_residue > tolerance
        n_iters = n_iters + 1;
        alpha = (residue' * residue) / (residue' * A * residue);
        x = x - alpha * (A * x - b);
        residue = b - A * x;
        normalized_residue = norm(residue) / norm(b);
    end
end

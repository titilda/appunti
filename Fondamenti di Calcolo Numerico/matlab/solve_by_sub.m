function [x] = solve_by_sub(A, b)
    [L, U, P] = lu(A);
    y = forward_sub(L, P*b);
    x = backward_sub(U, y);
end


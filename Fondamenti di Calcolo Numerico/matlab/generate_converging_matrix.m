function [A] = generate_converging_matrix(N)
    T = rand(N);
    v = sum(T, 2);
    A = T + diag(v);
end

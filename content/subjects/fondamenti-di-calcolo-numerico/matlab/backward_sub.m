function [x] = backward_sub(U, b)
    N = length(b);
    x = zeros(N, 1);

    x(N) = b(N) / U(N, N);

    for i = N-1:-1:1
        x(i) = (b(i) - U(i, i+1:N) * x(i+1:N)) / U(i, i);
    end
end
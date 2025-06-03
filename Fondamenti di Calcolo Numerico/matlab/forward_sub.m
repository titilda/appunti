function [x] = forward_sub(L, b)
    N = length(b);
    x = zeros(N, 1);

    for i = 1:N
        x(i) = (b(i) - L(i, 1:i-1) * x(1:i-1)) / L(i, i);
    end
end


function [ts, us] = ode_solve_cd(a, b, h, f, u_0)
    ts = (a:h:b)';
    N = length(ts);
    us = zeros(N, 1);

    us(1) = u_0;

    if N < 2
        return
    end

    us(2) = us(1) + h * f(ts(1), us(1));

    for i = 2:N-1
        us(i+1) = us(i-1) + 2 * h * f(ts(i), us(i));
    end
end


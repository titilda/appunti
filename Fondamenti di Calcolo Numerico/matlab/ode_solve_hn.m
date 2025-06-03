function [ts, us] = ode_solve_hn(a, b, h, f, u_0)
    ts = (a:h:b)';
    N = length(ts);
    us = zeros(N, 1);

    us(1) = u_0;

    for i = 1:N-1
        us(i+1) = us(i) + h / 2 * (f(ts(i), us(i)) + f(ts(i+1), us(i) + h * f(ts(i), us(i))));
    end
end


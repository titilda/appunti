function [ts, us] = ode_solve_ei(a, b, h, f, dfdu, toll, u_0)
    ts = (a:h:b)';
    N = length(ts);
    us = zeros(N, 1);

    us(1) = u_0;

    for i = 1:N-1
        [us(i+1), ~] = find_root_newton( ...
            @(u_next) us(i) + h * f(ts(i+1), u_next) - u_next, ...
            @(u_next) h * dfdu(ts(i+1), u_next) - 1, ...
            us(i), toll);
    end
end


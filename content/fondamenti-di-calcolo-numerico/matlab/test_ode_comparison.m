f = @(t, y) y / (t + 1) + 12 * t^2;
dfdy = @(t, y) 1 / (t + 1);

a = 0;
b = 2;
h = 0.01;

u_0 = 7;

[ts_ea, us_ea] = ode_solve_ea(a, b, h, f, u_0);
[ts_ei, us_ei] = ode_solve_ei(a, b, h, f, dfdy, 1e-12, u_0);
[ts_cd, us_cd] = ode_solve_cd(a, b, h, f, u_0);
[ts_cn, us_cn] = ode_solve_cn(a, b, h, f, dfdy, 1e-12, u_0);
[ts_hn, us_hn] = ode_solve_hn(a, b, h, f, u_0);

figure(1);
title('Confronto tra metodi di risoluzione ODE');
xlabel('t');
ylabel('u');

plot(ts_ea, us_ea, '-r', ...
    ts_ei, us_ei, '-b', ...
    ts_cd, us_cd, '-g', ...
    ts_cn, us_cn, '--k', ...
    ts_hn, us_hn, 'm.');

legend('Eulero in avanti', ...
    'Eulero all''indietro', ...
    'Differenze centrate', ...
    'Crank-Nicolson', ...
    'Heun');

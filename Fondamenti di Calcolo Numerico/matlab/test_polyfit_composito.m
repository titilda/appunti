xs = (0:1e-5:10)';

f = @(x) exp(3 * x) ./ (log(x) + x);
g = @(x) composite_polyfit(f, xs, 0.1, 3);

subplot(3, 1, 1);
plot(xs, f(xs));

subplot(3, 1, 2);
plot(xs, g(xs));

subplot(3, 1, 3);
plot(xs, abs(g(xs) - f(xs)));

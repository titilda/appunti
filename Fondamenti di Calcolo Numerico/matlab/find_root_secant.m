function [x, iters] = find_root_secant(f, x0, x_prec, toll)
    xs = [x_prec; x0];

    iters = 0;

    while abs(f(xs(2))) >= toll
        iters = iters + 1;

        disp(f(xs(2)));

        xs = [
            xs(2);
            xs(2) - (xs(2) - xs(1)) * f(xs(2)) / (f(xs(2)) - f(xs(1)));
        ];
    end

    x = xs(2);
end

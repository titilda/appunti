function [x, iters] = find_root_newton(f, df, x0, toll)
    x = x0;
    iters = 0;

    increment =  toll;

    phi = @(x_) x_ - f(x_)/df(x_);

    while increment >= toll
        iters = iters + 1;
        x_new = phi(x);
        increment = norm(x_new - x);
        x = x_new;
    end
end
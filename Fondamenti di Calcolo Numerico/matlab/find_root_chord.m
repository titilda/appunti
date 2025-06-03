function [x, iters] = find_root_chord(f, a, b, x0, toll)
    x = x0;
    iters = 0;

    q = (f(b) - f(a)) / (b - a);

    phi = @(x_) x_ - f(x_)/q;

    increment = toll;

    while increment >= toll
        iters = iters + 1;
        x_new = phi(x);
        increment = norm(x_new - x);
        x = x_new;
    end

end
function [f_approx] = composite_polyfit(f, xs, H, k)
    nodes_x = (xs(1):H/k:xs(end))';
    nodes_y = f(nodes_x);

    f_approx = zeros(length(xs), 1);

    for j = 1:k:length(nodes_x)-k
        p = polyfit(nodes_x(j:j+k), nodes_y(j:j+k), k);
        subset = (nodes_x(j) <= xs) & (xs <= nodes_x(j+k));
        xx_subset = xs(subset);
        f_approx(subset) = polyval(p, xx_subset);
    end
end

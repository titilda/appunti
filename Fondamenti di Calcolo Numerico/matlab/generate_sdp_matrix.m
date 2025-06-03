function [A] = generate_sdp_matrix(N)
    A = rand(N);
    A = A * A';
end


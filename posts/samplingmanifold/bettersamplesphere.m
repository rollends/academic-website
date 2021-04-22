%% Poorly Sampling S^3

%%
% Uniformly Sample in [0, 2\pi) \times [-\pi/2, \pi/2]
DP = makedist('Uniform', -2, 2);

% Generate the random numbers.
N = 10000;
points = random(DP, N, 3);
norms = sqrt(sum(points .^ 2, 2));
points = points((norms <= 2) & (norms >= 1), :);

while size(points, 1) < N
    morepoints = random(DP, N, 3);
    norms = sqrt(sum(morepoints .^ 2, 2));
    points = vertcat(points, morepoints((norms <= 2) & (norms >= 1), :));
end
points = points(1:N, :);

%% Normalize Points
norms = sqrt(sum(points .^ 2, 2));
normalizedpoints = points ./ repmat(norms, 1, 3);

X = normalizedpoints(:, 1);
Y = normalizedpoints(:, 2);
Z = normalizedpoints(:, 3);

%% Estimated Weight
e = 0.05;
W = WeightFunctionBuilder(X, Y, Z, e);

%% Color Points
C = arrayfun(W, X, Y, Z);

%% Scatter points.
figure(2);
colormap('turbo');
h = scatter3(X, Y, Z, '.');
h.CData = C';
hold on;
[Xs,Ys,Zs] = sphere(100);
surf(Xs, Ys, Zs, 'EdgeColor', 'none', 'FaceAlpha', 0.3);
colorbar();
hold off;

%% HELPERS
function W = WeightFunctionBuilder(X, Y, Z, e)
    W = @op;
    function c = op(x,y,z)
        dist = (X - x).^2 + (Y - y).^2 + (Z - z).^2 - e^2;
        c = numel(dist(dist <= 0));
    end
end
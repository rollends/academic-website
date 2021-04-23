%% Non-trivial Distribution on S^3

%%
% Uniformly Sample in [-2, 2]^2
DP = makedist('Uniform', -3, 3);

% Generate the random numbers.
N = 100000;
points = zeros(N, 3);
i = 1;
while i <= N
    M = N / 100;
    point = random(DP, M, 3);
    norms = sqrt(sum(point .^ 2, 2));
    filtered = point((norms > 0) & (norms < 3*(RV(point)).^(1/3)), :);
    for j = 1:size(filtered, 1)
        points(i, :) = filtered(j,:);
        i = i + 1;
        if i > N
            break
        end
    end
end

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
figure(3);
colormap('turbo');
[Xs,Ys,Zs] = sphere(100);
surf(Xs, Ys, Zs, 'EdgeColor', 'none', 'FaceAlpha', 0.3);
hold on;
h = scatter3(X, Y, Z, '.');
h.CData = C';
colorbar();
hold off;

%% Verify Distribution is Gaussian in Stereographic Coordinates.
figure(4);
P = horzcat(X, Z) ./ (1 - Y);
Xd = tall(P(:, 1));
Yd = tall(P(:, 2));
Xedges = [-Inf linspace(-2,2) Inf];
Yedges = [-Inf linspace(-2,2) Inf];
binScatterPlot(Xd, Yd, Xedges, Yedges)

%% HELPERS
function W = WeightFunctionBuilder(X, Y, Z, e)
    W = @op;
    function c = op(x,y,z)
        dist = (X - x).^2 + (Y - y).^2 + (Z - z).^2 - e^2;
        c = numel(dist(dist <= 0));
    end
end

%% Random Variable on S^2
% Gaussian over the stereographic projection.
function result = RV(x)
    v = zeros(size(x, 1), 1);
    u = zeros(size(x, 1), 1);
    
    I = (x(:, 2) < 1);
    J = (x(:, 2) > -1);
    
    % Point with no stereographic projection gets 0 weight.
    v(~I) = 0;
    u(~J) = 0;
    
    % Stereographic Projection
    p = x(:, 1:2);
    q = x(:, 1:2);
    p(I, :) = x(I, [1 3])./(1 - x(I, 2));
    q(J, :) = x(J, [1 3])./(1 + x(J, 2));
    
    sigma = 0.1;
    v(I) = exp( -1/2 * ((p(I, 1)/sigma) .^ 2 + (p(I, 2)/sigma) .^ 2) ) / (sigma^2*sqrt(2*pi));
    u(J) = exp( -1/2 * ((q(J, 1)/sigma) .^ 2 + (q(J, 2)/sigma) .^ 2) ) / (sigma^2*sqrt(2*pi));
    
    result = u + v;
end
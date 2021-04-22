%% Poorly Sampling S^3

%%
% Uniformly Sample in [0, 2\pi) \times [-\pi/2, \pi/2]
DTheta = makedist('Uniform', 0, 2*pi);
DPhi = makedist('Uniform', -pi/2, pi/2);

% Generate the random numbers.
N = 10000;
theta = random(DTheta, N, 1);
phi = random(DPhi, N, 1);

%% Transform (Theta, Phi) into points on the sphere.
X = cos(theta) .* cos(phi);
Y = sin(theta) .* cos(phi);
Z = sin(phi);

%% Estimated Weight
e = 0.05;
W = WeightFunctionBuilder(X, Y, Z, e);

%% Color Points
C = arrayfun(W, X, Y, Z);

%% Scatter points.
figure(1);
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
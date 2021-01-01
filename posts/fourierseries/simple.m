%% Symbolic Variable
t = sym('t', 'real');

%% Function In Question
w = 1/3;
f = heaviside(w - t) * heaviside(t + w) * exp(-1/(1 - (t/w)^2));

%% Up to order N
N = 5;

%% First Series: Standard Fourier Series up to order N.
F1_Coefficients = zeros(2*N+1, 1);
F1_Frequencies = pi * [0, repelem(1:N, 2)];

G = matlabFunction(f);
F1_Coefficients(1) = integral(G, -1, 1) / 2;

for i = 1:N
    Gcos = matlabFunction(f * cos(F1_Frequencies(2*i)*t));
    Gsin = matlabFunction(f * sin(F1_Frequencies(2*i+1)*t));
    
    F1_Coefficients(2*i) = integral(Gcos, -1, 1);
    F1_Coefficients(2*i+1) = integral(Gsin, -1, 1);
end

%% Second Series: Non Standard Series up to order N.
F2_Coefficients = zeros(2*N+1, 1);
F2_Frequencies = 0:(2*N);

for i = 1:numel(F2_Coefficients)
    G = matlabFunction(f * t^(F2_Frequencies(i)));
    
    F2_Coefficients(i) = integral(G, -1, 1);
end

% L^* L map is symmetric
LstarL = zeros(numel(F2_Coefficients));
for i = 1:size(LstarL, 1)
    for j = 1:size(LstarL, 2)
        if mod(i + j, 2) ~= 0
            continue
        end
        LstarL(i, j) = 2/(i + j - 1);
    end
end

F2_Coefficients = LstarL \ F2_Coefficients;
    

%% Plot
F = matlabFunction(f);
G1 = F1_Coefficients(1);
for i = 1:N
    G1 = G1 + F1_Coefficients(2*i) * cos(F1_Frequencies(2*i)*t) + F1_Coefficients(2*i+1) * sin(F1_Frequencies(2*i+1)*t);
end
G1 = matlabFunction(G1);

G2 = t.^(F2_Frequencies) * F2_Coefficients;
G2 = matlabFunction(G2);

figure(1);
T = linspace(-1, 1, 100);
L1 = plot(T, F(T), '.', 'MarkerSize', 7); hold on;
L2 = plot(T, G1(T), '-');
L3 = plot(T, G2(T), '-'); hold off;

L1.MarkerFaceColor = '#2e86de';
L2.Color = '#8395a7';
L3.Color = '#10ac84';

legend('Actual', 'Fourier', 'Poly');


%% Symbolic Variable
t = sym('t', 'real');

%% Function In Question
w = 1/3;
f = heaviside(w - t) * heaviside(t + w) * exp(-1/(1 - (t/w)^2));

%% Up to order N
N = 10;

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

%% Second Series: "Fourier" Series up to Order N with Weighted Inner Product
F2_Coefficients = zeros(2*N+1, 1);
F2_Frequencies = pi * [0, repelem(1:N, 2)];

weight = 1 + 10*exp(-(t/w)^2);
G = matlabFunction(f * weight);
F2_Coefficients(1) = integral(G, -1, 1);

for i = 1:N
    Gcos = matlabFunction(f * cos(F2_Frequencies(2*i)*t) * weight);
    Gsin = matlabFunction(f * sin(F2_Frequencies(2*i+1)*t) * weight);
    
    F2_Coefficients(2*i) = integral(Gcos, -1, 1);
    F2_Coefficients(2*i+1) = integral(Gsin, -1, 1);
end

% L^* L map is symmetric
LstarL = zeros(numel(F2_Coefficients));
for i = 1:size(LstarL, 1)
    for j = 1:size(LstarL, 2)
        if i == 1
            GA = sym(1);
        elseif mod(i, 2) == 0
            GA = cos(F2_Frequencies(i)*t);
        else
            GA = sin(F2_Frequencies(i)*t);
        end
        if j == 1
            GB = sym(1);
        elseif mod(j, 2) == 0
            GB = cos(F2_Frequencies(j)*t);
        else
            GB = sin(F2_Frequencies(j)*t);
        end
        LstarL(i, j) = integral(matlabFunction(GA*GB*weight),-1, 1);
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

G2 = F2_Coefficients(1);
for i = 1:N
    G2 = G2 + F2_Coefficients(2*i) * cos(F2_Frequencies(2*i)*t) + F2_Coefficients(2*i+1) * sin(F2_Frequencies(2*i+1)*t);
end
G2 = matlabFunction(G2);

figure(1);
T = linspace(-1, 1, 300);
L1 = plot(T, F(T), '.', 'MarkerSize', 7); hold on;
L2 = plot(T, G1(T), '-');
L3 = plot(T, G2(T), '-'); hold off;

L1.MarkerFaceColor = '#2e86de';
L2.Color = '#8395a7';
L3.Color = '#10ac84';

legend('Actual', 'Fourier', 'Poly');


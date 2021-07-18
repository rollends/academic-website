%% Simulating Bump System.

%% Numerical Approximation of h.
sX = -3:0.05:3;
sY = -3:0.05:3;
[sGridX, sGridY] = meshgrid(sX, sY);
Hv = zeros(size(sGridX));

% dH in components
dHx = @(x,y) Bump(1/2*(x - 2)) + 2.*(x + 1).*Bump(1/8*((x + 1).^2 + y.^2 - 1));
dHy = @(x,y) 2.*y.*Bump(1/8*((x + 1).^2 + y.^2 - 1));

% Integrate from (1, 0) to the point in question.
for i = 1:numel(sX)
    for j = 1:numel(sY)
        % Integrate dHx from 0 to sX(i, j)
        val1 = integral(@(x) dHx(x, 0), 0, sGridX(i, j));
        
        % Integrate dHy from 0 to sY(i, j)
        val2 = integral(@(y) dHy(sGridX(i, j), y), 0, sGridY(i, j));
        
        Hv(i, j) = val1 + val2;
    end
end

H = @(x, y) interp2(sGridX, sGridY, Hv, x, y);

%% Plot It
figure(1);
surf(sGridX, sGridY, Hv);

figure(2);
contourf(sGridX, sGridY, Hv);


%% Wish System Design.

%% Variables
global au
global ar

au = sym('au');
ar = sym('ar');


% Real Variables
assume(au, 'real');
assume(ar, 'real');

%% Now Solve the Optimization Problem
% We must find au and ar so that we achieve the desired probabilities in
% the limit.

% The matrix is too large to compute its eigenvector and get reasonable
% results. So let us set up a bounded optimization problem (we know 0 < au,
% au < 1) that uses known values of au, ar to create a numeric matrix which
% we can then compute the eigenvector in some stable manner.

% A good choice is particle swarm optimization here (we do not have a
% gradient in this problem).

options = optimoptions('simulannealbnd','PlotFcns', {@saplotbestx,@saplotbestf,@saplotx,@saplotf});%;, 'FunctionTolerance', 1e-12, 'MaxStallIterations', 3000);
sol = simulannealbnd(@(a) CostFunction(a(1), a(2)), [0.0763, 0.0052], [1e-4 1e-4], [1 - 1e-4, 1 - 1e-4], options)

%% Final Distribution
dist = FinalDistribution(sol(1), sol(2));

% New Array that Displays Probability at a given State.
distX = zeros(10, 90);
for xr = 0:9
    for xu = 0:89
        distX(xr + 1, xu + 1) = dist(StateToIndex(xr, xu));
    end
end
figure(2);
subplot(2, 1, 1);
bar(sum(distX, 1));
ylabel('Ratio of Time Spend in State');
subplot(2, 1, 2);
bar(cumsum(sum(distX, 1)));
ylabel('CDF');
xlabel('Rolls Since Ultra-Rare Win');

figure(3);
subplot(2, 1, 1);
bar(sum(distX, 2));
subplot(2, 1, 2);
bar(cumsum(sum(distX, 2)));

j = StateToIndex(0, 0); 
dist(j)

p = 0;
for xu = 1:89
    j = StateToIndex(0, xu);
    p = p + dist(j);
end
p

%% Useful Functions.
function cost = CostFunction(arvalue, auvalue)
    dist = FinalDistribution(arvalue, auvalue);
    cost = 0;
    
    % Winning an Ultra-Rare item should be 1.6%
    p = 0;
    j = StateToIndex(0, 0); 
    p = p + dist(j);
    cost = cost + abs((p - 1.6/100)/(1.6/100));      
    
    % Winning a Rare item should be 13%
    p = 0;
    for xu = 1:89
        j = StateToIndex(0, xu);
        p = p + dist(j);
    end
    cost = cost + abs((p - 13/100)/(13/100));   
end
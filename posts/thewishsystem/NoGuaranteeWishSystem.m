%% Wish System Design.


%% Now Solve the Optimization Problem
% We must find au and ar so that we achieve the desired probabilities in
% the limit.

% The matrix is too large to compute its eigenvector and get reasonable
% results. So let us set up a bounded optimization problem (we know 0 < au,
% au < 1) that uses known values of au, ar to create a numeric matrix which
% we can then compute the eigenvector in some stable manner.

% A good choice is particle swarm optimization here (we do not have a
% gradient in this problem).

options = optimoptions('simulannealbnd','PlotFcns', {@saplotbestx,@saplotbestf,@saplotx,@saplotf}, 'FunctionTolerance', 1e-11, 'MaxStallIterations', 1000);
sol = simulannealbnd(@(a) Cost2Function(a(1), a(2), a(3)), [0.0557    0.0203    0.0007], [1e-4 1e-4 1e-4], [1 - 1e-4, 1 - 1e-4, 1 - 1e-4], options)

% options = optimoptions('particleswarm', 'SwarmSize', 500, 'HybridFcn', @fmincon, 'FunctionTolerance', 1e-12, 'MaxStallIterations', 2000);
% sol = particleswarm(@(a) Cost2Function(a(1), a(2), a(3)), 3, [1e-4 1e-4 1e-4], [1 - 1e-4, 1 - 1e-4, 1 - 1e-4], options)

%% Final Distribution
dist = FinalDistributionNoGuarantee(sol(1), sol(2), sol(3));

% New Array that Displays Probability at a given State.
distX = zeros(10, 5);
for xr = 0:9
    for xu = 0:4
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
bar3(distX);

j = StateToIndex(0, 0); 
dist(j)

p = 0;
for xu = 1:4
    j = StateToIndex(0, xu);
    p = p + dist(j);
end
p

%% Useful Functions.

function cost = Cost2Function(arvalue, austart, auend)
    dist = FinalDistributionNoGuarantee(arvalue, austart, auend);
    cost = 0;
    
    % Winning an Ultra-Rare item should be 1.6%
    p = 0;
    j = StateToIndex(0, 0); 
    p = p + dist(j);
    cost = cost + abs((p - 1.6/100)/(1.6/100));      
    
    % Winning a Rare item should be 13%
    p = 0;
    for xu = 1:4
        j = StateToIndex(0, xu);
        p = p + dist(j);
    end
    cost = cost + abs((p - 13/100)/(13/100));    
end
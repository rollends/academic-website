function dist = FinalDistributionJump(ar, au)
    P = spalloc(10*90, 10*90, 10*90*4);
    
    % Basic Cases
    for xr = 0:8
        for xu = 0:88
            i = StateToIndex(xr, xu);

            v = 0;
            if mod(xu + 1, 10) == 0
            	% Transition to Winning Ultra-Rare Item
                j = StateToIndex(0, 0);
                v = au;
                P(i, j) = v;
            end

            % Transition to Winning Rare Item
            j = StateToIndex(0, xu + 1);
            P(i, j) = ar;

            % Transition to Default Win
            j = StateToIndex(xr + 1, xu + 1);
            P(i, j) = 1 - v - ar;
        end
    end

    % Edge Case 1: Win Ultra Rare.
    for xr = 0:9
        i = StateToIndex(xr, 89);

        % Transition to Winning Ultra-Rare Item
        j = StateToIndex(0, 0);
        P(i, j) = 1;
    end

    % Edge Case 2: Win Rare or Ultra Rare Item
    for xu = 0:88
        i = StateToIndex(9, xu);
        
        if mod(xu + 1, 10) == 0
            % Transition to Winning Ultra-Rare Item
            j = StateToIndex(0, 0);
            P(i, j) = au / (ar + au);
            
            % Transition to Winning Rare Item
            j = StateToIndex(0, xu + 1);
            P(i, j) = ar/(ar + au);
        else
            % Transition to Winning Rare Item
            j = StateToIndex(0, xu + 1);
            P(i, j) = 1;
        end
    end
    
    
    % Compute Left Eigenvector associated to eigenvalue 1.
    % We can prime it since we know that the left eigenvector derives from
    % the limit of any initial probability density.
    v0 = ones(1, size(P, 1)) / size(P, 1);
    for i = 1:100
        v0 = v0 * P;
    end
    [V, D] = eigs(P', 2, 1 + 1e-2, 'StartVector', v0(:));
    e = diag(D);
    
    % Pick one closest to eigenvalue 1.
    [~, I] = sort(abs(e(:) - 1));
    V = V(:, I(1));
    
    if numel(V) == 0
        dist = zeros(size(P, 1), 1);
        return
    end
    
    % Normalize V.
    V = V ./ sum(V);
    
    % This is the stable probability distribution.
    dist = V;
end
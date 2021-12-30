function dist = FinalDistributionNoGuarantee(arvalue, austart, auend)
    P = spalloc(5*10, 5*10, 5*10*4);
    ar = arvalue;
    
    % Initial State xu = 0 Uniformly Transitions to all other states (no wins)
    for xr = 0:9
        i = StateToIndex(xr, 0);
        for xu = 1:4
            j = StateToIndex(xr, xu);
            P(i, j) = 1/4;
        end
    end

    % Basic Cases
    for xr = 0:8
        for xu = 1:4
            i = StateToIndex(xr, xu);

            % Transition to Winning Ultra-Rare Item
            j = StateToIndex(0, 0);
            pr = (4 - xu)/4 * austart + (xu)/4 * auend;
            P(i, j) = pr;

            % Transition to Winning Rare Item
            j = StateToIndex(0, xu + 1);
            if xu == 4
                j = StateToIndex(0, 1);
            end
            P(i, j) = ar;

            % Transition to Default Win
            j = StateToIndex(xr + 1, xu + 1);
            if xu == 4
                j = StateToIndex(xr + 1, 1);
            end
            P(i, j) = 1 - pr - ar;
        end
    end

    % Edge Case 2: Win Rare or Ultra Rare Item
    for xu = 1:4
        i = StateToIndex(9, xu);

        % Transition to Winning Ultra-Rare Item
        j = StateToIndex(0, 0);
        pr = (4 - xu)/4 * austart + (xu)/4 * auend;
        P(i, j) = pr / (ar + pr);

        % Transition to Winning Rare Item
        j = StateToIndex(0, xu + 1);
        if xu == 4
            j = StateToIndex(0, 1);
        end
        P(i, j) = ar / (ar + pr);
    end

    assert(sum(abs(1 - (P * ones(size(P, 2), 1)))) < 1e-3);

    % Compute Left Eigenvector associated to eigenvalue 1.
%     V = null(P' - eye(size(P')));
    [V, D] = eigs(P', 3, 1 + 1e-2);
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
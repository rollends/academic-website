function dist = FinalDistribution(arvalue, auvalue)
    P = spalloc(900, 900, 900*4);
    au = auvalue;
    ar = arvalue;
    
    % Basic Cases
    for xr = 0:8
        for xu = 0:88
            i = StateToIndex(xr, xu);

            % Transition to Winning Ultra-Rare Item
            j = StateToIndex(0, 0);
            P(i, j) = au;

            % Transition to Winning Rare Item
            j = StateToIndex(0, xu + 1);
            P(i, j) = ar;

            % Transition to Default Win
            j = StateToIndex(xr + 1, xu + 1);
            P(i, j) = 1 - au - ar;
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

        % Transition to Winning Ultra-Rare Item
        j = StateToIndex(0, 0);
        P(i, j) = au / (ar + au);

        % Transition to Winning Rare Item
        j = StateToIndex(0, xu + 1);
        P(i, j) = ar / (ar + au);
    end

    assert(sum(abs(1 - (P * ones(size(P, 2), 1)))) < 1e-3);

    % Compute Left Eigenvector associated to eigenvalue 1.
%     V = null(P' - eye(size(P')));
    [V, D] = eigs(P', 20, 1 + 1e-2);
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
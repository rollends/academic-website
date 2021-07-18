function y = dBump(x)
    y = x;
    
    flag = (x < -1) | (x > 1);
    
    y(flag) = 0;
    y(~flag) = exp((1 - 1./(1 - x(~flag).^2)));
    y(~flag) = 2 .* x(~flag) .* y(~flag) ./ (1 - x(~flag).^2).^2;
end
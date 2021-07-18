function y = Bump(x)
    y = x;
    
    flag = (x < -1) | (x > 1);
    
    y(flag) = 0;
    y(~flag) = exp((1 - 1./(1 - x(~flag).^2)));
end
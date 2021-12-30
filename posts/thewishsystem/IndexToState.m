
function [xr, xu] = IndexToState(i)
    xr = rem(i - 1, 10);
    xu = ((i - 1) - xr) / 10;
end

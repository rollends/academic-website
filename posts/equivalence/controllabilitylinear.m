%% Controllable Canonical Form --- Frequency Domain
A = [ 0     3     1;
     2     1     1;
     0     1     0;];
B = [0; 1; 1];

%%
assert(rank([B, A*B, A*A*B]) == 3, 'Controllability Criterion must Hold.');

%%
s = sym('s');

Q1 = [ s*eye(3) - A'; B' ]
Q2 = sym([zeros(3, 1), eye(3)])'

%%
R1 = rref([Q1, eye(size(Q1, 1))]);
R1 = simplify(R1)

R2 = rref([Q2, eye(size(Q2, 1))]);
R2 = simplify(R2)
%% 
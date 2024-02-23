clear all

N = 1e6; % 1 * 10^6, numero totale di prove

v = randi(3, N, 1); % genero un vettore colonna di N interi casuali da 1 a 3

n1 = sum(v==1); % questo ritorna booleani

S = ones(N,1); %S = vettore scelta iniziale del concorrente, pacco 1

%SIMULO LA STRATEGIA

S(v==1) = 1+randi(2, n1, 1);%Se v è 1 allora viene scoperto il pacco 2 o il pacco 3
%con la stessa probabilità

S(v==2) = 2; %se v è 2 , allora viene aoerto il pacco 3, e il concorrente sceglie il pacco 2

S(v==3) = 3; %se v è 3 , allora viene aoerto il pacco 3, e il concorrente sceglie il pacco 3

%calcolo la stima della probabilità

P = sum(S==v)/N
clear all

N = 1e6; %numero di prove che voglio fare

c1 = [1, 1]; %carta con entrambi il lati blu
c2 = [1, 0]; %carta con un lato blu e uno rosso
c3 = [0, 0]; %carta con un due lati rossi

wins = 0; %punteggio del gioco


%SIMULO IL GIOCO
for i = 1:N

v = randi(3, 1, 1); %pesco un numero casuale tra 1 e 3 (carta pescata)

%la prima faccia che vedo è una a caso (S1 e S2 sono side 1 e side 2 della carta)
face = randi(2, 1, 1); %prendo un numero casuale tra 1 e 2 (prima o seconda faccia)
%ho pescato la prima carta
S1(v==1) = c1(face); %questa è la prima faccia che vedo
S2(v==1) = c1(3 - face); %prendo l'altra faccia della carta
%ho pescato la seconda carta
S1(v==2) = c2(face);
S2(v==2) = c2(3 - face); %se face = 1 allora la seconda sarà (3-1=2) quindi la seconda faccia
%ho pescato la terza carta
S1(v==3) = c3(face);
S2(v==3) = c3(3 - face); %se face = 2 allora la seconda sarà (3-2=1) quindi la prima faccia

if S1==1
    wins(S2==1) = wins + 1; 
end 

end

%RISULTATI

P = 1 - (wins/N)
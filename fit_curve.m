IP = depositioneffiiciency.IP;
depE = depositioneffiiciency.depE;

gaussEqn = '1 - (1/(a*x^b+1))';
startPoints = [3.5E-8 1.7];

f = fit(IP, depE, gaussEqn, 'Start', startPoints);


%1-(1/(3.988e-08*x^1.746+1))
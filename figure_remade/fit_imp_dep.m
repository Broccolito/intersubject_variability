imp = impdep.imp_cm3;
dep = impdep.dep;

gaussEqn = '1 - (1/(a*x^b+1))';
startPoints = [0.000000035 1.7];

f = fit(imp, dep, gaussEqn, 'Start', startPoints);
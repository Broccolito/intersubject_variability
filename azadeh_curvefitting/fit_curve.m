clear;
clc;
clf;

deposition = readtable('deposition.csv');

IP = deposition.parameter/0.06;
depE = deposition.deposition;

gaussEqn = '1 - (1/(a*x^b+1))';
startPoints = [3.5E-8 1.7];

f = fit(IP, depE, gaussEqn, 'Start', startPoints);

%      General model:
%      f(x) = 1 - (1/(a*x^b+1))
%      Coefficients (with 95% confidence bounds):
%        a =   6.733e-08  (-7.332e-09, 1.42e-07)
%        b =       1.645  (1.549, 1.74)

hold on
plot(IP, depE, '.', 'MarkerSize',15)
plot(f)
set(gca, 'XScale', 'log')
hold off
%1-(1/(3.988e-08*x^1.746+1))
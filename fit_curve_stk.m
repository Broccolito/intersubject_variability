stk1 = stkdep.stk1;
stk2 = stkdep.stk2;
stk3 = stkdep.stk3;
dep = stkdep.dep;

gaussEqn = '1 - exp(a*x)';
startPoints = [6.66];

f1 = fit(stk1, dep, gaussEqn, 'Start', startPoints);
f2 = fit(stk2, dep, gaussEqn, 'Start', startPoints);
f3 = fit(stk3, dep, gaussEqn, 'Start', startPoints);

f1
f2
f3
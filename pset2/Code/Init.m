%% Housekeeping
clc; clear all; close all; 

% Load colors
try
    global c
    myColors();
catch
    warning('Not in Jose computer. Formating figures might not work properly');
end

% Store parameters in structure
par.rho     = 0.05;
par.gamma   = 1/0.35;
par.tau     = 0.3;
par.s       = 0.05;
par.alpha   = 7.179;
par.alphaT  = 0.075;
par.lambda  = 1.044;
par.phi     = 0.0423;
par.m       = 100;

% Number of points
N = 20;

% Create vector for delta
delta   = linspace(0.015,5,N);

%% Solve the equilibrium and store the g value

% Initialize vector of growth
g = nan(N,1);

for j = 2:N
    % Update the parameter
    par.delta = delta(j);
    % Solve equilibrium
    [eq,flag] = auxFuncs.solveBGP(par);
    % Add to structure
    if(flag)
        g(j) = (1+eq.g)^50;
    end
end


%% Plot result

figure;
plot(delta,g,'Color',c.maroon)
xlabel('$\delta$')
ylabel('$g$')
export_fig('../Figures/figure8','-pdf','-transparent');

%% Export results

writematrix([delta',g],'../Other Files/results.xls')








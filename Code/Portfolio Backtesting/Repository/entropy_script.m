clc;
clear;
tic

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      Entropy Portfoilio Simulation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

opts = detectImportOptions('sample_buyback_portfolio_entropy.csv');
data = readtable('sample_buyback_portfolio_entropy.csv', opts);

probit_spec = 'buyback ~ cash_at + ex_che + age + sic3';

[entropy] = repurchase_entropy(data,probit_spec,2000,15);

%end
toc

function [quantile_rank] = yearly_quantiles(x,p,year_variable)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function computes 'p' quantile ranks for the variable 'x' within
% each year in the data
% Author: Sascha Jakob
%
% !!! Warning: The data needs to be sorted according to the year variable 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fhandle = @(x){quantileranks(x,p)};
ouput = splitapply(fhandle,x,findgroups(year_variable));
rank = ouput{1,1};

for j = 2:length(unique(year_variable))
    temp = ouput{j,1};
    rank = [rank;temp];
end
quantile_rank = rank;

end


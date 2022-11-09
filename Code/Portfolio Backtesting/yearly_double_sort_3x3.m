function [double_sorted_data] = yearly_double_sort_3x3(quantile_data,estimation_data,control_var,sort_var)

%This function constructs double-sorted 3x3 portfolios
%   

quantile_data = sortrows(quantile_data, {'year','permno','time'}); 

    quantile_data.control_quantile= yearly_quantiles(quantile_data(:,ismember(quantile_data.Properties.VariableNames,{convertStringsToChars(string(control_var))})),3,quantile_data.year);

    bottom_control_set= quantile_data.control_quantile == 1;
    bottom_control_data = quantile_data(bottom_control_set,:);
    bottom_control_data.var_quantiles= yearly_quantiles(bottom_control_data(:,ismember(bottom_control_data.Properties.VariableNames,{convertStringsToChars(string(sort_var))})),3,bottom_control_data.year);
    bottom_control_data.var_quantiles(bottom_control_data.var_quantiles==0)=NaN;
    bottom_control_data.portfolio_sort = bottom_control_data.var_quantiles;
    
    medium_control_set= quantile_data.control_quantile == 2;
    medium_control_data = quantile_data(medium_control_set,:);
    medium_control_data.var_quantiles= yearly_quantiles(medium_control_data(:,ismember(medium_control_data.Properties.VariableNames,{convertStringsToChars(string(sort_var))})),3,medium_control_data.year);
    medium_control_data.var_quantiles(medium_control_data.var_quantiles==0)=NaN;
    medium_control_data.var_quantiles(medium_control_data.var_quantiles==1)=4;
    medium_control_data.var_quantiles(medium_control_data.var_quantiles==2)=5;
    medium_control_data.var_quantiles(medium_control_data.var_quantiles==3)=6;
    medium_control_data.portfolio_sort = medium_control_data.var_quantiles;
    
    top_control_set= quantile_data.control_quantile == 3;
    top_control_data = quantile_data(top_control_set,:);
    top_control_data.var_quantiles= yearly_quantiles(top_control_data(:,ismember(top_control_data.Properties.VariableNames,{convertStringsToChars(string(sort_var))})),3,top_control_data.year);
    top_control_data.var_quantiles(top_control_data.var_quantiles==0)=NaN;
    top_control_data.var_quantiles(top_control_data.var_quantiles==1)=7;
    top_control_data.var_quantiles(top_control_data.var_quantiles==2)=8;
    top_control_data.var_quantiles(top_control_data.var_quantiles==3)=9;
    top_control_data.portfolio_sort = top_control_data.var_quantiles;
    
    double_sorted_data = [bottom_control_data; medium_control_data; top_control_data];
    double_sorted_data = sortrows(double_sorted_data, {'permno','time'}); % Sorting: Permno and time
    double_sorted_data = double_sorted_data(:, ismember(double_sorted_data.Properties.VariableNames, ...
        {'permno','dealnumber','portfolio_sort'}));
   
double_sorted_data = outerjoin(estimation_data,double_sorted_data,'Keys',{'permno','dealnumber'},'MergeKeys',true);
end


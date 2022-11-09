function [portfolio_returns,low_low,low_med1,low_med2,low_high,high_low,high_med1,high_med2,high_high,double_sort_table,performance_plot] = portfolio_double_sort_2x4...
    (dataset,year_start,year_end,window,control_var,sort_var,rebalancing_intervall)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function performs a 3x3 double sort and performs a backtesting on
% the 9 portfolios
%
% Inputs: Data set, beginning year, ending year, window lenght, control
% variable (first sort), variablr of interest (second sort), rebalancing
% intervall
%
% Output: Portfolio returns, 3x3 portfolio performances
%
% Author: Sascha Jakob
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Time Period and Variables
time_period = dataset.year>=(year_start-(window/12))&dataset.year<=(year_end);
dataset = dataset(time_period,:);
dataset.time = dataset.time-min(dataset.time)-(window-1);
dataset = dataset(:, ismember(dataset.Properties.VariableNames,...
    {'month','time','dealnumber','year','permno','time','entropy_H',...
    'entropy_I','cash_atl1','ivol','roal1','top10instown_percl1','payout_yield_all',...
    'ret','rf','mktrf','smb','hml','umd','rmw','cma','ps_vwf','mkvalt',...
    'vol_shares_mean', 'vol_dollar_mean','ret_sd','buyback', 'retrf','six_m_cum_ret',...
    'six_m_sum_ret', 'si_mean','si_delta','mtbl1','instown_perc','mbl1'}));

dataset.ret(isnan(dataset.ret)) = 0;
dataset.retrf(isnan(dataset.retrf))=0;

% Inital month
estimation_data = dataset.time>=(0-window+1)&dataset.time<=(0);
estimation_set = dataset(estimation_data,:);
quantile_data = estimation_set.buyback==1;
quantile_set = estimation_set(quantile_data,:);
quantile_set = quantile_set(:, ismember(quantile_set.Properties.VariableNames, {'dealnumber','year','permno','time','entropy_H',...
    'entropy_I','cash_atl1','ivol','top10instown_percl1','payout_yield_all',convertStringsToChars(string(control_var)),convertStringsToChars(string(sort_var))}));
return_sample  = dataset.time==0;
return_data = dataset(return_sample,:);

estimation_set = yearly_double_sort_2x4(quantile_set,estimation_set,control_var,sort_var);

for i = 1:8
    portolio{i} = estimation_set(estimation_set.portfolio_sort == i,:);
    portolio{i} = sortrows(portolio{i}, {'permno','time'}); 
    portfolio_stocks{i} = unique(portolio{i}.permno); 
end

portfolio_ret = [unique(return_data.month) unique(return_data.time) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0];

% Next Period weights
return_sample = dataset.time==1;
return_data = dataset(return_sample,:);

for i = 1:8
    portfolio_data{i} = return_data(ismember(return_data.permno,portfolio_stocks{i}),...
    ismember(return_data.Properties.VariableNames, {'ret' 'retrf' 'mktrf' 'rf'...
    'smb' 'hml' 'umd' 'cma' 'rmw' 'ps_vwf' 'mkvalt'})); %#ok<*AGROW>
    stocks_w{1,i} = zeros(height(portfolio_data{1,i}),1);
    stocks_w{1,i}(:) = 1/height(portfolio_data{1,i});
end

% subsequent months
for i = 1:max(dataset.time) 
    if mod(i,rebalancing_intervall)==0
         
    estimation_data = dataset.time>=(i-window+1)&dataset.time<=(i);
    estimation_set = dataset(estimation_data,:);
    quantile_data = estimation_set.buyback==1;
    quantile_set = estimation_set(quantile_data,:);
    quantile_set = quantile_set(:, ismember(quantile_set.Properties.VariableNames, {'dealnumber','year','permno','time','entropy_H',...
    'entropy_I','cash_atl1','ivol','top10instown_percl1','payout_yield_all',convertStringsToChars(string(control_var)),convertStringsToChars(string(sort_var))}));
    return_sample = dataset.time==i;
    return_data = dataset(return_sample,:);
    
    for j = 1:8      
        portfolio_data{j} = return_data(ismember(return_data.permno,portfolio_stocks{j}),...
        ismember(return_data.Properties.VariableNames, {'ret' 'retrf' 'mktrf' 'rf'...
        'smb' 'hml' 'umd' 'cma' 'rmw' 'ps_vwf' 'mkvalt'}));

        if  height(portfolio_data{j})==length(stocks_w{j})
            stocks_w{j} = stocks_w{j};
        else
            stocks_w{1,j} = zeros(height(portfolio_data{1,j}),1);
            stocks_w{1,j}(:) = 1/height(portfolio_data{1,j});
        end
    end

    temp_ret = [ unique(return_data.month) unique(return_data.time)...
        stocks_w{1}'*portfolio_data{1}.ret  stocks_w{2}'*portfolio_data{2}.ret stocks_w{3}'*portfolio_data{3}.ret...
        stocks_w{4}'*portfolio_data{4}.ret stocks_w{5}'*portfolio_data{5}.ret stocks_w{6}'*portfolio_data{6}.ret...
        stocks_w{7}'*portfolio_data{7}.ret stocks_w{8}'*portfolio_data{8}.ret...
        stocks_w{1}'*portfolio_data{1}.retrf  stocks_w{2}'*portfolio_data{2}.retrf stocks_w{3}'*portfolio_data{3}.retrf...
        stocks_w{4}'*portfolio_data{4}.retrf stocks_w{5}'*portfolio_data{5}.retrf stocks_w{6}'*portfolio_data{6}.retrf...
        stocks_w{7}'*portfolio_data{7}.retrf stocks_w{8}'*portfolio_data{8}.retrf...
        mean(portfolio_data{1}.mktrf + portfolio_data{1}.rf) mean(portfolio_data{1}.mktrf) mean(portfolio_data{1}.smb) mean(portfolio_data{1}.hml)...
        mean(portfolio_data{1}.umd) mean(portfolio_data{1}.rmw) mean(portfolio_data{1}.cma) mean(portfolio_data{1}.ps_vwf)...
        mean(portfolio_data{1}.rf) length(portfolio_data{1}.ret) length(portfolio_data{2}.ret) length(portfolio_data{3}.ret)...
        length(portfolio_data{4}.ret) length(portfolio_data{5}.ret) length(portfolio_data{6}.ret) length(portfolio_data{7}.ret)...
        length(portfolio_data{8}.ret)];
    
    portfolio_ret = [portfolio_ret; temp_ret]; 
    estimation_set = yearly_double_sort_2x4(quantile_set,estimation_set,control_var,sort_var);
    
    for j = 1:8
    portolio{j} = estimation_set(estimation_set.portfolio_sort == j,:);
    portolio{j} = sortrows(portolio{j}, {'permno','time'}); 
    portfolio_stocks{j} = unique(portolio{j}.permno); 
    end
    
    % Next Period weights
return_sample = dataset.time==i+1;
return_data = dataset(return_sample,:);

for j = 1:8
    portfolio_data{j} = return_data(ismember(return_data.permno,portfolio_stocks{j}),...
    ismember(return_data.Properties.VariableNames, {'ret' 'retrf' 'mktrf' 'rf'...
    'smb' 'hml' 'umd' 'cma' 'rmw' 'ps_vwf' 'mkvalt'}));
    stocks_w{1,j} = zeros(height(portfolio_data{1,j}),1);
    stocks_w{1,j}(:) = 1/height(portfolio_data{1,j});
end

else
    return_sample = dataset.time==i;
    return_data = dataset(return_sample,:);

      for j = 1:8      
        portfolio_data{j} = return_data(ismember(return_data.permno,portfolio_stocks{j}),...
        ismember(return_data.Properties.VariableNames, {'ret' 'retrf' 'mktrf' 'rf'...
        'smb' 'hml' 'umd' 'cma' 'rmw' 'ps_vwf' 'mkvalt'}));

        if  height(portfolio_data{j})==length(stocks_w{j})
            stocks_w{j} = stocks_w{j};
        else
            stocks_w{1,j} = zeros(height(portfolio_data{1,j}),1);
            stocks_w{1,j}(:) = 1/height(portfolio_data{1,j});
        end
     end
  
        temp_ret = [ unique(return_data.month) unique(return_data.time) ...
        stocks_w{1}'*portfolio_data{1}.ret  stocks_w{2}'*portfolio_data{2}.ret stocks_w{3}'*portfolio_data{3}.ret...
        stocks_w{4}'*portfolio_data{4}.ret stocks_w{5}'*portfolio_data{5}.ret stocks_w{6}'*portfolio_data{6}.ret...
        stocks_w{7}'*portfolio_data{7}.ret stocks_w{8}'*portfolio_data{8}.ret...
        stocks_w{1}'*portfolio_data{1}.retrf  stocks_w{2}'*portfolio_data{2}.retrf stocks_w{3}'*portfolio_data{3}.retrf...
        stocks_w{4}'*portfolio_data{4}.retrf stocks_w{5}'*portfolio_data{5}.retrf stocks_w{6}'*portfolio_data{6}.retrf...
        stocks_w{7}'*portfolio_data{7}.retrf stocks_w{8}'*portfolio_data{8}.retrf...
        mean(portfolio_data{1}.mktrf + portfolio_data{1}.rf) mean(portfolio_data{1}.mktrf) mean(portfolio_data{1}.smb) mean(portfolio_data{1}.hml)...
        mean(portfolio_data{1}.umd) mean(portfolio_data{1}.rmw) mean(portfolio_data{1}.cma) mean(portfolio_data{1}.ps_vwf)...
        mean(portfolio_data{1}.rf) length(portfolio_data{1}.ret) length(portfolio_data{2}.ret) length(portfolio_data{3}.ret)...
        length(portfolio_data{4}.ret) length(portfolio_data{5}.ret) length(portfolio_data{6}.ret) length(portfolio_data{7}.ret)...
        length(portfolio_data{8}.ret)];
    
    portfolio_ret = [portfolio_ret; temp_ret]; 
   
  % Next period normalized weights due to no rebalancing
          
       for j = 1:8
             stocks_w{j} = stocks_w{j}.*(1+portfolio_data{j}.ret);
             stocks_w{j} = normalize(stocks_w{j},'norm',1);
       end
    end
end

    portfolio_returns = cell2table(portfolio_ret);
    portfolio_returns = portfolio_returns(2:end,:);
 
portfolio_returns.Properties.VariableNames = {'Month' 'Time'...
     'P1' 'P2' 'P3' 'P4' 'P5' 'P6' 'P7' 'P8'...
     'Excess_P1' 'Excess_P2' 'Excess_P3' 'Excess_P4' 'Excess_P5' 'Excess_P6' 'Excess_P7' 'Excess_P8'...
     'MarketReturn' 'MKTRF' 'SMB' 'HML' 'UMD' 'RMW' 'CMA' 'LIQ' 'Rf' 'Obs_P1' 'Obs_P2' 'Obs_P3' 'Obs_P4' 'Obs_P5'...
     'Obs_P6' 'Obs_P7' 'Obs_P8'};
 
 Performance_P1 = fitlm(portfolio_returns,'Excess_P1 ~ MKTRF + SMB + HML + UMD + RMW + CMA');
 low_low = Performance_P1.Coefficients;
 Performance_P2 = fitlm(portfolio_returns,'Excess_P2 ~ MKTRF + SMB + HML + UMD + RMW + CMA');
 low_med1= Performance_P2.Coefficients;
 Performance_P3 = fitlm(portfolio_returns,'Excess_P3 ~ MKTRF + SMB + HML + UMD + RMW + CMA');
 low_med2= Performance_P3.Coefficients;
 Performance_P4 = fitlm(portfolio_returns,'Excess_P4 ~ MKTRF + SMB + HML + UMD + RMW + CMA');
 low_high = Performance_P4.Coefficients;
 
 Performance_P5 = fitlm(portfolio_returns,'Excess_P5 ~ MKTRF + SMB + HML + UMD + RMW + CMA');
 high_low = Performance_P5.Coefficients;
 Performance_P6 = fitlm(portfolio_returns,'Excess_P6 ~ MKTRF + SMB + HML + UMD + RMW + CMA');
 high_med1= Performance_P6.Coefficients;
 Performance_P7 = fitlm(portfolio_returns,'Excess_P7 ~ MKTRF + SMB + HML + UMD + RMW + CMA');
 high_med2 = Performance_P7.Coefficients;
 Performance_P8 = fitlm(portfolio_returns,'Excess_P8 ~ MKTRF + SMB + HML + UMD + RMW + CMA');
 high_high= Performance_P8.Coefficients;
 
 double_sort_table = {low_low low_med1 low_med2 low_high;...
                        high_low high_med1 high_med2 high_high};
                  
 plot_data = portfolio_returns(:,ismember(portfolio_returns.Properties.VariableNames,{'Month' 'Time' 'P1' 'P2' 'P3' 'P4' 'P5' 'P6' 'P7' 'P8' 'MarketReturn'}));
 plot_data = [{year_start,0, 1, 1, 1, 1, 1, 1, 1, 1, 1};plot_data];
 plot_data.P1(2:end) = cumprod(plot_data.P1(2:end)+1);
 plot_data.P2(2:end) = cumprod(plot_data.P2(2:end)+1);
 plot_data.P3(2:end) = cumprod(plot_data.P3(2:end)+1);
 plot_data.P4(2:end) = cumprod(plot_data.P4(2:end)+1);
 plot_data.P5(2:end) = cumprod(plot_data.P5(2:end)+1);
 plot_data.P6(2:end) = cumprod(plot_data.P6(2:end)+1);
 plot_data.P7(2:end) = cumprod(plot_data.P7(2:end)+1);
 plot_data.P8(2:end) = cumprod(plot_data.P8(2:end)+1);
 plot_data.MarketReturn(2:end) = cumprod(plot_data.MarketReturn(2:end)+1);
 
 figure;
 y = timeseries(plot_data{:,3:11});
 y.Name = 'Cumulative Return';
 y.TimeInfo.Units = 'months';
 y.TimeInfo.StartDate = convertStringsToChars(string(year_start));
 y.TimeInfo.Format = 'yyyy';
 performance_plot = plot(y);
 title('Portfolio Performance');
 xlabel('Year');
 ylabel('Cumulative Return');
 legend('Low/Low', 'Low/Med1', 'Low/Med2', 'Low/High', 'High/Low', 'High/Med1', 'High/Med2', 'High/High', 'MarketReturn','Location','northwest');
 grid on;
 
end

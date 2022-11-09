function [entropy] = repurchase_entropy(data, probit_spec, time_variable, window_length)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   This function computes the share repurchase specific entropy and self-
%   information content, as well as the perceived probability of announcing
%   a share repruchase program at a specified point im time

%   Input: data, probit specification, time variable, and window length
%   Output: [Entropy, Self-Information Content]

%   Author: Sascha Jakob
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

warning('off','stats:LinearModel:RankDefDesignMat');
warning('off','stats:glmfit:IterationLimit');

estimation_data = data.year>=(time_variable-window_length)&data.year<=(time_variable-1)&data.mth==data.fyr;
estimation_subset = data(estimation_data,:);
estimation_subset.sic3 = categorical(estimation_subset.sic3);

prediction_year = data.year==time_variable&data.buyback==1;
prediction_data = data(prediction_year,:);
prediction_data.sic3 = categorical(prediction_data.sic3);

p = predict(fitglm(estimation_subset, probit_spec,'Distribution','binomial','link','probit'),prediction_data);

    entropy_h = -(p.*log2(p) + (1-p).*log2(1-p));
    entropy_H = array2table(entropy_h);
    entropy_i = log2(1./p);
    entropy_I = array2table(entropy_i);
    
entropy = [table(prediction_data.dealnumber, prediction_data.year, prediction_data.month,...
    prediction_data.date, prediction_data.permno) entropy_H entropy_I];

end


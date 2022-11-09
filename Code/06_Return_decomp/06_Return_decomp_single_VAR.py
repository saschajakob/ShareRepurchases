import os
os.system('cls')

# Import essentials
import pandas as pd
pd.set_option("display.max_rows", None, "display.max_columns", None)
import datetime
import numpy as np
import matplotlib.pyplot as plt
import math
from tqdm import tqdm
import warnings

# Import Statsmodels fo VAR
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller
from statsmodels.tools.eval_measures import rmse, aic


# Read STATA file
df = pd.read_stata("A:\\Sascha\\Buyback Anomalies\\return_decomp_data_1980_2019.dta")
df.set_index('ccm_yq', inplace=True)
df.index = df.index.to_period("Q")
groups = df.groupby('permno')

# global variables and df
ID_mat = np.identity(3)
discount_f = 0.95 #replace by regression
e1 = np.array([1,0,0])
global_gamma = np.array(pd.read_excel("A:\\Sascha\\Buyback Anomalies\\var_coeff_mat.xlsx",index_col=None, header=None))
global_gamma = np.matrix(global_gamma.reshape(3,3))
ones = np.array([1,1,1])

columns = ['dealnumber','permno','ccm_yq','pre_cf_lvl','post_cf_lvl','pre_cf_var','post_cf_var','pre_disc_lvl','post_disc_lvl','pre_disc_var','post_disc_var']
decomp_df =  pd.DataFrame(columns=columns)


# decomposition loop
with warnings.catch_warnings():
    warnings.simplefilter('ignore')
    for permno, key in tqdm(groups):
        for index, row in key.iterrows():
            if row['rep_q']==1:
                try:
                    idx = index
                    event_df = key.loc[idx - 20 : idx +20]
                    if event_df.shape[0]<24:                                                    # define minimum number of observations prior to the repurchase announcement
                        raise ValueError('Too few observations')
                    drop_df = key.loc[idx - 8 : idx - 1]                                     # define minimum number of quarters between repurchase announcements (overlap)
                    if drop_df['rep_q'].sum() != 0:
                        raise ValueError('Overlap with previous repurchase announcement')

                    # residuals
                    resid_mat =np.matrix(event_df[['c_lq_ret','c_lbtm_q','c_lroe_q']][1:]) - np.matrix(event_df[['c_lq_ret','c_lbtm_q','c_lroe_q']][:-1]) @ global_gamma @ np.matrix(ones).T
                    pre_sigma_mat = np.matrix(pd.DataFrame(resid_mat[0:20]).cov())
                    post_sigma_mat = np.matrix(pd.DataFrame(resid_mat[21:]).cov())

                    # decomposition
                    lambda_global = np.matrix(e1 @ (discount_f*global_gamma) @ np.linalg.inv(ID_mat -discount_f*global_gamma)).T

                    # discount-rate
                    # level
                    pre_disc_lvl = np.mean(resid_mat[0:20].dot(lambda_global))
                    post_disc_lvl = np.mean(resid_mat[21:].dot(lambda_global))

                    # variance
                    pre_disc_var = (lambda_global.T @ pre_sigma_mat @ lambda_global)[0].item()
                    post_disc_var = (lambda_global.T @ post_sigma_mat @ lambda_global)[0].item()

                    # cash-flow
                    # level
                    pre_cf_lvl = np.mean(resid_mat[0:20].dot(e1 + lambda_global))
                    post_cf_lvl = np.mean(resid_mat[21:].dot(e1 + lambda_global))

                    # variance
                    pre_cf_var = ((e1 + lambda_global.T) @ pre_sigma_mat @ (e1 + lambda_global.T).T)[0].item()
                    post_cf_var = ((e1 + lambda_global.T) @ post_sigma_mat @ (e1 + lambda_global.T).T)[0].item()

                    # decomposition data frame
                    decomp_dic = {'dealnumber':key.loc[idx]['dealnumber'],'permno':key.loc[idx]['permno'],'ccm_yq':idx,'pre_cf_lvl':pre_cf_lvl,'post_cf_lvl':post_cf_lvl,'pre_cf_var':pre_cf_var,'post_cf_var':post_cf_var,'pre_disc_lvl':pre_disc_lvl,'post_disc_lvl':post_disc_lvl,'pre_disc_var':pre_disc_var,'post_disc_var':post_disc_var}

                    decomp_df = decomp_df.append(decomp_dic, ignore_index=True)
                    decomp_dic={}
                except:
                    continue
decomp_df.shape[0]

columns = ['pre_cf_lvl','post_cf_lvl','pre_cf_var','post_cf_var','pre_disc_lvl','post_disc_lvl','pre_disc_var','post_disc_var']
decomp_df[columns] = decomp_df[columns].clip(lower=decomp_df[columns].quantile(0.01), upper=decomp_df[columns].quantile(0.99), axis=1)


decomp_df['disc_lvl_change_abs'] = decomp_df['post_disc_lvl'] - decomp_df['pre_disc_lvl']
decomp_df['disc_lvl_change_rel'] = decomp_df['disc_lvl_change_abs']/decomp_df['pre_disc_lvl']
decomp_df['disc_var_change_abs'] = decomp_df['post_disc_var'] - decomp_df['pre_disc_var']
decomp_df['disc_var_change_rel'] = decomp_df['disc_var_change_abs']/decomp_df['pre_disc_var']
decomp_df['cf_lvl_change_abs'] = decomp_df['post_cf_lvl'] - decomp_df['pre_cf_lvl']
decomp_df['cf_lvl_change_rel'] = decomp_df['cf_lvl_change_abs']/decomp_df['pre_cf_lvl']
decomp_df['cf_var_change_abs'] = decomp_df['post_cf_var'] - decomp_df['pre_cf_var']
decomp_df['cf_var_change_rel'] = decomp_df['cf_var_change_abs']/decomp_df['pre_cf_var']

columns = ['cf_lvl_change_abs','cf_lvl_change_rel','cf_var_change_abs','cf_var_change_rel','disc_lvl_change_abs','disc_lvl_change_rel','disc_var_change_abs','disc_var_change_rel']
decomp_df[columns] = decomp_df[columns].clip(lower=decomp_df[columns].quantile(0.01), upper=decomp_df[columns].quantile(0.99), axis=1)


# scaled changes
decomp_df['cf_lvl_change_scaled'] = decomp_df['cf_lvl_change_abs']/np.mean(decomp_df['pre_cf_lvl'])
decomp_df['cf_var_change_scaled'] = decomp_df['cf_var_change_abs']/np.mean(decomp_df['pre_cf_var'])
decomp_df['disc_lvl_change_scaled'] = decomp_df['disc_lvl_change_abs']/np.mean(decomp_df['pre_disc_lvl'])
decomp_df['disc_var_change_scaled'] = decomp_df['disc_var_change_abs']/np.mean(decomp_df['pre_disc_var'])


columns = ['cf_lvl_change_scaled','cf_var_change_scaled','disc_lvl_change_scaled','disc_var_change_scaled']
decomp_df[columns] = decomp_df[columns].clip(lower=decomp_df[columns].quantile(0.01), upper=decomp_df[columns].quantile(0.99), axis=1)

decomp_df['ccm_yq'] = decomp_df['ccm_yq'].astype(str)
decomp_df.to_stata('A:\\Sascha\\Buyback Anomalies\\decomp_data_single_VAR.dta')

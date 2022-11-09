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
discount_f = 0.985 #replace by regression
e1 = np.array([1,0,0])
columns = ['dealnumber','permno','ccm_yq','pre_cf_lvl','post_cf_lvl','pre_cf_var','post_cf_var','pre_disc_lvl','post_disc_lvl','pre_disc_var','post_disc_var']
decomp_df =  pd.DataFrame(columns=columns)


# decomposition loop
with warnings.catch_warnings():
    warnings.simplefilter('ignore')
    for permno, key in tqdm(groups):
        # df_subset  = df.loc[df['permno'] == permno]
        for index, row in key.iterrows():
            if row['rep_q']==1:
                try:
                    idx = index
                    pre_event_df = key.loc[idx - 20 : idx - 1]
                    if pre_event_df.shape[0]<16:                                                    # define minimum number of observations prior to the repurchase announcement
                        raise ValueError('Too few observations')
                    drop_df = key.loc[idx - 8 : idx - 1]                                     # define minimum number of quarters between repurchase announcements (overlap)
                    if drop_df['rep_q'].sum() != 0:
                        raise ValueError('Overlap with previous repurchase announcement')
                    post_event_df = key.loc[idx + 1 : idx + 20]
                    if post_event_df.shape[0]<16:                                                   # define minimum number of observations subsequent to the repurchase announcement
                        raise ValueError('Too few observations')

                    # pre-event VAR
                    model = VAR(pre_event_df[['lq_ret','lbtm_q','lroe_q']])
                    results = model.fit(1)
                    pre_resid_mat = np.matrix(results.resid)
                    pre_coeff_mat = np.matrix(results.coefs)
                    pre_sigma_mat = np.matrix(results.sigma_u)

                    # post-event VAR
                    model = VAR(post_event_df[['lq_ret','lbtm_q','lroe_q']])
                    results = model.fit(1)
                    post_resid_mat= np.matrix(results.resid)
                    post_coeff_mat = np.matrix(results.coefs)
                    post_sigma_mat = np.matrix(results.sigma_u)

                    # decomposition
                    lambda_pre = np.matrix(e1 @ (discount_f*pre_coeff_mat) @ np.linalg.inv(ID_mat -discount_f*pre_coeff_mat)).T
                    lambda_post = np.matrix(e1 @ (discount_f*post_coeff_mat) @ np.linalg.inv(ID_mat -discount_f*post_coeff_mat)).T

                    # discount-rate
                    # level
                    pre_disc_lvl = np.mean(pre_resid_mat.dot(lambda_pre))
                    post_disc_lvl = np.mean(post_resid_mat.dot(lambda_post))

                    # variance
                    pre_disc_var = (lambda_pre.T @ pre_sigma_mat @ lambda_pre)[0].item()
                    post_disc_var = (lambda_post.T @ post_sigma_mat @ lambda_post)[0].item()

                    # cash-flow
                    # level
                    pre_cf_lvl = np.mean(pre_resid_mat.dot(e1 + lambda_pre))
                    post_cf_lvl = np.mean(post_resid_mat.dot(e1 + lambda_post))

                    # variance
                    pre_cf_var = ((e1 + lambda_pre.T) @ pre_sigma_mat @ (e1 + lambda_pre.T).T)[0].item()
                    post_cf_var = ((e1 + lambda_post.T) @ post_sigma_mat @ (e1 + lambda_post.T).T)[0].item()

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
decomp_df.to_stata('A:\\Sascha\\Buyback Anomalies\\decomp_data.dta')

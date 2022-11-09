from statsmodels.tools.eval_measures import rmse, aic
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.api import VAR
import warnings
from tqdm import tqdm
import math
import matplotlib.pyplot as plt
import numpy as np
import datetime
import pandas as pd
import os
#os.system('cls')

# Import essentials
pd.set_option("display.max_rows", None, "display.max_columns", None)


# Read STATA file
df = pd.read_stata("/Users/saschajakob/Desktop/Paper 1/return_decomp_data_1980_2019_uval.dta")
df.set_index('ccm_yq', inplace=True)
df.index = df.index.to_period("Q")
groups = df.groupby('permno')

# global variables and df
ID_mat = np.identity(3)
discount_f = 0.95 # from regression, substitutung with value from Michaely et al (2020) does not change results
e1 = np.array([1,0,0])
global_gamma = np.array(pd.read_excel("/Users/saschajakob/Desktop/Paper 1/var_coeff_mat_uval.xlsx",index_col=None, header=None))
global_gamma = np.matrix(global_gamma.reshape(3,3))
global_gamma
columns = ['dealnumber','permno','ccm_yq','pre_cf_lvl','post_cf_lvl','pre_cf_var','post_cf_var','pre_disc_lvl','post_disc_lvl','pre_disc_var','post_disc_var']
decomp_df =  pd.DataFrame(columns=columns)


# decomposition loop
with warnings.catch_warnings():
    warnings.simplefilter('ignore')
    for permno, key in tqdm(groups):
        for index, row in key.iterrows():
            if row['rep_q'] == 1 and row['mispricing_tag']==1:
                try:
                    idx = index
                    pre_event_df = key.loc[idx - 20: idx - 1]
                    # define minimum number of observations prior to the repurchase announcement
                    if pre_event_df.shape[0] < 12:
                        raise ValueError('Too few observations')
                    # define minimum number of quarters between repurchase announcements (overlap)
                    drop_df = key.loc[idx - 12: idx - 1]
                    if drop_df['rep_q'].sum() != 0:
                        raise ValueError('Overlap with previous repurchase announcement')
                    post_event_df = key.loc[idx + 1: idx + 20]
                    # define minimum number of observations subsequent to the repurchase announcement
                    if post_event_df.shape[0] < 12:
                        raise ValueError('Too few observations')

                    # pre-event VAR
                    model = VAR(pre_event_df[['c_lq_ret', 'c_lbtm_q', 'c_lroe_q']])
                    results = model.fit(1)
                    pre_resid_mat = np.matrix(results.resid)
                    pre_sigma_mat = np.matrix(results.sigma_u)
                    pre_fit_mat = np.matrix(results.fittedvalues)

                    # post-event VAR
                    model = VAR(post_event_df[['c_lq_ret', 'c_lbtm_q', 'c_lroe_q']])
                    results = model.fit(1)
                    post_resid_mat = np.matrix(results.resid)
                    post_sigma_mat = np.matrix(results.sigma_u)
                    post_fit_mat = np.matrix(results.fittedvalues)

                    # decomposition
                    lambda_pre = np.matrix(e1 @ (discount_f * global_gamma) @ np.linalg.inv(ID_mat - discount_f * global_gamma)).T
                    lambda_post = np.matrix(e1 @ (discount_f * global_gamma) @ np.linalg.inv(ID_mat - discount_f * global_gamma)).T

                    # fitted discount rate values
                    pre_disc_fit = np.mean(pre_fit_mat.dot(lambda_pre))
                    post_disc_fit = np.mean(post_fit_mat.dot(lambda_post))

                    pre_disc_fit_1y = np.mean(pre_fit_mat.dot(lambda_pre)[-4])
                    pre_disc_fit_3q = np.mean(pre_fit_mat.dot(lambda_pre)[-3])
                    pre_disc_fit_2q = np.mean(pre_fit_mat.dot(lambda_pre)[-2])
                    pre_disc_fit_1q = np.mean(pre_fit_mat.dot(lambda_pre)[-1])
                    post_disc_fit_1q = np.mean(post_fit_mat.dot(lambda_post)[0])
                    post_disc_fit_1y = np.mean(post_fit_mat.dot(lambda_post)[3])
                    post_disc_fit_2y = np.mean(post_fit_mat.dot(lambda_post)[7])
                    post_disc_fit_3y = np.mean(post_fit_mat.dot(lambda_post)[11])

                    # discount-rate news
                    # level
                    pre_disc_lvl = np.mean(pre_resid_mat.dot(lambda_pre))
                    post_disc_lvl = np.mean(post_resid_mat.dot(lambda_post))

                    pre_disc_lvl_1y = np.mean(pre_resid_mat.dot(lambda_pre)[-4])
                    pre_disc_lvl_3q = np.mean(pre_resid_mat.dot(lambda_pre)[-3])
                    pre_disc_lvl_2q = np.mean(pre_resid_mat.dot(lambda_pre)[-2])
                    pre_disc_lvl_1q = np.mean(pre_resid_mat.dot(lambda_pre)[-1])
                    post_disc_lvl_1q = np.mean(post_resid_mat.dot(lambda_post)[0])
                    post_disc_lvl_1y = np.mean(post_resid_mat.dot(lambda_post)[3])
                    post_disc_lvl_2y = np.mean(post_resid_mat.dot(lambda_post)[7])
                    post_disc_lvl_3y = np.mean(post_resid_mat.dot(lambda_post)[11])

                    # variance
                    pre_disc_var = (lambda_pre.T @ pre_sigma_mat @ lambda_pre)[0].item()
                    post_disc_var = (lambda_post.T @ post_sigma_mat @ lambda_post)[0].item()

                    # fitted cash flow values
                    pre_cf_fit = np.mean(pre_fit_mat.dot(e1 + lambda_pre))
                    post_cf_fit = np.mean(post_fit_mat.dot(e1 + lambda_post))

                    pre_cf_fit_1y = np.mean(pre_fit_mat.dot(e1 + lambda_pre)[-4])
                    pre_cf_fit_3q = np.mean(pre_fit_mat.dot(e1 + lambda_pre)[-3])
                    pre_cf_fit_2q = np.mean(pre_fit_mat.dot(e1 + lambda_pre)[-2])
                    pre_cf_fit_1q = np.mean(pre_fit_mat.dot(e1 + lambda_pre)[-1])
                    post_cf_fit_1q = np.mean(post_fit_mat.dot(e1 + lambda_post)[0])
                    post_cf_fit_1y = np.mean(post_fit_mat.dot(e1 + lambda_post)[3])
                    post_cf_fit_2y = np.mean(post_fit_mat.dot(e1 + lambda_post)[7])
                    post_cf_fit_3y = np.mean(post_fit_mat.dot(e1 + lambda_post)[11])

                    # cash-flow news
                    # level
                    pre_cf_lvl = np.mean(pre_resid_mat.dot(e1 + lambda_pre))
                    post_cf_lvl = np.mean(post_resid_mat.dot(e1 + lambda_post))

                    pre_cf_lvl_1y = np.mean(pre_resid_mat.dot(e1 + lambda_pre)[-4])
                    pre_cf_lvl_3q = np.mean(pre_resid_mat.dot(e1 + lambda_pre)[-3])
                    pre_cf_lvl_2q = np.mean(pre_resid_mat.dot(e1 + lambda_pre)[-2])
                    pre_cf_lvl_1q = np.mean(pre_resid_mat.dot(e1 + lambda_pre)[-1])
                    post_cf_lvl_1q = np.mean(post_resid_mat.dot(e1 + lambda_post)[0])
                    post_cf_lvl_1y = np.mean(post_resid_mat.dot(e1 + lambda_post)[3])
                    post_cf_lvl_2y = np.mean(post_resid_mat.dot(e1 + lambda_post)[7])
                    post_cf_lvl_3y = np.mean(post_resid_mat.dot(e1 + lambda_post)[11])

                    # variance
                    pre_cf_var = ((e1 + lambda_pre.T) @ pre_sigma_mat @ (e1 + lambda_pre.T).T)[0].item()
                    post_cf_var = ((e1 + lambda_post.T) @ post_sigma_mat @ (e1 + lambda_post.T).T)[0].item()

                    # decomposition data frame
                    decomp_dic = {'dealnumber': key.loc[idx]['dealnumber'], 'permno':key.loc[idx]['permno'], 'ccm_yq':idx, 'pre_cf_lvl':pre_cf_lvl, 'post_cf_lvl':post_cf_lvl, 'pre_cf_lvl_1y':pre_cf_lvl_1y, 'pre_cf_lvl_3q':pre_cf_lvl_3q, 'pre_cf_lvl_2q':pre_cf_lvl_2q, 'pre_cf_lvl_1q':pre_cf_lvl_1q, 'post_cf_lvl_1q':post_cf_lvl_1q, 'post_cf_lvl_1y':post_cf_lvl_1y, 'post_cf_lvl_2y':post_cf_lvl_2y, 'post_cf_lvl_3y':post_cf_lvl_3y, 'pre_cf_var':pre_cf_var, 'post_cf_var':post_cf_var, 'pre_disc_lvl':pre_disc_lvl, 'post_disc_lvl':post_disc_lvl, 'pre_disc_lvl_1y':pre_disc_lvl_1y, 'pre_disc_lvl_3q':pre_disc_lvl_3q, 'pre_disc_lvl_2q':pre_disc_lvl_2q, 'pre_disc_lvl_1q':pre_disc_lvl_1q, 'post_disc_lvl_1q':post_disc_lvl_1q,
                    'post_disc_lvl_1y':post_disc_lvl_1y, 'post_disc_lvl_2y':post_disc_lvl_2y, 'post_disc_lvl_3y':post_disc_lvl_3y, 'pre_disc_var':pre_disc_var, 'post_disc_var':post_disc_var, 'pre_disc_fit':pre_disc_fit, 'pre_disc_fit_1q':pre_disc_fit_1q, 'pre_disc_fit_2q':pre_disc_fit_2q, 'pre_disc_fit_3q':pre_disc_fit_3q, 'pre_disc_fit_1y':pre_disc_fit_1y, 'post_disc_fit':post_disc_fit, 'post_disc_fit_1q':post_disc_fit_1q, 'post_disc_fit_1y':post_disc_fit_1y, 'post_disc_fit_2y':post_disc_fit_2y, 'post_disc_fit_3y':post_disc_fit_3y,
                                  'pre_cf_fit':pre_cf_fit, 'pre_cf_fit_1q':pre_cf_fit_1q, 'pre_cf_fit_2q':pre_cf_fit_2q, 'pre_cf_fit_3q':pre_cf_fit_3q, 'pre_cf_fit_1y':pre_cf_fit_1y, 'post_cf_fit':post_cf_fit, 'post_cf_fit_1q':post_cf_fit_1q, 'post_cf_fit_1y':post_cf_fit_1y, 'post_cf_fit_2y':post_cf_fit_2y, 'post_cf_fit_3y':post_cf_fit_3y}

                    decomp_df = decomp_df.append(decomp_dic, ignore_index=True)
                    decomp_dic = {}

                except:
                    continue


columns = ['pre_cf_lvl', 'post_cf_lvl', 'pre_cf_lvl_1y', 'pre_cf_lvl_3q', 'pre_cf_lvl_2q', 'pre_cf_lvl_1q', 'post_cf_lvl_1q', 'post_cf_lvl_1y', 'post_cf_lvl_2y', 'post_cf_lvl_3y', 'pre_cf_var', 'post_cf_var',
           'pre_disc_lvl', 'post_disc_lvl', 'pre_disc_lvl_1y', 'pre_disc_lvl_3q', 'pre_disc_lvl_2q', 'pre_disc_lvl_1q', 'post_disc_lvl_1q', 'post_disc_lvl_1y', 'post_disc_lvl_2y', 'post_disc_lvl_3y', 'pre_disc_var', 'post_disc_var']
decomp_df[columns] = decomp_df[columns].clip(lower=decomp_df[columns].quantile(0.01), upper=decomp_df[columns].quantile(0.99), axis=1)


decomp_df['disc_lvl_change_abs'] = decomp_df['post_disc_lvl'] - decomp_df['pre_disc_lvl']
decomp_df['disc_lvl_change_rel'] = decomp_df['disc_lvl_change_abs'] / decomp_df['pre_disc_lvl']
decomp_df['disc_lvl_change_abs_1q_1q'] = decomp_df['post_disc_lvl_1q'] - decomp_df['pre_disc_lvl_1q']
decomp_df['disc_lvl_change_rel_1q'] = decomp_df['disc_lvl_change_abs_1q_1q'] / decomp_df['pre_disc_lvl_1q']

decomp_df['disc_lvl_change_abs_1q_1y'] = decomp_df['post_disc_lvl_1y'] - decomp_df['pre_disc_lvl_1q']
decomp_df['disc_lvl_change_abs_2q_1y'] = decomp_df['post_disc_lvl_1y'] - decomp_df['pre_disc_lvl_2q']
decomp_df['disc_lvl_change_abs_3q_1y'] = decomp_df['post_disc_lvl_1y'] - decomp_df['pre_disc_lvl_3q']
decomp_df['disc_lvl_change_abs_1y_1y'] = decomp_df['post_disc_lvl_1y'] - decomp_df['pre_disc_lvl_1y']

decomp_df['disc_lvl_change_rel_1y'] = decomp_df['disc_lvl_change_abs_1q_1y'] / decomp_df['pre_disc_lvl_1q']
decomp_df['disc_lvl_change_abs_2y'] = decomp_df['post_disc_lvl_2y'] - decomp_df['pre_disc_lvl_1q']
decomp_df['disc_lvl_change_rel_2y'] = decomp_df['disc_lvl_change_abs_2y'] / decomp_df['pre_disc_lvl_1q']
decomp_df['disc_lvl_change_abs_3y'] = decomp_df['post_disc_lvl_3y'] - decomp_df['pre_disc_lvl_1q']
decomp_df['disc_lvl_change_rel_3y'] = decomp_df['disc_lvl_change_abs_3y'] / decomp_df['pre_disc_lvl_1q']

decomp_df['disc_var_change_abs'] = decomp_df['post_disc_var'] - decomp_df['pre_disc_var']
decomp_df['disc_var_change_rel'] = decomp_df['disc_var_change_abs'] / decomp_df['pre_disc_var']

decomp_df['cf_lvl_change_abs'] = decomp_df['post_cf_lvl'] - decomp_df['pre_cf_lvl']
decomp_df['cf_lvl_change_rel'] = decomp_df['cf_lvl_change_abs'] / decomp_df['pre_cf_lvl']
decomp_df['cf_lvl_change_abs_1q_1q'] = decomp_df['post_cf_lvl_1q'] - decomp_df['pre_cf_lvl_1q']
decomp_df['cf_lvl_change_rel_1q'] = decomp_df['cf_lvl_change_abs_1q_1q'] / decomp_df['pre_cf_lvl_1q']

decomp_df['cf_lvl_change_abs_1q_1y'] = decomp_df['post_cf_lvl_1y'] - decomp_df['pre_cf_lvl_1q']
decomp_df['cf_lvl_change_abs_2q_1y'] = decomp_df['post_cf_lvl_1y'] - decomp_df['pre_cf_lvl_2q']
decomp_df['cf_lvl_change_abs_3q_1y'] = decomp_df['post_cf_lvl_1y'] - decomp_df['pre_cf_lvl_3q']
decomp_df['cf_lvl_change_abs_1y_1y'] = decomp_df['post_cf_lvl_1y'] - decomp_df['pre_cf_lvl_1y']


decomp_df['cf_lvl_change_rel_1y'] = decomp_df['cf_lvl_change_abs_1q_1y'] / decomp_df['pre_cf_lvl_1q']
decomp_df['cf_lvl_change_abs_2y'] = decomp_df['post_cf_lvl_2y'] - decomp_df['pre_cf_lvl_1q']
decomp_df['cf_lvl_change_rel_2y'] = decomp_df['cf_lvl_change_abs_2y'] / decomp_df['pre_cf_lvl_1q']
decomp_df['cf_lvl_change_abs_3y'] = decomp_df['post_cf_lvl_3y'] - decomp_df['pre_cf_lvl_1q']
decomp_df['cf_lvl_change_rel_3y'] = decomp_df['cf_lvl_change_abs_3y'] / decomp_df['pre_cf_lvl_1q']

decomp_df['cf_var_change_abs'] = decomp_df['post_cf_var'] - decomp_df['pre_cf_var']
decomp_df['cf_var_change_rel'] = decomp_df['cf_var_change_abs'] / decomp_df['pre_cf_var']

columns = ['cf_lvl_change_abs', 'cf_lvl_change_rel', 'cf_var_change_abs', 'cf_var_change_rel', 'disc_lvl_change_abs', 'disc_lvl_change_rel', 'disc_var_change_abs', 'disc_var_change_rel', 'cf_lvl_change_abs_1q_1q', 'cf_lvl_change_abs_1q_1y', 'cf_lvl_change_abs_2q_1y', 'cf_lvl_change_abs_3q_1y', 'cf_lvl_change_abs_1y_1y', 'cf_lvl_change_abs_2y', 'cf_lvl_change_abs_3y',
           'cf_lvl_change_rel_1q', 'cf_lvl_change_rel_1y', 'cf_lvl_change_rel_2y', 'cf_lvl_change_rel_3y', 'disc_lvl_change_abs_1q_1q', 'disc_lvl_change_abs_1q_1y', 'disc_lvl_change_abs_2q_1y', 'disc_lvl_change_abs_3q_1y', 'disc_lvl_change_abs_1y_1y', 'disc_lvl_change_abs_2y', 'disc_lvl_change_abs_3y', 'disc_lvl_change_rel_1q', 'disc_lvl_change_rel_1y', 'disc_lvl_change_rel_2y', 'disc_lvl_change_rel_3y']
decomp_df[columns] = decomp_df[columns].clip(lower=decomp_df[columns].quantile(0.01), upper=decomp_df[columns].quantile(0.99), axis=1)


# scaled changes
decomp_df['cf_lvl_change_scaled'] = decomp_df['cf_lvl_change_abs'] / np.mean(decomp_df['pre_cf_lvl'])
decomp_df['cf_lvl_change_scaled_1q'] = decomp_df['cf_lvl_change_abs_1q_1q'] / np.mean(decomp_df['pre_cf_lvl_1q'])
decomp_df['cf_lvl_change_scaled_1y'] = decomp_df['cf_lvl_change_abs_1q_1y'] / np.mean(decomp_df['pre_cf_lvl_1q'])
decomp_df['cf_lvl_change_scaled_2y'] = decomp_df['cf_lvl_change_abs_2y'] / np.mean(decomp_df['pre_cf_lvl_1q'])
decomp_df['cf_lvl_change_scaled_3y'] = decomp_df['cf_lvl_change_abs_3y'] / np.mean(decomp_df['pre_cf_lvl_1q'])

decomp_df['disc_lvl_change_scaled'] = decomp_df['disc_lvl_change_abs'] / np.mean(decomp_df['pre_disc_lvl'])
decomp_df['disc_lvl_change_scaled_1q'] = decomp_df['disc_lvl_change_abs_1q_1q'] / np.mean(decomp_df['pre_disc_lvl_1q'])
decomp_df['disc_lvl_change_scaled_1y'] = decomp_df['disc_lvl_change_abs_1q_1y'] / np.mean(decomp_df['pre_disc_lvl_1q'])
decomp_df['disc_lvl_change_scaled_2y'] = decomp_df['disc_lvl_change_abs_2y'] / np.mean(decomp_df['pre_disc_lvl_1q'])
decomp_df['disc_lvl_change_scaled_3y'] = decomp_df['disc_lvl_change_abs_3y'] / np.mean(decomp_df['pre_disc_lvl_1q'])

decomp_df['cf_var_change_scaled'] = decomp_df['cf_var_change_abs'] / np.mean(decomp_df['pre_cf_var'])
decomp_df['disc_var_change_scaled'] = decomp_df['disc_var_change_abs'] / np.mean(decomp_df['pre_disc_var'])


columns = ['cf_lvl_change_scaled', 'cf_var_change_scaled', 'disc_lvl_change_scaled', 'disc_var_change_scaled', 'cf_lvl_change_scaled_1q', 'cf_lvl_change_scaled_1y',
           'cf_lvl_change_scaled_2y', 'cf_lvl_change_scaled_3y', 'disc_lvl_change_scaled_1q', 'disc_lvl_change_scaled_1y', 'disc_lvl_change_scaled_2y', 'disc_lvl_change_scaled_3y']
decomp_df[columns] = decomp_df[columns].clip(lower=decomp_df[columns].quantile(0.01), upper=decomp_df[columns].quantile(0.99), axis=1)

decomp_df['ccm_yq'] = decomp_df['ccm_yq'].astype(str)
decomp_df.to_stata('/Users/saschajakob/Desktop/Paper 1/decomp_data_single_gamma_uval.dta')

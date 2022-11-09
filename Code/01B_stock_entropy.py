import time,numpy as np
import pandas as pd
pd.options.display.max_rows = 1000
pd.options.display.html.table_schema = True
pd.options.display.float_format = lambda x : '{:.0f}'.format(x) if round(x,0) == x else '{:,.3f}'.format(x)
import math as mt
import matplotlib as mpl 

#_______  Maximum Likelihood Entropy Estimator _____________________________________________________
def plugIn(msg,w,method):
    # Compute plug-in (ML) entropy rate
    if len(msg)<50:
        out = np.nan
    else:
        pmf = pmf1(msg,w,method)
        out = -sum([pmf[i]*np.log2(pmf[i]) for i in pmf])/w
    return out
#---------------------------------------------------------------------------------------------------
def pmf1(msg,w,method):
    #Compute the probability mass function for a one-dimensional discrete random variable
    #len(msg)-w occurences
    lib={}

    if method == 'Quantile':
        msg = msg.groupby('dealnumber')['ret'].transform(lambda x: pd.cut(x,10,labels=False, duplicates='drop'))
    elif method == 'Sigma':
        msg = np.floor(np.subtract(msg['ret'], msg.groupby(['dealnumber'])['ret'].transform('min'))/(msg.groupby(['dealnumber'])['ret'].transform('std')))
    elif method == 'Binary':
        msg = (msg['ret'] > 0).astype(int)

    if not isinstance(msg,str):msg=' '.join(map(str,msg))
    msg = msg.replace(".0","")
    msg = tuple(msg.split(' '))
    for i in range(w,len(msg)):
        msg_=msg[i-w:i]
        if msg_ not in lib:lib[msg_]=[i-w]
        else:lib[msg_]=lib[msg_]+[i-w]
    pmf = float(len(msg)-w)
    pmf = {i:len(lib[i])/pmf for i in lib}
    return pmf

#________ Kontoyiannis Entropy Estimator ___________________________________________________________
def kontoyiannis(msg,method,window=None):
    out={'num':0,'sum':0,'subS':[]}

    if method == 'Quantile':
        msg = msg.groupby('dealnumber')['ret'].transform(lambda x: pd.cut(x,10,labels=False, duplicates='drop'))
    elif method == 'Sigma':
        msg = np.floor(np.subtract(msg['ret'], msg.groupby(['dealnumber'])['ret'].transform('min'))/(msg.groupby(['dealnumber'])['ret'].transform('std')))
    elif method == 'Binary':
        msg = (msg['ret'] > 0).astype(int)

    if len(msg)<=50:
        out['h']= np.nan
    else:
        if not isinstance(msg,str):msg=' '.join(map(str,msg))
        msg = msg.replace(".0","")
        msg = tuple(msg.split(' '))
        if window is None:
            points = range(1,len(msg)//2+1)
        else:
            window = min(window,len(msg)//2)
            points = range(window,len(msg)-window+1)
            for i in points:
                if window is None:
                    l,msg_ = matchLength(msg,i,i)
                    out['sum'] += np.log2(i+1)/l
                else:
                    l,msg_ = matchLength(msg,i,window)
                    out['sum'] += np.log2(window+1)/l
                    out['subS'].append(msg_)
                    out['num'] += 1
                    out['h'] = out['sum']/out['num']
                    out['r'] = 1-out['h']/np.log2(len(msg))
    return out['h']
#---------------------------------------------------------------------------------------------------
def matchLength(msg,i,n):
    # Maximum matched length+1, with overlap.
    # i>=n & len(msg)>=i+n
    subS = ' '
    for l in range(n):
        msg1 = msg[i:i+l+1]
        for j in range(i-n,i):
            msg0 = msg[j:j+l+1]
            if msg1 == msg0:
                subS=msg1
                break
    return len(subS)+1, subS

#________ Lempel-Ziv Estimator _____________________________________________________________________
def lempelZiv_lib(msg):
    i,lib=1, [msg[0]]
    while i<len(msg):
        for j in range(i,len(msg)):
            msg_=msg[i:j+1]
            if msg_ not in lib:
                lib.append(msg_)
                break
        i = j+1
    return lib
#___________________________________________________________________________________________________

# load data
df = pd.read_csv('A:\\Sascha\\Buyback Anomalies\\daily_ret.csv', sep =',')
df = df.sort_values(by=['dealnumber','dist'])
df['dealnumber'].nunique()

### Myopic ###--------------------------------------------------------------------------------------
df_entropy = df.loc[df['dist']>=-250]
df_entropy = df_entropy.loc[df_entropy['dist']<0]

# Entropy estimation
ML_entropy = df_entropy.groupby('dealnumber').apply(lambda x: plugIn(x,2,'Quantile'))
KT_entropy = df_entropy.groupby('dealnumber').apply(lambda x: kontoyiannis(x,'Quantile',10))
Entropy_df = pd.DataFrame(dict(ML_entropy = ML_entropy, KT_Entropy = KT_entropy)).reset_index()
Entropy_df['dealnumber'].nunique()
df = df.set_index('dealnumber').join(Entropy_df.set_index('dealnumber')).reset_index()

### 1st Year ###------------------------------------------------------------------------------------
df_entropy = df.loc[df['dist']>=0]
df_entropy = df_entropy.loc[df_entropy['dist']<252]

# Entropy estimation
ML_entropy = df_entropy.groupby('dealnumber').apply(lambda x: plugIn(x,2,'Quantile'))
KT_entropy = df_entropy.groupby('dealnumber').apply(lambda x: kontoyiannis(x,'Quantile',10))
Entropy_df = pd.DataFrame(dict(ML_entropy_1Y = ML_entropy, KT_Entropy_1Y = KT_entropy )).reset_index()
Entropy_df['dealnumber'].nunique()
df = df.set_index('dealnumber').join(Entropy_df.set_index('dealnumber')).reset_index()

### 2nd Year ###------------------------------------------------------------------------------------
df_entropy = df.loc[df['dist']>=252]
df_entropy = df_entropy.loc[df_entropy['dist']<504]

# Entropy estimation
ML_entropy = df_entropy.groupby('dealnumber').apply(lambda x: plugIn(x,2,'Quantile'))
KT_entropy = df_entropy.groupby('dealnumber').apply(lambda x: kontoyiannis(x,'Quantile',10))
Entropy_df = pd.DataFrame(dict(ML_entropy_2Y = ML_entropy, KT_Entropy_2Y = KT_entropy )).reset_index()
Entropy_df['dealnumber'].nunique()
df = df.set_index('dealnumber').join(Entropy_df.set_index('dealnumber')).reset_index()

### 3rd Year ###------------------------------------------------------------------------------------
df_entropy = df.loc[df['dist']>=504]
df_entropy = df_entropy.loc[df_entropy['dist']<756]

# Entropy estimation
ML_entropy = df_entropy.groupby('dealnumber').apply(lambda x: plugIn(x,2,'Quantile'))
KT_entropy = df_entropy.groupby('dealnumber').apply(lambda x: kontoyiannis(x,'Quantile',10))
Entropy_df = pd.DataFrame(dict(ML_entropy_3Y = ML_entropy, KT_Entropy_3Y = KT_entropy )).reset_index()
Entropy_df['dealnumber'].nunique()
df = df.set_index('dealnumber').join(Entropy_df.set_index('dealnumber')).reset_index()

### 4th Year ###------------------------------------------------------------------------------------
df_entropy = df.loc[df['dist']>=756]
df_entropy = df_entropy.loc[df_entropy['dist']<1008]

# Entropy estimation
ML_entropy = df_entropy.groupby('dealnumber').apply(lambda x: plugIn(x,2,'Quantile'))
KT_entropy = df_entropy.groupby('dealnumber').apply(lambda x: kontoyiannis(x,'Quantile',10))
Entropy_df = pd.DataFrame(dict(ML_entropy_4Y = ML_entropy, KT_Entropy_4Y = KT_entropy )).reset_index()
Entropy_df['dealnumber'].nunique()
df = df.set_index('dealnumber').join(Entropy_df.set_index('dealnumber')).reset_index()

df = df[['dealnumber','ML_entropy','ML_entropy_1Y','ML_entropy_2Y','ML_entropy_3Y','ML_entropy_4Y','KT_Entropy','KT_Entropy_1Y','KT_Entropy_2Y','KT_Entropy_3Y','KT_Entropy_4Y']]
df = df.drop_duplicates()
df.to_stata('A:\\Sascha\\Buyback Anomalies\\stock_entropy.dta')
df

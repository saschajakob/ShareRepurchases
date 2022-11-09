import pandas as pd
import seaborn as sns
sns.set_style("darkgrid")
import matplotlib.pyplot as plt
import numpy as np

data = {'period':['-1 year ','','','-1 quarter','+1 quarter','','','+1 year','','','','+2 years','','','','+3 years'],
'VAR-implied discount rate':[0.0495575, 0.0513385, 0.053718 , 0.0570685, 0.0573945 , np.nan,np.nan, 0.0548795, np.nan,np.nan,np.nan, 0.0515673, np.nan,np.nan,np.nan, 0.0465071],
        'Market discount rate': [0.0495575-0.0000109, 0.0513385+0.0018063, 0.053718+0.0028059, 0.0570685+0.0079111, 0.0573945+0.0061552 , np.nan,np.nan, 0.0548795+0.0015352, np.nan,np.nan,np.nan, 0.0515673+0.0002993, np.nan,np.nan,np.nan, 0.0465071-0.0014047]}

df = pd.DataFrame(data)
df = df.interpolate()

df[['VAR-implied discount rate','Market discount rate']].plot.line(figsize=(15,5),xlabel='Distance to repurchase announcement', ylabel='Centered discount rate',style=['-','--'], color=['steelblue','lightskyblue'],ylim=(0.03,0.1),x_compat=True)
plt.xticks(df.index, df.period)
plt.vlines(x=[0,1,2,3,4,7,11,15],
           ymin=[0.0495575, 0.0513385, 0.053718 , 0.0570685, 0.0573945, 0.0548795, 0.0515673, 0.0465071],
           ymax=[0.0495575-0.0000109, 0.0513385+0.0018063, 0.053718+0.0028059, 0.0570685+0.0079111, 0.0573945+0.0061552 ,0.0548795+0.0015352, 0.0515673+0.0002993, 0.0465071-0.0014047],
           colors='deeppink', ls='--', lw=2, label='Discount rate news')
plt.vlines(x=3.5,
           ymin=0.03,
           ymax=0.1,
           colors='darkviolet', ls=':', lw=1.5, label='Repurchase announcement')
# plt.hlines(y=0.0655717,
#           xmin=3,
#           xmax=16,
#           colors='steelblue', ls=':', lw=1.5, label='VAR-implied discount rate 1 quarter prior to announcement')
# plt.hlines(y=0.0655717+0.0098558,
#           xmin=3,
#           xmax=16,
#           colors='lightskyblue', ls=':', lw=1.5, label='Market discount rate 1 quarter prior to announcement')
plt.legend(loc='upper right')
plt.ylim([0.03, 0.09])
plt.savefig('VAR_discount_rate.png',bbox_inches='tight',pad_inches = 0.025, dpi=600)
plt.show()
# within_effect_model
# check which python is in use.
import sys
print('Notebook is running:', sys.executable)

# further check your python version
from platform import python_version

print('The current HDDM version is', python_version())

# If you are sure that conda is installed, also check the package that install
#!conda list  # list the conda

import hddm, IPython
import numpy as np
import pandas as pd
print('The current HDDM version is', hddm.__version__) # 0.8.0

# Warning:`IPython.parallel` package has been deprecated since IPython 4.0. 
print('The current IPython version is', IPython.__version__) 

print('The current Numpy version is', np.__version__) 

print('The current Pandas version is', pd.__version__)

# Preparation
import os, time, csv
from datetime import date
import random

import kabuki, hddm
from patsy import dmatrix

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

n_subjects = 10
trials_per_level = 150

level1a = {'v':.3, 'a':2, 't':.3, 'sv':0, 'z':.5, 'sz':0, 'st':0}
level2a = {'v':.4, 'a':2, 't':.3, 'sv':0, 'z':.6, 'sz':0, 'st':0}
level3a = {'v':.5, 'a':2, 't':.3, 'sv':0, 'z':.7, 'sz':0, 'st':0}

level1b = {'v':.3, 'a':2, 't':.3, 'sv':0, 'z':.5, 'sz':0, 'st':0}
level2b = {'v':.4, 'a':2, 't':.3, 'sv':0, 'z':.4, 'sz':0, 'st':0}
level3b = {'v':.5, 'a':2, 't':.3, 'sv':0, 'z':.3, 'sz':0, 'st':0}

random.seed(123)
np.random.seed(123)
data_a, params_a = hddm.generate.gen_rand_data({'level1': level1a,
                                                'level2': level2a,
                                                'level3': level3a},
                                                size=trials_per_level,
                                                subjs=n_subjects)

data_b, params_b = hddm.generate.gen_rand_data({'level1': level1b,
                                                'level2': level2b,
                                                'level3': level3b},
                                                size=trials_per_level,
                                                subjs=n_subjects)

data_a['stimulus'] = pd.Series(np.ones((len(data_a))), index=data_a.index)
data_b['stimulus'] = pd.Series(np.ones((len(data_b)))*2, index=data_a.index)
mydata = data_a.append(data_b,ignore_index=True)

mydata.head(10)

def z_link_func(x, data=mydata):
    stim = (dmatrix('0 + C(s, [[1], [-1]])',
                    {'s':data.stimulus.loc[x.index]},
                    return_type="dataframe")
#    stim = (np.asarray(dmatrix('0 + C(s, [[1], [-1]])',
#                               {'s':data.stimulus.ix[x.index]})) # original .ix is deprecated.
           )
    # print(x.shape)
    return 1/(1+np.exp(-np.multiply(x.to_frame(), stim)))

z_reg = {'model': 'z ~ 1 + C(condition)', 'link_func':z_link_func}

v_reg = {'model': 'v ~ 1 + C(condition)', 'link_func': lambda x:x}

reg_descr = [z_reg, v_reg]

m_reg = hddm.HDDMRegressor(mydata, reg_descr, include='z')

m_reg.sample(5000, burn=200, dbname='within_effect.db', db='pickle')

m_reg.save('within_effect')

m_reg.print_stats()
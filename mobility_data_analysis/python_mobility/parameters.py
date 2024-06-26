import pandas as pd
import numpy as np
import os
from pkg_resources import resource_filename

years = ['2018', '2019', '2020', '2021', '2022']
months = ['01', '02', '03', '04', '05',
          '06', '07', '08', '09', '10', '11', '12']

cluster_name = {0: 'Glocery&Pharmacies', 1: 'Retails', 2: 'Arts&Entertainment', 3: 'Restaurants&Bars',
                4: 'Educations', 5: 'Healthcares', 6: 'others'}
# 医疗的流动可视化表现好像不是很好 没有呈现出去大医院的趋势，然后密度好像也不高
# 0和1没有表现出太大的区别，好像1还有点什么问题来着，后来干脆就和在一起了

# cluster_name = {0: 'Retails', 1: 'Arts&Entertainment', 2: 'Restaurants&Bars',
#                 3: 'Educations', 4: 'others'}

# cluster = pd.read_csv('naics_qing_43.csv')
# ym_datesofweek = np.load('ym_datesofweek.npy',allow_pickle=True).item()

# Define the path to the files relative to the package directory
# csv_file_path = resource_filename(__name__, 'naics_qing_43.csv')
csv_file_path = resource_filename(__name__, 'naics_qing_43_7cate.csv')
npy_file_path = resource_filename(__name__, 'ym_datesofweek.npy')

cluster = pd.read_csv(csv_file_path)
ym_datesofweek = np.load(npy_file_path, allow_pickle=True).item()

# cluster_name = {0: 'Retails',
#  1: 'Arts&Entertainment',
#  2: 'Restaurants&Bars',
#  3: 'Educations',
#  4: 'others'}
### 5 clusters
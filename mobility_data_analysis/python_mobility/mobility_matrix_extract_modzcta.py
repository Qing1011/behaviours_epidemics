import pandas as pd
import numpy as np
import os
import csv
import ast
# from mobility_matrix_extract import *
import matplotlib.pyplot as plt
from collections import Counter
import geopandas as gpd
import h5py
from datetime import datetime, timedelta

# df_m['simple'] = df_m['raw_visits_count'] >= df_m['sum_visitors_cbgs']
# the condition to adjust the raw visit counts in each cbgs to match the total raw visit counts
# True  when raw >= sum(visitors in cbgs) there are unknow visitors, you won't know where to add visitor, therefore, use the raw visitor number in each cbg
# copy the cbg_list ---> case 1
# False when raw < sum(visitors in cbgs) cencs tract use 4 to add some noise
# do something: , substract from the raw ---> case 2
# however!!!!!! the way the algorithsm calculate the home cbgs is track where they are during the nighttime
# If visits by home cbg is desired,
# we recommend taking the visitors from each CBG and multiplying by the average
# visits/visitor (i.e., raw_visit_counts / raw_visitor_counts) as an approximation.
# thus, you just need a multiplier to adjust the number of visits from one cbgs


def pre_process_df(df_i, cluster, modzcta, digits):
    """
    pre_process_df

    """
    col_name = 'tract' if digits == 11 else 'tract_10'
    df_i['naics_4'] = df_i['naics_code'].astype(str).str[:4].astype(float)
    cluster['2017 NAICS US Code'] = cluster['2017 NAICS US Code'].astype(float)
    df_i = pd.merge(df_i, cluster, left_on='naics_4',
                    right_on='2017 NAICS US Code', how='left')
    df_ii = df_i[['raw_visit_counts', 'raw_visitor_counts',
                  'visits_by_day', 'poi_cbg', 'visitor_home_cbgs', 'cluster_qing']]
    df_ii = df_ii.dropna(axis=0)
    no_cbg_indx = df_ii[df_ii['visitor_home_cbgs'].str.len() < 3].index
    df_ii = df_ii.drop(no_cbg_indx).reset_index()

    df_ii['poi_tract'] = df_ii['poi_cbg'].astype(str).str[:digits].astype(int)
    df_iii = pd.merge(df_ii, modzcta, left_on='poi_tract',
                      right_on=col_name, how='left')  # this
    # note that the poi cbgs and postal codes do not match, we drop the cbg(fips) can not been found where we select by postal codes very few
    df_iii = df_iii.dropna(axis=0)

    return df_iii


# def remove_CA_non_NY_old(x,digits):
#     # remove_CA_non_NY
#     # count the possible reassign numbers for the later usage
#     # the simple version does not need other veriables, if need complex see the arxive notebook!!
#     # this is complex version and it is no use to count ca number, just remove them!
#     nums = []
#     temp1 = []  # remove CA from the home cbgs
#     num_non_ny = 0
#     for characters in x:
#         if 'CA' not in characters:
#             temp1.append(characters)
# #             num_non_ny = 0
#         else:
#             n_ca_i = int(characters.split(':')[2])
#             num_non_ny = num_non_ny + n_ca_i
#             nums.append(n_ca_i)
#     temp2 = [characters.split(':') for characters in temp1]
#     cbg_list_visitors = []
#     for [c, num] in temp2:
#         if num != '':
#             nums.append(int(num))
#             if int(c[:2]) == 36:
#                 cbg_list_visitors.append((int(c[:digits]), float(num)))
#             else:
#                 num_non_ny = num_non_ny + 1
#     num_reassign = np.sum(np.array(nums) <= 4)
#     return pd.Series([cbg_list_visitors, num_non_ny, num_reassign])

def remove_CA_non_NY(x,digits):
    filtered_no_ca = [item for item in x if 'CA:' not in item]
    filtered_slipt = [cha.split(':') for cha in filtered_no_ca]
    filtered_ca_na = [[int(c[:digits]),int(n)] for c,n in filtered_slipt if n != '' and c[:2] == '36']
    return filtered_ca_na

def account_for_loss_visitors(cbg_list_visitor,raw_visitor):
    sum_known_visitors = sum([ x for [_, x] in cbg_list_visitor])
    unknown_visitors = raw_visitor - sum_known_visitors
    assigned_cbg_list_visitor = [[cbg, no+no*unknown_visitors/sum_known_visitors] for [cbg, no] in cbg_list_visitor]
    return assigned_cbg_list_visitor 

def assign_visits_by_raw_visitors(cbg_list_visitor, multiplier, my_tract):
    """
    Adjusts and filters visitor data based on a multiplier and exclusion list.

    :param cbg_list_visitor: List of tuples with (fips, visitor count)
    :param multiplier: Value to adjust the visitor count
    :param my_modzcta: List of fips values to be excluded
    :return: Adjusted and filtered list of (fips, visitor count)
    """
    # please check!!!,count errors of their codes')###
    # the sum of visitor num in cbgs > the raw numbers,
    # it suppose to be the noise caused by imposing 4,
    # but there is no cbg has number of people == 4 to adjust
    # therefore, adjust to raw visitors
    # ignore the possible non-ny visitors' weights
    # just rescale to raw visitors not do reassign them!!!!!
    # Filter out entries where the FIPS is in my_modzcta
    cbg_list_visitor = [(a, b) for (a, b) in cbg_list_visitor if a in my_tract]
    visitor_record_arr = np.array([b for (a, b) in cbg_list_visitor])
    visitor_fips_arr = np.array([a for (a, b) in cbg_list_visitor])
    cbg_list_visitors_real = list(
        zip(visitor_fips_arr, (visitor_record_arr*multiplier)))
    return cbg_list_visitors_real


def map_modzcta_visitor_list(cbg_visitor_real, my_map):
    """
    one to one mapping of cbg to modzcta
    """
    # using index location of modzcta is much quicker than search
    vis_fips_list = [(my_map.loc[a, 'mod_idx'], b)
                     for (a, b) in cbg_visitor_real]  # m_index
    return vis_fips_list


def distribute_visitor_modzcta(cbg_nums, cbg_to_modid_ratios):
    """
    Distributes numbers across modzcta/zip codes according to specified ratios.

    Args:
    - cbg_nums (list of tuples): Each tuple contains (cbg, num)
    - cbg_to_zip_ratios (dict): A dictionary where keys are cbg and values are lists of tuples (zip, ratio)

    Returns:
    - list of tuples: Each tuple contains (modzcta_idx, distributed_num)
    """
    result = []
    # Process each CBG and its corresponding visitor count
    for (cbg, num) in cbg_nums:
        # Get the modzcta distribution ratios for this CBG
        df_cbg_ratios = cbg_to_modid_ratios.loc[cbg].copy()
        # Calculate distributed numbers as a new column
        df_cbg_ratios['distributed_num'] = df_cbg_ratios['res_ratio'] * num
        # Add results to the final list
        result_tuples = list(zip(df_cbg_ratios['mod_idx'], df_cbg_ratios['distributed_num']))
        result.extend(result_tuples)
    return result


def find_fips_dayvisits(fips_visitor_list, visits_by_day_list, raw_visits_count):
    # find [fips, the visits by day (a list of j:list of 7 length)]
    vis_daily_matrix = []  # a list of j:list of 7 length,
    for (f, n) in fips_visitor_list:
        vis_daily_j = np.array(visits_by_day_list)/raw_visits_count*n
        # every single visitor number in each cbg distributes to the 7 days
        vis_daily_matrix.append((f, vis_daily_j))
    return vis_daily_matrix


def mobility_extract_per_poi(df_j, M_raw, dates_idx):
    cluster_idx = df_j['cluster_idx'].values[0]
    i = df_j['i'].values[0]
    cbg_list_visitors_real = df_j['vis_daily_matrix'].values[0]
    for (j, vis_daily_a) in cbg_list_visitors_real:
        #         if np.sum(np.array(vis_daily_a) < 0):
        #             print(j_data_idx, 'has negative values')
        # else:
        j = int(j)
        M_raw[dates_idx, cluster_idx, i, j] = vis_daily_a + \
            M_raw[dates_idx, cluster_idx, i, j]
    return M_raw


def store_data_to_hdf5(M_raw, dates, y, m, num_cate, num_mod, filepath='../Data/mobility/M_raw_{}{}.h5'):
    """
    Store high dimensional time series data into an HDF5 file.

    Parameters:
    - M_raw: The data to be stored.
    - dates: List of date strings.
    - y: Year as a string.
    - m: Month as a string.
    - num_cate: Number of categories.
    - num_mod: Number of modules.
    - filepath: The path to the HDF5 file, with placeholders for year and month.

    Returns:
    None
    """
    num_days = len(dates)
    with h5py.File(filepath.format(y, m), 'w') as f:
        for date_idx in range(num_days):
            day_data = M_raw[date_idx, :, :, :]
            date = dates[date_idx]
            dset = f.create_dataset(
                date, (num_cate, num_mod, num_mod), dtype='float64')
            dset[:] = day_data


def read_hdf5_file(filepath):
    """
    Read all datasets from an HDF5 file.

    Parameters:
    - filepath: The path to the HDF5 file.

    Returns:
    - data_dict: Dictionary containing datasets with dataset names as keys.
    """
    data_dict = {}

    with h5py.File(filepath, 'r') as f:
        for key in f.keys():
            data_dict[key] = f[key][:]  # Reading the entire dataset

    return data_dict

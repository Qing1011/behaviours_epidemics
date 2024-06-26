import pandas as pd
import numpy as np
import os
import csv
import ast
import matplotlib.pyplot as plt
from collections import Counter
import geopandas as gpd
import h5py
from datetime import datetime, timedelta
import sys


def get_weekday(date_str, date_format='%Y-%m-%d'):
    # Convert string to datetime object
    date_obj = datetime.strptime(date_str, date_format)
    
    # Get the day of the week as an integer (Monday=0, Sunday=6)
    day_int = date_obj.weekday()
    
    # Map the integer to its corresponding day name
    days = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
    return days[day_int]


def h5py_to_4d_array(file_path):
    """
    Convert datasets from an h5py file to a 4D numpy array.

    Parameters:
    - file_path: str, path to the h5py file.

    Returns:
    - 4D numpy array with shape (number_of_dates, d1, d2, d3)
    """
    with h5py.File(file_path, 'r') as f:
        # Get all dataset names (dates) in the file
        dataset_names = list(f.keys())
        
        # Load the datasets into a list of arrays
        data_arrays = [f[date][:] for date in dataset_names]
        
        # Stack the datasets along a new axis to form a 4D numpy array
        combined_array = np.stack(data_arrays, axis=0)
    
    return combined_array


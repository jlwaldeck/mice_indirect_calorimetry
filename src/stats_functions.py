import pandas as pd
import numpy as np

def se(x):
    """
    Calculate the standard error of the mean for a given array or list, ignoring NaN values.
    Args:
        x (array-like): Input data.
    Returns:
        float: Standard error of the mean.
    """
    x = np.array(x)
    x = x[~np.isnan(x)]  # Exclude NaN values
    return np.std(x, ddof=1) / np.sqrt(len(x))


def data_summary(data, varname, groupnames):
    """
    Summarize data by calculating the mean and standard error for a given variable, grouped by specified columns.

    Args:
        data (pd.DataFrame): The input DataFrame.
        varname (str): The column name of the variable to summarize.
        groupnames (list): List of column names to group by.

    Returns:
        pd.DataFrame: A DataFrame with the grouped mean and standard error.
    """
    # Group by the specified columns and calculate mean and standard error
    summary = data.groupby(groupnames).agg(
        mean=(varname, 'mean'),
        se=(varname, lambda x: se(x))
    ).reset_index()

    # Rename the 'mean' column to the variable name
    summary = summary.rename(columns={'mean': varname})

    return summary
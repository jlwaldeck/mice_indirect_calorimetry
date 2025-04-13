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
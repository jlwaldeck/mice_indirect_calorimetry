import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

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


def data_summary_line_plot(data, x_var, y_var, group, output_file, title):
    """
    Create a line plot with error bars for summarized data.

    Args:
        data (pd.DataFrame): The summarized data with columns [x_var, y_var, group, 'se'].
        x_var (str): The column name for the x-axis variable.
        y_var (str): The column name for the y-axis variable.
        group (str): The column name for the grouping variable (e.g., 'Genotype').
        output_file (str): Path to save the plot as a PDF.
        title (str): Title of the plot.
    """
    # Set Seaborn theme
    sns.set_theme(style="whitegrid")

    # Create the plot
    plt.figure(figsize=(7, 5))
    ax = sns.lineplot(
        data=data,
        x=x_var,  # X-axis
        y=y_var,  # Y-axis
        hue=group,  # Color by grouping variable
        style=group,  # Line style by grouping variable
        markers=True,
        dashes=True,
        errorbar=None  # Disable Seaborn's built-in error bars
    )

    # Add error bars manually
    for group_value in data[group].unique():
        subset = data[data[group] == group_value]
        plt.errorbar(
            subset[x_var],  # X values
            subset[y_var],  # Y values
            yerr=subset["se"],  # Error values
            fmt='none',  # No marker
            capsize=3,  # Error bar cap size
            color=sns.color_palette()[list(data[group].unique()).index(group_value)]
        )

    # Add title and labels
    plt.title(title, fontsize=14)
    plt.xlabel(x_var, fontsize=12)
    plt.ylabel(y_var, fontsize=12)
    plt.legend(title=group, fontsize=10)

    # Save the plot to a PDF
    plt.tight_layout()
    plt.savefig(output_file)
    plt.close()


def process_data_summaries(df, data_summay_config):
    """
    Process summary tasks defined in the configuration.

    Args:
        df (pd.DataFrame): The transformed DataFrame to process.
        data_summay_config (list): List of summary tasks from the configuration.
    """
    # Iterate over the tasks in the configuration
    for summary in data_summay_config:
        # Extract task parameters
        varname = summary['varname']
        groupnames = summary['groupnames']
        x_var = summary['x_var']
        y_var = summary['y_var']
        group = summary['group']
        output_file = summary['output_file']
        title = summary['title']

        # Perform data summary
        summary = data_summary(df, varname=varname, groupnames=groupnames)

        # Generate the plot
        data_summary_line_plot(
            data=summary,
            x_var=x_var,
            y_var=y_var,
            group=group,
            output_file=output_file,
            title=title
        )
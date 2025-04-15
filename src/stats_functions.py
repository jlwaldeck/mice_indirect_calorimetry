import os
import yaml
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the general configuration from YAML
script_dir = os.path.dirname(__file__)
gen_config_file_path = os.path.join(script_dir, '../config/general_config.yaml')
with open(gen_config_file_path, 'r') as file:
    config = yaml.safe_load(file)
general_config = config.get('general_settings')

r_path = general_config.get('R_path')
os.environ["R_HOME"] = r_path
# TODO: Do I need to clear the R global variables here?



from rpy2.robjects import pandas2ri, Formula, r
from rpy2.robjects.packages import importr

pandas2ri.activate()

# Import R libraries
stats = importr('stats')
lme4 = importr('lme4')
emmeans = importr('emmeans')

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


def generate_zt_column(df, time_variable, group_variable):
    """
    Generate the 'zt' column based on the time variable and group variable.

    Args:
        df (pd.DataFrame): The DataFrame to modify.
        time_variable (str): The column representing the time variable (e.g., 'one_hour').
        group_variable (str): The column representing the grouping variable (e.g., 'Genotype').

    Returns:
        list: A list of values for the 'zt' column.
    """
    # Determine the range of time values (e.g., 1 through 24 for a 24-hour day)
    time_range = range(1, len(df[time_variable].unique()) + 1)

    # Determine the number of unique groups (e.g., number of unique Genotypes)
    num_groups = len(df[group_variable].unique())

    # Repeat each time value for the number of unique groups
    zt_column = [zt for zt in time_range for _ in range(num_groups)]

    # Ensure the length matches the DataFrame
    if len(zt_column) != len(df):
        raise ValueError("Generated 'zt' column length does not match the DataFrame length.")

    return zt_column


def map_numeric_to_original_labels(df, mapping_df, columns):
    """
    Map numeric levels in a DataFrame back to their original categorical labels.

    Args:
        df (pd.DataFrame): The DataFrame containing numeric levels to be mapped.
        mapping_df (pd.DataFrame): The DataFrame containing the original categorical columns.
        columns (list): List of column names to map.

    Returns:
        pd.DataFrame: The updated DataFrame with mapped categorical labels.
    """
    for column in columns:
        # Create a mapping of numeric levels to original labels
        mapping = dict(enumerate(mapping_df[column].astype("category").cat.categories, start=1))
        # Map numeric levels back to original labels
        df[column] = df[column].map(mapping)
    return df


def fit_lmer_model(df, formula):
    """
    Fit a linear mixed-effects model using lmer.
    """
    r_df = pandas2ri.py2rpy(df)
    lmer_formula = Formula(formula)
    lmer_model = lme4.lmer(lmer_formula, data=r_df, REML=False)
    return lmer_model

def perform_anova(lmer_model):
    """
    Perform ANOVA on the fitted lmer model.
    """
    anova_results = stats.anova(lmer_model)
    anova_df = pandas2ri.rpy2py(r['as.data.frame'](anova_results))
    return anova_df

def calculate_emmeans(lmer_model, formula, adjust_method):
    """
    Calculate Estimated Marginal Means (EMMs) with pairwise contrasts.
    """
    # Set emm_options to disable sorting
    r('emmeans::emm_options(sort = FALSE)')
    
    emm_formula = Formula(formula)
    emmeans_results = emmeans.emmeans(lmer_model, specs=emm_formula, adjust=adjust_method)
    intrxn_emmeans_r = emmeans_results.rx2('emmeans')
    print(intrxn_emmeans_r)
    intrxn_emmeans_r_df = r['as.data.frame'](intrxn_emmeans_r)
    
    print(intrxn_emmeans_r_df)
    intrxn_emmeans_df = pandas2ri.rpy2py(intrxn_emmeans_r_df)
    print(intrxn_emmeans_df)
    intrxn_emmeans_df.to_csv("./output/emms_test_output.csv")
    contrasts_r = emmeans_results.rx2('contrasts')
    contrasts_df = pandas2ri.rpy2py(r['as.data.frame'](contrasts_r))
    return intrxn_emmeans_df, contrasts_df


def sort_and_generate_zt_column(df, time_variable, group_variable):
    """
    Sort the DataFrame and generate the 'zt' column using the existing generate_zt_column function.
    Handles ranges in the time variable (e.g., "ZT0_ZT2", "ZT2_ZT4").

    Args:
        df (pd.DataFrame): The DataFrame to process.
        time_variable (str): The column representing the time variable (e.g., 'one_hour').
        group_variable (str): The column representing the grouping variable (e.g., 'Genotype').

    Returns:
        pd.DataFrame: The updated DataFrame with sorted values and a 'zt' column.
    """
    # Extract the start and end of the range from the time variable
    df["time_var_start"] = df[time_variable].str.extract(r'(\d+)_?')[0].astype(int)
    df["time_var_end"] = df[time_variable].str.extract(r'_(\d+)$')[0].fillna(df["time_var_start"]).astype(int)

    # Sort by the start of the range, then by the group variable
    df = df.sort_values(by=["time_var_start", group_variable])

    # Generate the 'zt' column using the existing function
    df["zt"] = generate_zt_column(df, time_variable, group_variable)

    # Drop temporary columns used for sorting
    df = df.drop(columns=["time_var_start", "time_var_end"])

    return df

def write_results_to_csv(anova_df, contrasts_df, output_file):
    """
    Write ANOVA results and contrasts to a CSV file.
    """
    with open(output_file, 'w') as f:
        f.write("ANOVA Results\n")
        anova_df.to_csv(f, index=False)
    with open(output_file, 'a') as f:
        f.write("\nPairwise Contrasts\n")
        contrasts_df.to_csv(f, index=False)

def plot_results(df, output_file, title):
    """
    Plot the results using seaborn and matplotlib.

    Args:
        df (pd.DataFrame): The DataFrame containing the data to plot.
        output_file (str): The file path to save the plot.
        title (str): The title of the plot.
    """
    plt.figure(figsize=(7, 5))
    sns.lineplot(
        data=df,
        x="zt",
        y="emmean",
        hue="Genotype",
        style="Genotype",
        markers=True,
        dashes=True,
        ci=None
    )
    for var in df["Genotype"].unique():
        subset = df[df["Genotype"] == var]
        plt.errorbar(
            subset["zt"],
            subset["emmean"],
            yerr=subset["SE"],
            fmt='none',
            capsize=3,
            label=None
        )
    plt.title(title)
    plt.xlabel("ZT")
    plt.ylabel("Estimated Marginal Means")
    plt.legend(title="Genotype")
    plt.tight_layout()
    plt.savefig(output_file)
    plt.show()

def process_anova_and_plot(df, anova_contrast_config):
    """
    Process multiple ANOVA and contrast sections, calculate contrasts, and plot results based on the configuration.

    Args:
        df (pd.DataFrame): The input DataFrame.
        anova_contrast_config (dict): The configuration dictionary containing multiple sections for ANOVA and contrasts.
    """
    # Iterate over each section in the anova_contrast configuration
    for section_name, config in anova_contrast_config.items():
        print(f"Processing section: {section_name}")

        # Fit the model
        lmer_model = fit_lmer_model(df, config["lmer_formula"])

        # Perform ANOVA
        anova_df = perform_anova(lmer_model)

        # Calculate EMMs and contrasts
        intrxn_emmeans_df, contrasts_df = calculate_emmeans(
            lmer_model,
            formula=config["emm_formula"],
            adjust_method=config["adjust_method"]
        )

        # Map numeric levels back to original labels
        intrxn_emmeans_df = map_numeric_to_original_labels(
            df=intrxn_emmeans_df,
            mapping_df=df,
            columns=config["columns_to_map"]
        )

        intrxn_emmeans_df.to_csv("./output/emms_test_post_map.csv")

        # Sort and generate 'zt' column
        intrxn_emmeans_df = sort_and_generate_zt_column(
            df=intrxn_emmeans_df,
            time_variable=config["time_variable"],
            group_variable=config["group_variable"]
        )

        intrxn_emmeans_df.to_csv("./output/emms_test_post_sort.csv")

        # Write results to CSV
        write_results_to_csv(anova_df, contrasts_df, config["output_file"])

        # Plot results with the specified title
        plot_results(intrxn_emmeans_df, config["plot_output_file"], config["plot_title"])
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

# Set R_HOME to enable interaction with R in Python
r_path = general_config.get('R_path')
os.environ["R_HOME"] = r_path
# TODO: Perform equivalent of rm(list = ls()) here?

# Import R libraries using rpy2
from rpy2.robjects import pandas2ri, Formula, r
from rpy2.robjects.packages import importr

# Enable automatic conversion between R objects and Python objects
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
    # Set seaborn theme
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
    # Iterate over the tasks specified in user config
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

    The 'zt' column is a numeric representation of time intervals (e.g., ZT0, ZT2, ZT4) 
    that is required for consistent plotting of time-series data. This column ensures 
    that the data is properly aligned and grouped by both time and the specified grouping 
    variable (e.g., 'Genotype') when creating line plots or other visualizations.

    Args:
        df (pd.DataFrame): The DataFrame to modify.
        time_variable (str): The column representing the time variable (e.g., 'one_hour' or 'ZT0_ZT2').
                             This column should contain unique time intervals or ranges.
        group_variable (str): The column representing the grouping variable (e.g., 'Genotype').
                              This column is used to group the data for each time interval.

    Returns:
        list: A list of numeric values for the 'zt' column, where each unique time interval 
              is repeated for each unique group in the grouping variable.

    Raises:
        ValueError: If the generated 'zt' column length does not match the length of the DataFrame.

    Why the 'zt' Column is Needed:
        - Time-series plots often require a numeric representation of time intervals to ensure 
          proper alignment on the x-axis.
        - The 'zt' column provides a consistent numeric mapping for time intervals, even if the 
          original time variable contains non-numeric labels (e.g., 'ZT0', 'ZT2').
        - By repeating each time interval for all unique groups, the 'zt' column ensures that 
          the data is structured correctly for grouped plotting (e.g., plotting separate lines 
          for each 'Genotype' over time).

    Example:
        Suppose the DataFrame contains the following columns:
            time_variable: ['ZT0', 'ZT2', 'ZT4']
            group_variable: ['A', 'B']
        The generated 'zt' column will look like:
            [1, 1, 2, 2, 3, 3]
        This ensures that each time interval ('ZT0', 'ZT2', 'ZT4') is repeated for each group ('A', 'B').

    Workflow:
        1. Determine the range of unique time intervals in the time variable.
        2. Determine the number of unique groups in the grouping variable.
        3. Generate a numeric sequence for the time intervals, repeated for each group.
        4. Validate that the generated 'zt' column matches the length of the DataFrame.

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
    # Convert the input pandas DataFrame to an R dataframe
    r_df = pandas2ri.py2rpy(df)

    # Define the formula for the lmer model
    lmer_formula = Formula(formula)

    # Fit the lmer model, using REML = False for maximum likelihood estimation
    # TODO: Add REML as a parameter to the function, configure it via YAML
    lmer_model = lme4.lmer(lmer_formula, data=r_df, REML=False)

    return lmer_model

def calculate_anova(lmer_model):
    """
    Calculate ANOVA on the fitted lmer model.
    Stores the results in a pandas DataFrame.
    """
    anova_results = stats.anova(lmer_model)
    anova_df = pandas2ri.rpy2py(r['as.data.frame'](anova_results))

    return anova_df


def get_categorical_columns_and_levels(r_df):
    """
    Identify categorical columns in an R dataframe and retrieve their levels.

    Args:
        r_df: The R dataframe.

    Returns:
        tuple: A tuple containing:
            - A list of unique categorical column names as Python strings.
            - A dictionary mapping each categorical column to its levels.
    """
    # Identify all categorical (factor) columns in the R dataframe
    categorical_columns = list(r['names'](r_df)[r['sapply'](r_df, r['is.factor'])])

    # Convert column names to native Python strings and remove duplicates
    categorical_columns = list(set([str(col) for col in categorical_columns]))

    # Create a dictionary to store the levels for each categorical column
    column_levels = {col: list(r['levels'](r_df.rx2(col))) for col in categorical_columns}

    return categorical_columns, column_levels


def map_numeric_levels_to_labels(df, categorical_columns, column_levels):
    """
    Map numeric levels in a DataFrame back to their original categorical labels.

    Args:
        df (pd.DataFrame): The DataFrame containing numeric levels to be mapped.
        categorical_columns (list): List of categorical column names.
        column_levels (dict): Dictionary mapping each categorical column to its levels.

    Returns:
        pd.DataFrame: The updated DataFrame with mapped categorical labels.
    """
    for col in categorical_columns:
        df[col] = df[col].map(
            lambda x: column_levels[col][int(x) - 1] if isinstance(x, (int, np.integer)) else column_levels[col][column_levels[col].index(x)]
        )
    return df


def calculate_emmeans(lmer_model, formula, adjust_method):
    """
    Calculate Estimated Marginal Means (EMMs) and pairwise contrasts from a linear mixed-effects model.

    This function computes the Estimated Marginal Means (EMMs) for a given linear mixed-effects model 
    using the specified formula and adjustment method. It also calculates pairwise contrasts for the EMMs 
    and returns both the EMMs and contrasts as pandas DataFrames.

    Args:
        lmer_model (rpy2.robjects.methods.RS4): The fitted linear mixed-effects model (from lme4::lmer in R).
        formula (str): The formula specifying the terms for which EMMs should be calculated (e.g., "Genotype").
        adjust_method (str): The method for p-value adjustment in pairwise contrasts (e.g., "tukey", "bonferroni").

    Returns:
        tuple:
            - intrxn_emmeans_df (pd.DataFrame): A pandas DataFrame containing the EMMs with categorical labels restored.
              Columns typically include the factors specified in the formula, the estimated means (`emmean`), 
              and standard errors (`SE`).
            - contrasts_df (pd.DataFrame): A pandas DataFrame containing the pairwise contrasts for the EMMs, 
              including the contrast estimates, confidence intervals, and adjusted p-values.

    Workflow:
        1. The function uses the `emmeans` R package to calculate EMMs and pairwise contrasts for the given model.
        2. The EMMs are extracted and converted from an R dataframe to a pandas DataFrame.
        3. Categorical columns in the EMMs are identified, and their numeric levels are mapped back to their original labels.
            Note: This is done using the `get_categorical_columns_and_levels` and `map_numeric_levels_to_labels` functions.
            Note: This is not ideal, but necessary due to differences between R and Python interoperability.
        4. Pairwise contrasts are extracted and converted to a pandas DataFrame.

    Raises:
        ValueError: If the `emmeans` function returns NULL for the EMMs, indicating an issue with the model or formula.
    """
    emm_formula = Formula(formula)
    emmeans_results = emmeans.emmeans(lmer_model, specs=emm_formula, adjust=adjust_method)

    # Extract the EMMs
    intrxn_emmeans_r = emmeans_results.rx2('emmeans')

    # Check if intrxn_emmeans_r is NULL
    if intrxn_emmeans_r is None:
        print("Error: 'emmeans' returned NULL.")
        return None, None

    # Convert the EMMs to a DataFrame
    intrxn_emmeans_r_df = r['as.data.frame'](intrxn_emmeans_r)

    # Get categorical columns and their levels
    categorical_columns, column_levels = get_categorical_columns_and_levels(intrxn_emmeans_r_df)

    # Convert the R dataframe to a pandas DataFrame
    intrxn_emmeans_df = pandas2ri.rpy2py(intrxn_emmeans_r_df)

    # Map numeric levels back to original labels for all categorical columns
    intrxn_emmeans_df = map_numeric_levels_to_labels(intrxn_emmeans_df, categorical_columns, column_levels)

    # Calculate pairwise contrasts and convert to pandas DataFrame
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
        errorbar=None
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


def analyze_and_plot(df, anova_contrast_config):
    """
    Perform ANOVA, calculate EMMS, and generate plots based on the provided configuration.

    This function processes section(s) in config file. For each section, 
    it fits a linear mixed-effects model, performs ANOVA, calculates Estimated Marginal Means (EMMs) 
    and pairwise contrasts, and generates time-series plots with error bars. The results are saved to 
    CSV files and plots are exported as PDFs.

    Args:
        df (pd.DataFrame): The input DataFrame containing the data to analyze.
        anova_contrast_config (dict): A dictionary containing configuration for multiple sections. 
            Each section specifies:
                - "lmer_formula" (str): The formula for fitting the linear mixed-effects model.
                - "emm_formula" (str): The formula for calculating EMMs.
                - "adjust_method" (str): The method for p-value adjustment in pairwise contrasts 
                  (e.g., "tukey", "bonferroni").
                - "time_variable" (str): The column representing the time variable (e.g., 'one_hour').
                - "group_variable" (str): The column representing the grouping variable (e.g., 'Genotype').
                - "output_file" (str): The file path to save ANOVA and contrast results as a CSV.
                - "plot_output_file" (str): The file path to save the generated plot as a PDF.
                - "plot_title" (str): The title for the generated plot.

    Workflow:
        1. Iterate over each section in the configuration.
        2. Fit a linear mixed-effects model using the specified formula.
        3. Perform ANOVA on the fitted model and save the results.
        4. Calculate EMMs and pairwise contrasts
        5. Sort the EMMs DataFrame and generate a numeric 'zt' column for time-series plotting.
        6. Save the ANOVA and contrast results to a CSV file.
        7. Generate and save a time-series plot with error bars.

    Outputs:
        - CSV files containing ANOVA results and pairwise contrasts for each section.
        - PDF files containing time-series plots for each section.

    Raises:
        ValueError: If the generated 'zt' column length does not match the DataFrame length.

    Notes:
        - This function relies on several helper functions, including:
            - `fit_lmer_model`: Fits the linear mixed-effects model.
            - `calculate_anova`: Performs ANOVA on the fitted model.
            - `calculate_emmeans`: Calculates EMMs and pairwise contrasts.
            - `sort_and_generate_zt_column`: Sorts the DataFrame and generates the 'zt' column.
            - `write_results_to_csv`: Saves results to a CSV file.
            - `plot_results`: Generates and saves the time-series plot.
        - The 'zt' column is essential for aligning time intervals on the x-axis in time-series plots.
    """
    # Iterate over each section in the anova_contrast configuration
    for section_name, config in anova_contrast_config.items():
        print(f"Processing section: {section_name}")

        # Fit the model
        lmer_model = fit_lmer_model(df, config["lmer_formula"])

        # Calculate ANOVA
        anova_df = calculate_anova(lmer_model)

        # Calculate EMMs and contrasts
        intrxn_emmeans_df, contrasts_df = calculate_emmeans(
            lmer_model,
            formula=config["emm_formula"],
            adjust_method=config["adjust_method"]
        )

        # Determin plot type
        if config["plot_type"] == "line":
            # Sort and generate 'zt' column
            intrxn_emmeans_df = sort_and_generate_zt_column(
                df=intrxn_emmeans_df,
                time_variable=config["time_variable"],
                group_variable=config["group_variable"]
        )
        else:
            print("plot_type = " + config["plot_type"] + " not supported. Skipping plot generation.")
            continue
        
        # Write ANOVA and contrast results to CSV
        write_results_to_csv(anova_df, contrasts_df, config["output_file"])

        # Plot results with the specified title
        plot_results(intrxn_emmeans_df, config["plot_output_file"], config["plot_title"])
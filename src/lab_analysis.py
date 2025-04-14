import os
import pandas as pd
import yaml
from data_processing import load_data, transform_via_map, apply_grouping_operations, apply_filters
from stats_functions import data_summary
import matplotlib.pyplot as plt
import seaborn as sns

# Dynamically determine the path to this script
script_dir = os.path.dirname(__file__)

gen_config_file_path = os.path.join(script_dir, '../config/general_config.yaml')
col_value_map_file_path = os.path.join(script_dir, '../config/column_mapping.yaml')
col_group_agg_file_path = os.path.join(script_dir, '../config/group_aggregation.yaml')

# Load the column mapping configuration from YAML
with open(col_value_map_file_path, 'r') as file:
    config = yaml.safe_load(file)
column_mappings = config.get('column_value_mappings', {})

# Load the grouping/aggregation configuration from YAML
with open(col_group_agg_file_path, 'r') as file:
    config = yaml.safe_load(file)
grouping_config = config.get('column_group_agg_ops', {})

# Load the general configuration from YAML
with open(gen_config_file_path, 'r') as file:
    config = yaml.safe_load(file)
general_config = config.get('general_settings')
data_filters = config.get('data_filters', {})

# Retrieve the raw_data_path from the general configuration
raw_data_path = general_config.get('raw_data_path')
output_path = general_config.get('output_path')


def plot_food_intake(data, output_file, title):
    """
    Create a line plot with error bars for food intake data.

    Args:
        data (pd.DataFrame): The summarized data with columns ['Cycle', 'Genotype', 'UptakeA_Sum', 'se'].
        output_file (str): Path to save the plot as a PDF.
        title (str): Title of the plot.
    """
    # Set Seaborn style
    sns.set_theme(style="whitegrid")

    # Create the plot
    plt.figure(figsize=(7, 5))
    ax = sns.lineplot(
        data=data,
        x="Cycle",  # X-axis
        y="UptakeA_Sum",  # Y-axis
        hue="Genotype",  # Color by Genotype
        style="Genotype",  # Line style by Genotype
        markers=True,
        dashes=True,
        errorbar=None  # Disable Seaborn's built-in CI
    )

    # Add error bars manually
    for genotype in data["Genotype"].unique():
        subset = data[data["Genotype"] == genotype]
        plt.errorbar(
            subset["Cycle"],  # X values
            subset["UptakeA_Sum"],  # Y values
            yerr=subset["se"],  # Error values
            fmt='none',  # No marker
            capsize=3,  # Error bar cap size
            color=sns.color_palette()[list(data["Genotype"].unique()).index(genotype)]
        )

    # Add title and labels
    plt.title(title, fontsize=14)
    plt.xlabel("Cycle", fontsize=12)
    plt.ylabel("UptakeA_Sum", fontsize=12)
    plt.legend(title="Genotype", fontsize=10)

    # Save the plot to a PDF
    plt.tight_layout()
    plt.savefig(output_file)
    plt.close()


def main():

    raw_data = load_data(raw_data_path)  # Load the raw data using path in general_config
    df = apply_filters(raw_data, data_filters)  # Apply filters listed in general_config
    df = transform_via_map(df, column_mappings)  # Apply mappings listed in column_mapping config
    df = apply_grouping_operations(df, grouping_config)  # Apply grouping and aggregation operations listed in group_aggregation config

    # TODO: Replace with logger statements and/or error handling
    if df is not None:
        print(df.head())
    else:
        print("No data to display after transformation.")
        return

    # Call the data_summary function
    summary = data_summary(df, varname='UptakeA_Sum', groupnames=['Cycle', 'Genotype'])

    # Call the plot_food_intake function
    plot_food_intake(
        data=summary,
        output_file=os.path.join(output_path, "202305_DATBFX_Females_LD_Sable_RC_AL_FoodIntake_30mins_Line_Plot.pdf"),
        title="Food Intake (30 mins)"
    )


if __name__ == "__main__":
    main()
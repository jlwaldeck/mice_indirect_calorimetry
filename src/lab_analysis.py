import os
import pandas as pd
import yaml
from data_processing import load_data, transform_via_map, apply_grouping_operations, apply_filters
from stats_functions import process_data_summaries, process_anova_and_plot

# Dynamically determine the path to this script
script_dir = os.path.dirname(__file__)

gen_config_file_path = os.path.join(script_dir, '../config/general_config.yaml')
col_value_map_file_path = os.path.join(script_dir, '../config/column_mapping.yaml')
col_group_agg_file_path = os.path.join(script_dir, '../config/group_aggregation.yaml')
data_summary_config_path = os.path.join(script_dir, '../config/data_summary.yaml')
anova_contrast_config_path = os.path.join(script_dir, '../config/anova_contrast_config.yaml')

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

# Load the data_summary.yaml configuration
with open(data_summary_config_path, 'r') as file:
    config = yaml.safe_load(file)
data_summary_config = config.get('data_summaries', [])

# Load the anova_contrast_config.yaml configuration
with open(anova_contrast_config_path, "r") as file:
    config = yaml.safe_load(file)
anova_contrast_config = config.get('anova_contrast', [])


def main():
    # Load the raw data
    raw_data_path = general_config.get('raw_data_path')
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

    # Process summary tasks from the loaded configuration
    process_data_summaries(df, data_summary_config)

    # Build lmer, calculate ANOVA, EMM and pairwise contrasts, plot results
    process_anova_and_plot(df, anova_contrast_config)


if __name__ == "__main__":
    main()
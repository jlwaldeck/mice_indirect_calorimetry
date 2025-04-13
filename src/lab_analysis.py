import os
import pandas as pd
import yaml
from data_processing import load_data, transform_via_map, apply_grouping_operations, apply_filters
from stats_functions import data_summary

# Dynamically determine the path to this script
script_dir = os.path.dirname(__file__)

gen_config_file_path = os.path.join(script_dir, '../config/general_config.yaml')
col_value_map_file_path = os.path.join(script_dir, '../config/column_mapping.yaml')
col_group_agg_file_path = os.path.join(script_dir, '../config/group_aggregation.yaml')

# Load the columm mapping configuration from YAML
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


def main():

    raw_data = load_data(raw_data_path)  # Load the raw data using path in general_config
    df = apply_filters(raw_data, data_filters) # Apply filters listed in gerneral_config
    df = transform_via_map(df, column_mappings)  # Apply mappings listed in column_mapping config
    df = apply_grouping_operations(df, grouping_config) # Apply grouping and aggregation operations listed in group_aggregation config
    
    # TODO: Replace with logger statements and/or error handling
    if df is not None:
        print(df.head())
    else:
        print("No data to display after transformation.")

    # Call the function
    summary = data_summary(df, varname='UptakeA_Sum', groupnames=['Cycle', 'Genotype'])
    print(summary.iloc[0:50])

if __name__ == "__main__":
    main()
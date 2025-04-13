import os
import pandas as pd
import yaml
from data_processing import load_data, transform_via_map

# TODO: Move this path to a config file or environment variable
raw_data_path = ('C:/Users/nwald/OneDrive/Desktop/202305_Sable_DATBFX_Females_n16_AL_RC/202305_DATBFX_Females_RC_AL_Sable_n16.csv')

# Dynamically determine the path to the YAML file
script_dir = os.path.dirname(__file__)
config_file_path = os.path.join(script_dir, 'config.yaml')

# Load the columm mapping configuration from YAML
with open(config_file_path, 'r') as file:
    config = yaml.safe_load(file)
column_mappings = config['column_mappings']


def main():

    raw_data = load_data(raw_data_path)  # Load the raw data
    df = transform_via_map(raw_data, column_mappings)  # Apply mappings based on config
    
    # TODO: Replace with logger statements and/or assertions as needed
    if df is not None:
        print(df.tail())
        print(df['four_hours'].unique())
        print(df['two_hours'].unique())
        print(df['one_hour'].unique())
        print(df['day_night'].unique())
    else:
        print("No data to display after transformation.")

if __name__ == "__main__":
    main()
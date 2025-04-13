import pandas as pd

def load_data(file_path):
    """Load data from a CSV file."""
    try:
        data = pd.read_csv(file_path)
        print(f"Data loaded successfully from {file_path}")
        print(f"Data shape: {data.shape}")
        return data
    except Exception as e:
        print(f"Error loading data: {e}")
        return None


def apply_mapping(df, column_config):
    """Apply mapping and default value to create a new column based on an existing column.
    Args:
        df (pd.DataFrame): The DataFrame to transform.
        column_config (dict): Configuration for the column including source, target, mapping, and default value.
    Returns:
        pd.DataFrame: The transformed DataFrame.
    Notes:
        This function creates a new column based on the mapping of an existing column.
    """

    source_column = column_config.get('source')
    target_column = column_config.get('target')
    mapping = column_config.get('mapping', {})
    print(mapping)
    default_value = column_config.get('default', None)
    dtype = column_config.get('dtype', None)  # Get the desired data type

    # Apply the specified data type to the new column if provided
    if dtype:
        print(dtype)
        df[source_column] = df[source_column].astype(dtype)

    if not source_column or not target_column:
        raise ValueError("Both 'source' and 'target' must be specified in the column configuration.")

    # Create the new column based on the mapping with the default value
    df[target_column] = df[source_column].map(mapping).fillna(default_value)

    return df


def transform_via_map(df, column_mappings):
    """Transform columns based on mappings in the config."""
    for column_name, column_config in column_mappings.items():
        df = apply_mapping(df, column_config)
    return df
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


def apply_mapping(df, column_name, column_config):
    """Apply mapping and default value to a specific column.
    Args:
        df (pd.DataFrame): The DataFrame to transform.
        column_name (str): The name of the column to transform.
        column_config (dict): Configuration for the column including mapping and default value.
    Returns:
        pd.DataFrame: The transformed DataFrame.
    Notes:
        This function also allows for changing the data type of the column.
        If config contains explict dtype, it will convert the column to that type.
        Otherwise, it will keep the original data type inferred by pandas when loading csv.
    """

    mapping = column_config.get('mapping', {})
    default_value = column_config.get('default', None)
    dtype = column_config.get('dtype', None)  # Get the desired data type

    # Apply the specified data type if provided
    if dtype:
        df[column_name] = df[column_name].astype(dtype)

    # Apply the mapping with the default value
    df[column_name] = df[column_name].map(mapping).fillna(default_value)

    return df

def transform_via_map(df, column_mappings):
    """Transform columns based on mappings in the config."""
    for column_name, column_config in column_mappings.items():
        if column_name in df.columns:
            df = apply_mapping(df, column_name, column_config)
    return df
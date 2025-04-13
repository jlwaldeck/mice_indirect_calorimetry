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
    
def apply_filters(df, filters_config):
    """
    Apply filters to a DataFrame based on the configuration.
    Args:
        df (pd.DataFrame): The DataFrame to filter.
        filters_config (dict): Configuration for filtering.
    Returns:
        pd.DataFrame: The filtered DataFrame.
    """
    for column_name, filter_values in filters_config.items():
        if column_name in df.columns:
            # Normalize the column and filter values
            df[column_name] = df[column_name].astype(str).str.strip()
            filter_values = [str(value).strip() for value in filter_values]
            
            # Apply the filter
            df = df[~df[column_name].isin(filter_values)]
        else:
            print(f"Warning: Column '{column_name}' not found in DataFrame. Skipping filter.")
    return df


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
    default_value = column_config.get('default', None)
    dtype = column_config.get('dtype', None)  # Get the desired data type

    if not source_column or not target_column:
        raise ValueError("Both 'source' and 'target' must be specified in the column configuration.")

    # Ensure the DataFrame is a copy to avoid SettingWithCopyWarning
    df = df.copy()

    # Apply the specified data type to the source column if provided
    if dtype:
        df.loc[:, source_column] = df[source_column].astype(dtype)

    # Create the new column based on the mapping with the default value
    df.loc[:, target_column] = df[source_column].map(mapping).fillna(default_value)

    return df


def transform_via_map(df, column_mappings):
    """Transform columns based on mappings in the config."""
    for column_name, column_config in column_mappings.items():
        df = apply_mapping(df, column_config)
    return df


def apply_grouping_operations(df, grouping_config):
    """
    Apply grouping and aggregation operations to the DataFrame based on the configuration.
    Args:
        df (pd.DataFrame): The DataFrame to transform.
        grouping_config (dict): Configuration for grouping and aggregation.
    Returns:
        pd.DataFrame: The transformed DataFrame.
    """
    for operation_name, config in grouping_config.items():
        group_by_columns = config.get("group_by", [])
        aggregation_column = config.get("aggregation", {}).get("column")
        operation = config.get("aggregation", {}).get("operation")
        target_column = config.get("target")  # Explicitly get the target column name

        if not group_by_columns or not aggregation_column or not operation or not target_column:
            raise ValueError(f"Invalid configuration for {operation_name}")

        # Perform grouping and aggregation, and assign the result to the target column
        df[target_column] = (
            df.groupby(group_by_columns)[aggregation_column]
            .transform(operation)
        )

    return df
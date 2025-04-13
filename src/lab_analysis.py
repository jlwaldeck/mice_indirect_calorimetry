import pandas as pd

# TODO: Add logging

file_path = 'C:/Users/nwald/OneDrive/Desktop/202305_Sable_DATBFX_Females_n16_AL_RC/202305_DATBFX_Females_RC_AL_Sable_n16.csv'

def load_data(file_path):
    """Load data from a CSV file."""
    try:
        data = pd.read_csv(file_path)
        print(data.shape)
        return data
    except Exception as e:
        print(f"Error loading data: {e}")
        return None
    

def set_column_types(df):
    """Set the column types for the DataFrame."""
    df['Animal'] = df['Animal'].astype(str)
    return df
    

def transform_via_map(df):
    """Transform the 'Animal' column to 'Genotype' using a mapping."""

    # Define the mapping for the 'Animal' column
    genotype_mapping = {
        "1": "BKO", "2": "Ctrl", "3": "BKO", "4": "Ctrl",
        "5": "BKO", "6": "Ctrl", "7": "BKO", "8": "Ctrl",
        "9": "BKO", "10": "Ctrl", "11": "BKO", "12": "Ctrl",
        "13": "BKO", "14": "Ctrl", "15": "BKO", "16": "Ctrl"
    }

    # Apply the mapping to create the 'Genotype' column
    df['Genotype'] = df['Animal'].map(genotype_mapping).fillna('Low')

    return df
    

def main():
    raw_data = load_data(file_path)
    raw_data = set_column_types(raw_data)
    df = transform_via_map(raw_data)
    if df is not None:
        print(df.head())

if __name__ == "__main__":
    main()
import os
import pandas as pd
import yaml
from data_processing import load_data, transform_via_map, apply_grouping_operations, apply_filters
from stats_functions import process_data_summaries
from stats_functions import process_data_summaries, generate_zt_column
import matplotlib.pyplot as plt
import seaborn as sns

# Dynamically determine the path to this script
script_dir = os.path.dirname(__file__)

gen_config_file_path = os.path.join(script_dir, '../config/general_config.yaml')
col_value_map_file_path = os.path.join(script_dir, '../config/column_mapping.yaml')
col_group_agg_file_path = os.path.join(script_dir, '../config/group_aggregation.yaml')
data_summary_config_path = os.path.join(script_dir, '../config/data_summary.yaml')

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

# Retrieve the relevant file paths from the general configuration
raw_data_path = general_config.get('raw_data_path')
output_path = general_config.get('output_path')
r_path = general_config.get('R_path')
os.environ["R_HOME"] = r_path
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
from rpy2.robjects import Formula
from rpy2.robjects import r



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

    # Process summary tasks from the loaded configuration
    process_data_summaries(df, data_summary_config)

    # Activate pandas-to-R conversion
    pandas2ri.activate()

    # Import R libraries
    base = importr('base')
    stats = importr('stats')
    lme4 = importr('lme4')
    emmeans = importr('emmeans')
    utils = importr('utils')

    # Load the data into a pandas DataFrame
    data2 = df

    # Convert pandas DataFrame to R DataFrame
    r_data2 = pandas2ri.py2rpy(data2)

    # Fit the linear mixed-effects model using lmer
    formula = Formula('UptakeASum_1hr ~ Genotype * one_hour + (1|Animal) + (1|StartDate)')
    lmer_model = lme4.lmer(formula, data=r_data2, REML=False)

    # Perform ANOVA
    anova_results = stats.anova(lmer_model)
    anova_df = pandas2ri.rpy2py(r['as.data.frame'](anova_results))
    print("ANOVA Results:")
    print(anova_results)

    # Calculate Estimated Marginal Means (EMMs) with pairwise contrasts
    formula = Formula("pairwise ~ Genotype:one_hour")
    emmeans_results = emmeans.emmeans(lmer_model, specs=formula, adjust="fdr")
    print('emmeans_results:')
    print(emmeans_results)

    # Extract emmeans dataframe for each combination of iteraction terms
    intrxn_emmeans_r = emmeans_results.rx2('emmeans')
    intrxn_emmeans_df = pandas2ri.rpy2py(r['as.data.frame'](intrxn_emmeans_r))

    # Create a mapping of numeric levels to original one_hour labels
    one_hour_mapping = dict(enumerate(data2["one_hour"].astype("category").cat.categories, start=1))

    # Map numeric levels back to original one_hour labels
    intrxn_emmeans_df["one_hour"] = intrxn_emmeans_df["one_hour"].map(one_hour_mapping)
    print("Interaction Emmeans DataFrame:")
    print(intrxn_emmeans_df)

    # Create a mapping of numeric levels to original Genotype labels
    genotype_mapping = dict(enumerate(data2["Genotype"].astype("category").cat.categories, start=1))

    # Map numeric levels back to original Genotype labels
    intrxn_emmeans_df["Genotype"] = intrxn_emmeans_df["Genotype"].map(genotype_mapping)

    # Extract pairwise contrasts
    contrasts_r = emmeans_results.rx2('contrasts')
    contrasts_df = pandas2ri.rpy2py(r['as.data.frame'](contrasts_r))
    print("Pairwise Contrasts DataFrame:")
    print(contrasts_df)

    # Sort interaction emmeans df by numeric form of one_hour and Genotype
    # Note: Assumes values in x_hour column are in some alpha numeric form (e.g. ZT01)
    # Necessary to ensure the x-axis is in correct order for plotting
    intrxn_emmeans_df["one_hour_num"] = intrxn_emmeans_df["one_hour"].str.extract(r'(\d+)').astype(int)
    intrxn_emmeans_df = intrxn_emmeans_df.sort_values(by=["one_hour_num", "Genotype"])
    intrxn_emmeans_df["zt"] = generate_zt_column(
    df=intrxn_emmeans_df,
    time_variable="one_hour",
    group_variable="Genotype"
    )

    # Append anova_results and contrasts_df to the same CSV file
    output_file = "./output/202305_DATBFX_Females_Sable_RC_AL_LD_Food_Intake_contrasts_1hr.csv"
    anova_df.to_csv(output_file, index=False)  
    with open(output_file, 'a') as f:
        f.write("\nPairwise Contrasts\n")
        contrasts_df.to_csv(f, index=False)

    # Plot the results using Python
    plt.figure(figsize=(7, 5))
    sns.lineplot(
        data=intrxn_emmeans_df,
        x="zt",
        y="emmean",
        hue="Genotype",
        style="Genotype",
        markers=True,
        dashes=True,
        ci=None
    )

    # Add error bars
    for genotype in intrxn_emmeans_df["Genotype"].unique():
        subset = intrxn_emmeans_df[intrxn_emmeans_df["Genotype"] == genotype]
        plt.errorbar(
            subset["zt"],
            subset["emmean"],
            yerr=subset["SE"],
            fmt='none',
            capsize=3,
            label=None
        )

    plt.title("Food Intake (1hr)")
    plt.xlabel("ZT")
    plt.ylabel("Estimated Marginal Means")
    plt.legend(title="Genotype")
    plt.tight_layout()
    plt.savefig("./output/202305_DATBFX_Females_Sable_RC_AL_FoodIntake_1hr_Line_Plot.pdf")
    plt.show()


if __name__ == "__main__":
    main()
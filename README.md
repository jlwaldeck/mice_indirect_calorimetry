# Mice Indirect Calorimetry

## Project Goal
The goal of this project is to analyze indirect calorimetry data for female mice with different genotypes (BKO and Ctrl) over various time intervals (e.g., 4-hour, day/night, daily).

## General Workflow
1. **Data Processing**  
   - Processes raw data by grouping and summarizing metrics such as food intake, activity, sleep percentage, VO2, RER, and energy expenditure (EE) across time periods and conditions.

2. **Statistical Analysis**  
   - Uses statistical models (e.g., `lmer`, `glm`) to evaluate the effects of genotype and time on these metrics.  
   - Performs post-hoc comparisons using the `emmeans` package.

3. **Visualization**  
   - Generates summary tables and visualizations (e.g., bar plots, line plots) to compare groups and highlight significant differences.

4. **Result Storage**  
   - Stores results as CSV files and PDFs for further interpretation.
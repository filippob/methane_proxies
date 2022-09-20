# methane_proxies
Combine new and old data on methane emissions and proxy data to develop robust predictive models

- 40k data from previous project (10 countries)
- ~40k data from new contributions (Finland, Spain, The Netherlands): for now, 13k from Finland + ~20k from Spain

## Workflow
1. 1.combine_data.R --> combined_data.csv.gz
2. 2.filter_data.R --> filtered_data.csv.gz
3. 3.imputation.R --> imputed_data.csv.gz
4. 4.standardization.R --> standardized_data.csv.gz

*Models*
- **Random Forest**: random_forest.r --> results.csv

!! When running each step make sure to save the log in the corresponding output folder, e.g. Rscript <script_name.R> &> <folder>/log & !!
!! After each step, you should compress the output, e.g. gzip <output_file> !!

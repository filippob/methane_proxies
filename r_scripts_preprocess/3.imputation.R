## Project ML 2022: compare ML methods for CH4 predictions
## script used to impute missing datapoints

# INPUT CONFIGURATION MANAGEMENT ------------------------------------------
args = commandArgs(trailingOnly=TRUE)
if (length(args) == 1){
  #loading the parameters
  source(args[1])
} else {
  #this is the default configuration, used for development and debug
  writeLines('Using default config')
  
  #this dataframe should be always present in config files, and declared
  #as follows
  config = NULL
  config = rbind(config, data.frame(
    base_folder = '~/Documents/metano/WG3/Task2/project_ML_2022',
    input_data = 'Analysis/2.filtering/filtered_data.csv.gz',
    k = 5, # size of neighbourhood for KNN imputation
    outdir = 'Analysis/3.imputation',
    force_overwrite = FALSE
  ))
  
}

# SETUP -------------------------------------------------------------------
library("knitr")
library("stringr")
library("ggplot2")
library("tidyverse")
library("tidymodels")
library("data.table")

## READ THE COMBINED DATASET
writeLines(" - reading in the filtered data")
fname = file.path(config$base_folder,config$input_data)
fdata = fread(fname)
print(paste(nrow(fdata),"have been read from the following file:", fname))

missing_data = fdata

## subsample data
n = 1000
missing_data <- sample_n(fdata, n, replace = FALSE)

## convert all character variables to factors
missing_data <- mutate(missing_data, across(where(is.character), as.factor))

writeLines(" - imputing missing data points with KNN imputation")
imputation_recipe <- recipe(missing_data) %>% 
  update_role(everything(), new_role = "predictor") %>%
  update_role(CH4, new_role = "outcome") %>%
  step_impute_knn(all_predictors(), impute_with = imp_vars(everything()), neighbors = config$k)

imputation_recipe_prep <- prep(imputation_recipe)
imputed <- bake(imputation_recipe_prep, missing_data)

## alternative approach
# temp <- select(missing_data, -CH4) ## remove outcome variable
# imputation_recipe <- recipe(temp) %>% 
#   step_impute_knn(all_numeric(), impute_with = imp_vars(everything()), neighbors = config$k)
# imputation_recipe_prep <- prep(imputation_recipe)
# imputed <- bake(imputation_recipe_prep, temp)
# imputed <- bind_cols(imputed, missing_data$CH4)

residual_missing = sum(colSums(is.na(imputed)))
print(paste("The number of residual missing data point is:", residual_missing))
print(paste("N. of records in the impiuted data:", nrow(imputed)))

## WRITING OUT IMPUTED DATA
writeLines(" - writing out imputed data")
dir.create(file.path(config$base_folder, config$outdir), recursive = TRUE, showWarnings = FALSE)
fname = file.path(config$base_folder, config$outdir, "imputed_data.csv")
fwrite(x = filtered_data, file = fname, sep = ",", col.names = TRUE)
print(paste("The imputed data file has been written out to:", fname))

print("DONE!")

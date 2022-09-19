## Project ML 2022: compare ML methods for CH4 predictions
## script used to run Random Forest models

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
    input_data = 'Analysis/4.standardization/standardized_data.csv.gz',
    test_prop = 0.8,
    ntrees = 100,
    outdir = 'Analysis/random_forest',
    force_overwrite = FALSE
  ))
  
}

# SETUP -------------------------------------------------------------------
library("knitr")
library("ggplot2")
library("tidyverse")
library("tidymodels")
library("data.table")

## READ THE IMPUTED DATASET
writeLines(" - reading in the standardized data")
fname = file.path(config$base_folder,config$input_data)
sdata = fread(fname)
print(paste(nrow(sdata),"have been read from the following file:", fname))

inpdata = sdata

## SUBSAMPLING
## subsample
n = 5000
inpdata = sample_n(sdata, n)
inpdata <- mutate(inpdata, across(where(is.character), as.factor))
inpdata <- filter(inpdata, RecordingYear >= 2000)
rm(sdata)

## PREPROCESSING
## create stratifying variable (combination of country and recording year)
writeLines(" - data preprocessing")
inpdata <- mutate(inpdata, recency = ifelse(RecordingYear > 2015, "recent", "old"), time_space = paste(country, recency, sep="_"))
inpdata <- select(inpdata, -recency)

## DATA SPLIT (TRAINING/TEST)
writeLines(" - data splitting (trining/test data)")
## 1. random (stratified) cross-validation
data_split <- initial_split(inpdata, 
                            prop = config$test_prop, 
                            strata = time_space)

# Create dataframes for the two sets:
train_data <- training(data_split) 
test_data <- testing(data_split)
print(paste("N. of records in the training set:", nrow(train_data)))
print(paste("N. of records in the test set:", nrow(test_data)))

print("distribution of records in the stratified training set")
train_data %>% group_by(country, RecordingYear) %>% summarise(N=n()) %>% spread(key = RecordingYear, value = N) %>% kable()

## RANDOM FOREST MODEL
#### 1. Create the recipe
writeLines(" - creating the tidymodels recipe")
rf_recipe <- 
  recipe(CH4 ~ ., train_data) %>%
  update_role(everything(), new_role = "predictor") %>%
  update_role(CH4, new_role = "outcome") %>%
  update_role(time_space, new_role = "splitting variable") %>%
  step_nzv(all_predictors()) %>%
  step_rm(removals = "time_space")

#### 2. Specify the model
cores <- parallel::detectCores()

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = config$ntrees) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")

#### 3. Create the workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

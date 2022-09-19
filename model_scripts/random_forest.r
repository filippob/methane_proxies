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
    nfolds = 5, #n. of folds for the internal cross-validation (model tuning)
    nrepeates = 2, #n. of repetitions for the internal cross-validation (model tuning)
    gridsize = 50, #n. of combinations of the tuning parameters to try
    outdir = 'Analysis/random_forest',
    force_overwrite = FALSE
  ))
  
}

# SETUP -------------------------------------------------------------------
library("vip")
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
dir.create(file.path(config$base_folder, config$outdir), recursive = TRUE, showWarnings = FALSE)

## SUBSAMPLING
## subsample
n = 2000
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
writeLines(" - specifying the random forest model")
cores <- parallel::detectCores()

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = config$ntrees) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")

print(paste("N. of cores detected: ", cores))

#### 3. Create the workflow
writeLines(" - creating the tidymodels workflow")
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

#### 4. Make (cross)validation splits
writeLines(" - making the partitions for model tuning")
folds <- vfold_cv(train_data, v = config$nfolds, repeats = config$nrepeates)

#### 5. Tuning the hyperparameters
writeLines(" - hyperparameter tuning")
rf_res <- rf_workflow %>% 
  tune_grid(folds,
            grid = config$gridsize,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse,ccc))

print("best models from hyperparameter tuning")
rf_res %>% 
  show_best('ccc')

fname = file.path(config$base_folder, config$outdir, "model_tuning.pdf")
pdf(file = fname, width = 7, height = 7)
autoplot(rf_res)
dev.off()

#### 6. Select best model
writeLines(" - select the best model")
rf_best <- rf_res %>% 
  select_best(metric = "rmse")

print("best model hyperparameters")
kable(rf_best)

rf_ccc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  ccc(CH4, .pred) %>% 
  mutate(model = "Random Forest")

print("best model performance - corrleation")
rf_ccc

#### 7. Fit the last model
writeLines(" - fit the best model")
mtry_tune = select_best(rf_res, metric = "rmse") %>% pull("mtry")
min_n_tune = select_best(rf_res, metric = "rmse") %>% pull("min_n")

# the last model
last_rf_mod <- 
  rand_forest(mtry = mtry_tune, min_n = min_n_tune, trees = ntrees) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("regression")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(data_split)

kable(last_rf_fit)

#### 8. Evaluate final model
writeLines(" - evaluate the best model")
last_rf_fit %>% 
  collect_metrics() %>%
  kable()

print("First rows of predictions")
preds <- last_rf_fit %>% collect_predictions()
head(preds) %>% kable()

g <- ggplot(preds, aes(x = .pred, y = CH4)) + geom_point()
fname = file.path(config$base_folder, config$outdir, "predictions.pdf")
ggsave(filename = fname, plot = g, device = "pdf")

pearson = cor(preds$CH4, preds$.pred, method = "pearson")
spearman = cor(preds$CH4, preds$.pred, method = "spearman")
rmse = sqrt(sum((preds$.pred-preds$CH4)^2)/nrow(preds))

print(paste("Pearson correlation on test data:", pearson))
print(paste("Spearman correlation on test data:", spearman))
print(paste("RMSE on test data:", rmse))

## VARIABLE IMPORTANCE
writeLines(" - extract variable importance")
fname = file.path(config$base_folder, config$outdir, "variable_importance.pdf")
pdf(file = fname, width = 7, height = 7)
last_rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)
dev.off()

print("DONE!")

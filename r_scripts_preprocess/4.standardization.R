## Project ML 2022: compare ML methods for CH4 predictions
## script used to standardize numerical variables

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
    input_data = 'Analysis/3.imputation/imputed_data.csv.gz',
    outdir = 'Analysis/4.standardization',
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

## READ THE IMPUTED DATASET
writeLines(" - reading in the imputed data")
fname = file.path(config$base_folder,config$input_data)
fdata = fread(fname)
print(paste(nrow(fdata),"have been read from the following file:", fname))

# relocate variables
fdata <- fdata %>% relocate(Method, .after = breed) #to make a first block of categorical variables and a second block of numerical variables
fdata <- fdata %>% relocate(CH4, .after = DMI) #to move the outcome variable -CH4- at the end of the dataset


## STANDARDIZE NUMERICAL PREDICTORS
writeLines(" - standardizing the numerical predictors")
#1. obtain averages for each variables
avg_parity = mean(fdata$parity)
avg_dim = mean(fdata$DIM)
avg_bw = mean(fdata$BW)
avg_milk = mean(fdata$milk)
avg_fat = mean(fdata$fat)
avg_prot = mean(fdata$protein)
avg_lact = mean(fdata$lactose)
avg_dmi = mean(fdata$DMI)

#1. obtain standard deviations for each variables
sd_parity = sd(fdata$parity)
sd_dim = sd(fdata$DIM)
sd_bw = sd(fdata$BW)
sd_milk = sd(fdata$milk)
sd_fat = sd(fdata$fat)
sd_prot = sd(fdata$protein)
sd_lact = sd(fdata$lactose)
sd_dmi = sd(fdata$DMI)

print("+ variable +-- avg --+-- std. dev. --+-")
print(paste("parity ", round(avg_parity,3), round(sd_parity,3), sep=" | "))
print(paste("DIM ", round(avg_dim, 3), round(sd_dim, 3), sep=" | "))
print("+------------------------------------------+")

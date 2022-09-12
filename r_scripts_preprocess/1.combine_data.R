

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
      old_data = '2018_data/combinedData.csv',
      new_data_folder = '2022_data',
      fam_file = 'Analysis/apple/admixture/FruitBreedomics_apple_320K_SNP_thin.fam',
      outdir = 'Analysis/1.combining',
      force_overwrite = FALSE
    ))
    
}
  
# SETUP -------------------------------------------------------------------
library("ggplot2")
library("tidyverse")
library("data.table")

## READ THE OLD DATASET
fname = file.path(config$base_folder,config$old_data)
old_data = fread(fname)  


## READ THE NEW DATA
# 1. Finland
ffs = list.files(file.path(config$base_folder, config$new_data_folder)) # get files in folder
fname = file.path(config$base_folder, config$new_data_folder,ffs[!grepl(pattern = ".zip", ffs)]) # get file which is not .zip
findata = fread(fname)

header = c("Inst","renCode","yrmonwk","calvdate","ndim","lactwk","milk","fat","prot","lactose","scc","ecm","dmi","bw2","CH4pg") # column names for new Finnish data
names(findata) <- header

### TODO - normalize column names and add missing columns before combining new and old data #####

dir.create(file.path(config$base_folder, config$outdir), showWarnings = FALSE)


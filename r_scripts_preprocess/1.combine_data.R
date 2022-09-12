

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
      old_data = '2018_data/combinedData_updated.csv',
      new_data_folder = '2022_data',
      fam_file = 'Analysis/apple/admixture/FruitBreedomics_apple_320K_SNP_thin.fam',
      outdir = 'Analysis/1.combining',
      force_overwrite = FALSE
    ))
    
}
  
# SETUP -------------------------------------------------------------------
library("knitr")
library("ggplot2")
library("tidyverse")
library("data.table")

## READ THE OLD DATASET
fname = file.path(config$base_folder,config$old_data)
old_data = fread(fname) 

## arrange by country name (increasing)
old_data <- arrange(old_data, country)

## recode countries
oldcountry <- unique(old_data$country)
newcountry = c("Belgium", "Belgium", "Germany", "Northern_Ireland", "Ireland", "Denmark", "Finland", "Finland", "Netherlands", "Poland", "Switzerland", "Spain", "UK", "UK")
old_data$country = newcountry[match(old_data$country, oldcountry)]
old_data %>% group_by(country, herd) %>% summarise(n()) %>% kable()

## READ THE NEW DATA
# 1. Finland
ffs = list.files(file.path(config$base_folder, config$new_data_folder)) # get files in folder
fname = file.path(config$base_folder, config$new_data_folder,ffs[!grepl(pattern = ".zip", ffs)]) # get file which is not .zip
findata = fread(fname)

header = c("Inst","renCode","yrmonwk","calvdate","ndim","lactwk","milk","fat","prot","lactose","scc","ecm","dmi","bw2","CH4pg") # column names for new Finnish data
names(findata) <- header

### TODO - normalize column names and add missing columns before combining new and old data #####
names(findata)
names(old_data)


# dir.create(file.path(config$base_folder, config$outdir), showWarnings = FALSE)


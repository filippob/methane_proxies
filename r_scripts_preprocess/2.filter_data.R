## Project ML 2022: compare ML methods for CH4 predictions
## script used to clean and filter the combined data (old and new)

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
    input_data = 'Analysis/1.combining/combined_data.csv.gz',
    vars_to_drop = "RecordingDay,status,CalvingDay",
    outdir = 'Analysis/2.filtering',
    force_overwrite = FALSE
  ))
  
}

# SETUP -------------------------------------------------------------------
library("knitr")
library("stringr")
library("ggplot2")
library("tidyverse")
library("data.table")

## READ THE COMBINED DATASET
writeLines(" - reading in the combined data")
fname = file.path(config$base_folder,config$input_data)
cdata = fread(fname)
print(paste(nrow(cdata),"have been read from the following file:", fname))

## DATA CLEANING
#1. put together Norwegian and Nor Cross (same cross from herds in Northern Ireland)
#   ' ' (space) replaced by '_' (underscore) in the column "breed" 
cdata <- mutate(cdata, breed = ifelse(breed == "Norwegian", "Nor Cross", breed))
cdata$breed = gsub(" ","_",cdata$breed)

#2. put together sniffer and sniffer-guardian
cdata <- mutate(cdata, Method = ifelse(Method == "sniffer-guardian", "sniffer", Method))

## SELECTING COLUMNS
# columns to 
variables = strsplit(x = config$vars_to_drop, split = ",")[[1]]
cdata <- cdata %>%
  rename(BW = Body_weight) %>%
  select(-all_of(variables))

## DATA FILTERING
filter(cdata, parity == 14)

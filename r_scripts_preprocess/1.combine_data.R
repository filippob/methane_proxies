## Project ML 2022: compare ML methods for CH4 predictions
## script used to combine old data with new datasets from Finland, Spain ,The Netherlands

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
library("stringr")
library("ggplot2")
library("tidyverse")
library("data.table")

## READ THE OLD DATASET
writeLines(" - reading in the old (previous) data")
fname = file.path(config$base_folder,config$old_data)
old_data = fread(fname) 
print(paste("The following file has been read:", fname))

writeLines(" - a little pre- preprocessing (country names, status Lactating)")
## arrange by country name (increasing)
old_data <- arrange(old_data, country)

## recode countries
oldcountry <- unique(old_data$country)
newcountry = c("Belgium", "Belgium", "Germany", "Northern_Ireland", "Ireland", "Denmark", "Finland", "Finland", "Netherlands", "Poland", "Switzerland", "Spain", "UK", "UK")
old_data$country = newcountry[match(old_data$country, oldcountry)]
old_data %>% group_by(country, herd) %>% summarise(n()) %>% kable()
old_data <- mutate(old_data, status = ifelse(status == "Lactation", "Lactating", status),
                   breed = ifelse(breed == "Nordic Red: Ayrshire", "Nordic_Red", breed)) %>% select(-IDcountry)

## READ THE NEW DATA
writeLines(" - reading in the new data")

# 1. Finland
writeLines(" - FINLAND")
ffs = list.files(file.path(config$base_folder, config$new_data_folder)) # get files in folder
fname = file.path(config$base_folder, config$new_data_folder,ffs[!grepl(pattern = ".zip", ffs)]) # get file which is not .zip
findata = fread(fname)
print(paste("The folowing new dataset has been read", fname))

writeLines(" - normalise new data: column names and data type")
header = c("Inst","renCode","yrmonwk","calvdate","ndim","lactwk","milk","fat","prot","lactose","scc","ecm","dmi","bw2","CH4pg") # column names for new Finnish data
names(findata) <- header
findata <- findata %>% 
  rename(cow = renCode, DIM = ndim, DMI = dmi, Body_weight = bw2, CH4 = CH4pg, protein = prot) %>%
  mutate(country = "Finland", herd="L003", RecordingYear = as.integer(str_sub(yrmonwk, 1, 4)), 
         RecordingMonth = as.integer(gsub('0','',str_sub(yrmonwk, 5, 6))), 
         RecordingDay = 7*(as.integer(str_sub(yrmonwk, 7, 8)) - RecordingMonth),
         status = ifelse(lactwk >= 1, "Lactating", "Dry"),
         CalvingYear = ifelse(calvdate <= -9999, NA, str_sub(calvdate, 1, 4)),
         CalvingMonth = ifelse(calvdate <= -9999, NA, str_sub(calvdate, 5, 6)),
         CalvingDay = ifelse(calvdate <= -9999, NA, str_sub(calvdate, 7, 8)),
         breed = "Nordic_Red",
         parity = 1,
         Method = "sniffer",
         cow = as.character(cow)) %>% 
  select(-c(Inst, yrmonwk, calvdate, ecm, scc, lactwk))

## COMBINE THE DATA
writeLines(" - combining old and new data records")
combined_data <- old_data %>% bind_rows(findata)

## CREATE OUTPUT FOLDER AND WRITE OUT COMBINED DATA
writeLines(" - writing out the combined file")
dir.create(file.path(config$base_folder, config$outdir), recursive = TRUE)
fname = file.path(config$base_folder, config$outdir, "combined_data.csv")
fwrite(x = combined_data, file = fname, sep = ",", col.names = TRUE)
print(paste("The combined file has been written out to:", fname))

print("DONE!")

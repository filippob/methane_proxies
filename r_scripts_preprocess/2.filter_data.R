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
    dim_max = 500, #threshold for DIM (remove what's above)
    milk_max = 60, #threshold for milk yield (kg/d) (remove what's above)
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
writeLines(" - cleaning the data")
#1. put together Norwegian and Nor Cross (same cross from herds in Northern Ireland)
#   ' ' (space) replaced by '_' (underscore) in the column "breed" 
cdata <- mutate(cdata, breed = ifelse(breed == "Norwegian", "Nor Cross", breed))
cdata$breed = gsub(" ","_",cdata$breed)

#2. put together sniffer and sniffer-guardian
cdata <- mutate(cdata, Method = ifelse(Method == "sniffer-guardian", "sniffer", Method))

#3. set negative BW values to NA
cdata <- rename(cdata, BW = Body_weight) %>% mutate(BW = ifelse(BW <= 0, NA, BW))

#4. set DIM = 0 to NA
cdata <- mutate(cdata, DIM = ifelse(DIM <= 0, NA, DIM))

#5. set milk < 1 to NA
cdata <- mutate(cdata, milk = ifelse(milk <= 1, NA, milk))

#6. set protein < 0.5 to NA
cdata <- mutate(cdata, protein = ifelse(protein <= 0.5, NA, protein))

#7. set lactose < 0.5 to NA
cdata <- mutate(cdata, lactose = ifelse(lactose <= 0.5, NA, lactose))

## SELECTING COLUMNS
writeLines(" - removing unnecessary columns")
# columns to drop
print(paste(config$vars_to_drop, collapse = ", "))
variables = strsplit(x = config$vars_to_drop, split = ",")[[1]]
cdata <- cdata %>%
  select(-all_of(variables))

## DATA FILTERING
writeLines(" - filtering the data")
filtered_data <- filter(cdata, DIM <= config$dim_max, milk <= config$milk_max)

print(paste("N. of records in input file:", nrow(cdata)))
print(paste("N. of records in filtered file:", nrow(filtered_data)))
print(paste("N. of records removed with filtering:", nrow(cdata)-nrow(filtered_data)))

## WRITING OUT CLEANED DATA
writeLines(" - writing out cleaned and filtered data")
dir.create(file.path(config$base_folder, config$outdir), recursive = TRUE, showWarnings = FALSE)
fname = file.path(config$base_folder, config$outdir, "filtered_data.csv")
fwrite(x = filtered_data, file = fname, sep = ",", col.names = TRUE)
print(paste("The filtered data file has been written out to:", fname))

print("DONE!")



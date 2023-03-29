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

ffs = list.files(file.path(config$base_folder, config$new_data_folder)) # get files in folder
ffs = sort(ffs[!grepl(pattern = ".zip|.rar", ffs)]) # get file which is not .zip or .rar
print(paste("The following files have been found", paste(ffs, collapse = ", ")))

# 1. Finland
writeLines(" - FINLAND")
fname = file.path(config$base_folder, config$new_data_folder, ffs[1]) 
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

# 2. Spain - INIA
writeLines(" - SPAIN-INIA")
## features
fname = file.path(config$base_folder, config$new_data_folder, ffs[5]) 
spaindata = fread(fname)
print(paste("The folowing new dataset has been read", fname))

spaindata <- spaindata |>
  rename(cow = ANIMAL.ID, country = COUNTRY.OF.DATA.ORIGIN, herd = HERD.CODE, DMI = DRY.MATTER.INTAKE.GRAMS.PER.DAY, Body_weight = LIVE.BODY.WEIGHT,
         CH4 = CH4.EMISSION.GDAY, Method = CH4.MEASURE.METHOD, parity = `LACTATION NUMBER`, milk = MILK.YIELD, protein = PROTEIN.YIELD,
         fat = FAT.YIELD, lactose = LACTOSE.YIELD) |>
  filter(!is.na(CH4)) |>
  mutate(FECHA = as.Date(FECHA, "%Y-%m-%d"), 
         RecordingYear = as.integer(format(FECHA, format = "%Y")),
         RecordingMonth = as.integer(format(FECHA, format = "%m")),
         RecordingDay = as.integer(format(FECHA, format = "%d")),
         status = ifelse(DIM >= 1, "Lactating", "Dry"),
         breed = "Holstein", ## sum(grepl(pattern = "HOL", x = spaindata$ANIMAL.ID)) == nrow(spaindata) !! ALL ANIMALS FROM THIS DATASET ARE HOLSTEINS !!
         CalvingYear = NA,
         CalvingMonth = NA,
         CalvingDay = NA,
         country = "Spain",
         Body_weight = as.numeric(gsub(",","\\.",Body_weight))
         ) |>
  select(-FECHA)
  
# 3. Spain - NEIKER
writeLines(" - SPAIN-NEIKER")
## features
fname = file.path(config$base_folder, config$new_data_folder, ffs[2]) 
neiker = readxl::read_xlsx(fname, skip = 5)
print(paste("The folowing new dataset has been read", fname))

neiker <- neiker |>
  rename(institute = `Abbr. Institute`, id = `Animal ID`, herd = Herd, breed = Breed, CH4 = `CH4(gm/d)`,
         Method = `CH4 method`, DMI = `DMintake(kg/d)`, milk = `Milk(kg/d)`, protein = `Prot%`, fat = `Fat%`,
         lactose = Lactose, parity = Parity, Body_weight = `Bodywt(kg)`, 
         RecDate = `Recording date (ddmmyyyy)`, CalvDate = `Calving date(ddmmyyyy)`)  |>
  mutate(cow = paste("Neiker",institute,id,sep="_"), country = "Spain",
         status = ifelse(DIM >= 1, "Lactating", "Dry"),
         RecordingYear = RecDate %% 10000,
         RecordingMonth = as.integer(gsub("2019|2020","",RecDate)) %% 100,
         RecordingDay = as.integer(substr(RecDate, 1, nchar(RecDate)-6)),
         CalvingYear = CalvDate %% 10000,
         CalvingMonth = as.integer(gsub("2018|2019|2020","",CalvDate)) %% 100,
         CalvingDay = as.integer(substr(CalvDate, 1, nchar(CalvDate)-6))
         ) |>
  mutate(across(where(is.numeric), .fns = ~replace(., . <= -99, NA))) |>
  select(-c(institute, id, SCC, `Age(month)`, RecDate, CalvDate))


## COMBINE NEW DATA
new_data <- bind_rows(findata, spaindata, neiker)
new_data %>% group_by(country, herd) %>% summarise(n()) %>% kable()

## COMBINE THE DATA
writeLines(" - combining old and new data records")
combined_data <- old_data %>% bind_rows(new_data)

## CREATE OUTPUT FOLDER AND WRITE OUT COMBINED DATA
writeLines(" - writing out the combined file")
dir.create(file.path(config$base_folder, config$outdir), recursive = TRUE, showWarnings = FALSE)
fname = file.path(config$base_folder, config$outdir, "combined_data.csv")
fwrite(x = combined_data, file = fname, sep = ",", col.names = TRUE)
print(paste("The combined file has been written out to:", fname))

print("DONE!")

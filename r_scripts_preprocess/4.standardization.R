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
library("ggfortify")
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

vars = c("parity","DIM","BW", "milk", "fat", "protein", "lactose", "DMI")
avgs = c(avg_parity,avg_dim,avg_bw,avg_milk,avg_fat,avg_prot,avg_lact,avg_dmi)
sds = c(sd_parity,sd_dim,sd_bw,sd_milk,sd_fat,sd_prot,sd_lact,sd_dmi)

print("Parameters used for standardization:")
kable(data.frame("variable" = vars, "average" = round(avgs,3), "std. ddevs" = round(sds,3)))

num_vars <- select(fdata, all_of(vars))
header = names(num_vars)

print("CENTERING")
num_vars = sweep(as.matrix(num_vars), 2, avgs)

print("SCALING")
num_vars = (num_vars %*% diag(1/sds))
num_vars = as.data.frame(num_vars)
names(num_vars) <- header

## PCA
writeLines(" - PCA on the standardized numerical predictors")
pca_res = prcomp(num_vars, center = FALSE, scale. = FALSE)

dir.create(file.path(config$base_folder, config$outdir), recursive = TRUE, showWarnings = FALSE)
fname = file.path(config$base_folder, config$outdir, "pca.pdf")

pdf(file = fname, width = 7, height = 7)
autoplot(pca_res, data = fdata, colour = 'country')
autoplot(pca_res, data = fdata, colour = 'Method')
autoplot(pca_res, data = fdata, colour = 'RecordingYear')
autoplot(pca_res, data = fdata, colour = 'herd')
dev.off()

#3. put together standardized data and categorical variables
other_vars <- select(fdata, !all_of(vars))
sdata = bind_cols(other_vars, num_vars)

## STANDARDIZE CH4
writeLines(" - standardizing CH4 emissions")
avg = mean(sdata$CH4)
std = sd(sdata$CH4)

print("+-------------------------------------+")
print("Parameters used for standardization:")
print(paste("Average CH4:", round(avg, 3)))
print(paste("Std deviation of CH4:", round(std, 3)))
print("+-------------------------------------+")

sdata$CH4 = (sdata$CH4-avg)/std

print("write out histogram of standardized CH4 distribution")
fname = file.path(config$base_folder, config$outdir, "std_ch4.png")
png(filename = fname)
hist(sdata$CH4)
dev.off()


## WRITING OUT IMPUTED DATA
writeLines(" - writing out imputed data")
sdata <- sdata %>% relocate(CH4, .after = DMI)
fname = file.path(config$base_folder, config$outdir, "standardized_data.csv")
fwrite(x = sdata, file = fname, sep = ",", col.names = TRUE)
print(paste("The standardized data file has been written out to:", fname))

print("DONE!")

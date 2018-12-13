library(rgdal)
library(RcppCNPy)
library(tidyverse)
library(magrittr)
library(lubridate)

# Takes in a list of DeliveryTime+DeliveryHours and matching ForecastFiles.
# Reads in all the GRIB files and store separate downsampled band files for each.

# Requires in working directory:
## timeline_files.csv list of DeliveryTime+DeliveryHours and matching ForecastFiles
## grib_path folder (with '/' at end) where raw GRIB files are located
## bands_path folder to store downsampled band arrays in. Requires 'band1' through 'band5' subfolders.

# Folder where all the raw grib files are located
grib_path <- 'grib/'
# Folder where the band-array files should be stored. Needs band1 through band5 subfolders.
bands_path <- 'bands/'

# Downsample image by half in both direction, by averaging 2x2 cells into 1 cell
downsample <- function(m){
  dims = dim(m)
  dm <- matrix(0, nrow = dims[1]/2, ncol = dims[2]/2)
  for (row in seq(1, dims[1]/2)) {
    for (col in seq(1, dims[2]/2)) {
      dm[row, col] <- mean(m[(row*2-1):(row*2), (col*2-1):(col*2)])
    }
  }
  return(dm)
}

# Read in prepared timeline of DeliveryTime and matching forecast file names
tl <- read_csv('timeline_files.csv')

# Pull the list of unique files to be read in
## All files appear in ForecastFile1. ForecastFiles2 are effectively only lagged.
files_want <- tl %>% select(ForecastFile1) %>% unique() %>% pull()
length(files_want)

# Pull the list of files actually available
files_have <- list.files(path = grib_path, pattern = '.grb$', recursive = FALSE)
length(files_have)

# Intersect to get list of files to read in
files_read <- intersect(files_want, files_have)
length(files_read)

i <- 1
for (file in files_read) {
  # Read in file with hardcoded offsets to crop to Nordics
  ## Assumed consistent over time!
  ## Sink to quiet it. The 'log' file in the same directory will show the error if it crashed.
  sink('log')
  grib <- readGDAL(paste0(grib_path, file), offset = c(5, 125), region.dim = c(110, 110))
  sink()
  # Iterate over the five bands (i.e. forecasts) in that file
  for (bnum in seq(1,5)) {
    # Extract image as matrix
    band <- as.matrix(grib[bnum])
    # Downsample the band by 50% in both directions
    band_d <- downsample(band)
    # Transpose the band (so plotting in matplotlib looks right) and store in band subfolder
    ## Replace . in filename with blank, since some horizons <10. E.g. '8.'grb vs '18'.grb
    npySave(paste0(bands_path, 'band', bnum, '/', gsub('\\.', '', substr(file, 8, 22)), '.npy'), t(band_d))
  }
  # Print progress
  print(paste0(i, '/', length(files_read)))
  i <- i + 1
}
library(tidyverse)
library(lubridate)
library(magrittr)

rm(list = ls())

par_dir <- "/Users/johannes/OneDrive/Master Thesis - ML/02 - Data/FTP/RAW/Regulating_power_market/Regulating_prices"

columns_new <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Hour1", "Hour2", "Hour3A", "Hour3B", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Average")

columns_old <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Hour1", "Hour2", "Hour3", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Average")

for(year in c(seq(2010, 2017), "")){
  temp_tib = tibble()
  files = list.files(path = paste0(par_dir, "/", year), pattern = "*.sdv", full.names = T)
  for (i in 1:length(files)) {
    cat("\nYear ", year, ": ", i, "/", length(files), sep = "")
    if (year %in% seq(2010, 2013)) {
      cols <- columns_old
    } else {
      cols <- columns_new
    }
    suppressWarnings(
      suppressMessages(
        file <- read_delim(
          file = files[i], 
          delim = ";", 
          col_names = cols, 
          comment = "#", 
          locale = locale(decimal_mark = ",")
        ) 
      )
    )
    temp_tib = rbind(temp_tib, file)
  }
  # Limit to desired rows based on DataType values
  temp_tib %<>% filter(DataType == "PR")
  assign(paste0("regulating_year", year), temp_tib)
  remove(temp_tib, file, files, i, year, cols)
}

# Merge, starting with 2018 since widest (Hour3A+Hour3B > Hour3); apropos returns "capacity_year_" first
merged_regulating <- bind_rows(lapply(apropos("regulating_year.*"), get))

# Fix datatypes
merged_regulating %<>% 
  mutate(
    Year = as.integer(Year),
    # Takes a dmy string and returns a ymd date object
    Date = dmy(Date),
    # Hour1 and Hour2 are for some reason chr rather than num, so replace "," with "." and make numeric
    Hour1 = as.numeric(gsub(",", ".", Hour1)),
    Hour2 = as.numeric(gsub(",", ".", Hour2)),
    Hour3B = as.numeric(Hour3B)
  )

# Sort and arrange
merged_regulating %<>% 
  select(DataType, Code, Year, Week, Day, Date, Alias, Hour1, Hour2, Hour3, everything()) %>%
  arrange(desc(Date))

# Save file
merged_regulating %>% write_csv("data_merged/regulating_price_merged.csv")


# ==========================================================================================
# SOME EXPLORATORY PLOTS

# A single potential outlier (unless there really was an extreme case)
ggplot(merged_regulating) + geom_point(aes(x = Date, y = Average))

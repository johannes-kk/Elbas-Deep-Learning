library(tidyverse)
library(lubridate)
library(magrittr)

rm(list = ls())

par_dir <- "/Users/johannes/OneDrive/Master Thesis - ML/02 - Data/FTP/RAW/Elspot/Elspot_capacity"

# Starting with 2015, Hour3 is split into Hour3A and Hour3B
## Changes in 2014, but we keep 2014 in 24h so we have to combine fewer records later

columns_new <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Hour1", "Hour2", "Hour3A", "Hour3B", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Sum")

columns_old <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Hour1", "Hour2", "Hour3", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Sum")

for(year in c(seq(2009, 2017), "")){
    temp_tib = tibble()
    files = list.files(path = paste0(par_dir, "/", year), pattern = "*.sdv", full.names = T)
    for (i in 1:length(files)) {
        cat("\nYear ", year, ": ", i, "/", length(files), sep = "")
        if (year %in% seq(2000, 2013)) {
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
    temp_tib %<>% filter(DataType %in% c("UE", "CR"))
    assign(paste0("elspot_cap_", year), temp_tib)
    remove(temp_tib, file, files, i, year, cols)
}

# Merge the files, starting with latter and descending date, since those have one more column
merged_elsp_cap <- bind_rows(lapply(apropos("elspot_cap_*"), get))

# Fix column data types
merged_elsp_cap %<>% 
  mutate(
    # Takes a dmy string and returns a ymd date object
    Date = dmy(Date),
    Hour1 = as.integer(Hour1),
    Hour2 = as.integer(Hour2),
    Hour3B = as.integer(Hour3B)
  ) %>%
  # Change all integers (to catch all Hours) to numeric, in case we will be scaling etc. later
  mutate_if(
    .pred = is.integer, 
    .funs = as.numeric
  ) %>%
  # Change some (back) to integer and extract From/To -Areas
  mutate(
    Year = as.integer(Year),
    Week = as.integer(Week),
    Day = as.integer(Day),
    FromArea = str_sub(str_extract(Alias, ".+_"), 1, -2),
    ToArea = str_sub(str_extract(Alias, "_.+"), 2, -1)
  )

# Sort columns
merged_elsp_cap %<>% 
  select(DataType, Code, Year, Week, Day, Date, Alias, FromArea, ToArea, Hour1, Hour2, Hour3, Hour3A, Hour3B, everything())

# Arrange descending
merged_elsp_cap %<>% arrange(desc(Date))

# Save file
merged_elsp_cap %>% write_csv("data_merged/elspot_capacity_merged.csv")

# ==========================================================================================
# SOME EXPLORATORY PLOTS

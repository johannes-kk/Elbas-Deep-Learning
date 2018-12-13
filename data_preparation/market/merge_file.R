library(tidyverse)
library(lubridate)
library(magrittr)

rm(list = ls())

par_dir <- "/Users/johannes/OneDrive/Master Thesis - ML/02 - Data/FTP/RAW/Elspot/Elspot_file"

# Starting with 2014, Hour3 is split into Hour3A and Hour3B

columns_new <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Unit", "Hour1", "Hour2", "Hour3A", "Hour3B", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Total")

columns_old <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Unit", "Hour1", "Hour2", "Hour3", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Total")

for(year in c(seq(2000, 2017), "")){
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
    temp_tib %<>% filter(DataType %in% c("PR", "OM"))
    assign(paste0("file_year_", year), temp_tib)
    remove(temp_tib, file, files, i, year, cols)
}

# Convert 2018 Totals to numeric, first
file_year_ %<>% mutate(Total = as.numeric(Total))
# Merge the files, starting with latter and descending date, since those have one more column
merged_file <- bind_rows(lapply(apropos("file_year_*"), get))

# Fix column data types
merged_file %<>% 
  mutate(
    Year = as.integer(Year),
    # Takes a dmy string and returns a ymd date object
    Date = dmy(Date),
    Hour3B = as.numeric(Hour3B)
  )

# Sort columns
merged_file %<>% 
  select(DataType, Code, Year, Week, Day, Date, Alias, Unit, Hour1, Hour2, Hour3, Hour3A, Hour3B, everything())

# Arrange descending
merged_file %<>% arrange(desc(Date))

# Save file
merged_file %>% write_csv("data_merged/elspot_file_merged.csv")

# ==========================================================================================
# SOME EXPLORATORY PLOTS
merged_file %>% filter(Unit == "EUR") %>% ggplot() + geom_histogram(aes(x = Total), binwidth = 2)

merged_file %>% filter(Unit == "EUR") %>% ggplot() + geom_line(aes(x = Date, y = Total))
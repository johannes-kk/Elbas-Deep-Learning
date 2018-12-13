library(tidyverse)
library(lubridate)
library(magrittr)

rm(list = ls())

par_dir <- "/Users/johannes/OneDrive/Master Thesis - ML/02 - Data/FTP/RAW/Elbas/Elbas_capacity"

# Helper functions to split Alias into FromArea and ToArea
get1st <- function(var){
  return(str_split(var, "_")[[1]][1])
}
get2nd <- function(var){
  return(str_split(var, "_")[[1]][2])
}

# Date(dd.mm.yyyy)
# Timestamp(dd.mm.yyyy HH:MM:SS)

columns_new <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Timestamp", "Hour1", "Hour2", "Hour3A", "Hour3B", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Sum")

columns_old <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Timestamp", "Hour1", "Hour2", "Hour3", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Sum")

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
  temp_tib %<>% filter(DataType == "IC")
  assign(paste0("capacity_year_", year), temp_tib)
  remove(temp_tib, file, files, i, year, cols)
}

# Merge, starting with 2018 since widest (Hour3A+Hour3B > Hour3); apropos returns "capacity_year_" first
merged_capacity <- bind_rows(lapply(apropos("capacity_year_.*"), get))

# Fix data types and dates
merged_capacity %<>% 
  mutate(
    Year = as.integer(Year),
    # Takes a dmy string and returns a ymd date object
    Date = dmy(Date),
    Timestamp = dmy_hms(Timestamp),
    # Hour1 is for some reason chr rather than num, so replace "," with "." and make numeric
    Hour1 = as.numeric(gsub(",", ".", Hour1)),
    Hour3B = as.numeric(Hour3B),
    # Integer in raw data (like all Hours), but convert to numeric since all hours >= 2014 are decimals
    Hour3 = as.numeric(Hour3),
    # Split alias into from- and to- bidding areas !!VERIFY!!
    FromArea = map_chr(Alias, get1st),
    ToArea = map_chr(Alias, get2nd)
  )

# Sort and arrange
merged_capacity %<>% 
  select(DataType, Code, Year, Week, Day, Date, Alias, FromArea, 
         ToArea, Timestamp, Hour1, Hour2, Hour3, everything()) %>%
  arrange(desc(Timestamp))

# Exclude 3 outliers of negative capacity (might be other cases within each hour)
merged_capacity %<>% filter(Sum >= 0)

# Save file
merged_capacity %>% write_csv("data_merged/elbas_capacity_merged.csv")


# ==========================================================================================
# SOME EXPLORATORY PLOTS
# Outliers (view before excluding above)
merged_capacity %>% ggplot() + geom_point(aes(x = Date, y = Sum))

# Distribution
ggplot(merged_capacity) + geom_histogram(aes(x = Sum), binwidth = 1000)

# Total capacities into Finland
merged_capacity %>% 
  filter(ToArea == "FI") %>% 
  group_by(FromArea, Date) %>% 
  summarise(Capacity = sum(Sum)) %>%
  ggplot() +
    geom_line(aes(x = Date, y = Capacity)) +
    facet_wrap(c("FromArea"))

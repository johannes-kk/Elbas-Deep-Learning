library(tidyverse)
library(lubridate)
library(magrittr)

rm(list = ls())

par_dir <- "/Users/johannes/OneDrive/Master Thesis - ML/02 - Data/FTP/RAW/Operating_data"

columns_new <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Hour1", "Hour2", "Hour3A", "Hour3B", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Sum")

columns_old <- c("DataType", "Code", "Year", "Week", "Day", "Date", "Alias", "Hour1", "Hour2", "Hour3", "Hour4", "Hour5", "Hour6", "Hour7", "Hour8", "Hour9", "Hour10", "Hour11", "Hour12", "Hour13", "Hour14", "Hour15", "Hour16", "Hour17", "Hour18", "Hour19", "Hour20", "Hour21", "Hour22", "Hour23", "Hour24", "Sum")

# Parse all files in given years for a given country
parse_years <- function(country, years){
  for(year in years){
    temp_tib = tibble()
    files = list.files(path = paste0(par_dir, "/", country, "/", year), pattern = "*.sdv", full.names = T)
    for (i in 1:length(files)) {
      cat("\n", country, " - ", year, ": ", i, "/", length(files), sep = "")
      if (!is.na(as.numeric(year)) & year <= 2013) {
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
    temp_tib %<>% filter(DataType %in% c("FB", "PR", "OM", "PS", "UT"))
    assign(paste0("operating_", country, "_", year), temp_tib, envir = parent.frame())
    remove(temp_tib, file, files, i, year, cols)
  }
}

# Nordics have long history (in terms of data)
for (country in c("Sweden", "Norway", "Denmark", "Finland")) {
  parse_years(country, c(seq(2009, 2017), ""))
}

# Eastern countries' data starts in various years
parse_years("Estonia", c(seq(2011, 2017), ""))
parse_years("Lithuania", c(seq(2012, 2017), ""))
parse_years("Latvia", c(seq(2013, 2017), ""))

# Fix data types, which for some reason are all out of whack
unfuck <- function(tib){
  temp <- tib
  #print(max(temp$Year))
  if (max(temp$Year) <= 2013) {
    temp %<>% mutate(
      Year = as.integer(Year),
      Week = as.integer(Week),
      Day = as.integer(Day),
      Date = dmy(Date),
      Hour1 = as.numeric(gsub(",", ".", Hour1)),
      Hour2 = as.numeric(gsub(",", ".", Hour2)),
      Hour3 = as.numeric(gsub(",", ".", Hour3)),
      #Hour3A = as.numeric(gsub(",", ".", Hour3A)),
      #Hour3B = as.numeric(gsub(",", ".", Hour3B)),
      Hour4 = as.numeric(gsub(",", ".", Hour4)),
      Hour5 = as.numeric(gsub(",", ".", Hour5)),
      Hour6 = as.numeric(gsub(",", ".", Hour6)),
      Hour7 = as.numeric(gsub(",", ".", Hour7)),
      Hour8 = as.numeric(gsub(",", ".", Hour8)),
      Hour9 = as.numeric(gsub(",", ".", Hour9)),
      Hour10 = as.numeric(gsub(",", ".", Hour10)),
      Hour11 = as.numeric(gsub(",", ".", Hour11)),
      Hour12 = as.numeric(gsub(",", ".", Hour12)),
      Hour13 = as.numeric(gsub(",", ".", Hour13)),
      Hour14 = as.numeric(gsub(",", ".", Hour14)),
      Hour15 = as.numeric(gsub(",", ".", Hour15)),
      Hour16 = as.numeric(gsub(",", ".", Hour16)),
      Hour17 = as.numeric(gsub(",", ".", Hour17)),
      Hour18 = as.numeric(gsub(",", ".", Hour18)),
      Hour19 = as.numeric(gsub(",", ".", Hour19)),
      Hour20 = as.numeric(gsub(",", ".", Hour20)),
      Hour21 = as.numeric(gsub(",", ".", Hour21)),
      Hour22 = as.numeric(gsub(",", ".", Hour22)),
      Hour23 = as.numeric(gsub(",", ".", Hour23)),
      Hour24 = as.numeric(gsub(",", ".", Hour24)),
      Sum = as.numeric(gsub(",", ".", Sum))
    )
  }else{
    temp %<>% mutate(
      Year = as.integer(Year),
      Week = as.integer(Week),
      Day = as.integer(Day),
      Date = dmy(Date),
      Hour1 = as.numeric(gsub(",", ".", Hour1)),
      Hour2 = as.numeric(gsub(",", ".", Hour2)),
      #Hour3 = as.numeric(gsub(",", ".", Hour3)),
      Hour3A = as.numeric(gsub(",", ".", Hour3A)),
      Hour3B = as.numeric(gsub(",", ".", Hour3B)),
      Hour4 = as.numeric(gsub(",", ".", Hour4)),
      Hour5 = as.numeric(gsub(",", ".", Hour5)),
      Hour6 = as.numeric(gsub(",", ".", Hour6)),
      Hour7 = as.numeric(gsub(",", ".", Hour7)),
      Hour8 = as.numeric(gsub(",", ".", Hour8)),
      Hour9 = as.numeric(gsub(",", ".", Hour9)),
      Hour10 = as.numeric(gsub(",", ".", Hour10)),
      Hour11 = as.numeric(gsub(",", ".", Hour11)),
      Hour12 = as.numeric(gsub(",", ".", Hour12)),
      Hour13 = as.numeric(gsub(",", ".", Hour13)),
      Hour14 = as.numeric(gsub(",", ".", Hour14)),
      Hour15 = as.numeric(gsub(",", ".", Hour15)),
      Hour16 = as.numeric(gsub(",", ".", Hour16)),
      Hour17 = as.numeric(gsub(",", ".", Hour17)),
      Hour18 = as.numeric(gsub(",", ".", Hour18)),
      Hour19 = as.numeric(gsub(",", ".", Hour19)),
      Hour20 = as.numeric(gsub(",", ".", Hour20)),
      Hour21 = as.numeric(gsub(",", ".", Hour21)),
      Hour22 = as.numeric(gsub(",", ".", Hour22)),
      Hour23 = as.numeric(gsub(",", ".", Hour23)),
      Hour24 = as.numeric(gsub(",", ".", Hour24)),
      Sum = as.numeric(gsub(",", ".", Sum))
    )
  }
  #tib <<- temp
  return(temp)
}

# Can't for the life of me get an lapply to assign globally here, so a for-loop it is...
for (tib in apropos("operating_*")) {
  cat("\n", tib, "(begun)")
  assign(tib, unfuck(get(tib)))
}

# Merge country data
## c("Norway", "Sweden", "Denmark", "Finland", "Estonia", "Lithuania", "Latvia")
for (cntry in c("Norway", "Sweden", "Denmark", "Finland", "Estonia", "Lithuania", "Latvia")) {
  temp <- bind_rows(lapply(apropos(paste0("operating_", cntry, "_*")), get))
  temp %<>% mutate(Country = cntry)
  assign(paste0("merged_", cntry), temp)
  remove(temp)
}

# One tibble to rule them all, and in the darkness bind them
merged_operating <- bind_rows(mget(apropos("merged_*")))

# Sort and arrange
merged_operating %<>% 
  select(DataType, Code, Year, Week, Day, Date, Alias, Country, Hour1, Hour2, Hour3, everything()) %>%
  arrange(desc(Date))

# Save file
merged_operating %>% write_csv("data_merged/operating_merged.csv")

# ==========================================================================================
# SOME EXPLORATORY PLOTS
merged_operating %>% filter(DataType == "PR", Country == "Norway") %>% ggplot() + geom_line(aes(x = Date, y = Sum))

merged_operating %>% 
  filter(DataType == "PR") %>% 
  #group_by(Date, Country) %>% 
  ggplot() + geom_line(aes(x = Date, y = Sum)) + facet_wrap(c("Country"))

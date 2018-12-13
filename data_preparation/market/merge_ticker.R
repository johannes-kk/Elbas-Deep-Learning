library(tidyverse)
library(lubridate)
library(magrittr)

rm(list = ls())

# Parent directory
par_dir = "/Users/johannes/OneDrive/Master Thesis - ML/02 - Data/FTP/RAW/Elbas/Elbas_ticker_data"

# Format changes between 2014 November 25 and 26: more columns, different filename
## Handle 2014 separately

# Have to loop over each individual year, since number of columns changes between years
for (year in c(seq(2010, 2013), seq(2015, 2017), "")) {
  temp_tib = tibble()
  files = list.files(path = paste0(par_dir, "/", year), pattern = "*.csv", full.names = T)
  for (i in 1:length(files)) {
    cat("\nYear ", year, ": ", i, "/", length(files), sep = "")
    suppressMessages(file <- read_csv(files[i]))
    temp_tib = rbind(temp_tib, file)
  }
  assign(paste0("merge_", year), temp_tib)
  remove(temp_tib, file, files, i, year)
}

# Handle 2014 separately since format changes in middle of the year
merge_2014_1 = tibble()
merge_2014_2 = tibble()
files_2014 = list.files(path = paste0(par_dir, "/2014"), pattern = "*.csv", full.names = T)
for (i in 1:length(files_2014)) {
  suppressMessages(file <- read_csv(files_2014[i]))
  tryCatch({
    merge_2014_1 <<- rbind(merge_2014_1, file)
  }, error = function(e) {
    # Have to do super-assign in error handling to avoid updating a locally scoped copy
    merge_2014_2 <<- rbind(merge_2014_2, file)
  }, finally = {
    cat("\nYear 2014: ", i, "/", length(files_2014), sep = "")
  }
  )
}
remove(files_2014, i, file)

# Merge and extract DeliveryDate + DeliveryHour from each half of dataset
## Format <= 2014: PH01140101 (HHYYMMDD)
merged2 <- bind_rows(merge_, merge_2017, merge_2016, merge_2015, merge_2014_2) %>%
  mutate(DeliveryDate = ymd(substring(`Product Code`, 4, 11)), 
         DeliveryHour = as.numeric(substring(`Product Code`, 13, 14)))
## Format >= 2015: PH-20180101-13 (YYYYMMDD-HH)
merged1 <- bind_rows(merge_2014_1, merge_2013, merge_2012, merge_2011, merge_2010) %>% 
  mutate(DeliveryDate = ymd(substring(`Product Code`, 5, 12)), 
         DeliveryHour = as.numeric(substring(`Product Code`, 3, 4)))

# Combine, starting with largest first so merged1 has empty columns rather than merge2 missing its
merged <- bind_rows(merged2, merged1)

# Strip whitespace from name
names(merged) = gsub(" ", "", names(merged))

# Merge the two SArea fields (typo in one); no cases where both or none is NA
merged %<>% 
  mutate(SArea_NEW = coalesce(SArea, Sarea)) %>% 
  select(-Sarea, -SArea) %>% 
  rename(SArea = SArea_NEW)

# Change character to datetime
merged %<>% mutate(TradeTime = ymd_hms(TradeTime, tz = "UTC"))
# Adding +1, since e.g. 15:11:09 is TradeHour = 14
merged %<>% mutate(TradeDate = as_date(TradeTime), 
                   TradeHour = hour(TradeTime) + 1)

# Reorder
merged %<>% select(TradeTime, TradeDate, TradeHour, DeliveryDate, DeliveryHour, Price, QTY, BArea, SArea, everything())

# Arrange
merged %<>% arrange(desc(DeliveryDate), desc(DeliveryHour))

# Save file
merged %>% write_csv("data_merged/elbas_ticker_merged.csv")

# ==========================================================================================
# SOME EXPLORATORY PLOTS

# Total volume over time
merged %>% group_by(TradeDate) %>% summarise(Volume = sum(QTY)) %>% arrange(TradeDate) %>%
  ggplot() + geom_line(aes(x = TradeDate, y = Volume))

# Volume over time faceted by BAreas: seems area codes have changed. Some should be merged?
merged %>% group_by(TradeDate, BArea) %>% summarise(Volume = sum(QTY)) %>%
  ggplot() + geom_line(aes(x = TradeDate, y = Volume)) + facet_wrap(c("BArea"))

# Clearly some outliers here
ggplot(merged) +
  geom_histogram(aes(x = Price), binwidth = 10)
table(cut(merged$Price, c(-250, -50, 0, 50, 100, 250, 500, 1000, 3000)))

# Distribution of Price when ignoring suspected outliers
merged %>% filter(Price > 0 & Price < 100) %>% ggplot() + geom_histogram(aes(Price), binwidth = 2)

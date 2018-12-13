library(plyr)
library(tidyverse)
library(lubridate)
library(magrittr)

rm(list = ls())

# Takes merged long files and combine into a wide format

# ------------------------------------------------------------------------------------
# IMPORT ALL MERGED RAW DATA
ticker_raw <- read_csv("data_merged/elbas_ticker_merged.csv")
capacity_raw <- read_csv("data_merged/elbas_capacity_merged.csv")
elspot_raw <- read_csv("data_merged/elspot_file_merged.csv")
operating_raw <- read_csv("data_merged/operating_merged.csv")
regulating_raw <- read_csv("data_merged/regulating_price_merged.csv")
elspot_cap_raw <- read_csv("data_merged/elspot_capacity_merged.csv")
load("data_merged/UMM_Total.RData")
umm_raw <- as_tibble(UMM_TOTAL)
rm(UMM_TOTAL)

# Create complete timeline in period to join in and identify gaps
timeline <- tibble(DeliveryTime = seq(ymd_h("2011-11-02-00"), ymd_h("2017-12-31-24"), by = "1 hour")) %>% 
  mutate(
    DeliveryHour = ifelse(hour(DeliveryTime) == 0, 24, hour(DeliveryTime)),
    DeliveryDate = as_date(ifelse(DeliveryHour == 24, date(DeliveryTime) - days(1), date(DeliveryTime)))
  )



# ------------------------------------------------------------------------------------
# ELBAS TICKER
# Adjusting timezones and daylight savings
elbas_ticker_merged <- as.data.frame(ticker_raw)

# FORMAT 1: CET! OK!
elbas.tz.format1 <- elbas_ticker_merged[elbas_ticker_merged$TradeDate < "2014-11-19", ]

# FORMAT 2:
elbas.tz.format2 <- elbas_ticker_merged[elbas_ticker_merged$TradeDate > "2014-11-18", ]

# FIX DST IN FORMAT 2:
elbas.tz.format2$Group <- ifelse(elbas.tz.format2$TradeTime <= "2015-03-29T02:00:00Z", "Group 2: UTC +01:00",
                                 ifelse(elbas.tz.format2$TradeTime > "2015-03-29T02:00:00Z" & elbas.tz.format2$TradeTime <= "2015-10-25T03:00:00Z", "Group 3: UTC +02:00",
                                        ifelse(elbas.tz.format2$TradeTime > "2015-10-25T03:00:00Z" & elbas.tz.format2$TradeTime <= "2016-03-27T02:00:00Z", "Group 2: UTC +01:00",
                                               ifelse(elbas.tz.format2$TradeTime > "2016-03-27T02:00:00Z" & elbas.tz.format2$TradeTime <= "2016-10-30T03:00:00Z", "Group 3: UTC +02:00",
                                                      ifelse(elbas.tz.format2$TradeTime > "2016-10-30T03:00:00Z" & elbas.tz.format2$TradeTime <= "2017-03-26T02:00:00Z", "Group 2: UTC +01:00",
                                                             ifelse(elbas.tz.format2$TradeTime > "2017-03-26T02:00:00Z" & elbas.tz.format2$TradeTime <= "2017-10-29T03:00:00Z", "Group 3: UTC +02:00", "Group 2: UTC +01:00"))))))

# columns to help adjusting:
elbas.tz.format2$TZ_hour_adjustment <- ifelse(elbas.tz.format2$Group == "Group 2: UTC +01:00", 1, 2)
elbas.tz.format2$TZ_date_adjustment_g2 <- ifelse(elbas.tz.format2$Group == "Group 2: UTC +01:00" & elbas.tz.format2$TradeHour == 24, 1, 0)

elbas.tz.format2$TZ_date_adjustment_g3 <- ifelse(elbas.tz.format2$Group == "Group 3: UTC +02:00" & elbas.tz.format2$TradeHour %in% c(23, 24), 1, 0)

# adjustments:
elbas.tz.format2$TZ_hour_new <- ifelse((elbas.tz.format2$TradeHour + elbas.tz.format2$TZ_hour_adjustment) == 25, 1,
                                       ifelse((elbas.tz.format2$TradeHour + elbas.tz.format2$TZ_hour_adjustment) == 26, 2, elbas.tz.format2$TradeHour + elbas.tz.format2$TZ_hour_adjustment))

elbas.tz.format2$TZ_date_new <- ifelse(elbas.tz.format2$TZ_date_adjustment_g2 == 1 | elbas.tz.format2$TZ_date_adjustment_g3 == 1, elbas.tz.format2$TradeDate + 1, elbas.tz.format2$TradeDate) 

elbas.tz.format2$TZ_date_new <- as.Date(elbas.tz.format2$TZ_date_new, origin = "1970-01-01")

# FINAL:
elbas.tz.adjusted <- elbas.tz.format2
elbas.tz.adjusted$TradeDate <- elbas.tz.adjusted$TZ_date_new
elbas.tz.adjusted$TradeHour <- elbas.tz.adjusted$TZ_hour_new
elbas.tz.adjusted$TZ_values <- NULL; elbas.tz.adjusted$include <- NULL; elbas.tz.adjusted$Group <- NULL
elbas.tz.adjusted$TZ_hour_adjustment <- NULL; elbas.tz.adjusted$TZ_date_adjustment_g2 <- NULL; elbas.tz.adjusted$TZ_date_adjustment_g3 <- NULL
elbas.tz.adjusted$TZ_hour_new <- NULL; elbas.tz.adjusted$TZ_date_new <- NULL

elbas.tz.format1$TZ_values <- NULL
elbas.tz.format1$include <- NULL

elbas.tz.adjusted.final <- rbind(elbas.tz.format1,
                                 elbas.tz.adjusted)

ticker <- elbas.tz.adjusted.final %>% as_tibble()
# ------------------------------------------------------------------

#ticker <- ticker_raw

# Filter to cancelled != 1
ticker %>% count(Cancelled)
ticker %<>% filter(coalesce(Cancelled, as.integer(0)) != 1)

# Limit to Nordic buyer aras
ticker %<>% filter(BArea %in% c("FI", "DK2", "SE2", "DK1", "SE3", "NO1", "NO3", "SE4", "SE1", "NO2", "NO4", "NO5", "FIN", "SWE"))

# Exclude seller areas outside Nordics and Germany
ticker %<>% filter(!(SArea %in% c("LT", "LV", "EE", "BE", "NL")))

# Remove some now redundant columns
ticker %<>% select(-ProductCode, -Cancelled, -Currency)

# Calculate difference in time between trading and delivery (rounding an hour up)
ticker %<>% mutate(
  TradeHour = as.integer(TradeHour),
  HoursDiff = as.integer(
    difftime(
      ymd_h(paste0(DeliveryDate, "-", DeliveryHour)), 
      ymd_h(paste0(TradeDate, "-", TradeHour)),
      units = "hours")),
  #HoursDiff = as.integer(ymd_h(paste0(DeliveryDate, "-", DeliveryHour)) - ymd_h(paste0(TradeDate, "-", TradeHour))),
  DeliveryTime = ymd_h(paste0(DeliveryDate, "-", DeliveryHour))
)

# Create buckets for how far in advance it was traded
## Buckets: [0, 1, 2, 3, 4, 5], [6, 7, 8, 9, 10, 11, 12], [13 -23], [24 - Infinity)
ticker %<>% mutate(Window = cut(HoursDiff, breaks = c(1, 3, 5, 7, 12, Inf), right = F))
ticker %<>% mutate(Window2 = cut(HoursDiff, breaks = c(1, 7, Inf), right = F))
# Rename factor levels to make it easier later
ticker %<>% mutate(Window2New = revalue(Window2, c("[1,7)" = "Near", "[7,Inf)" = "Far")))

# Reorder and arrange
ticker %<>% 
  select(DeliveryTime, DeliveryDate, DeliveryHour, TradeTime, TradeDate, 
         TradeHour, HoursDiff, Window, Window2, Window2New, BArea, SArea, everything()) %>% 
  arrange(DeliveryTime)

# Aggregate by DeliveryDate + DeliveryHour
ticker_total <- ticker %>%
  group_by(DeliveryTime, DeliveryDate, DeliveryHour, Window2New) %>% 
  summarise(Volume = round(sum(QTY), 3), Total = sum(Price*QTY), VWP = round(Total/Volume, 3)) %>%
  ungroup() %>%
  rename(Win = Window2New)

# Spread it out
ticker_total_wide <- ticker_total %>%
  select(DeliveryTime, Win, VWP) %>%
  # First spread volume weighted prices
  spread(key = Win, value = VWP, sep = "_VWP_") %>%
  # Then spread volumes
  left_join(
    ticker_total %>% select(DeliveryTime, Win, Volume) %>% spread(key = Win, value = Volume, sep = "_Volume_"),
    by = "DeliveryTime"
  ) %>%
  arrange(DeliveryTime) %>%
  # Create two lags: at DeliveryHour 14, we only "know" the VWP of the last 6 hours of trading for hour 7
  mutate(
    Win_VWP_Near_Lag = lag(Win_VWP_Near, 6),
    Win_Volume_Near_Lag = lag(Win_Volume_Near, 6)
  )

# Wide version matching timeline
ticker_wide <- timeline %>%
  left_join(ticker_total_wide, by = "DeliveryTime") %>%
  mutate(MissingTicker = ifelse(is.na(Win_VWP_Near) & is.na(Win_VWP_Far), 1, 0))

# Wide version
# ticker_wide <- timeline %>%
#   left_join(
#     ticker_total %>% select(DeliveryTime, Win, VWP) %>% spread(key = Win, value = VWP, sep = "_VWP_"),
#     by = "DeliveryTime"
#   ) %>%
#   left_join(
#     ticker_total %>% select(DeliveryTime, Win, Volume) %>% spread(key = Win, value = Volume, sep = "_Volume_"),
#     by = "DeliveryTime"
#   ) %>%
#   mutate(MissingTicker = ifelse(is.na(Win_VWP_Near) & is.na(Win_VWP_Far), 1, 0))
# 
rm(ticker, ticker_raw, ticker_total, elbas.tz.adjusted, elbas.tz.format1, 
   elbas.tz.format2, elbas.tz.adjusted.final, elbas_ticker_merged, ticker_total_wide)



# ------------------------------------------------------------------------------------
# ELSPOT
elspot <- elspot_raw

# Limit to Nordic countries
elspot %<>% filter(
    Alias %in% c("DK1", "DK2", "FI", "NO1", "NO2", "NO3", "NO4", "NO5", "SE1", "SE2", "SE3", "SE4", "SP1"),
    Unit %in% c("EUR", "MWh/h"),
    Code != "SF",
    Date >= ymd("2010-01-01")
  )

# Fix some wrong data types, then combine Hour3A+Hour3B into Hour3
elspot %<>% 
  mutate(Hour3 = as.numeric(Hour3), 
         Hour3B = as.numeric(Hour3B),
         # Per row, set Hour3=Hour3 if exists, =mean(Hour3A+Hour3B) if both exist, =either if one, =mean(Hour2+Hour4) if neither
         Hour3 = coalesce(Hour3, round((Hour3A+Hour3B)/2, 2), Hour3A, Hour3B, round((Hour2+Hour4)/2, 2))) %>%
  select(-Hour3A, -Hour3B)
  
# "Transpose" dataset
elspot_long <- elspot %>% 
  select(-Total) %>%
  gather(Hour1, Hour2, Hour3, Hour4, Hour5, Hour6, Hour7, Hour8, Hour9, Hour10, Hour11, Hour12, Hour13, Hour14, 
         Hour15, Hour16, Hour17, Hour18, Hour19, Hour20, Hour21, Hour22, Hour23, Hour24, key = "Hour", value = "Value") 

elspot_long %<>% 
  rename(DeliveryDate = Date) %>% 
  mutate(DeliveryHour = str_extract(Hour, "[0-9]+") %>% as.numeric(.)) %>%
  select(-c(Hour, Day, Week, Year))

elspot_wide_price <- elspot_long %>%
  filter(DataType == "PR") %>%
  mutate(Derp = paste0("Spot_", DataType, "_", Alias)) %>%
  select(DeliveryDate, DeliveryHour, Value, Derp) %>%
  spread(key = Derp, value = Value)

elspot_wide_turnover <- elspot_long %>%
  filter(DataType == "OM") %>%
  mutate(Type = ifelse(Code == "SK", "Buy", "Sell")) %>%
  mutate(Derp = paste0("Spot_", DataType, "_", Type, "_", Alias)) %>%
  select(DeliveryDate, DeliveryHour, Value, Derp) %>%
  spread(key = Derp, value = Value)

# No need for a missing flag here, since every PowerHour (DeliveryDate+DeliveryHour) is represented
elspot_wide <- timeline %>%
  left_join(elspot_wide_price, by = c("DeliveryHour", "DeliveryDate")) %>%
  left_join(elspot_wide_turnover, by = c("DeliveryHour", "DeliveryDate")) %>%
  select(-DeliveryDate, -DeliveryHour)

rm(elspot_wide_turnover, elspot_wide_price, elspot, elspot_long, elspot_raw)



# ------------------------------------------------------------------------------------
# ELBAS CAPACITY
capacity <- capacity_raw

# Filter to Nordics + Germany
capacity %<>% filter(
  FromArea %in% c("NO1", "NO1A", "NO2", "NO3", "NO4", "NO5", "DK1", "DK1A", "DK2", "FI", "SE1", "SE2", "SE3", "SE4"),
  ToArea %in% c("NO1", "NO1A", "NO2", "NO3", "NO4", "NO5", "DK1", "DK1A", "DK2", "FI", "SE1", "SE2", "SE3", "SE4")
) %>% filter(
  !(FromArea %in% c("NO3", "NO5") & ToArea %in% c("NO3", "NO5"))
)

# Time-component of timestamp is virtually always the same: can drop the variable
#capacity %>% mutate(tiem = hour(Timestamp)) %>% ggplot() + geom_histogram(aes(x = tiem), binwidth = 1)

# Only one level of DataType and Code. Drop along with other columns we don't need
capacity %<>% select(-c(DataType, Code, Year, Week, Day, FromArea, ToArea, Sum))

# Merge Hour3A+Hour3B into Hour3
capacity %<>% 
  mutate(Hour3 = as.numeric(Hour3), 
         Hour3B = as.numeric(Hour3B),
         # Per row, set Hour3=Hour3 if exists, =mean(Hour3A+Hour3B) if both exist, =either if one, =mean(Hour2+Hour4) if neither
         Hour3 = coalesce(Hour3, round((Hour3A+Hour3B)/2, 2), Hour3A, Hour3B, round((Hour2+Hour4)/2, 2))) %>%
  select(-Hour3A, -Hour3B)

# Total of 43 rows where at least one column is missing a value
#View(capacity %>% filter_all(any_vars(is.na(.))))

# Impute 0 for NAs in Hourly capacities; only HourX's contain NAs
## When all Hours are missing, Sum = 0.0 already, so reasonable to replace NA with 0
capacity %<>% replace(is.na(.), 0.0)

# Transpose long: HourX into a DeliveryHour column
capacity_long <- capacity %>% 
  gather(Hour1, Hour2, Hour3, Hour4, Hour5, Hour6, Hour7, Hour8, Hour9, Hour10, Hour11, Hour12, Hour13, Hour14, 
         Hour15, Hour16, Hour17, Hour18, Hour19, Hour20, Hour21, Hour22, Hour23, Hour24, key = "Hour", value = "Value") %>%
  rename(DeliveryDate = Date) %>%
  mutate(DeliveryHour = str_extract(Hour, "[0-9]+") %>% as.numeric(.)) %>%
  select(-Hour)

# There are 2,784 cases where there are multiple rows for the same Alias+DeliveryDate+DeliveryHour
## Seems to be an update or correction of capacity: limit to the latest Timestamp for each Alias+DeliveryDate+DeliveryHour
capacity_long %<>%
  group_by(DeliveryDate, DeliveryHour, Alias) %>%
  top_n(1, Timestamp) %>%
  ungroup()

# Transpose wide by Alias (instead of original Hour), combine with full timeline, and flag missing PowerHours
capacity_wide <- capacity_long %>%
  mutate(Alias = paste0("Cap_", Alias)) %>%
  select(DeliveryDate, DeliveryHour, Alias, Value) %>%
  spread(key = Alias, value = Value) %>%
  # Flag for whether capacity data is missing given Power Hour (6,553 are missing)
  mutate(MissingCap = 0) %>%
  right_join(timeline, by = c("DeliveryDate", "DeliveryHour")) %>%
  replace_na(list(MissingCap = 1)) %>%
  select(-DeliveryDate, -DeliveryHour)

rm(capacity, capacity_raw, capacity_long)



# ------------------------------------------------------------------------------------
# OPERATING
operating <- operating_raw

# Filter and fix data types
operating %<>% 
  filter(
    Country %in% c("Norway", "Sweden", "Denmark", "Finland"),
    Code %in% c("E", "PE")) %>%
  # Convert data types and merge Hour3A+Hour3B into Hour3
  mutate(
    Hour3 = as.numeric(Hour3), 
    Hour3B = as.numeric(Hour3B),
    Hour3 = coalesce(Hour3, round((Hour3A+Hour3B)/2, 2), Hour3A, Hour3B, round((Hour2+Hour4)/2, 2)),
    # Change E code to CE to make our soon-to-be variable names more intuitive
    Code = replace(Code, Code == "E", "CE"),
    # PE and CE are estimates for that hour the next day, so shift DeliveryDate ahead by one day (NEVERMIND!)
    #DeliveryDate = Date + days(1)
    # Rename areas to ones consistent across all data
    Alias = replace(Alias, Alias == "JY", "DK1"),
    Alias = replace(Alias, Alias == "SJ", "DK2"),
    Col = paste0("Op_", Code, "_", Alias)) %>%
  filter(Alias != "SE", Col != "Op_PE_NO") %>%
  rename(DeliveryDate = Date) %>%
  # Remove some redundant columns
  select(DeliveryDate, Col, everything(), -c(Year, Week, Day, Hour3A, Hour3B, Sum, 
                                             DataType, Country, Code, Alias))

# Make long by transposing hours long rather than wide
operating_long <- operating %>%
  gather(Hour1, Hour2, Hour3, Hour4, Hour5, Hour6, Hour7, Hour8, Hour9, Hour10, Hour11, Hour12, Hour13, Hour14, 
         Hour15, Hour16, Hour17, Hour18, Hour19, Hour20, Hour21, Hour22, Hour23, Hour24, key = "Hour", value = "Value") %>%
  mutate(DeliveryHour = str_extract(Hour, "[0-9]+") %>% as.numeric(.)) %>%
  select(-Hour) %>%
  # 1,344 observations are exact duplicates: would prevent widening since no unique identifier then
  distinct(DeliveryDate, DeliveryHour, Col, Value)

# Widen by Area+EstimateType columns over complete timeline
## No missing data now
operating_wide <- operating_long %>%
  spread(key = Col, value = Value) %>%
  select(DeliveryDate, DeliveryHour, everything()) %>%
  right_join(timeline, by = c("DeliveryDate", "DeliveryHour")) %>%
  select(-DeliveryDate, -DeliveryHour)

# Pretty plot
operating_long %>% 
  mutate(DeliveryTime = ymd_h(paste0(DeliveryDate, "-", DeliveryHour))) %>% 
  filter(DeliveryDate >= ymd("2011-11-02")) %>%
  group_by(Col) %>% 
  ggplot() + geom_line(aes(x = DeliveryTime, y = Value)) + facet_wrap("Col")

rm(operating, operating_raw, operating_long)



# ------------------------------------------------------------------------------------
# REGULATING
regulating <- regulating_raw

# Filter
regulating %<>% 
  filter(!(Alias %in% c("DK1", "DK2"))) %>%
  mutate(
    # Rename JY > DK1 and SJ > DK2; identical where both exist, but JY and SJ go further back in time
    Alias = replace(Alias, Alias == "JY", "DK1"),
    Alias = replace(Alias, Alias == "SJ", "DK2"),
    # Fix data types
    Hour3 = as.numeric(Hour3), 
    Hour3B = as.numeric(Hour3B),
    # Combine Hour3A+Hour3B into Hour3
    Hour3 = coalesce(Hour3, round((Hour3A+Hour3B)/2, 2), Hour3A, Hour3B, round((Hour2+Hour4)/2, 2)),
    # Combine Alias and Code into (future) column name
    Col = paste0("Reg_", Code, "_", Alias)) %>%
  filter(Alias != "SE") %>%
  rename(DeliveryDate = Date) %>%
  select(DeliveryDate, Col, everything(), -c(DataType, Code, Year, Week, Day, Alias, Average, Hour3A, Hour3B))

# Transpose Hours into rows, then Alias+Code into columns
regulating_wide <- regulating %>%
  gather(Hour1, Hour2, Hour3, Hour4, Hour5, Hour6, Hour7, Hour8, Hour9, Hour10, 
         Hour11, Hour12, Hour13, Hour14, Hour15, Hour16, Hour17, Hour18, Hour19, 
         Hour20, Hour21, Hour22, Hour23, Hour24, key = "DeliveryHour", value = "Value") %>%
  mutate(DeliveryHour = str_extract(DeliveryHour, "[0-9]+") %>% as.numeric(.),
         # Combine DeliveryHour and DeliveryDate into DeliveryTime
         ## Then delay it by two hours: regulating data for a given PowerHour is only available a posterior
         DeliveryTime = ymd_h(paste0(DeliveryDate, "-", DeliveryHour)) - hours(8)) %>%
  select(-c(DeliveryDate, DeliveryHour)) %>%
  spread(key = Col, value = Value) %>%
  # Data only available from mid-2010, so some is missing along timeline
  mutate(MissingReg = 0) %>%
  right_join(timeline, by = c("DeliveryTime")) %>%
  replace_na(list(MissingReg = 1)) %>%
  select(DeliveryTime, MissingReg, everything(), -c(DeliveryDate, DeliveryHour)) %>%
  mutate(
    Reg_DD_DK1 = ifelse(abs(Reg_DD_DK1) < 1, 0, Reg_DD_DK1),
    Reg_DD_DK2 = ifelse(abs(Reg_DD_DK1) < 1, 0, Reg_DD_DK2),
    Reg_DD_FI = ifelse(abs(Reg_DD_FI) < 1, 0, Reg_DD_FI),
    Reg_DD_NO1 = ifelse(abs(Reg_DD_NO1) < 1, 0, Reg_DD_NO1),
    Reg_DD_NO2 = ifelse(abs(Reg_DD_NO2) < 1, 0, Reg_DD_NO2),
    Reg_DD_NO3 = ifelse(abs(Reg_DD_NO3) < 1, 0, Reg_DD_NO3),
    Reg_DD_NO4 = ifelse(abs(Reg_DD_NO4) < 1, 0, Reg_DD_NO4),
    Reg_DD_NO5 = ifelse(abs(Reg_DD_NO5) < 1, 0, Reg_DD_NO5),
    Reg_DD_SE1 = ifelse(abs(Reg_DD_SE1) < 1, 0, Reg_DD_SE1),
    Reg_DD_SE2 = ifelse(abs(Reg_DD_SE2) < 1, 0, Reg_DD_SE2),
    Reg_DD_SE3 = ifelse(abs(Reg_DD_SE3) < 1, 0, Reg_DD_SE3),
    Reg_DD_SE4 = ifelse(abs(Reg_DD_SE4) < 1, 0, Reg_DD_SE4)
  )

rm(regulating, regulating_raw)



# ------------------------------------------------------------------------------------
# ELSPOT CAPACITY
elspot_cap <- elspot_cap_raw

# Limit to Exchange Capacity (UE) and Nordics
elspot_cap %<>% filter(
  DataType == "UE",
  FromArea %in% c("NO1", "NO1A", "NO2", "NO3", "NO4", "NO5", "DK1", "DK1A", "DK2", "FI", "SE1", "SE2", "SE3", "SE4"),
  ToArea %in% c("NO1", "NO1A", "NO2", "NO3", "NO4", "NO5", "DK1", "DK1A", "DK2", "FI", "SE1", "SE2", "SE3", "SE4")
) %>% filter(
  # The NO3_NO5 line was built so that the sequential training data contains only NAs, which breaks sklearn
  !(FromArea %in% c("NO3", "NO5") & ToArea %in% c("NO3", "NO5"))
)

# Fix data types, merge Hour3, create wide dataset column names
elspot_cap %<>% 
  mutate(
    Hour3 = as.numeric(Hour3),
    Hour3B = as.numeric(Hour3B),
    # Combine Hour3A+Hour3B into Hour3
    Hour3 = coalesce(Hour3, round((Hour3A+Hour3B)/2, 2), Hour3A, Hour3B, round((Hour2+Hour4)/2, 2)),
    Col = paste0("Spot_", DataType, "_", Alias)
  ) %>%
  rename(DeliveryDate = Date) %>%
  select(DeliveryDate, Col, everything(), -c(DataType, Code, Year, Week, Day, FromArea, ToArea, Alias, Hour3A, Hour3B, Sum))

# Transpose: first Hours into long, then Aliases/Cols to wide
elspot_cap_wide <- elspot_cap %>%
  # Make hours long
  gather(Hour1, Hour2, Hour3, Hour4, Hour5, Hour6, Hour7, Hour8, Hour9, Hour10, Hour11, Hour12, Hour13, Hour14, 
         Hour15, Hour16, Hour17, Hour18, Hour19, Hour20, Hour21, Hour22, Hour23, Hour24, key = "DeliveryHour", value = "Value") %>%
  mutate(DeliveryHour = str_extract(DeliveryHour, "[0-9]+") %>% as.numeric(.)) %>%
  # Make Aliases wide
  spread(key = Col, value = Value) %>%
  # There are 1,729 DeliveryHours missing in the timeline
  ## FIXED BY USING 25H DATA FOR 2014 RATHER THAN 24H
  #mutate(MissingSpotCap = 0) %>%
  right_join(timeline, by = c("DeliveryDate", "DeliveryHour")) %>%
  #replace_na(list(MissingSpotCap = 1)) %>%
  select(-DeliveryDate, -DeliveryHour)

rm(elspot_cap, elspot_cap_raw)



# ------------------------------------------------------------------------------------
# Urgent Market Messages (UMMs)
umm <- umm_raw %>%
  rename(
    UMM_Tot_Cnt = Count_all,
    UMM_PE_Vol = Volume_prod,
    UMM_PE_Cnt = Count_prod,
    UMM_CE_Vol = Volume_cons,
    UMM_CE_Cnt = Count_cons
  ) %>% right_join(
    timeline, by = c("DeliveryDate", "DeliveryHour")
  ) %>%
  arrange(
    DeliveryTime
  ) %>%
  select(
    DeliveryTime, everything(), -c(DeliveryDate, DeliveryHour)
  ) %>%
  replace(is.na(.), 0)



# ------------------------------------------------------------------------------------
# COMBINE EVERYTHING
merged_wide <- ticker_wide %>%
  left_join(elspot_wide, by = "DeliveryTime") %>%
  left_join(capacity_wide, by = "DeliveryTime") %>%
  left_join(operating_wide, by = "DeliveryTime") %>%
  left_join(regulating_wide, by = "DeliveryTime") %>%
  left_join(elspot_cap_wide, by = "DeliveryTime") %>%
  left_join(umm, by = "DeliveryTime") %>%
  # Easier to merge in DeliveryMonth and WeekDay here
  mutate(
    #DeliveryYear = year(DeliveryDate),
    DeliveryMonth = month(DeliveryDate),
    DeliveryWeekday = wday(DeliveryDate),
    # Combine NO1 and NO1A to create continuous timeline
    ## Otherwise a lot of variable-pairs are missing 49.4% and 50.6% respectively
    Cap_NO5_NO1 = coalesce(Cap_NO5_NO1, Cap_NO5_NO1A),
    Cap_NO1_NO5 = coalesce(Cap_NO1_NO5, Cap_NO1A_NO5),
    Cap_NO2_NO1 = coalesce(Cap_NO2_NO1, Cap_NO2_NO1A),
    Cap_NO1_NO2 = coalesce(Cap_NO1_NO2, Cap_NO1A_NO2),
    Spot_UE_NO5_NO1 = coalesce(Spot_UE_NO5_NO1, Spot_UE_NO5_NO1A),
    Spot_UE_NO1_NO5 = coalesce(Spot_UE_NO1_NO5, Spot_UE_NO1A_NO5),
    Spot_UE_NO2_NO1 = coalesce(Spot_UE_NO2_NO1, Spot_UE_NO2_NO1A),
    Spot_UE_NO1_NO2 = coalesce(Spot_UE_NO1_NO2, Spot_UE_NO1A_NO2)
  ) %>%
  mutate(
    # Note: these numbers are if the timeline starts with 2010-01-01
    # There are 540 missing output values. We can't have gaps in timeline, so have to impute
    ## Replacing cases where lead(out) and lag(out) exist with their average solves 217 of these cases
    Win_VWP_Near = coalesce(Win_VWP_Near, round((lead(Win_VWP_Near)+lag(Win_VWP_Near))/2, 2)),
    Win_Volume_Near = coalesce(Win_Volume_Near, round((lead(Win_Volume_Near)+lag(Win_Volume_Near))/2, 2)),
    # Replace NA lagged VWP with the now imputed VWP in cases where lead() and lag() VWP exist
    Win_VWP_Near_Lag = coalesce(Win_VWP_Near_Lag, lag(Win_VWP_Near, 6)),
    Win_Volume_Near_Lag = coalesce(Win_Volume_Near_Lag, lag(Win_Volume_Near, 6))
  ) %>%
  # Order: DeliveryTime (index/grain), output (Win_VWP_Near), DeliveryDate (to be dropped), DeliveryHour, the rest
  ## The remaining Delivery.+ features must be one-hot-encoded
  ## Drop some that don't make sense to include: POSIXct-dates (numerics only), Win_Volume_Near (cheating!)
  select(DeliveryTime, Win_VWP_Near, DeliveryDate, DeliveryHour, DeliveryMonth, DeliveryWeekday,
         # The dominating direction indicators need to be one-hot-encoded alongside datetime info
         Reg_DD_DK1, Reg_DD_DK2, Reg_DD_FI, Reg_DD_NO1, Reg_DD_NO2, Reg_DD_NO3, 
         Reg_DD_NO4, Reg_DD_NO5, Reg_DD_SE1, Reg_DD_SE2, Reg_DD_SE3, Reg_DD_SE4,
         everything(), -c(MissingTicker, MissingCap, MissingReg, Win_Volume_Near,
                          # Drop all NO1A variables as they have been merged with NO1 (continuous timeline)
                          Cap_NO1_NO1A, Cap_NO1A_NO1, Cap_NO5_NO1A, Cap_NO1A_NO5, Cap_NO2_NO1A, Cap_NO1A_NO2,
                          Spot_UE_NO5_NO1A, Spot_UE_NO1A_NO5, Spot_UE_NO2_NO1A, Spot_UE_NO1A_NO2,
                          Spot_UE_NO1_NO1A, Spot_UE_NO1A_NO1)
  ) %>%
  # Limit to after Nov 17 2010, since a lot of data is missing before that
  # By doing that, we lose 1,056 observations when dropping DeliveryDates with >0 missing outputs (among its 24)
  #filter(DeliveryDate >= ymd("2010-11-17")) %>%
  filter(DeliveryDate >= ymd("2011-11-02")) %>%
  # Arrange by ascending DeliveryTime, just to be absolute sure it's right
  arrange(DeliveryTime)

# Drop DeliveryDates with at least one missing output (649 observations)
merged_wide %<>%
  filter(!(DeliveryDate %in% (merged_wide %>% 
                                filter(is.na(Win_VWP_Near)) %>% 
                                select(DeliveryDate) %>% 
                                unique() %>% 
                                pull(DeliveryDate)))
  ) %>%
  # Drop some variables we no longer need/want
  select(-c(DeliveryDate))

# Sparsity in terms of NAs
## A lot of these are because areas have been renamed, e.g. SE split into SE1, SE2, etc.
nas <- merged_wide %>% 
  summarise_all(funs(sum(is.na(.)))) %>%
  select_if(. > 0) %>%
  gather(key = "variable", value = "NAs") %>%
  mutate(ratio = round(100*(NAs / nrow(merged_wide)), 1)) %>%
  arrange(desc(NAs))

# View(nas)

# # Plot variable sparsities above 5% (in terms of ratio of NAs)
# nas %>% filter(ratio > 5) %>%
#   ggplot() + 
#   geom_col(aes(x = reorder(variable, ratio), y = ratio)) + 
#   coord_flip() + 
#   labs(
#     title = "Variable sparsity (ratio of NAs)",
#     y = "Sparsity",
#     x = ""
#   )

# # NO1-NO2 and NO1A-NO2 vs time
# ggplot(merged_wide) + 
#   geom_line(aes(x = DeliveryTime, y = Cap_NO1_NO2), col = "darkblue") +
#   geom_line(aes(x = DeliveryTime, y = Cap_NO1A_NO2), col = "black") +

# Drop features with high sparsity
## In practice only drops 6 variables with 100% NAs
merged_wide %<>% select(-one_of(nas %>% filter(ratio > 50.0) %>% pull(variable)))

# Create "missing" flag for all features
#for (colname in names(merged_wide)[7:ncol(merged_wide)]) {
for (colname in pull(nas %>% filter(ratio < 50.0), variable)) {
  merged_wide[, paste0("miss_", colname)] = as.integer(is.na(pull(merged_wide, colname)))
}

# Save data
merged_wide %>% write_csv("data_wide/merged_wide.csv")
merged_wide %>% write_rds("data_wide/merged_wide.RData")



# ================================== DEPRECATED ======================================
# Testing to inspect missing outputs
mgd <- ticker_wide %>%
  left_join(elspot_wide, by = "DeliveryTime") %>%
  left_join(capacity_wide, by = "DeliveryTime") %>%
  left_join(operating_wide, by = "DeliveryTime") %>%
  left_join(regulating_wide, by = "DeliveryTime") %>%
  left_join(elspot_cap_wide, by = "DeliveryTime") %>%
  mutate(
    # Drop since don't need/want this in the data
    #DeliveryYear = year(DeliveryDate),
    DeliveryMonth = month(DeliveryDate),
    DeliveryWeekday = wday(DeliveryDate)
  ) %>%
  mutate(derp = Win_VWP_Near, MissOut = ifelse(is.na(Win_VWP_Near), 1, 0)) %>% 
  select(DeliveryYear, DeliveryDate, DeliveryHour, derp, Win_VWP_Near, Win_Volume_Near, MissOut) %>%
  mutate(
    Win_VWP_Near = coalesce(Win_VWP_Near, round((lead(Win_VWP_Near)+lag(Win_VWP_Near))/2, 2), -1)
  )

mgd %>% 
  filter(MissOut == 1 | lead(MissOut) == 1 | lag(MissOut) == 1) %>% 
  arrange(DeliveryDate, DeliveryHour) %>% 
  View()

unique(mgd %>% filter(MissOut == 1) %>% select(DeliveryDate) %>% mutate(DeliveryDate = year(DeliveryDate)))

mgd %>% filter(MissOut == 1) %>% group_by(DeliveryYear) %>% summarise(cnt = n())

# There are 240 DeliveryDates with >1 missing outputs
# After imputing average of lead() and lag() where both exist, this is narrowed to 76
merged_wide %>% filter(is.na(Win_VWP_Near)) %>% select(DeliveryDate) %>% unique(.) %>% count(.)
# Simply dropping these dates means discarding 1,824 observations after imputation, vs. 5,760, of 70,128
merged_wide %>% filter(is.na(Win_VWP_Near)) %>% group_by(DeliveryYear) %>% summarise(cnt = n())

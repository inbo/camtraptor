library(lubridate) # require
library(tidyverse) # require
library(readr) # require
library(camtrapR) # require

# Test scripts ################################################################

## For Meerdaal ###############################################################

# test df from Agouti for Florian
mrdl <- read.delim("./tests/mrdl_deployments.csv",
                   sep=",") # input: data
# parse dates
mrdl <- mrdl %>%
  mutate(start = ymd_hms(start, tz="UTC"),
         end = ymd_hms(end, tz="UTC")) # input: start/end column names
# using camtrapR to get operations matrix
operations1 <- cameraOperation(mrdl, 
                              stationCol = "deploymentID", 
                              setupCol = "start", 
                              retrievalCol = "end")
# Adding up the number of cams active each day
dailyOps1 <- colSums(operations1, na.rm = T, dims = 1) # ouput : vector
# change vector to df
dailyOps1 <- data.frame(date = names(dailyOps1),
                       ncams = dailyOps1,
                       row.names = NULL)
# parse dates again, extract month and year
dailyOps1 <- dailyOps1 %>%
  mutate(date = ymd(date)) %>%
  mutate(m = month(date, label=T, abbr=F),
         y = year(date))
# tally by month/year for effort
effort1 <- dailyOps1 %>%
  group_by(m, y) %>%
  tally(ncams, name="trapEffort_days") %>%
  ungroup() %>%
  arrange(y, m) %>%
  mutate(trapEffort_hours = trapEffort_days * 24)

## For Hoge Kempen #############################################################

# test df from Agouti for Florian
nphk <- read.delim("./tests/nphk_deployments.csv",
                   sep=",")
# parse dates
nphk <- nphk %>%
  mutate(start = ymd_hms(start, tz="UTC"),
         end = ymd_hms(end, tz="UTC")) %>% # JIM CHECK DATES THAT DON'T PARSE (7)
  filter(!is.na(start)) %>% # unparsed dates give NA
  filter(!is.na(end)) # same

# using camtrapR to get operations matrix
operations2 <- cameraOperation(nphk, 
                              stationCol = "deploymentID", 
                              setupCol = "start", 
                              retrievalCol = "end")
# Adding up the number of cams active each day
dailyOps2 <- colSums(operations2, na.rm = T, dims = 1) # ouput : vector
# change vector to df
dailyOps2 <- data.frame(date = names(dailyOps2),
                       ncams = dailyOps2,
                       row.names = NULL)
# parse dates again, extract month and year
dailyOps2 <- dailyOps2 %>%
  mutate(date = ymd(date)) %>%
  mutate(m = month(date, label=T, abbr=F),
         y = year(date))
# tally by month/year for effort
effort2 <- dailyOps2 %>%
  group_by(m, y) %>%
  tally(ncams, name="trapEffort_days") %>%
  ungroup() %>%
  arrange(y, m) %>%
  mutate(trapEffort_hours = trapEffort_days * 24)


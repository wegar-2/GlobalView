library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)

# 1. load the data
dfYieldCurve <- read.csv(file = file.path(getwd(), "data", "UsaYieldCurveTimeSeries.csv"),
                         header = TRUE, sep = ",", dec = ".", na.strings = "N/A",
                         check.names = FALSE, stringsAsFactors = FALSE)
dtYieldCurve <- data.table::as.data.table(x = dfYieldCurve)
data.table::set(x = dtYieldCurve, j = "my_id_col", value = NULL)
data.table::setnames(x = dtYieldCurve, old = colnames(dtYieldCurve),
                     new = gsub(pattern = " ", replacement = "_", x = colnames(dtYieldCurve)))
data.table::setnames(x = dtYieldCurve, old = "Date", new = "data_date")
dtYieldCurve[, data_date := as.Date(x = data_date, format = "%m/%d/%y")]
data.table::setkey(x = dtYieldCurve, "data_date")


# 2. search for first non-NA quotes of 1_mo and 2_mo
lFirstNonNaSearch <- lapply(X = dtYieldCurve[, c("1_mo", "2_mo")], 
                            FUN = function(x) {  `[[`(which(!is.na(x)), 1L) })
dtYieldCurve[[data.table::key(x = dtYieldCurve)]][lFirstNonNaSearch$`1_mo`]
dtYieldCurve[[data.table::key(x = dtYieldCurve)]][lFirstNonNaSearch$`2_mo`]

# 3. handling of NAs
bColsNAsFilter <- sapply(X = dtYieldCurve, FUN = function(x) { sum(is.na(x))/length(x) < 0.05})
cColsNAsFilter <- names(bColsNAsFilter)[bColsNAsFilter]
dtYieldCurve <- dtYieldCurve[, ..cColsNAsFilter]
# remove all columns with at least one NA
cMyQuotesCols <- colnames(dtYieldCurve)[2:ncol(dtYieldCurve)]
dtYieldCurve[, cDropRowCheck := (sum(is.na(x = .SD)) > 0), by = "data_date",  .SDcols = cMyQuotesCols]
dtYieldCurve <- dtYieldCurve[cDropRowCheck == FALSE, ]
data.table::set(x = dtYieldCurve, j = "cDropRowCheck", value = NULL)

# 4. calculate monthly, querterly, and yearly statistics
# add multiple columns at the same time
dtYieldCurve[, `:=`(
  yearmonCol = zoo::as.yearmon(x = data_date),
  yearqtrCol = zoo::as.yearqtr(x = data_date),
  yearCol = lubridate::year(x = data_date))]
dtMonthlyStats
dtQuarterlyStats
dtYearlyStats
             
                    

# 5. plot data - all the time series in the same plot
# 5.1. freq daily
# 5.2. freq monthly
# 5.3. freq yearly

# 6. prepare all the possible scatter plots of changes of the rates







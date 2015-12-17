# This is the first script in the data cleansing process on files from iVolatility.com
#  **************  Uncomment to run after downloading data. *********************
# **************    Raw data is not included in package.   *********************
# **************       Instructions to get data below.     *********************

comment.for.build <- TRUE
if (comment.for.build == FALSE) {
  
# Ensure environment includes libraries needed
library(data.table)
library(dplyr)
library(TTR)
library(RcppBDT)

# 1.  Download data from http://www.ivolatility.com/data_download.j
#     - Individual contracts (Raw IV) data
#     - include greeks in file download
#     - Following symbols:  AAPL, AMZN, AAPL, GOOG, GS, SLV, SPY, EEM, EWZ, FXI, GDX, XLE
# 2.  Combine multiple raw files to one file
# 3.  Ensure that the market closed dates are up to date below
# 4.  IVRank data gathered from http://www.cboe.com/micro/equityvix/introduction.aspx
#     - Make sure you remove the disclaimer at the top line of file
#     - Last downloaded 11-3-15

# Find third friday of month
calendar <- data.frame(mon = c(1:12, 1:12, 1:12, 1:12, 1:12, 1:12),
                       year = c(rep(2010, 12),rep(2011, 12), rep(2012, 12),
                                rep(2013, 12), rep(2014, 12), rep(2015, 12)))
exp_fri <- sapply(1:72, function(i) format(getNthDayOfWeek(third, Fri,
                                                           calendar[i,1],
                                                           calendar[i,2])))
exp_fri <- as.Date(exp_fri)

# ********************* BEGIN - Area of customization for different symbols*********************

# Assign symbol variable to process
symbol <- "SPY"

# Add IVRank data to dataset
data(vx.vix.daily.prices)

# Change working directory to where the files are located so this script doesn't need to change
# Comment out and adjust for the number of files you have
# Read in the files to data.frames so they can be combined
# This removes (exchange, style, and *)
data0 <- fread("SPY-2010.csv", drop = c(2, 9, 16, 17))
data1 <- fread("SPY-2011-1.csv", drop = c(2, 9, 16, 17))
data2 <- fread("SPY-2011-2.csv", drop = c(2, 9, 16, 17))
data3 <- fread("SPY-2012-1.csv", drop = c(2, 9, 16, 17))
data4 <- fread("SPY-2012-2.csv", drop = c(2, 9, 16, 17))
data5 <- fread("SPY-2013-1.csv", drop = c(2, 9, 16, 17))
data6 <- fread("SPY-2013-2.csv", drop = c(2, 9, 16, 17))
data7 <- fread("SPY-2014-1.csv", drop = c(2, 9, 16, 17))
data8 <- fread("SPY-2014-2.csv", drop = c(2, 9, 16, 17))
data9 <- fread("SPY-2015-1.csv", drop = c(2, 9, 16, 17))
data10 <- fread("SPY-2015-2.csv", drop = c(2, 9, 16, 17))

# Combine all the files into one data.frame
combined.raw.data <- rbind(data0, data1, data2, data3, data4, data5, data6, data7, data8, data9, data10)

# ********************* END - Area of customization for different symbols*********************

# Rename columns
names(combined.raw.data) <- c("symbol", "date", "price", "option", "expiration",
                              "strike", "call.put", "ask", "bid", "mid.price",
                              "iv", "volume", "open.interest", "delta", "vega",
                              "gamma", "theta", "rho")

# Remove rows for dates that the market was closed, for some reason iVolatility sometimes includes these
combined.raw.data$expiration <- as.Date(combined.raw.data$expiration, "%m/%d/%y")
combined.raw.data$date <- as.Date(combined.raw.data$date, "%m/%d/%y")

# These dates are found by looking in thinkorswim calendar
closed.dates <- c("2010-01-01", "2010-01-18","2010-02-15", "2010-04-02",
                  "2010-05-31", "2010-07-05", "2010-09-06", "2010-11-25",
                  "2010-12-24", "2011-01-17", "2011-02-21", "2011-04-22",
                  "2011-05-30", "2011-07-04", "2011-09-05", "2011-11-24",
                  "2011-12-26", "2012-01-02", "2012-01-16", "2012-02-20",
                  "2012-04-06", "2012-05-28", "2012-07-04", "2012-09-03",
                  "2012-11-22", "2012-12-25", "2013-01-01", "2013-01-21",
                  "2013-02-18", "2013-03-29", "2013-05-27", "2013-07-04",
                  "2013-09-02", "2013-11-28", "2013-12-25", "2014-01-01",
                  "2014-01-20", "2014-02-17", "2014-04-18", "2014-05-26",
                  "2014-07-04", "2014-09-11", "2014-11-27", "2014-12-25",
                  "2015-01-19", "2015-02-16", "2015-05-25", "2015-07-03",
                  "2015-09-07")
closed.dates <- as.Date(closed.dates)
combined.raw.data <- filter(combined.raw.data, !date %in% closed.dates)

# Sometimes iVolatility shows the expiration on a Saturday, we need to move this to Friday
combined.raw.data <- mutate(combined.raw.data,
                            exp.day = weekdays(expiration, abbreviate = FALSE))
combined.raw.data <- mutate(combined.raw.data,
                            expiration = ifelse(exp.day == "Saturday", expiration - 1,
                                                                   expiration))
# Previous command converts expiration to an integer
combined.raw.data$expiration  <- as.Date(combined.raw.data$expiration,
                                         origin = "1970-01-01")
# Removing the exp.day column as we don't need it anymore
combined.raw.data <- select(combined.raw.data, -exp.day)

# Sometimes moving the Saturday dates to a Friday moves them to date the market is closed
# We then move the expiration to the preceeding Thursday
bad.exp.dates <- filter(combined.raw.data, expiration %in% closed.dates)
combined.raw.data <- mutate(combined.raw.data,
                            expiration = ifelse(expiration == unique(bad.exp.dates$expiration),
                                                                   expiration - 1, expiration))
# Previous command converts expiration to an integer
combined.raw.data$expiration  <- as.Date(combined.raw.data$expiration,
                                         origin = "1970-01-01")

# Calculate DTE for use with trade entry requirements
combined.raw.data[,"dte"] <- as.integer(combined.raw.data$expiration - combined.raw.data$date)

# Calculate RSI 14 day for use in studies
# Gather all the unique trading dates and include closing price to calculate study values
unique.data <- unique(combined.raw.data, "date", incomparables = FALSE, fromLast = FALSE)

# Add RSI study data to dataset
unique.data[, "rsi.14"] <- RSI(unique.data$price, 14)
# **************** NOTE:  This creates NAs for the first 14 days since the history is not there to calculate RSI.14

# Merge the data together adding RSI
complete.data <- merge(combined.raw.data, unique.data, by = "date")

# Remove columns added by the merge
complete.data <- select(complete.data, -c(20:37))

# Rename columns
names(complete.data) <- c("date", "symbol", "price", "option", "expiration",
                          "strike", "call.put", "ask", "bid", "mid.price", "iv",
                          "volume", "open.interest", "delta", "vega", "gamma",
                          "theta", "rho", "dte", "rsi.14")

#Remove market closed dates from IV data so that they don't create an inaccurate moving 252 day window
iv.raw.data$Date <- as.Date(iv.raw.data$Date, "%m/%d/%Y")
iv.raw.data <- filter(iv.raw.data, !Date %in% closed.dates)

# Add IVRankToS data to dataset
for (i in 1:nrow(iv.raw.data))  {
  if (i - 252 > 0)
  {
    iv.raw.data <- iv.raw.data[i, max.IV := max(iv.raw.data$Close[(i - 252):i])]
    iv.raw.data <- iv.raw.data[i, min.IV := min(iv.raw.data$Close[(i - 252):i])]
    iv.raw.data <- iv.raw.data[i, iv.rank := 100 * ((iv.raw.data[i, Close] - iv.raw.data[i, min.IV])
                                                    / (iv.raw.data[i, max.IV] - iv.raw.data[i, min.IV]))]
  }
}

# Create data.frame with only date and iv.rank to merge with
iv.raw.data <- select(iv.raw.data, Date, iv.rank)
iv.raw.data$date <- as.Date(iv.raw.data$Date, "%m/%d/%Y")

# Merge the data together adding iv.rank
complete.data <- merge(complete.data, iv.raw.data, by = "date")
complete.data <- select(complete.data, -Date)

# Remove dates where RSI is NA since they can not be selected in studies
complete.data <- filter(complete.data, rsi.14 != "NA")
complete.data <- filter(complete.data, iv.rank != "NA")

complete.data <- mutate(complete.data, date = as.Date(date))
complete.data <- mutate(complete.data, expiration = as.Date(expiration,
                                                            origin = "1970-01-01"))
complete.data <- mutate(complete.data,
                        exp_type = ifelse(is.element(expiration, exp_fri),
                                          "Monthly", "Weekly"))

# Export to .rda file for package
save(complete.data, file = paste0(symbol, ".options.RData"))
}

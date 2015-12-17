# title: "open.first.day.month.R"
# author: "Jason Taylor"

# Todos:
# - None at this time

# Description:
# - Use this script to recreate first day month file for trade entry

# Instructions:
# - Update the market closed dates prior to running
# - Calendar is created through 2020 update if needed

# Ensure environment includes libraries needed
library(dplyr)

# Dates the market was closed and can'te be used in studies
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
                  "2015-09-07", "2015-11-26")
closed.dates <- as.Date(closed.dates)

# Create first day of the month data file for use on trade entry
open.first.day.month <- data.frame(day = 1,
                       mon = c(1:12, 1:12, 1:12, 1:12, 1:12, 1:12, 1:12, 1:12,
                               1:12, 1:12, 1:12),
                       year = c(rep(2010, 12),rep(2011, 12), rep(2012, 12),
                                rep(2013, 12), rep(2014, 12), rep(2015, 12),
                                rep(2016, 12), rep(2017, 12), rep(2018, 12),
                                rep(2019, 12), rep(2020, 12)))
# Combine the columns to create data frame of dates
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                 date = as.Date(mdy(paste0(mon, "-", day,
                                                           "-", year))))

# Remove closed dates
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                      date = ifelse(date %in% closed.dates,
                                                    date + 1, date))

# Find day of week for each date
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                 day.week = weekdays(open.first.day.month$date,
                                                     abbreviate = FALSE))
# If date is Sat or Sun move to Monday
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                 date = ifelse(day.week == "Saturday",
                                               date + 2, date))
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                 date = ifelse(day.week == "Sunday",
                                               date + 1, date))
open.first.day.month <- dplyr::select(open.first.day.month, date)
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                 date = as.Date(date, origin = "1970-01-01"))

# Remove closed dates second time for start of 2012
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                      date = ifelse(date %in% closed.dates,
                                                    date + 1, date))
# Find day of week for each date
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                      day.week = weekdays(open.first.day.month$date,
                                                          abbreviate = FALSE))
# If date is Sat or Sun move to Monday
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                      date = ifelse(day.week == "Saturday",
                                                    date + 2, date))
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                      date = ifelse(day.week == "Sunday",
                                                    date + 1, date))
open.first.day.month <- dplyr::select(open.first.day.month, date)
open.first.day.month <- dplyr::mutate(open.first.day.month,
                                      date = as.Date(date, origin = "1970-01-01"))

# Save data to be resused in options.data package
save(open.first.day.month, file = "data/open.first.day.month.RData")




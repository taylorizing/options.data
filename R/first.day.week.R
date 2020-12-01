# title: "open.first.day.week.R"
# author: "Jason Taylor"

# Todos:
# - None at this time

# Description:
# - Use this script to recreate first day week file for trade entry

# Instructions:
# - Update the market closed dates prior to running
# - Calendar is created through 2020 update if needed

# Ensure environment includes libraries needed
library(dplyr)
library(RcppBDT)

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

# Create first day of the week data file for use on trade entry

open.first.day.week <- data.frame(mon = c(1:12, 1:12, 1:12, 1:12, 1:12, 1:12, 1:12,
                                     1:12, 1:12, 1:12, 1:12),
                              year = c(rep(2010, 12),rep(2011, 12), rep(2012, 12),
                                       rep(2013, 12), rep(2014, 12), rep(2015, 12),
                                       rep(2016, 12), rep(2017, 12), rep(2018, 12),
                                       rep(2019, 12), rep(2020, 12)))
first.monday <- sapply(1:120,
                       function(i) format(getNthDayOfWeek(first, Mon,
                                                          open.first.day.week[i,1],
                                                          open.first.day.week[i,2])))
second.monday <- sapply(1:120,
                        function(i) format(getNthDayOfWeek(second, Mon,
                                           open.first.day.week[i,1],
                                           open.first.day.week[i,2])))
third.monday <- sapply(1:120,
                        function(i) format(getNthDayOfWeek(third, Mon,
                                                           open.first.day.week[i,1],
                                                           open.first.day.week[i,2])))
fourth.monday <- sapply(1:120,
                        function(i) format(getNthDayOfWeek(fourth, Mon,
                                                           open.first.day.week[i,1],
                                                           open.first.day.week[i,2])))
fifth.monday <- sapply(1:120,
                        function(i) format(getNthDayOfWeek(fifth, Mon,
                                                           open.first.day.week[i,1],
                                                           open.first.day.week[i,2])))

open.first.day.week <- cbind(first.monday, second.monday, third.monday, fourth.monday,
                        fifth.monday)
open.first.day.week <- as.data.frame(open.first.day.week, stringsAsFactors = FALSE)
open.first.day.week <- mutate(open.first.day.week,
                         fifth.monday = ifelse(fifth.monday == fourth.monday,
                                               "", fifth.monday))

open.first.day.week <- unlist(open.first.day.week)
open.first.day.week <- as.data.frame(open.first.day.week)
names(open.first.day.week) <- "date"
open.first.day.week <- mutate(open.first.day.week, date = as.Date(date))
open.first.day.week <- arrange(open.first.day.week, date)

# If date is a market closed date move to next day
open.first.day.week <- dplyr::mutate(open.first.day.week,
                      date = case_when(date %in% closed.dates ~ date + lubridate::days(1),
                                       TRUE ~ date))

# Find day of week for each date
open.first.day.week <- dplyr::mutate(open.first.day.week,
                                 day.week = weekdays(open.first.day.week$date,
                                                     abbreviate = FALSE))
# If date is Sat or Sun move to Monday
open.first.day.week <- dplyr::mutate(open.first.day.week,
                                 date = case_when(day.week == "Saturday" ~
                                               date + lubridate::days(2), TRUE ~  date))
open.first.day.week <- dplyr::mutate(open.first.day.week,
                                 date = case_when(day.week == "Sunday" ~
                                               date + lubridate::days(1), TRUE ~ date))
open.first.day.week <- dplyr::select(open.first.day.week, date)
# open.first.day.week <- dplyr::mutate(open.first.day.week,
#                                  date = as.Date(date, origin = "1970-01-01"))

# Remove NA values
open.first.day.week <- as.data.frame(open.first.day.week[complete.cases(open.first.day.week),])

names(open.first.day.week) <- "date"

# Save data to be resused in options.data package
save(open.first.day.week, file = "data/open.first.day.week.RData")




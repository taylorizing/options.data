# Find Mondays to build the first day of the week

dates <- data.table::fread("data/vxapldailyprices.csv")
dates <- dplyr::distinct(dates, Date)
dates$Date <- as.Date(dates$Date, "%m/%d/%Y")

dates <- dplyr::mutate(dates, day_of_week = weekdays(dates$Date, abbreviate = FALSE))
dates <- dplyr::filter(dates, day_of_week == "Monday")
dates <- dplyr::select(dates, Date)
names(dates) <- "date"

# Filter out dates that we already have in existing file and then rbind them
# need to read into memory whatever file you already have to build on
dates <- dplyr::filter(dates, date > "2014-12-29")

# write out the file to create the V1 column that the existing file has
utils::write.csv(dates, file = "temp.csv")
dates <- data.table::fread("temp.csv")
new.dates <- dplyr::rbind(existingData, dates)

# Set working directory to where you want to save file
# write out the file to create the V1 column that the existing file has
new.dates <- dplyr::select(new.dates, -V1)
utils::write.csv(new.dates, file = "first.day.week.csv")

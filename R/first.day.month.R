# Read in the first day of the month for opening trade dates

# Read in the csv that has all the first trading days of the month
# Make data frame so that it can be used as entry dates
first.day.file <- "first.day.month.csv"
first.day.month <- fread(first.day.file)
first.day.month <- as.data.frame(first.day.month)
colnames(first.day.month) <- c("row.num", "open.date")
first.day.month[, "open.date"] <- as.Date(first.day.month[, "open.date"])
save(first.day.month, file = "first.day.month.Rdata")

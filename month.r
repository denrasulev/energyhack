# function to extract daily meter readings for selected meter, month and year
# © 2017 Denis Rasulev - ranklord@gmail.com

# set working directory
setwd("/Volumes/data/projects/energyhack")

# get smart meters readings
load_month <- function(meter = 1, month = 1, year = 2016) {

    # validate arguments
    if (meter < 1 | meter > 1000) {
        stop("Meter number must be between 1 and 1000", call. = FALSE)
    }
    if (month < 1 | month > 12) {
        stop("Month must be between 1 and 12", call. = FALSE)
    }
    if (year < 2016 | year > 2017) {
        stop("Year must be 2016 or 2017", call. = FALSE)
    }

    # read data for required meter
    path <- paste("data/2016_maf_", meter, ".rds", sep = "")
    data <- readRDS(path)

    # reduce meter data by subsetting to only required day, month and year
    data <- data[   data$measurements.meterID == meter &
                    data$measurements.month   == month &
                    data$measurements.year    == year, ]$measurements.consumption

    # sum all data per days
    days <- 0
    for (i in 1:length(data)) {
         days[i] <- sum(Reduce("+", data[i]))
    }
    return(days)
}

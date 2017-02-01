# functions to extract meter readings for selected meter, day, month and year
# © 2017 Denis Rasulev - ranklord@gmail.com

# set working directory
setwd("/Volumes/data/projects/energyhack")

# meter readings by 15 minutes intervals
day_by_min <- function(meter = 1, day = 1, month = 1, year = 2016) {

    # validate arguments
    if (meter < 1 | meter > 1000) {
        stop("Meter number must be between 1 and 1000", call. = FALSE)
    }
    if (day < 1 | day > 31) {
        stop("Day must be between 1 and 31", call. = FALSE)
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
    data <- data[   data$measurements.day == day &
                    data$measurements.month == month &
                    data$measurements.year == year, ]$measurements.consumption

    # return values
    return(data[[1]])
}

# all 15 minutes readings are sumed by 4 to return hourly values
day_by_hour <- function(meter = 1, day = 1, month = 1, year = 2016) {

    # validate arguments
    if (meter < 1 | meter > 1000) {
        stop("Meter number must be between 1 and 1000", call. = FALSE)
    }
    if (day < 1 | day > 31) {
        stop("Day must be between 1 and 31", call. = FALSE)
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
    data <- data[   data$measurements.day   == day &
                    data$measurements.month == month &
                    data$measurements.year  == year, ]$measurements.consumption

    # sum all meter readings by hour (1:4 for hour 0, 5:8 for hour 1, etc)
    # since meter saves readings once every 15 minutes we need to sum by 4
    j    <- 1
    hour <- 0
    for (i in seq(1, length(unlist(data)), 4)) {
        hour[j] <- sum( data[[1]][i:(i + 3)] )
        j <- j + 1
    }

    # return values
    return(hour)
}

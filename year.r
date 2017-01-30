# year by month

# set working directory
setwd("/Volumes/data/projects/energyhack")

# set variables
meter <- 677
year  <- 2016

# read data for required meter
path <- paste("data/2016_maf_", meter, ".rds", sep = "")
data <- readRDS(path)

month <- 0
for (i in 1:12) {
     # extract values according required variables
     month_data <- data[ which( data$measurements.month   == i &
                                data$measurements.year    == year &
                                data$measurements.meterID == meter), ]
     # sum all measurements per day
     day <- data.frame(0)
     for (j in 1:nrow(month_data)) {
          day[j] <- sum(Reduce("+", month_data$measurements.consumption[j]))
     }
     # sum all days per month
     month[i] <- sum(day)
}

# plot
par(las = 1, mar = c(3, 4, 3, 1) + .1)
barplot(month,
        main = paste("Consumption per", year, "by month"),
        xlab = paste("Meter", meter, "- Total consumption per this year:", sum(month), "kWh"),
        yaxp = c(0, round(max(month)), 5),
        col = "#b2dfd8",
        cex.names = .5,
        names.arg = c(1:length(month)))

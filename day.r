# day by hour

# set working directory
setwd("/Volumes/data/projects/energyhack")

# set variables
meter <- 662
day   <- 1
month <- 1
year  <- 2016

# read data for required meter
path <- paste("data/2016_maf_", meter, ".rds", sep = "")
data <- readRDS(path)

# extract values according to required variables
newdata <- data[ which(  data$measurements.meterID == meter &
                         data$measurements.day     == day &
                         data$measurements.month   == month &
                         data$measurements.year    == year), "measurements.consumption" ]
newdata <- as.data.frame(newdata)
colnames(newdata) <- "cons"

z    <- 1
hour <- 0
for (i in seq(1, nrow(newdata), 4)) {
     hour[z] <- sum(newdata[i:(i + 3), ])
     z <- z + 1
}

# plot
par(las = 1, mar = c(3, 4, 3, 1) + .1)
barplot(hour,
        main = paste("Consumption per Month", month, "Day", day, "by hour"),
        xlab = paste("Meter", meter, "- Total consumption per this day:", sum(hour), "kWh"),
        yaxp = c(0, round(max(hour)), 5),
        col = "aquamarine",
        cex.names = .5,
        names.arg = c(1:length(hour)))

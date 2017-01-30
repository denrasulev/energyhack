# month by day

# set working directory
setwd("/Volumes/data/projects/energyhack")

# set variables
meter <- 662
month <- 1
year  <- 2016

# read data for required meter
path <- paste("data/2016_maf_", meter, ".rds", sep = "")
data <- readRDS(path)

# extract values according to required variables
newdata <- data[ which(  data$measurements.meterID == meter &
                         data$measurements.month   == month &
                         data$measurements.year    == year), ]

# sum all data per days
days <- data.frame(0)
for (i in 1:nrow(newdata)) {
     days[i] <- sum(Reduce("+", newdata$measurements.consumption[i]))
}

# plot
par(las = 1, mar = c(3, 4, 3, 1) + .1)
barplot(as.matrix(days),
        main = paste("Consumption per Month", month, "by day"),
        xlab = paste("Meter", meter, "- Total consumption per this month:", sum(days), "kWh"),
        yaxp = c(0, round(max(days)), 5),
        col = "aquamarine",
        cex.names = .5,
        names.arg = c(1:length(days)))

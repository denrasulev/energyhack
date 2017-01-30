# Get data

# read all json files from working directory
files <- list.files(pattern = ".json")
for (i in seq_along(files)) {
     name    <- gsub(".json", "", files[i])
     df_name <- paste("df", name, sep = "_")
     assign(df_name, as.data.frame(fromJSON(files[i])))
}

# clear memory from temporary variables
remove(i, name, files, df_name)

# download all measurements for all 1000 meters for whole 2016
part1 <- "http://api.energyhack.sk/meters/"
part2 <- "/measurements?from=01-2016&to=01-2017&fields=consumption%2CleadingReactivePower%2ClaggingReactivePower"

# and save them to data folder in wd
for (i in 1:1000) {
     path <- paste(part1, i, part2, sep = "")
     name <- paste("2016_maf_", i, sep = "")
     save_name <- paste("data/", name, ".rds", sep = "")
     saveRDS(as.data.frame(fromJSON(path)), file = save_name)
}

c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")


# combine 15-min values to hour values
values <- subset(readings,
                     readings$measurements.meterID == meter &
                     readings$measurements.month   == month &
                     readings$measurements.year    == year)$measurements.consumption
values <- as.data.frame(values)
colnames(values) <- "consumption"

z    <- 1
hour <- 0
for (i in seq(1,nrow(values),4)) {
     hour[z] <- sum(values[i:i + 3,])
     z <- z + 1
}

barplot(hour,
        main = "Consumption per hour",
        col = "lightblue",
        cex.names = .6,
        names.arg = c(1:length(hour)))

#---------------------------------------------

t <- as.matrix(test["Difference Cost", ])
par(las = 1)
barplot(t,
        yaxp = c(0, round(max(t)), 5),
        col = "red",
        cex.names = .5,
        names.arg = c(1:length(t)))
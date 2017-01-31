# get smart meters readings for meters 1 - 1000

# set working directory
setwd("/Volumes/data/projects/energyhack")

# download all measurements for all 1000 meters for whole 2016
part1 <- "http://api.energyhack.sk/meters/"
part2 <- "/measurements?from=01-2016&to=01-2017&fields=consumption%2CleadingReactivePower%2ClaggingReactivePower"

# and save 1000 readings to data folder in working directory
for (i in 1:1000) {
     path <- paste(part1, i, part2, sep = "")
     name <- paste("2016_maf_", i, sep = "")
     save_name <- paste("data/", name, ".rds", sep = "")
     saveRDS(as.data.frame(fromJSON(path)), file = save_name)
}

# consumption by zip code

# load libraries
library(jsonlite)

# set working directory
setwd("/Volumes/data/projects/energyhack")

# read data in
meters <- as.data.frame(fromJSON("meters.json"))

# set variables
zip <- 81102

zip_data <- meters[ which(meters$meters.address$zip == zip), ]


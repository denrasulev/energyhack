# cost per month calculation

# load libraries
library(jsonlite)

# set working directory
setwd("/Volumes/data/projects/energyhacks")

# read data in
meters       <- as.data.frame(fromJSON("meters.json"))
suppliers    <- as.data.frame(fromJSON("suppliers.json"))
distributors <- as.data.frame(fromJSON("distributors.json"))

cost_per_month <- function(meter = 1, month = 1, year = 2016) {

     t1 <- 0
     t2 <- 0
     t3 <- 0
     t5 <- 0
     t6 <- 0
     t7 <- 0
     df <- data.frame(matrix(NA, nrow = 7, ncol = 1))

     # read data for required meter
     path <- paste("data/2016_maf_", meter, ".rds", sep = "")
     data <- readRDS(path)

     # 1. cena za spotrebu bez strat - monthly cost for consumption without loss
     cinny_odber_vt <- sum(subset(data,
                                  data$measurements.meterID == meter &
                                  data$measurements.month == month &
                                  data$measurements.year == year)$measurements.highConsumptionSum)

     cinny_odber_nt <- sum(subset(data,
                                  data$measurements.meterID == meter &
                                  data$measurements.month == month &
                                  data$measurements.year == year)$measurements.lowConsumptionSum)

     cinny_odber_1t <- cinny_odber_vt + cinny_odber_nt

     t1 <- cinny_odber_1t * distributors$distributors.costConsumptionWithoutLoss

     # 2. cena za spotrebu so stratmi - monthly cost for consumption with loss
     t2 <- cinny_odber_1t * distributors$distributors.costConsumptionWithLoss

     # 3. druhá zložka za výkon - monthly cost for reserved capacity
     t3 <- subset(meters, meters$meters.meterID == meter)$meters.reservedCapacity * distributors$distributors.costReservedCapacity

     # 4. prekročenie rezervovanej kapacity - monthly cost for overshooting reserved capacity
     max_consumption <- max(subset(data,
                                   data$measurements.meterID == meter &
                                        data$measurements.month == month &
                                        data$measurements.year == year)$measurements.maxConsumption)

     rezerved_capacity <- subset(meters, meters$meters.meterID == meter)$meters.reservedCapacity
     rozdiel <- max_consumption - rezerved_capacity

     ifelse(rozdiel <= 0, overshoot_cost <- 0, overshoot_cost <- rozdiel * distributors$distributors.costReservedCapacityOvershoot)

     # 5. jalová dodávka energie - monthly cost for leading reactive power
     jal_dodavka <- sum(subset(data,
                               data$measurements.meterID == meter &
                                    data$measurements.month == month &
                                    data$measurements.year == year)$measurements.leadingReactivePowerSum)
     t5 <- jal_dodavka * distributors$distributors.costLeadingReactivePower

     # 6. nedodržanie účinníka - monthly cost for not effective consumption
     jal_odber <- sum(subset(data,
                             data$measurements.meterID == meter &
                                  data$measurements.month == month &
                                  data$measurements.year == year)$measurements.laggingReactivePowerSum)
     podiel_tan_fi <- jal_odber / cinny_odber_1t

     factor2 <- distributors$distributors.costModifierCosFi

     penalties <- as.data.frame(distributors$distributors.powerFactorPenalties)
     factor1 <- subset(penalties, podiel_tan_fi >= penalties$start & podiel_tan_fi <= penalties$end)
     factor1 <- factor1$modifier

     mod2 <- t1 * (1 + factor2)
     mod1 <- (mod2 + t3) * factor1
     t6 <- mod1

     # 7. OKTE poplatok - monthly OKTE cost
     t7 <- cinny_odber_1t * distributors$distributors.costOKTE

     # 8. Cena za spotrebu - dodávateľ - cost for consumption

     # развернуть список в списке в удобный дата фрейм
     tariffs <- as.data.frame(suppliers$suppliers.tariffValues)
     tariffs <- t(tariffs)
     row.names(tariffs) <- NULL
     # name columns according to tariff values
     colnames(tariffs) <- c("1", "2", "3", "4", "5", "6")
     # combine them all to one table excluding old column with list
     tariffs <- cbind(tariffs, suppliers[ ,-2])

     # construct supplier name
     sup_id <- subset(meters, meters$meters.meterID == meter)$meters.supplier$supplierID
     tariff <- subset(meters, meters$meters.meterID == meter)$meters.tariff
     sup_name <- subset(suppliers, suppliers$suppliers.supplierID == sup_id)$suppliers.name

     values <- subset(tariffs, tariffs$suppliers.name == sup_name)
     rez_mes_spotreba <- values[ ,tariff]

     odchylka <- cinny_odber_1t / rez_mes_spotreba * 100

     df[1,1] <- max_consumption
     df[2,1] <- rezerved_capacity
     df[3,1] <- rozdiel
     df[4,1] <- overshoot_cost
     df[5,1] <- rez_mes_spotreba
     df[6,1] <- cinny_odber_1t
     df[7,1] <- odchylka

     return(df)
}

test <- data.frame(matrix(NA, nrow = 7, ncol = 12), row.names = c("Peak Capacity Used",
                                                                  "Rezerved Peak Capacity",
                                                                  "Difference",
                                                                  "Difference Cost",
                                                                  "Rezerved Monthly Capacity",
                                                                  "Actually Used",
                                                                  "Ratio"))
colnames(test) <- c("1","2","3","4","5","6","7","8","9","10","11","12")

averages <- data.frame(matrix(NA, nrow = 7, ncol = 1000))

for (j in 1:1000) {

     for (i in 1:12) {
          test[,i] <- cost_per_month(j, i, 2016)
     }

     averages[1,j] <- mean(as.numeric(test[1,1:12]))
     averages[2,j] <- mean(as.numeric(test[2,1:12]))
     averages[3,j] <- mean(as.numeric(test[3,1:12]))
     averages[4,j] <- mean(as.numeric(test[4,1:12]))
     averages[5,j] <- mean(as.numeric(test[5,1:12]))
     averages[6,j] <- mean(as.numeric(test[6,1:12]))
     averages[7,j] <- mean(as.numeric(test[7,1:12]))

}

avg <- t(averages)
avg <- as.data.frame(avg)
colnames(avg) <- c("Peak Capacity Used",
                   "Rezerved Peak Capacity",
                   "Difference",
                   "Difference Cost",
                   "Rezerved Monthly Capacity",
                   "Actually Used",
                   "Ratio")

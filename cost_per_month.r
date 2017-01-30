# function for calculation of electricity cost per month

# load libraries
library(jsonlite)

# set working directory
setwd("/Volumes/data/projects/energyhack")

# read data in
meters       <- as.data.frame(fromJSON("meters.json"))
suppliers    <- as.data.frame(fromJSON("suppliers.json"))
distributors <- as.data.frame(fromJSON("distributors.json"))

# set variables
meter <- 596
month <- 6
year  <- 2016

cost_per_month <- function(meter = 1, month = 1, year = 2016) {

     # read data for required meter
     path <- paste("data/2016_maf_", meter, ".rds", sep = "")
     data <- readRDS(path)

     # 1. monthly cost for consumption without loss
     consumption_high <- sum(data[which(data$measurements.meterID == meter &
                                        data$measurements.month   == month &
                                        data$measurements.year    == year), "measurements.highConsumptionSum"])

     consumption_low  <- sum(data[which(data$measurements.meterID == meter &
                                        data$measurements.month   == month &
                                        data$measurements.year    == year), "measurements.lowConsumptionSum"])

     consumption_all <- consumption_high + consumption_low

     t1 <- consumption_all * distributors$distributors.costConsumptionWithoutLoss
     cat(paste("Monthly cost for consumption without loss:", t1, "\n"))

     # 2. monthly cost for consumption with loss
     t2 <- consumption_all * distributors$distributors.costConsumptionWithLoss
     cat(paste("Monthly cost for consumption with loss:", t2, "\n"))

     # 3. monthly cost for reserved capacity
     t3 <- subset(meters, meters$meters.meterID == meter)$meters.reservedCapacity * distributors$distributors.costReservedCapacity
     cat(paste("Monthly cost for reserved capacity:", t3, "\n"))

     # 4. monthly cost for overshooting reserved capacity
     consumption_peak <- max(data[which(data$measurements.meterID == meter &
                                        data$measurements.month   == month &
                                        data$measurements.year    == year), "measurements.maxConsumption"])

     cat(paste("Max consumption peak value:", consumption_peak, "\n"))
     cat(paste("Reserved max consumption:", subset(meters, meters$meters.meterID == meter)$meters.reservedCapacity), "\n")

     difference <- consumption_peak - meters[which(meters$meters.meterID == 1), "meters.reservedCapacity"]
     cat(paste("Difference:", difference, "\n"))

     ifelse(difference <= 0, t4 <- 0, t4 <- difference * distributors$distributors.costReservedCapacityOvershoot)
     cat(paste("Cost for overshooting reserved capacity:", t4, "\n"))

     # 5. monthly cost for leading reactive power
     reactive_power <- sum(data[ which( data$measurements.meterID == meter &
                                        data$measurements.month   == month &
                                        data$measurements.year    == year), "measurements.leadingReactivePowerSum"])
     t5 <- reactive_power * distributors$distributors.costLeadingReactivePower
     cat(paste("Cost for leading reactive power:", t5, "\n"))

     # 6. monthly cost for non effective consumption
     consumption_non_eff <- sum(data[ which(data$measurements.meterID == meter &
                                            data$measurements.month   == month &
                                            data$measurements.year    == year), "measurements.laggingReactivePowerSum"])
     ratio1 <- consumption_non_eff / consumption_all

     # find modifiers
     penalties <- as.data.frame(distributors$distributors.powerFactorPenalties)
     mod2 <- distributors$distributors.costModifierCosFi
     mod1 <- subset(penalties, ratio1 >= penalties$start & ratio1 <= penalties$end)
     mod1 <- mod1$modifier

     mod2 <- t1 * (1 + mod2)
     mod1 <- (mod2 + t3) * mod1
     t6   <- mod1
     cat(paste("Cost for non effective consumption:", t6, "\n"))

     # 7. monthly OKTE cost
     t7 <- consumption_all * distributors$distributors.costOKTE
     cat(paste("Monthly OKTE cost:", t7, "\n"))

     # 8. cost for consumption

     # expand list into a data frame
     tariffs <- as.data.frame(suppliers$suppliers.tariffValues)
     tariffs <- t(tariffs)
     row.names(tariffs) <- NULL

     # name columns according to tariff values
     colnames(tariffs) <- c("1", "2", "3", "4", "5", "6")

     # combine everything to one table excluding old column with list
     tariffs <- cbind(tariffs, suppliers[ ,-2])

     # construct supplier name
     sup_id <- subset(meters, meters$meters.meterID == meter)$meters.supplier$supplierID
     tariff <- subset(meters, meters$meters.meterID == meter)$meters.tariff
     sup_name <- subset(suppliers, suppliers$suppliers.supplierID == sup_id)$suppliers.name
     supplier_id <- paste(sup_name, tariff, sep = "")

     # find values required for further acalculations
     values <- subset(tariffs, tariffs$suppliers.name == sup_name)
     monthly_tariff_value <- values[ ,tariff]

     # high (day) and low (night) cost per kWh
     cost_high <- values$suppliers.costHigh
     cost_low  <- values$suppliers.costLow

     # tolerance +/- % for tolerated measurement
     tolerance <- values$suppliers.tolerance

     # cost modifiers for over and under consumption
     over_consumption  <- values$suppliers.costModifierOverconsumption
     under_consumption <- values$suppliers.costModifierUnderconsumption
     cat("Monthly consumption tariff value:", monthly_tariff_value, "\n")

     # ratio of monthly reserved capacity usage
     ratio2 <- consumption_all / monthly_tariff_value * 100
     cat("Capacity used:", consumption_all, "\n")
     cat("Capacity usage ratio:", ratio2, "\n")

     # calculate cost modifiers
     tolerance_high <- 1 + tolerance
     tolerance_low  <- 1 - tolerance

     # calculate day and night consumptions costs
     if ( ratio2 >= tolerance_low & ratio2 <= tolerance_high ) {
          t8_vt <- consumption_high * cost_high
          t8_nt <- consumption_low * cost_low
     } else if ( ratio2 < tolerance_low ) {
          t8_vt <- consumption_high * cost_high * under_consumption
          t8_nt <- consumption_low * cost_low * under_consumption
     } else if ( ratio2 > tolerance_high ) {
          t8_vt <- consumption_high * cost_high * over_consumption
          t8_nt <- consumption_low * cost_low * over_consumption
     }

     # total consumption
     t8 <- t8_vt + t8_nt
     cat(paste("Total cost for consumption:", t8, "\n"))

     # 9. tax
     t9 <- consumption_all * values$suppliers.costTax

     # Distributor cost
     distributor_cost <- t1 + t2 + t3 + t4 + t5 + t6 + t7
     cat(paste("Distributor cost:", distributor_cost, "\n"))

     # Supplier cost
     supplier_cost <- t8 + t9
     cat(paste("Supplier cost:", supplier_cost, "\n"))

     # Monthly cost without VAT
     monthly_cost <- distributor_cost + supplier_cost
     cat(paste("Monthly cost for meter", meter, "is", monthly_cost, "\n"))



}

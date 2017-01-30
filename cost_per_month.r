# function for calculation of electricity cost per month

# load libraries
library(jsonlite)

# set working directory
setwd("/Volumes/data/projects/energyhack")

# read data in
meters       <- as.data.frame(fromJSON("meters.json"))
suppliers    <- as.data.frame(fromJSON("suppliers.json"))
distributors <- as.data.frame(fromJSON("distributors.json"))

cost_per_month <- function(meter = 1, month = 1, year = 2016) {

    # data frame with cost related information to return
    cost <- data.frame(0)

    # read data for required meter
    path <- paste("data/2016_maf_", meter, ".rds", sep = "")
    data <- readRDS(path)

    # reduce data by subsetting to only required month and year
    data <- data[data$measurements.month == month & data$measurements.year == year, ]

    # monthly consumption at high rate hours 10:00 - 22:00
    consumption_high <- sum(data[data$measurements.meterID == meter, ]$measurements.highConsumptionSum)
    cat(paste("Consumption in high rate hours:", consumption_high, "\n"))

    # monthly consumption at low rate hours 22:00 - 10:00
    consumption_low  <- sum(data[data$measurements.meterID == meter, ]$measurements.lowConsumptionSum)
    cat(paste("Consumption in low rate hours:", consumption_low, "\n"))

    consumption_all <- consumption_high + consumption_low

    # monthly cost for consumption without loss
    t1 <- consumption_all * distributors$distributors.costConsumptionWithoutLoss
    cat(paste("Cost for consumption without loss:", t1, "\n"))

    # monthly cost for consumption with loss
    t2 <- consumption_all * distributors$distributors.costConsumptionWithLoss
    cat(paste("Cost for consumption with loss:", t2, "\n"))

    # reserved peak capacity
    reserved_capacity <- meters[meters$meters.meterID == meter, ]$meters.reservedCapacity
    cat(paste("Reserved capacity:", reserved_capacity, "\n"))

    # peak value of consumption
    consumption_peak <- max(data[data$measurements.meterID == meter, ]$measurements.maxConsumption)
    cat(paste("Peak consumption value:", consumption_peak, "\n"))

    difference <- consumption_peak - reserved_capacity
    cat(paste("Difference:", difference, "\n"))

    # monthly cost for reserved capacity
    t3 <- reserved_capacity * distributors$distributors.costReservedCapacity
    cat(paste("Cost for reserved capacity:", t3, "\n"))

    # monthly cost for overshooting reserved capacity
    ifelse(difference <= 0, t4 <- 0, t4 <- difference * distributors$distributors.costReservedCapacityOvershoot)
    cat(paste("Cost for overshooting reserved capacity:", t4, "\n"))

    # monthly cost for leading reactive power
    reactive_power <- sum(data[ which( data$measurements.meterID == meter &
                                    data$measurements.month   == month &
                                    data$measurements.year    == year), "measurements.leadingReactivePowerSum"])
    t5 <- reactive_power * distributors$distributors.costLeadingReactivePower
    cat(paste("Cost for leading reactive power:", t5, "\n"))

    # monthly cost for non effective consumption
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

    # monthly OKTE cost
    t7 <- consumption_all * distributors$distributors.costOKTE
    cat(paste("Monthly OKTE cost:", t7, "\n"))

    # cost for consumption

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

    # tax
    t9 <- consumption_all * values$suppliers.costTax

    # distributor cost
    distributor_cost <- t1 + t2 + t3 + t4 + t5 + t6 + t7
    cat(paste("Distributor cost:", distributor_cost, "\n"))

    # supplier cost
    supplier_cost <- t8 + t9
    cat(paste("Supplier cost:", supplier_cost, "\n"))

    # monthly cost without VAT
    monthly_cost <- distributor_cost + supplier_cost
    cat(paste("Monthly cost for meter", meter, "is", monthly_cost, "\n"))

    cost[1,1] <- t1
    cost[2,1] <- t2
    cost[3,1] <- t3
    cost[4,1] <- t4
    cost[5,1] <- t5
    cost[6,1] <- t6
    cost[7,1] <- t7
    cost[8,1] <- t8
    cost[9,1] <- t9
    cost[10,1] <- monthly_cost

    # return data frame with found values
    return(cost)
}

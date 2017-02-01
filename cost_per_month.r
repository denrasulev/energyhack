# function for calculation of electricity cost per month

# set working directory
setwd("/Volumes/data/projects/energyhack")

cost_per_month <- function(meter = 1, month = 1, year = 2016) {

    # validate arguments
    if (meter < 1 | meter > 1000) {
        stop("Meter number must be between 1 and 1000", call. = FALSE)
    }
    if (month < 1 | month > 12) {
        stop("Month must be between 1 and 12", call. = FALSE)
    }
    if (year < 2016 | year > 2017) {
        stop("Year must be 2016 or 2017", call. = FALSE)
    }

    # validate that required data is in memory
    library(jsonlite)
    if (!exists("meters"))       { meters       <- as.data.frame(fromJSON("meters.json")) }
    if (!exists("suppliers"))    { suppliers    <- as.data.frame(fromJSON("suppliers.json")) }
    if (!exists("distributors")) { distributors <- as.data.frame(fromJSON("distributors.json")) }

    # data frame with cost related information to return
    cost <- as.data.frame(0)

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
    cost_consumption_wout_loss <- consumption_all * distributors$distributors.costConsumptionWithoutLoss
    cat(paste("Cost for consumption without loss:", cost_consumption_wout_loss, "\n"))

    # monthly cost for consumption with loss
    cost_consumption_with_loss <- consumption_all * distributors$distributors.costConsumptionWithLoss
    cat(paste("Cost for consumption with loss:", cost_consumption_with_loss, "\n"))

    # reserved peak capacity
    reserved_peak_capacity <- meters[meters$meters.meterID == meter, ]$meters.reservedCapacity
    cat(paste("Reserved capacity:", reserved_peak_capacity, "\n"))

    # monthly cost for reserved capacity
    cost_reserved_peak_capacity <- reserved_peak_capacity * distributors$distributors.costReservedCapacity
    cat(paste("Cost for reserved capacity:", cost_reserved_peak_capacity, "\n"))

    # peak value of consumption
    consumption_max_peak <- max(data[data$measurements.meterID == meter, ]$measurements.maxConsumption)
    cat(paste("Peak consumption value:", consumption_max_peak, "\n"))

    difference <- consumption_max_peak - reserved_peak_capacity
    cat(paste("Difference, peak - reserved:", difference, "\n"))

    # monthly cost for overshooting reserved capacity
    ifelse(difference <= 0, cost_exceed_reserved_capacity <- 0, cost_exceed_reserved_capacity <- difference * distributors$distributors.costReservedCapacityOvershoot)
    cat(paste("Cost for overshooting reserved capacity:", cost_exceed_reserved_capacity, "\n"))

    # monthly cost for leading reactive power
    leading_reactive_power <- sum(data[data$measurements.meterID == meter, ]$measurements.leadingReactivePowerSum)
    cost_leading_reactive_power <- leading_reactive_power * distributors$distributors.costLeadingReactivePower
    cat(paste("Cost for leading reactive power:", cost_leading_reactive_power, "\n"))

    # monthly cost for lagging reactive power
    lagging_reactive_power <- sum(data[data$measurements.meterID == meter, ]$measurements.laggingReactivePowerSum)
    ratio1 <- lagging_reactive_power / consumption_all

    # find modifiers
    mod2 <- distributors$distributors.costModifierCosFi
    penalties <- as.data.frame(distributors$distributors.powerFactorPenalties)
    mod1 <- subset(penalties, ratio1 >= penalties$start & ratio1 <= penalties$end)
    mod1 <- mod1$modifier

    mod2 <- cost_consumption_wout_loss * (1 + mod2)
    mod1 <- (mod2 + cost_reserved_peak_capacity) * mod1
    cost_lagging_reactive_power   <- mod1
    cat(paste("Cost for lagging reactive power:", cost_lagging_reactive_power, "\n"))

    # monthly OKTE cost
    cost_okte <- consumption_all * distributors$distributors.costOKTE
    cat(paste("Cost OKTE:", cost_okte, "\n"))

    # tariff value
    sup_id <- as.numeric(meters[meters$meters.meterID == meter, ]$meters.supplier$supplierID)
    tariff <- as.numeric(meters[meters$meters.meterID == meter, ]$meters.tariff)
    tariff_monthly_capacity <- suppliers$suppliers.tariffValues[[sup_id]][tariff]
    cat("Monthly consumption tariff value:", tariff_monthly_capacity, "\n")

    # tariff name
    sup_name <- suppliers[suppliers$suppliers.supplierID == sup_id, ]$suppliers.name
    tariff_name <- as.character(paste(sup_name, tariff, sep = ""))
    cat("Tariff name:", tariff_name, "\n")

    # high (day) and low (night) cost per kWh
    cost_high <- suppliers[suppliers$suppliers.supplierID == sup_id, ]$suppliers.costHigh
    cost_low  <- suppliers[suppliers$suppliers.supplierID == sup_id, ]$suppliers.costLow

    # tolerance - % for tolerated measurement
    tolerance <- suppliers[suppliers$suppliers.supplierID == sup_id, ]$suppliers.tolerance

    # calculate upper and lower limits of tolerance
    tolerance_upper <- 1 + tolerance
    tolerance_lower <- 1 - tolerance

    # cost modifiers for over and under consumption
    over_consumption <- suppliers[suppliers$suppliers.supplierID == sup_id, ]$suppliers.costModifierOverconsumption
    undr_consumption <- suppliers[suppliers$suppliers.supplierID == sup_id, ]$suppliers.costModifierUnderconsumption

    # ratio of monthly tariff value usage
    ratio2 <- consumption_all / tariff_monthly_capacity * 100
    cat("Capacity used:", consumption_all, "\n")
    cat("Capacity usage ratio:", ratio2, "\n")

    # calculate day and night consumptions costs
    if (ratio2 >= tolerance_lower & ratio2 <= tolerance_upper) {
        cost_consumption_high <- consumption_high * cost_high
        cost_consumption_low <- consumption_low  * cost_low
    } else if (ratio2 < tolerance_lower) {
        cost_consumption_high <- consumption_high * cost_high * undr_consumption
        cost_consumption_low <- consumption_low  * cost_low  * undr_consumption
    } else if (ratio2 > tolerance_upper) {
        cost_consumption_high <- consumption_high * cost_high * over_consumption
        cost_consumption_low <- consumption_low  * cost_low  * over_consumption
    }
    cat(paste("Cost of consumption in high rate hours:", cost_consumption_high, "\n"))
    cat(paste("Cost of consumption in low rate hours:", cost_consumption_low, "\n"))

    # total consumption
    cost_consumption_all <- cost_consumption_high + cost_consumption_low
    cat(paste("Total cost for consumption:", cost_consumption_all, "\n"))

    # tax
    tax <- consumption_all * suppliers[suppliers$suppliers.supplierID == sup_id, ]$suppliers.costTax

    # distributor cost
    cost_distributor <- sum(cost_consumption_wout_loss,
                            cost_consumption_with_loss,
                            cost_reserved_peak_capacity,
                            cost_exceed_reserved_capacity,
                            cost_leading_reactive_power,
                            cost_lagging_reactive_power,
                            cost_okte)
    cat(paste("Distributor cost:", cost_distributor, "\n"))

    # supplier cost
    cost_supplier <- cost_consumption_all + tax
    cat(paste("Supplier cost:", cost_supplier, "\n"))

    # monthly cost without VAT
    cost_for_month <- cost_distributor + cost_supplier
    cat(paste("Total cost for meter", meter, "month", month, "is", cost_for_month, "\n"))

    # usage values
    cost[1,1]  <- tariff_name
    cost[2,1]  <- reserved_peak_capacity
    cost[3,1]  <- consumption_max_peak
    cost[4,1]  <- difference
    cost[5,1]  <- tariff_monthly_capacity
    cost[6,1]  <- tolerance
    cost[7,1]  <- tolerance_upper
    cost[8,1]  <- tolerance_lower
    cost[9,1]  <- consumption_high
    cost[10,1] <- consumption_low
    cost[11,1] <- consumption_all
    cost[12,1] <- ratio2

    # cost of usage
    cost[13,1] <- cost_consumption_wout_loss
    cost[14,1] <- cost_consumption_with_loss
    cost[15,1] <- cost_reserved_peak_capacity
    cost[16,1] <- cost_exceed_reserved_capacity
    cost[17,1] <- cost_leading_reactive_power
    cost[18,1] <- cost_lagging_reactive_power
    cost[19,1] <- cost_consumption_high
    cost[20,1] <- cost_consumption_low
    cost[21,1] <- cost_consumption_all
    cost[22,1] <- cost_okte
    cost[23,1] <- tax

    # overall cost for month
    cost[24,1] <- cost_distributor
    cost[25,1] <- cost_supplier
    cost[26,1] <- cost_for_month

    # return data frame with found values
    return(cost)
}

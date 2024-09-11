################################################################################
# Title: Climate Data Import
# Author: Quentin Bell (quentin.bell@fmi.fi)
# Intention: This script reads in the climate files (which have been renamed but
# otherwise not altered from the versions downloaded from the FMI open data
# access), then exports the data in a consistent form expected by Stics. This
# involves transforming all observations to daily equivalents, as well as
# checking the rainfall amounts across time scales. Last updated to include
# relative humidity as I had left that out accidentally.
################################################################################

library(rio)
library(magrittr)
setwd("/home/bell/Documents/TWINWIN/obsData/TWINWINdata/climate_data")
################################################################################
# Importing csv files
daily_precip_temp <- import("Kumpula2022_daily_precip_max_temp_min_temp.csv")
hourly_rad <- import("Kumpula2022_hourly_global_rad.csv")
hourly_pres_wind_precip <- import("Kumpula2022_hourly_pressure_precip_humid_wind.csv")
# monthly_precip <- import("Kumpula2021_monthly_precip.csv")
################################################################################
# Removing negative values (setting them to zero) from daily precipitation and hourly global radiation reasurements, and assigning NA's to measurements labelled "-" elsewhere
daily_precip_temp["Precipitation amount (mm)"][daily_precip_temp["Precipitation amount (mm)"] == -1] <- 0
hourly_rad["Global radiation (W/m2)"][hourly_rad["Global radiation (W/m2)"] < 0] <- 0
# In hourly pressure, precipitation, and wind measurements there are missing values (which instead have a dash "-"), for the few pressure ones I replace them with NA, as for any averaging and creation of daily pressure I want them to be ignored entirely, while for wind and precipitation I assume a missing value means no observable wind or precipitation, so set them each to zero for the purposes of calculating daily averages or totals.
hourly_pres_wind_precip["Pressure (msl) (hPa)"][hourly_pres_wind_precip["Pressure (msl) (hPa)"] == "-"] <- NA
hourly_pres_wind_precip[hourly_pres_wind_precip == "-"] <- 0
# now we can convert the pressure, wind speed, and precipitation columns to numeric values.
hourly_pres_wind_precip$`Pressure (msl) (hPa)` <- as.numeric(hourly_pres_wind_precip$`Pressure (msl) (hPa)`)
hourly_pres_wind_precip$`Precipitation amount (mm)` <- as.numeric(hourly_pres_wind_precip$`Precipitation amount (mm)`)
hourly_pres_wind_precip$`Wind speed (m/s)` <- as.numeric(hourly_pres_wind_precip$`Wind speed (m/s)`)
hourly_pres_wind_precip$`Relative humidity (%)` <- as.numeric(hourly_pres_wind_precip$`Relative humidity (%)`)
################################################################################
# Converting hourly data observations to daily observations
hourly_rad$Date <- as.Date(paste(hourly_rad$Year, hourly_rad$m, hourly_rad$d), "%Y %m %d")
# Conversion function that both sums the hourly observations into the whole day and converts from W/m2 to MJ/m2 using the factor 3600/10^6
conv_aggr_rad <- function(x) (sum(as.numeric(x)) * 3600 / 10^6)
daily_rad <- aggregate(`Global radiation (W/m2)` ~ Date, hourly_rad, conv_aggr_rad)
colnames(daily_rad)[2] <- "GlobalRadiationMJperm2" # Rename column as we changed the units

hourly_pres_wind_precip$Date <- as.Date(paste(hourly_rad$Year, hourly_rad$m, hourly_rad$d), "%Y %m %d")[1 : nrow(hourly_pres_wind_precip)]
# Convert pressure (at mean sea level) from hPa to Pa, and take the mean over the day.
conv_aggr_pres <- function(x) (mean(x) /100)
daily_pres <- aggregate(`Pressure (msl) (hPa)` ~ Date, hourly_pres_wind_precip, conv_aggr_pres)
colnames(daily_pres)[2] <- "Pressure (Pa)"

daily_wind <- aggregate(`Wind speed (m/s)` ~ Date, hourly_pres_wind_precip, mean)
# daily_precip <- aggregate(`Precipitation amount (mm)` ~ Date, hourly_pres_wind_precip, sum)
daily_humid <- aggregate(`Relative humidity (%)` ~ Date, hourly_pres_wind_precip, mean)
# Note that these include one observation for the last hour of 2021 which creates a small problem of adding a day to hour aggregated observations. It doesn't really matter as none of our crops will be growing over the new year, so instead we can just drop this final entry when adding the aggregated observations to our daily_data collection.
################################################################################
# Gathering into one data frame
# Initially dropping columns from daily_precip_temp as the time and time zone are constant and unneeded.
daily_data <- daily_precip_temp[, c(1, 2, 3, 6, 7, 8)]
daily_data$jul_day <- 1 : 365

# Merge our aggregated daily data frames
daily_aggs <- daily_wind %>%
  merge(daily_humid, by = "Date") %>%
  # merge(daily_pres, by = "Date") %>% # Pressure isn't actually used, need humidity.
  # merge(daily_precip, by = "Date") %>% # Commenting out the converted hourly precipitation as we are just using daily
  merge(daily_rad, by = "Date")
# %>%
#   head(-1) # this removes the erroneous added day at the start of the next year

# Create a jul_day variable as the easiest one to merge with daily_data
daily_aggs$jul_day <- julian(daily_aggs$Date, origin = as.Date("2021/12/31"))
# Merge our aggregated data with the daily observations
daily_data <- merge(daily_data, daily_aggs, by = "jul_day")
daily_data$Date <- NULL
# Reorder columns to more closely match what Pinja was using.
daily_data <- daily_data[, c(2:4, 1, 5:7, 10:8)]
################################################################################
# Writing out to a space separated file, as apparently csv didn't work as well.
write.table(daily_data, "climate_kumpula_2022.prn", quote = FALSE, row.names = FALSE)








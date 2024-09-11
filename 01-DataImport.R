################################################################################
# Title: Data Import
# Author: Quentin Bell (quentin.bell@fmi.fi)
# Intention: This script reads in the data sources used in the experiments,
# including observations from the fields and climate data used by STICS. The
# observations are combined into one .csv file for use in model runs and
# analysis while the climate data is exported as a space separated plaintext
# file (.prn filetype) that is then imported and handled by STICS.
################################################################################
library(tidyverse)
library(rio)
setwd("/home/bell/Documents/TWINWIN/obsData")
################################################################################
# This dictionary associates plot numbers with labels for the variety undersown
# on that plot with barley. The first, "barley", is just for barley by itself
# with no undersown crop.
plot_dict <- list(
  "Barley" = c(5, 9, 20, 21, 42, 43, 54, 55),
  "RC"     = c(13, 28, 39),
  "AA"     = c(15, 26, 41),
  "IR"     = c(4, 37, 57),
  "AC"     = c(7, 19, 60),
  "TG"     = c(11, 32, 47),
  "WC"     = c(6, 23, 48),
  "CI"     = c(30, 34, 53),
  "FA"     = c(24, 36, 59)
)

################################################################################
# Read in GAI observations

obs_gai <- import("GAI_in2groups_v2.csv")
obs_gai$crop <- NA
for (i in 1 : length(plot_dict)) {
  obs_gai$crop[obs_gai$GAI %in% plot_dict[[i]]] <- names(plot_dict)[i]
}

obs_long <- pivot_longer(obs_gai, c(5 : (ncol(obs_gai) - 1)), names_to = "date", values_to = "obs") %>%
  na.omit()
obs_long$date <- as.Date(obs_long$date, format = "%d.%m.%Y")

# Take means and drop unneeded columns
obs_mean <- obs_long %>%
  group_by(date, crop, What) %>%
  summarise(GAI = mean(obs), GAI_se = sd(obs), se_prop = GAI_se/GAI)

# Substituting the relative uncertainty from barley on the same date to the other plots which were only measured once per combination, thus have no uncertainty associated with them.
obs_gaps <- obs_mean %>%
  group_by(date, What) %>%
  fill(c(se_prop), .direction = "downup") %>%
  mutate(GAI_se = se_prop * GAI)
# obs_gaps$GAI_se <- obs_gaps$se_prop * obs_gaps$GAI
# min(obs_gaps$GAI_se[which(obs_gaps$GAI_se > 0)], na.rm = TRUE)

# We cannot deal with zero uncertainty at the moment as our error-covariance matrix is simply diagonal, any zero uncertainties will cause it to be singular and unable to be used.
obs_gaps$GAI_se[obs_gaps$What == "Barley" & is.na(obs_gaps$GAI_se)] <- 0
obs_gaps$GAI_se[obs_gaps$What == "Barley" & obs_gaps$GAI_se <= 0] <- min(obs_gaps$GAI_se[which(obs_gaps$GAI_se > 0)], na.rm = TRUE)
# We only use the barley observations, not those of the cover crops.
gai_obs <- obs_gaps %>%
  filter(What == "Barley") %>%
  mutate(obs = "GAI") %>%
  rename(value = GAI, uncertainty = GAI_se)
# Remove extra columns and reorder
gai_obs <- gai_obs[, c("date", "crop", "obs", "value", "uncertainty")]

################################################################################
# Read in NEE observations and convert to daily estimated values

par <- import("KumpulaPAR30minAvg2019_2022.csv") # Observations of PAR averaged to half-hour blocks covering 2019-2022. # There are some missing values, which we omit immediately
lr_bio <- import("LR_diversity.csv") # Observations and calculated values of flux-related variables from the TWINWIN sites.
# Checking NAs
# sapply(par, function(x) sum(length(which(is.na(x)))))
# sapply(lr_bio, function(x) sum(length(which(is.na(x)))))
# We remove NAs from par, as they mostly occur in October 2022 (which doesn't matter for us, and one occurs around midnight on May 30th 2022, so its absence should not impact the estimated NEE to any significant degree). Particularly as none of these dates have observations in the lr_bio file.
par <- na.omit(par)
# We have observations on certain days, and for each plot an estimated alpha and GP_max. We then need to take the observed PAR for these days and calculate GPP for the day by summing the estimated GPP for each half hour interval.
# Read in the observed dates
dates_obs <- unique(lr_bio$Date)
# Create a data frame for storing the estimated GPP and anything needed to calculate the NEE
gpp <- lr_bio[, c(1, 3, 10, 11)]
gpp$GPP <- NA
gpp$GPP_se <- NA
colnames(gpp)[1] <- "date" # Changing the column name to align with everything else so I stop making annoying errors of using Date instead of date when plotting.
calc_gpp <- function(alpha, gp_max, par) {
  return(alpha * gp_max * par / (alpha * par + gp_max))
}
# Commenting out the variance calculation until the calculation of the NEE is confirmed to be correct.
calc_gpp_var <- function(alpha, alpha_var, gp_max, gp_max_var, par) {
  gpp <- calc_gpp(alpha, gp_max, par)
  if (is.na(alpha_var)) {alpha_var <- 0}
  if (is.na(gp_max_var)) {gp_max_var <- 0}
  sigmasq <- gpp^2 * (alpha_var / alpha^2 + gp_max_var / gp_max^2 + (par^2 * alpha_var + gp_max_var) / (alpha * par + gp_max)^2)
  return(sigmasq)
}
# Loop over dates of observations, extracting the PAR for the day and the measurements from the same day.
for (day in dates_obs) {
  par_day <- par$KUM_META.par[which(as.Date(paste0(par$Year, "/", par$Month, "/", par$Day)) == day)]
  plots_obs <- lr_bio[which(lr_bio$Date == day), "Collar"]
  for (plot in plots_obs) {
    # Read out the values for the given plot and day combination
    alpha <- lr_bio$alpha[lr_bio$Date == day & lr_bio$Collar == plot]
    gp_max <- lr_bio$GPmax[lr_bio$Date == day & lr_bio$Collar == plot]
    alpha_se <- lr_bio$alpha_se[lr_bio$Date == day & lr_bio$Collar == plot]
    gp_max_se <- lr_bio$GPmax_se[lr_bio$Date == day & lr_bio$Collar == plot]
    gpp$GPP[gpp$date == day & gpp$Collar == plot] <- sum(calc_gpp(alpha, gp_max, par_day) * 60 * 30) # calc_gpp returns a vector of average GPPs in units of mg CO2 m^-2 s^-1 for a half hour period, by multiplying by 60 * 30 we convert that average for the half hour into the total GPP in units of mg CO2 m^-2 in that half hour period. Summing the vector we get the daily GPP in mg CO2 m^-2, and so only need to convert the mass and area units later.
    gpp$GPP_se[gpp$date == day & gpp$Collar == plot] <- sqrt(sum(calc_gpp_var(alpha, alpha_se^2, gp_max, gp_max_se^2, par_day) * (60 * 30)^2))
  }
}
# Calculate daily NEE, accounting for differing units. We also change from mg m^-2 to tn ha^-1, and this is all per day.
# Note that since \alpha is negative in the data set provided, our calculated GPP is also negative, and thus the calculation of NEE from Reco (Respiration of the ecosystem) and GPP is a simple sum and not a difference as done previously.
gpp$NEE <- (gpp$`Reco [mg CO2 m-2 s-1]` * 60 * 60 * 24 + gpp$GPP) * 10^-9 * 10^4
# Reco_se has many missing values, which lacking further information I am currently setting to zero.
gpp$Reco_se[is.na((gpp$Reco_se))] <- 0
gpp$NEE_se <- sqrt(((gpp$Reco_se * 60 * 60 * 24)^2 + gpp$GPP_se^2)) * 10^-9 * 10^4

nee_only <- gpp[, c("date", "Collar", "NEE", "NEE_se")]

nee_only$crop <- NA
for (i in 1 : length(plot_dict)) {
  nee_only$crop[nee_only$Collar %in% plot_dict[[i]]] <- names(plot_dict)[i]
}
# Currently removing any observations that are not covered here as they are for more than two crop plots
nee_only <- na.omit(nee_only)
nee_obs <- nee_only %>%
  group_by(date, crop) %>%
  summarise(value = mean(NEE), uncertainty = abs(mean(NEE)) * mean(NEE_se / abs(NEE))) %>%
  mutate(obs = "NEE", date = as.Date(date))
################################################################################
# Read in yield observations

# Read in observed yield data for 2020
obs_2020 <- import("Twinwin_yield_2020.xlsx", which = "Jussi")
obs_2020 <- na.omit(obs_2020[str_detect(obs_2020$Treatment, paste(c("D1", "D0"), collapse = '|')),c("tn/ha", "Treatment")])
names(obs_2020) <- c("Yield", "Treatment")
# Set the harvest dates to 11.9.2020 according to the harvest of the middle from Field_Management_info.xlsx
obs_2020$date <- as.Date("11.09.2020", "%d.%m.%Y")
obs_2020$Treatment <- gsub('[D1= ]', '', obs_2020$Treatment)
obs_2020$Treatment[obs_2020$Treatment == 0] <- "Barley"
obs_2020$Treatment[obs_2020$Treatment == "0H"] <- "Barley + Herbicide"

# Read in observed yield data for 2021
obs_2021 <- import("Twinwin_yield_2021.xlsx", which = "Yield_calculation")
obs_2021 <- na.omit(obs_2021[, c("tn/ha", "Plots")])
names(obs_2021) <- c("Yield", "Plot")
# Plot treatment is not on the same page as the calculated tn/ha, so extract that and merge (after renaming columns to match as appropriate)
obs_2021_treatment <- import("Twinwin_yield_2021.xlsx", which = "Yield 2021")
obs_2021_treatment <- na.omit(obs_2021_treatment[, c("Plot", "Treatment")])
obs_2021 <- merge(obs_2021, obs_2021_treatment, by = "Plot")
obs_2021$Plot <- NULL
obs_2021 <- obs_2021[str_detect(obs_2021$Treatment, paste(c("D1", "D0"), collapse = '|')), ]
# Set the harvest dates to 14.9.2021 according to the harvest of the middle from Field_Management_info.xlsx
obs_2021$date <- as.Date("14.9.2021", "%d.%m.%Y")
obs_2021$Treatment <- gsub('[D1= ]', '', obs_2021$Treatment)
obs_2021$Treatment[obs_2021$Treatment == 0] <- "Barley"
obs_2021$Treatment[obs_2021$Treatment == "0H"] <- "Barley + Herbicide"

yield_obs <- obs_2020 %>%
  rbind(obs_2021) %>%
  group_by(Treatment, date) %>%
  summarise(value = mean(Yield), uncertainty = sd(Yield)) %>%
  rename(crop = Treatment) %>%
  mutate(obs = "yield")

################################################################################
# Combine and export the observation data.
obs <- gai_obs %>%
  rbind(nee_obs) %>%
  rbind(yield_obs)

export(obs, "TWINWINobs.csv")
################################################################################
# Importing climate data for use in STICS simulations.
setwd("/home/bell/Documents/TWINWIN/obsData/climate_data")
# Looping over the years used.
for (year in 2019:2022) {
  # Importing csv files
  daily_precip_temp <- import(paste0("Kumpula", year, "_daily_precip_max_temp_min_temp.csv"))
  hourly_rad <- import(paste0("Kumpula", year, "_hourly_global_rad.csv"))
  hourly_pres_wind_precip <- import(paste0("Kumpula", year, "_hourly_pressure_precip_humid_wind.csv"))
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
  # Account for leap year
  if (year == 2020) {
    daily_data$jul_day <- 1 : 366
  } else {
    daily_data$jul_day <- 1 : 365
  }

  # Merge our aggregated daily data frames
  daily_aggs <- daily_wind %>%
    merge(daily_humid, by = "Date") %>%
    # merge(daily_pres, by = "Date") %>% # Pressure isn't actually used, need humidity.
    # merge(daily_precip, by = "Date") %>% # Commenting out the converted hourly precipitation as we are just using daily
    merge(daily_rad, by = "Date")
  # %>%
  #   head(-1) # this removes the erroneous added day at the start of the next year

  # Create a jul_day variable as the easiest one to merge with daily_data
  daily_aggs$jul_day <- julian(daily_aggs$Date, origin = as.Date(paste0(year - 1, "/12/31")))
  # Merge our aggregated data with the daily observations
  daily_data <- merge(daily_data, daily_aggs, by = "jul_day")
  daily_data$Date <- NULL
  # Reorder columns to more closely match what Pinja was using.
  daily_data <- daily_data[, c(2:4, 1, 5:7, 10:8)]
  ################################################################################
  # Writing out to a space separated file, as apparently csv didn't work as well.
  write.table(daily_data, paste0("climate_kumpula_", year, ".prn"), quote = FALSE, row.names = FALSE)
}



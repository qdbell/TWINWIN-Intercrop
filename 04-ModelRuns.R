################################################################################
# Title: Model Runs
# Author: Quentin Bell (quentin.bell@fmi.fi)
# Intention: This script takes the observation inputs and runs the model and
# calibration code for all experiments and forms used in the later analysis.
# The purpose of separating these is to ensure that model runs remain consistent
# and avoid costly rerunning when not needed.
################################################################################
# Libraries
library(SticsOnR)
library(SticsRFiles)
library(tidyverse)
library(xfun) # Specifically for cache_rds to reduce rerunning.
################################################################################
# Core options and set up
setwd("~/Documents/TWINWIN-Intercrop/RunOutput/ModelRuns")
dir_path <- "/home/bell/Documents/TWINWIN-Intercrop/RunOutput/ModelRuns"
fourDEnVarPath <- "/home/bell/Documents/TWINWIN-Intercrop/4DEnVar_engine/4DEnVar"
set.seed(2)
################################################################################
# Stics Options Set Up
# Path to the folder containing Stics and workspace/example folders.
javastics <- "/home/bell/Documents/TWINWIN-Intercrop/Stics_v9.2"
# Path to the workspace containing our weather data and crop initialisations.
workspace <- "/home/bell/Documents/TWINWIN-Intercrop/Stics_v9.2/TWINWIN_workspace"
# Path to store simulation results in, while the previous directories must
# exist, this may be created automatically
output    <- "/home/bell/Documents/TWINWIN-Intercrop/Stics_v9.2/TWINWIN_output"
# Set options for stics_wrapper, where is the data (workspace) and where to find
# the Stics executables (from the base folder of Stics as given by javastics).
# CORRECTION: SticsOnR states that this should be the workspace folder, but it
# is actually the out_dir folder from SticsRFiles::gen_usms_xml2txt() (which is
# used to convert the STICS .xml input files into text files for use with the
# command-line version of STICS rather than JavaSTICS), running stics_wrapper
# with the workspace folder leads to no Stics directories being found as they
# were created in another directory.
sim_options <- stics_wrapper_options(javastics = javastics, workspace = output, verbose = FALSE, time_display = FALSE, parallel = TRUE)
################################################################################
# Functions
run_ensemble <- function(ensemble_size, parameter_ensemble, name_USM,
                         sim_options) {
  # This function takes an ensemble size, a set of parameter ensembles, and a
  # name or names of USMs to run STICS with the given options. The results are
  # returned as a list of STICS output for each run in the ensemble.
  # Create an empty list to store the ensemble results before we output them.
  ensemble_results <- list()
  # Loop over the ensemble members, running STICS for each parameter set.
  for (i in 1:ensemble_size) {
    # Generate name for this member of the ensemble results
    name <- paste0("Results_", i)
    # Run STICS, capturing the output
    stics_output <- stics_wrapper(model_options = sim_options,
                                  param_values = parameter_ensemble[i, ],
                                  situation = name_USM,
                                  var = c("mafruit", "lai_n", "dltams_n",
                                          "CO2sol", "MSexporte", "masec_n"))
    # If there is an error message print a warning detailing in which run.
    if (stics_output$error) warning(paste0("Error in ", name_USM, " run ", i))
    # Store only the simulation list.
    ensemble_results[[i]] <- stics_output$sim_list
    # Name the stored simulation.
    names(ensemble_results)[i] <- name
  }
  return(ensemble_results)
}

write_calib_files <- function(hx, y, R, hx_bar, suffix = "",
                              dir_path = "~/Documents/TWINWIN-Intercrop/RunOutput/ModelRuns/TwinExperiment") {
  # Writing the output files that 4DEnVar needs.
  write.table(hx, paste0(dir_path, "/hx_", suffix, ".dat"),
              col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
  write.table(y, paste0(dir_path, "/y_", suffix, ".dat"),
              col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
  write.table(R, paste0(dir_path, "/R_", suffix, ".dat"),
              col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
  write.table(hx_bar, paste0(dir_path, "/hx_bar_", suffix, ".dat"),
              col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
}

gen_4DEnVar_input <- function(usm_calib, obs, prior_est, dir_path,
                              run_suffix = "", xb = NULL) {
  # This function generates the matrices needed for 4DEnVar.
  # The required inputs are: the usm name being used for calibration, which is
  # currently restricted to only one, but in future may need to be increased;
  # the list obs, containing one or more elements of the form
  # obs_type = data frame containing dates of observations, values, associated
  # uncertainty, and which plot type (i.e. undersown species);
  # the data of prior estimates;
  # the directory path that the output files will be created in;
  # optionally the suffix to attach to this run (for when there is another
  # signifier that is not part of the observation set or usm being used to
  # identify the run);
  # and optionally the prior ensemble xb if there is modification of that
  # happening, currently this writes a single xb per function call, if xb ever
  # changes between usms this will either require separate function calls or
  # other reworking of the function.

  # This currently only works for calibrating a single usm and hence a single
  # year. It is not possible for it to calibrate multiple years (or multiple
  # usms of any kind) at the moment.
  for (name in usm_calib) {
    # Read in specific estimation and observation data for this variety:
    est <- prior_est %>%
      filter(usm == name)
    n_ens <- length(unique(est$ens))
    # First getting the acronym used for this undersown species.
    infix <- gsub("[Barley_1290long]", "", name)
    year <- gsub("[A-Za-z_]", "", name)
    if (infix == "s") {
      infix <- "Barley"
    }

    use_yield <- "yield" %in% names(obs)
    use_lai <- "lai" %in% names(obs)
    use_nee <- "nee" %in% names(obs)

    yield_obs <- NULL
    lai_obs <- NULL
    nee_obs <- NULL

    var_yield_obs <- NULL
    var_lai_obs <- NULL
    var_nee_obs <- NULL

    if (use_yield) {
      crop_yield <- obs$yield[which(obs$yield$crop == infix), ]
      date_yield_obs <- crop_yield$date
      yield_obs <- crop_yield$value
      var_yield_obs <- crop_yield$uncertainty^2
    }

    if (use_lai) {
      crop_lai <- obs$lai[which(obs$lai$crop == infix), ]
      date_lai_obs <- crop_lai$date
      lai_obs <- crop_lai$value
      var_lai_obs <- crop_lai$uncertainty^2
    }

    if (use_nee){
      crop_nee <- obs$nee[which(obs$nee$crop == infix), ]
      date_nee_obs <- crop_nee$date
      nee_obs <- crop_nee$value
      var_nee_obs <- crop_nee$uncertainty^2
    }

    # Now declaring lengths of our observations
    n_obs_yield <- length(yield_obs)
    n_obs_lai <- length(lai_obs)
    n_obs_nee <- length(nee_obs)
    n_obs_calib <- n_obs_yield + n_obs_lai + n_obs_nee

    # Now store our observations and measurement uncertainty in y and R.
    y <- matrix(data = c(yield_obs, lai_obs, nee_obs), ncol = 1, nrow = n_obs_calib)

    if (n_obs_calib == 1) {
      # Have to handle the case where there is only one observation as diag then creates a square matrix of the given value's size.
      R <- c(var_yield_obs, var_lai_obs, var_nee_obs)
    } else {
      R <- diag(c(var_yield_obs, var_lai_obs, var_nee_obs))
    }
    hx <- matrix(ncol = n_ens, nrow = n_obs_calib)
    # y and R are created below, as both contain information on observations
    hx_bar <- matrix(ncol = 1, nrow = n_obs_calib)


    # Extract the estimated values for the observation dates acording to what was observed
    # When not varying yield or nee it is possible we could use the exact same readings out, but because I build them in place in the matrix, and there could be a different number of observations, I don't want to mess with it at the moment.
    for (i in 1 : n_ens) {
      if (use_yield) {
        hx[1 : n_obs_yield, i] <- est[est$ens == i, ]$mafruit_plant_1[which(est[est$ens == i, ]$date %in% date_yield_obs)]
      }
      if (use_lai) {
        hx[(n_obs_yield + 1) : (n_obs_yield + n_obs_lai) , i] <- est[est$ens == i, ]$lai_n_plant_1[which(est[est$ens == i, ]$date %in% date_lai_obs)]
      }
      if (use_nee) {
        # Control whether we calculate the NEE with growth from both plants or just the one plant being modelled.
        if (infix == "Barley") {
          # Calculate NEE as CO2sol / 1000 - 0.48 * (44 / 12) * dltams(n) with only the first plant.
          hx[(n_obs_yield + n_obs_lai + 1) : n_obs_calib, i] <- est[est$ens == i, ]$CO2sol_plant_1[which(est[est$ens == i, ]$date %in% date_nee_obs)] / 1000 - 0.48 * (44 / 12) * est[est$ens == i, ]$dltams_n_plant_1[which(est[est$ens == i, ]$date %in% date_nee_obs)]
        } else {
          # Calculate NEE as CO2sol / 1000 - 0.48 * (44 / 12) * dltams(n) with both plants contributing to the NEE by their growth and only one soil respiration term.
          hx[(n_obs_yield + n_obs_lai + 1) : n_obs_calib, i] <- est[est$ens == i, ]$CO2sol_plant_1[which(est[est$ens == i, ]$date %in% date_nee_obs)] / 1000 - 0.48 * (44 / 12) * (est[est$ens == i, ]$dltams_n_plant_1[which(est[est$ens == i, ]$date %in% date_nee_obs)] + est[est$ens == i, ]$dltams_n_plant_2[which(est[est$ens == i, ]$date %in% date_nee_obs)])
        }
      }
    }

    ##############################################################################
    # Calculate hx_bar, the mean of the ensemble predictions
    hx_bar <- rowMeans(hx)

    ##############################################################################
    write_calib_files(hx, y, R, hx_bar, suffix = paste0(infix, "_", paste0(sort(names(obs)), sep = "", collapse = ""), run_suffix), dir_path = dir_path)

    if (!is.null(xb)) {
      write.table(t(xb), paste0(dir_path, "/xb", run_suffix, ".dat"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
    }
  }
}

calc_cost <- function(param_vals, suffix) {
  # This function calculates the cost function, given a set of parameter values, and the suffix denoting which set up is used, accounting for possible crop set ups, which observations are used, and how many parameters are being varied.
  # Reading in the
  prior_param <- t(read.table(paste0(dir_path, "/xb_", length(param_vals), ".dat")))
  prior_obs <- read.table(paste0(dir_path, "/hx_", suffix, ".dat"))
  hhat_xb_bar <- read.table(paste0(dir_path, "/hx_bar_", suffix, ".dat"))
  obs <- read.table(paste0(dir_path, "/y_", suffix, ".dat"))
  obs_cov <- read.table(paste0(dir_path, "/R_", suffix, ".dat"))

  xb_bar <- colMeans(prior_param)
  Xbprime_inv <- MASS::ginv(t(prior_param - do.call(rbind, replicate(n_ens, xb_bar, simplify = FALSE))))
  w <- Xbprime_inv %*% (param_vals - xb_bar)
  # hhat_xb_bar <- apply(prior_obs, 1, mean) # ELEPHANT This mean isn't correct, but just there for the idea, I will need to take the mean for each measurement instance across ensemble members. So it somewhat depends on how I end up feeding in prior_obs.
  Ybprime <- as.matrix(prior_obs - do.call(rbind, replicate(n_ens, t(hhat_xb_bar), simplify = FALSE)))
  # Now calculate the approximate estimates, hhat_xb_bar + Ybprime * w, which is calculated as an update to the ensemble mean estimates by a weighted combination of the observation space perturbation matrix, and subtract the observations themselves.
  est_error <- as.matrix(hhat_xb_bar + Ybprime %*% w - obs)
  # Finally, combine and return the value of the cost function.
  return(1/2 * t(w) %*% w + 1/2 * t(est_error) %*%  solve(obs_cov) %*% est_error)
}
################################################################################
# USM sets
long_run_usms <- c("Barley_sole20_long", "Barley_AC20_long", "Barley_RC20_long",
                   "Barley_WC20_long", "Barley_FA20_long", "Barley_AA20_long",
                   "Barley_IR20_long", "Barley_CI20_long", "Barley_sole21_long",
                   "Barley_AC21_long", "Barley_RC21_long", "Barley_WC21_long",
                   "Barley_FA21_long", "Barley_AA21_long", "Barley_IR21_long",
                   "Barley_CI21_long")
calib_usms <- long_run_usms[str_detect(long_run_usms, "20")]
all_usms <- long_run_usms
################################################################################
# Parameter Setup
# These are a selection of sensitive and influential parameters based on the
# interactive plotting of target variables while varying parameters.
name_params <- c("adens", "efcroiveg", "vitircarb", "INNmin",
                 "stlevdrp", "efcroirepro", "vlaimax", "dlaimax")
# These parameters are identical in single and intercropped barley, so we can
# retrieve the baseline values from the sole barley files.
plant <- get_plant_txt(file.path(paste0(output, "/", all_usms[1]), "ficplt1.txt"))
# The cork variety is an appropriate baseline choice.
params <- as.data.frame(plant[names(plant) %in% name_params])["cork", ]
n_dims <- length(params)
# Reorder the parameters so that the most sensitive is the first and the least is the last in the vector
ordered_names <- names(params)
params <- params[order(factor(ordered_names, levels = name_params))]
rm(plant)
saveRDS(params, file = paste0(dir_path, "/AnalysisInputs/baseline_params.rds"))
################################################################################
# Setting up synthetic parameter sets.
# Starting with 20 possible synthetic truths that are randomly drawn from the parameter means with 10% standard deviation, with the exception of vlaimax as detailed below.
num_alt_truths <- 20
rand_params <- data.frame(params)[rep(1, num_alt_truths), 1 : n_dims]
rownames(rand_params) <- c(paste0("rand_", 1 : num_alt_truths))
for (i in 1 : n_dims) {
  if(names(rand_params)[i] == "vlaimax") { # Since vlaimax is close to the edge of its valid range, want to avoid overshooting. It doesn't appear to cause much trouble when not far below the lower bound, but there do seem to be undesirable effects further away from the valid range.
    rand_params[, i] <-rnorm(nrow(rand_params), rand_params[1, i], abs(rand_params[1, i]) * 0.05)
  } else {
    rand_params[, i] <-rnorm(nrow(rand_params), rand_params[1, i], abs(rand_params[1, i]) * 0.1)
  }
}
# Looking at the drawn values and in particular for any that have a clear difference between the default parameters and the drawn ones. Initially I think the focus should be on ensuring that they are distinct from the baseline parameters, so the minimum percentage difference is what I care about more.
# rand_diff_pct <- NULL
# for (i in 1 : nrow(rand_params)) {
#   rand_diff_pct <- rbind(rand_diff_pct, round((rand_params[i, ] - params[1 : n_dims])/abs(params[1 : n_dims]) * 100, 1))
# }
# rand_diff_pct
# sort(apply(rand_diff_pct, 1, function(x) min(abs(x))), decreasing = TRUE)
# sort(apply(rand_diff_pct, 1, function(x) mean(abs(x))), decreasing = TRUE)
# Using the first parameter set generated to create our main synthetic truth, the others will be used to check the decisions made.
# synth_truth <- rand_params[1, ]
# Setting up a parameter set that is baseline parameters for all but either 1 to 8 of the parameters, which are replaced by the synthetic parameter set
synth_params <- data.frame(params)[rep(1, n_dims * num_alt_truths), 1 : n_dims]
rownames(synth_params) <- do.call(paste0, expand.grid(paste0("_params_changed_", 1 : n_dims), paste0("truth_", 1:20))[, c(2, 1)])
for (i in 1 : (n_dims * num_alt_truths)) {
  num_params_varied <- (i - 1) %% 8 + 1
  which_truth <- as.integer((i - 1) / 8 + 1)
  synth_params[i, 1 : num_params_varied] <- rand_params[which_truth, 1 : num_params_varied]
}
saveRDS(synth_params, file = paste0(dir_path, "/AnalysisInputs/synth_params.rds"))
################################################################################
# Running the model to obtain the synthetic observations.
synth_ens <- cache_rds(run_ensemble(nrow(synth_params), synth_params, calib_usms, sim_options), file = "synth_truth", dir = dir_path, hash = list(synth_params, calib_usms))
# Combine the ensemble results into a single dataframe.
tmp_est_res <- list()
for (i in 1 : nrow(synth_params)) {
  tmp_est_res[[i]] <- data.table::rbindlist(synth_ens[[i]], use.names = TRUE, idcol = "usm")
}
synth_long <- data.table::rbindlist(tmp_est_res, use.names = TRUE, idcol = "truth_params_changed")
synth_long$truth_params_changed <- rownames(synth_params)[synth_long$truth_params_changed]

synth_truth <- synth_long %>%
  separate_wider_delim(truth_params_changed, "_", names = c(NA, "truth_num", NA, NA, "params_changed")) %>%
  pivot_wider(names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n)) %>%
  mutate(date = as.Date(Date), .keep = "unused", .after = usm)

# Set up nee and crop labels
synth_truth$nee <- synth_truth$CO2sol_plant_1 / 1000 - 0.48 * (44 / 12) * (synth_truth$dltams_n_plant_1 + synth_truth$dltams_n_plant_2)
synth_truth$crop <- gsub("[Barley_1290long]", "", synth_truth$usm)
synth_truth$crop[synth_truth$crop == "s"] <- "Barley"
# Fix the barley single crop nee calculations
synth_truth$nee[which(is.na(synth_truth$nee))] <- synth_truth$CO2sol_plant_1[which(is.na(synth_truth$nee))] / 1000 - 0.48 * (44 / 12) * (synth_truth$dltams_n_plant_1[which(is.na(synth_truth$nee))])
saveRDS(synth_truth, file = paste0(dir_path, "/AnalysisInputs/synth_truth.rds"))
rm(synth_long, synth_ens)
################################################################################
# Main twin experiment prior, 500 ensemble members, covering 2-8 parameters being varied (approximately in order of sensitivity).
# There's a possiblity to use the runs done here for the TWINWIN calibration too, but then the 2021 priors will need to be run separately and combined later.
n_ens <- 500
xb <- matrix(ncol = n_dims, nrow = n_ens)
colnames(xb) <- name_params[1 : n_dims]
for (i in 1 : n_dims) {
  if(names(params[i]) == "vlaimax") { # Since vlaimax is close to the edge of its valid range, want to avoid overshooting. It doesn't appear to cause much trouble when not far below the lower bound, but there do seem to be undesirable effects further away from the valid range.
    xb[, i] <- rnorm(n_ens, params[[i]], abs(params[[i]] * 0.1))
  } else {
    xb[, i] <- rnorm(n_ens, params[[i]], abs(params[[i]] * 0.2))
  }
}

twin_exp_prior <- data.frame()
for (num_params_varied in c(2:8)) {
  print(paste0("Prior estimation for ", num_params_varied, " parameters varying."))
  stics_results <- cache_rds(run_ensemble(n_ens, xb[, 1 : num_params_varied], calib_usms, sim_options), file = paste0("twin_exp_prior_", num_params_varied, "params"), dir = dir_path, hash = list(xb, calib_usms))
  tmp_res <- list()
  # Extract all members of the ensemble, label them as the prior for future use.
  for (i in 1 : n_ens) {
    tmp <- data.table::rbindlist(stics_results[[i]], use.names = TRUE, idcol = "usm")
    tmp$dist <- "Prior"
    tmp_res[[i]] <- tmp
  }
  tmp_prior <- data.table::rbindlist(tmp_res, use.names = TRUE, idcol = "ens")
  tmp_prior$varying <- num_params_varied
  twin_exp_prior <- rbind(twin_exp_prior, tmp_prior)
  write.table(t(xb[, 1 : num_params_varied]), paste0(dir_path, "/xb_", num_params_varied, ".dat"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
}

twin_exp_prior <- pivot_wider(twin_exp_prior, names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n)) %>%
  mutate(date = as.Date(Date), .keep = "unused", .after = usm) %>%
  mutate(crop = gsub("[Barley_1290long]", "", usm), .after = usm) %>%
  mutate(nee = CO2sol_plant_1 / 1000 - 0.48 * (44 / 12) * (dltams_n_plant_1 + dltams_n_plant_2))
twin_exp_prior$crop[twin_exp_prior$crop == "s"] <- "Barley"
twin_exp_prior$nee[which(is.na(twin_exp_prior$nee))] <- twin_exp_prior$CO2sol_plant_1[which(is.na(twin_exp_prior$nee))] / 1000 - 0.48 * (44 / 12) * (twin_exp_prior$dltams_n_plant_1[which(is.na(twin_exp_prior$nee))])

saveRDS(xb, file = paste0(dir_path, "/AnalysisInputs/twin_xb.rds"))
saveRDS(twin_exp_prior, file = paste0(dir_path, "/AnalysisInputs/twin_prior.rds"))
################################################################################
# twin experiment baseline posterior (2-8 parameters being varied, 500 ensemble) using 1st synthetic parameter set as the truth.
all_analysis_list <- list()
all_posterior_draws <- list()
twin_exp_post <- list()
for (num_params_varied in c(2:8)) {
  print(paste0("Running 4DEnVar on ", num_params_varied, " parameters varying."))
  basic_truth <- synth_truth %>%
    filter(truth_num == "1", params_changed == num_params_varied)
  basic_prior <- twin_exp_prior %>%
    filter(varying == num_params_varied)

  # Setting these as the initial observations dates being used for the synthetic truth, as these are approximately the frequency and spread of observations we actually have.
  yield_obs_dates <- c(as.Date(c("2020-9-11"), format = "%Y-%m-%d")) # , as.Date(c("2021-9-14"), format = "%Y-%m-%d")
  nee_obs_dates <- as.Date(c("2020-06-03", "2020-06-15", "2020-06-29", "2020-07-03", "2020-07-14", "2020-07-27", "2020-07-31", "2020-08-11", "2020-08-27", "2020-08-28")) # , "2021-06-28", "2021-07-22", "2021-08-30"
  lai_obs_dates <- as.Date(c("2020-06-04", "2020-06-18", "2020-07-01", "2020-07-16", "2020-07-27", "2020-08-28")) # , "2021-07-01", "2021-07-26", "2021-08-23"

  yield_obs <- basic_truth[which(basic_truth$date %in% yield_obs_dates), c("date", "crop", "mafruit_plant_1")]
  lai_obs <- basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "lai_n_plant_1")]
  nee_obs <-  basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "nee")]

  # Starting with a fixed 1% uncertainty.
  UNCERTAINTY_PROP <- 0.01

  yield_uncertainty <- yield_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(mafruit_plant_1) * UNCERTAINTY_PROP)

  lai_uncertainty <- lai_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(lai_n_plant_1) * UNCERTAINTY_PROP)

  nee_uncertainty <- nee_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(abs(nee)) * UNCERTAINTY_PROP) # Introducing the absolute value here as nee can be negative and I don't want to risk zero uncertainty. The overall effect should not be much as the negative values are usually much smaller than the positive values, but also this makes this slightly more uncertain, which reflects my opinion of the real measurements.

  yield_obs <- merge(yield_obs, yield_uncertainty)
  lai_obs <- merge(lai_obs, lai_uncertainty)
  nee_obs <- merge(nee_obs, nee_uncertainty)

  # Rename the column of the actual observation value for consistency and simple formatting for the function to handle
  colnames(yield_obs)[3] <- "value"
  colnames(lai_obs)[3] <- "value"
  colnames(nee_obs)[3] <- "value"

  obs <- list(yield = yield_obs, lai = lai_obs, nee = nee_obs)
  obs_names <- paste0(sort(names(obs)), sep = "", collapse = "")

  gen_4DEnVar_input(calib_usms, obs, basic_prior, dir_path = dir_path, run_suffix = paste0("_varying_", num_params_varied))

  # Call 4DEnVar directly
  analysis_list <- list()
  posterior_draws <- list()
  # all_costs <- list()
  for (name in calib_usms) {
    infix <- gsub("[Barley_1290long]", "", name)
    if (infix == "s") {
      infix <- "Barley"
    }
    print(infix)
    tmp <- system2(command = fourDEnVarPath,
                   args = c(paste0(dir_path, "/xb_", num_params_varied, ".dat"),
                            paste0(dir_path, "/", c("hx", "y", "R", "hx_bar"), "_", infix, "_", obs_names, "_varying_", num_params_varied, ".dat")),
                   wait = TRUE,
                   stdout = TRUE
    )
    # Read from the output the analysis vector xa (the "best" estimate) and the posterior ensemble xa_draws.
    xa <- as.numeric(tmp[1 : num_params_varied])
    names(xa) <- name_params[1 : num_params_varied]
    analysis_list[[name]] <- as.data.frame(t(xa))
    xa_draws <- apply((str_split_fixed(tmp[(1 + num_params_varied) + 1 : num_params_varied], " ", n_ens)), 1:2, as.numeric)
    rownames(xa_draws) <- name_params[1 : num_params_varied]
    posterior_draws[[name]] <- as.data.frame(t(xa_draws))

    # all_costs[[paste0(name, ":", num_params_varied)]] <- round(calc_cost(xa, paste0(infix, "_", obs_names, "_varying_", num_params_varied)), 1)
    all_analysis_list[[paste0(name, ":", num_params_varied)]] <- as.data.frame(t(xa))
    all_posterior_draws[[paste0(name, ":", num_params_varied)]] <- as.data.frame(t(xa_draws))
  }

  # Estimate posteriors based on the above calibrations.
  post_est <- NULL
  for (obs in obs_names) {
    post_est_run <- NULL
    for (name in calib_usms) {
      # First getting the infix used for this undersown species.
      infix <- gsub("[Barley_1290long]", "", name)
      if (infix == "s") {
        infix <- "Barley"
      }

      post_dist <- posterior_draws[[name]]
      # Run the posterior ensemble estimation through Stics
      usms_run <- calib_usms[str_detect(calib_usms, infix)]
      tmp_post_est <- cache_rds(run_ensemble(n_ens, post_dist, usms_run, sim_options), file = paste0("basic_posterior_", infix, "_", num_params_varied, "params"), dir = dir_path, hash = list(xb, calib_usms, posterior_draws[[name]]))
      tmp_post_res <- list()
      for (i in 1 : n_ens) {
        tmp_post_res[[i]] <- data.table::rbindlist(tmp_post_est[[i]], use.names = TRUE, idcol = "usm")
      }
      post_tmp <- data.table::rbindlist(tmp_post_res, use.names = TRUE, idcol = "ens")
      post_tmp$obs_calib <- obs
      post_tmp$usm_calib <- infix
      post_est_run <- rbind(post_est_run, post_tmp)
    }
    # Combining each parameter set run into one.
    post_est <- rbind(post_est, post_est_run)
  }
  warnings()
  post_est$dist <- "Posterior"
  post_est <- pivot_wider(post_est, names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n))
  colnames(post_est)[3] <- "date"
  post_est$date <- as.Date(post_est$date)
  twin_exp_post[[num_params_varied]] <- post_est
}
twin_exp_post <- data.table::rbindlist(twin_exp_post, use.names = TRUE, idcol = "num_params_varied")
twin_exp_post$nee <- twin_exp_post$CO2sol_plant_1 / 1000 - 0.48 * (44 / 12) * (twin_exp_post$dltams_n_plant_1 + twin_exp_post$dltams_n_plant_2)
twin_exp_post$crop <- gsub("[Barley_1290long]", "", twin_exp_post$usm)
twin_exp_post$crop[twin_exp_post$crop == "s"] <- "Barley"
# Fix the barley single crop nee calculations
twin_exp_post$nee[which(is.na(twin_exp_post$nee))] <- twin_exp_post$CO2sol_plant_1[which(is.na(twin_exp_post$nee))] / 1000 - 0.48 * (44 / 12) * (twin_exp_post$dltams_n_plant_1[which(is.na(twin_exp_post$nee))])

twin_analysis <- all_analysis_list %>% data.table::rbindlist(use.names = TRUE, idcol = "usm", fill = TRUE) %>%
  separate_wider_delim(usm, ":", names = c("usm", "num_params")) %>%
  pivot_longer(2 + 1 : 8, names_to = "parameter", values_drop_na = TRUE)
twin_posterior_draws <- all_posterior_draws %>% data.table::rbindlist(use.names = TRUE, idcol = "usm", fill = TRUE) %>%
  separate_wider_delim(usm, ":", names = c("usm", "num_params")) %>%
  pivot_longer(2 + 1 : 8, names_to = "parameter", values_drop_na = TRUE)


saveRDS(twin_analysis, file = paste0(dir_path, "/AnalysisInputs/twin_analysis.rds"))
saveRDS(twin_posterior_draws, file = paste0(dir_path, "/AnalysisInputs/twin_posterior_draws.rds"))
saveRDS(twin_exp_post, file = paste0(dir_path, "/AnalysisInputs/twin_posterior.rds"))
rm(twin_exp_post, tmp_post_res, all_analysis_list, all_posterior_draws, tmp_post_est, post_tmp, post_est_run, post_est, twin_analysis, twin_posterior_draws)
################################################################################
# twin experiment with perfect measurement coverage (full time series of synthetic observations) during the barley growing season (sowing until harvest)
all_analysis_list <- list()
all_posterior_draws <- list()
twin_exp_post <- list()
for (num_params_varied in c(2:8)) {
  print(paste0("Running 4DEnVar on ", num_params_varied, " parameters varying. Full timeseries of measurements"))
  basic_truth <- synth_truth %>%
    filter(truth_num == "1", params_changed == num_params_varied)
  basic_prior <- twin_exp_prior %>%
    filter(varying == num_params_varied)

  # Only variation with the previous is setting these tiumeseries to cover every day from bartley sowing to harvest.
  yield_obs_dates <- unique(basic_truth$date)[between(unique(basic_truth$date), as.Date("2020-05-22", format = "%Y-%m-%d"), as.Date(c("2020-9-11"), format = "%Y-%m-%d"))]
  nee_obs_dates <- unique(basic_truth$date)[between(unique(basic_truth$date), as.Date("2020-05-22", format = "%Y-%m-%d"), as.Date(c("2020-9-11"), format = "%Y-%m-%d"))]
  lai_obs_dates <- unique(basic_truth$date)[between(unique(basic_truth$date), as.Date("2020-05-22", format = "%Y-%m-%d"), as.Date(c("2020-9-11"), format = "%Y-%m-%d"))]

  yield_obs <- basic_truth[which(basic_truth$date %in% yield_obs_dates), c("date", "crop", "mafruit_plant_1")]
  lai_obs <- basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "lai_n_plant_1")]
  nee_obs <-  basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "nee")]

  # Starting with a fixed 1% uncertainty.
  UNCERTAINTY_PROP <- 0.01

  yield_uncertainty <- yield_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(mafruit_plant_1) * UNCERTAINTY_PROP)

  lai_uncertainty <- lai_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(lai_n_plant_1) * UNCERTAINTY_PROP)

  nee_uncertainty <- nee_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(abs(nee)) * UNCERTAINTY_PROP) # Introducing the absolute value here as nee can be negative and I don't want to risk zero uncertainty. The overall effect should not be much as the negative values are usually much smaller than the positive values, but also this makes this slightly more uncertain, which reflects my opinion of the real measurements.

  yield_obs <- merge(yield_obs, yield_uncertainty)
  lai_obs <- merge(lai_obs, lai_uncertainty)
  nee_obs <- merge(nee_obs, nee_uncertainty)

  # Rename the column of the actual observation value for consistency and simple formatting for the function to handle
  colnames(yield_obs)[3] <- "value"
  colnames(lai_obs)[3] <- "value"
  colnames(nee_obs)[3] <- "value"

  obs <- list(yield = yield_obs, lai = lai_obs, nee = nee_obs)
  obs_names <- paste0(sort(names(obs)), sep = "", collapse = "")

  gen_4DEnVar_input(calib_usms, obs, basic_prior, dir_path = dir_path, run_suffix = paste0("_perfect_varying_", num_params_varied))

  # Call 4DEnVar directly
  analysis_list <- list()
  posterior_draws <- list()
  # all_costs <- list()
  for (name in calib_usms) {
    infix <- gsub("[Barley_1290long]", "", name)
    if (infix == "s") {
      infix <- "Barley"
    }
    print(infix)
    tmp <- system2(command = fourDEnVarPath,
                   args = c(paste0(dir_path, "/xb_", num_params_varied, ".dat"),
                            paste0(dir_path, "/", c("hx", "y", "R", "hx_bar"), "_", infix, "_", obs_names, "_perfect_varying_", num_params_varied, ".dat")),
                   wait = TRUE,
                   stdout = TRUE
    )
    # Read from the output the analysis vector xa (the "best" estimate) and the posterior ensemble xa_draws.
    xa <- as.numeric(tmp[1 : num_params_varied])
    names(xa) <- name_params[1 : num_params_varied]
    analysis_list[[name]] <- as.data.frame(t(xa))
    xa_draws <- apply((str_split_fixed(tmp[(1 + num_params_varied) + 1 : num_params_varied], " ", n_ens)), 1:2, as.numeric)
    rownames(xa_draws) <- name_params[1 : num_params_varied]
    posterior_draws[[name]] <- as.data.frame(t(xa_draws))

    # all_costs[[paste0(name, ":", num_params_varied)]] <- round(calc_cost(xa, paste0(infix, "_", obs_names, "_varying_", num_params_varied)), 1)
    all_analysis_list[[paste0(name, ":", num_params_varied)]] <- as.data.frame(t(xa))
    all_posterior_draws[[paste0(name, ":", num_params_varied)]] <- as.data.frame(t(xa_draws))
  }

  # Estimate posteriors based on the above calibrations.
  post_est <- NULL
  for (obs in obs_names) {
    post_est_run <- NULL
    for (name in calib_usms) {
      # First getting the infix used for this undersown species.
      infix <- gsub("[Barley_1290long]", "", name)
      if (infix == "s") {
        infix <- "Barley"
      }

      post_dist <- posterior_draws[[name]]
      # Run the posterior ensemble estimation through Stics
      usms_run <- calib_usms[str_detect(calib_usms, infix)]
      tmp_post_est <- cache_rds(run_ensemble(n_ens, post_dist, usms_run, sim_options), file = paste0("perfect_twin_posterior_", infix, "_", num_params_varied, "params"), dir = dir_path, hash = list(xb, calib_usms, posterior_draws[[name]]))
      tmp_post_res <- list()
      for (i in 1 : n_ens) {
        tmp_post_res[[i]] <- data.table::rbindlist(tmp_post_est[[i]], use.names = TRUE, idcol = "usm")
      }
      post_tmp <- data.table::rbindlist(tmp_post_res, use.names = TRUE, idcol = "ens")
      post_tmp$obs_calib <- obs
      post_tmp$usm_calib <- infix
      post_est_run <- rbind(post_est_run, post_tmp)
    }
    # Combining each parameter set run into one.
    post_est <- rbind(post_est, post_est_run)
  }
  warnings()
  post_est$dist <- "Posterior"
  post_est <- pivot_wider(post_est, names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n))
  colnames(post_est)[3] <- "date"
  post_est$date <- as.Date(post_est$date)
  twin_exp_post[[num_params_varied]] <- post_est
}
perfect_twin_exp_post <- data.table::rbindlist(twin_exp_post, use.names = TRUE, idcol = "num_params_varied")
perfect_twin_exp_post$nee <- perfect_twin_exp_post$CO2sol_plant_1 / 1000 - 0.48 * (44 / 12) * (perfect_twin_exp_post$dltams_n_plant_1 + perfect_twin_exp_post$dltams_n_plant_2)
perfect_twin_exp_post$crop <- gsub("[Barley_1290long]", "", twin_exp_post$usm)
perfect_twin_exp_post$crop[perfect_twin_exp_post$crop == "s"] <- "Barley"
# Fix the barley single crop nee calculations
perfect_twin_exp_post$nee[which(is.na(perfect_twin_exp_post$nee))] <- perfect_twin_exp_post$CO2sol_plant_1[which(is.na(perfect_twin_exp_post$nee))] / 1000 - 0.48 * (44 / 12) * (perfect_twin_exp_post$dltams_n_plant_1[which(is.na(perfect_twin_exp_post$nee))])

perfect_twin_analysis <- all_analysis_list %>% data.table::rbindlist(use.names = TRUE, idcol = "usm", fill = TRUE) %>%
  separate_wider_delim(usm, ":", names = c("usm", "num_params")) %>%
  pivot_longer(2 + 1 : 8, names_to = "parameter", values_drop_na = TRUE)
perfect_twin_posterior_draws <- all_posterior_draws %>% data.table::rbindlist(use.names = TRUE, idcol = "usm", fill = TRUE) %>%
  separate_wider_delim(usm, ":", names = c("usm", "num_params")) %>%
  pivot_longer(2 + 1 : 8, names_to = "parameter", values_drop_na = TRUE)


saveRDS(perfect_twin_analysis, file = paste0(dir_path, "/AnalysisInputs/perfect_twin_analysis.rds"))
saveRDS(perfect_twin_posterior_draws, file = paste0(dir_path, "/AnalysisInputs/perfect_twin_posterior_draws.rds"))
saveRDS(perfect_twin_exp_post, file = paste0(dir_path, "/AnalysisInputs/perfect_twin_posterior.rds"))
rm(twin_exp_post, tmp_post_res, all_analysis_list, all_posterior_draws, tmp_post_est, post_tmp, post_est_run, post_est, perfect_twin_analysis, perfect_twin_posterior_draws, perfect_twin_exp_post)
################################################################################
# Twin experiment 3+1 truths, priors
# Need to take a set of synthetic parameters and create all combinations of the core 3 plus one of the remaining 5. Create the synthetic truth for each, and a prior for each. Then run calibration but hold off on posteriors for the moment.
# Starting with the baseline of the first parameter set generated earlier.
# This should be easily expandable to run this for alternate truths, apart from including which truth was used in calibration we just need to make the parameter set a list in the same way as we did for xb.
fourth_param_set <- data.frame(extra_param = names(rand_params)[4:8], adens = rand_params[1, 1], efcroiveg = rand_params[1, 2], vitircarb = rand_params[1, 3], extra_value = t(rand_params[1, 4:8])) %>%
  rename(extra_value = rand_1)
extra_params <- rownames(fourth_param_set)
fourth_truth <- list()
fourth_xb <- list()
fourth_prior <- data.frame()
for (i in 1 : length(extra_params)) {
  fourth_params <- fourth_param_set[i, 2 : 5]
  colnames(fourth_params)[4] <- extra_params[i]
  fourth_truth[[extra_params[i]]] <- cache_rds(run_ensemble(1, fourth_params, calib_usms, sim_options), file = "fourth_truth", dir = dir_path, hash = list(fourth_param_set, calib_usms))[[1]] %>%
    data.table::rbindlist(use.names = TRUE, idcol = "usm")

  # creating an appropriate set of prior ensembles at the same time.
  fourth_xb[[extra_params[i]]] <- xb[, c(1:3, 3 + i)]

  print(paste0("Prior estimation for 3 core and ", extra_params[i], " varying."))
  stics_results <- cache_rds(run_ensemble(n_ens, fourth_xb[[extra_params[i]]], calib_usms, sim_options), file = paste0("three_plus_one_prior_", extra_params[i]), dir = dir_path, hash = list(xb, calib_usms, extra_params))
  tmp_res <- list()
  # Extract all members of the ensemble, label them as the prior for future use.
  for (j in 1 : n_ens) {
    tmp <- data.table::rbindlist(stics_results[[j]], use.names = TRUE, idcol = "usm")
    tmp$dist <- "Prior"
    tmp_res[[j]] <- tmp
  }
  tmp_prior <- data.table::rbindlist(tmp_res, use.names = TRUE, idcol = "ens")
  tmp_prior$extra_param <- extra_params[i]
  fourth_prior <- rbind(fourth_prior, tmp_prior)
  write.table(t(as.matrix(fourth_xb[[extra_params[i]]])), paste0(dir_path, "/xb_fourth_", extra_params[i], ".dat"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
}
fourth_truth <- data.table::rbindlist(fourth_truth, use.names = TRUE, idcol = "extra_param") %>%
  pivot_wider(names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n)) %>%
  mutate(date = as.Date(Date), .keep = "unused", .after = usm) %>%
  mutate(nee = CO2sol_plant_1 / 1000 - 0.48 * (44 / 12) * (dltams_n_plant_1 + dltams_n_plant_2), crop = gsub("[Barley_1290long]", "", usm))
# Fix the sole barley issues
fourth_truth$crop[fourth_truth$crop == "s"] <- "Barley"
fourth_truth$nee[which(is.na(fourth_truth$nee))] <- fourth_truth$CO2sol_plant_1[which(is.na(fourth_truth$nee))] / 1000 - 0.48 * (44 / 12) * (fourth_truth$dltams_n_plant_1[which(is.na(fourth_truth$nee))])

fourth_prior <- pivot_wider(fourth_prior, names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n)) %>%
  mutate(date = as.Date(Date), .keep = "unused", .after = usm) %>%
  mutate(crop = gsub("[Barley_1290long]", "", usm), .after = usm) %>%
  mutate(nee = CO2sol_plant_1 / 1000 - 0.48 * (44 / 12) * (dltams_n_plant_1 + dltams_n_plant_2))
fourth_prior$crop[fourth_prior$crop == "s"] <- "Barley"
fourth_prior$nee[which(is.na(fourth_prior$nee))] <- fourth_prior$CO2sol_plant_1[which(is.na(fourth_prior$nee))] / 1000 - 0.48 * (44 / 12) * (fourth_prior$dltams_n_plant_1[which(is.na(fourth_prior$nee))])

# Running calibration
fourth_analysis_list <- list()
fourth_posterior_draws <- list()
for (i in 1 : length(extra_params)) {
  print(paste0("Running 4DEnVar on 3 core and ", extra_params[i], " varying."))
  basic_truth <- fourth_truth %>%
    filter(extra_param == extra_params[i])
  basic_prior <- fourth_prior %>%
    filter(extra_param == extra_params[i])

  # Setting these as the initial observations dates being used for the synthetic truth, as these are approximately the frequency and spread of observations we actually have.
  yield_obs_dates <- c(as.Date(c("2020-9-11"), format = "%Y-%m-%d")) # , as.Date(c("2021-9-14"), format = "%Y-%m-%d")
  nee_obs_dates <- as.Date(c("2020-06-03", "2020-06-15", "2020-06-29", "2020-07-03", "2020-07-14", "2020-07-27", "2020-07-31", "2020-08-11", "2020-08-27", "2020-08-28")) # , "2021-06-28", "2021-07-22", "2021-08-30"
  lai_obs_dates <- as.Date(c("2020-06-04", "2020-06-18", "2020-07-01", "2020-07-16", "2020-07-27", "2020-08-28")) # , "2021-07-01", "2021-07-26", "2021-08-23"

  yield_obs <- basic_truth[which(basic_truth$date %in% yield_obs_dates), c("date", "crop", "mafruit_plant_1")]
  lai_obs <- basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "lai_n_plant_1")]
  nee_obs <-  basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "nee")]

  # Starting with a fixed 1% uncertainty.
  UNCERTAINTY_PROP <- 0.01

  yield_uncertainty <- yield_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(mafruit_plant_1) * UNCERTAINTY_PROP)

  lai_uncertainty <- lai_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(lai_n_plant_1) * UNCERTAINTY_PROP)

  nee_uncertainty <- nee_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(abs(nee)) * UNCERTAINTY_PROP) # Introducing the absolute value here as nee can be negative and I don't want to risk zero uncertainty. The overall effect should not be much as the negative values are usually much smaller than the positive values, but also this makes this slightly more uncertain, which reflects my opinion of the real measurements.

  yield_obs <- merge(yield_obs, yield_uncertainty)
  lai_obs <- merge(lai_obs, lai_uncertainty)
  nee_obs <- merge(nee_obs, nee_uncertainty)

  # Rename the column of the actual observation value for consistency and simple formatting for the function to handle
  colnames(yield_obs)[3] <- "value"
  colnames(lai_obs)[3] <- "value"
  colnames(nee_obs)[3] <- "value"

  obs <- list(yield = yield_obs, lai = lai_obs, nee = nee_obs)
  obs_names <- paste0(sort(names(obs)), sep = "", collapse = "")

  gen_4DEnVar_input(calib_usms, obs, basic_prior, dir_path = dir_path, run_suffix = paste0("_fourth_", extra_params[i]))

  # Call 4DEnVar directly
  analysis_list <- list()
  posterior_draws <- list()
  all_costs <- list()
  for (name in calib_usms) {
    infix <- gsub("[Barley_1290long]", "", name)
    if (infix == "s") {
      infix <- "Barley"
    }
    print(infix)
    tmp <- system2(command = fourDEnVarPath,
                   args = c(paste0(dir_path, "/xb_fourth_", extra_params[i], ".dat"),
                            paste0(dir_path, "/", c("hx", "y", "R", "hx_bar"), "_", infix, "_", obs_names, "_fourth_", extra_params[i], ".dat")),
                   wait = TRUE,
                   stdout = TRUE
    )
    # Read from the output the analysis vector xa (the "best" estimate) and the posterior ensemble xa_draws.
    xa <- as.numeric(tmp[1 : 4])
    names(xa) <- name_params[1 : 4]
    analysis_list[[name]] <- as.data.frame(t(xa))
    xa_draws <- apply((str_split_fixed(tmp[(1 + 4) + 1 : 4], " ", n_ens)), 1:2, as.numeric)
    rownames(xa_draws) <- name_params[1 : 4]
    posterior_draws[[name]] <- as.data.frame(t(xa_draws))

    # all_costs[[paste0(name, ":", num_params_varied)]] <- round(calc_cost(xa, paste0(infix, "_", obs_names, "_varying_", num_params_varied)), 1)
    fourth_analysis_list[[paste0(name, ":", extra_params[i])]] <- as.data.frame(t(xa))
    fourth_posterior_draws[[paste0(name, ":", extra_params[i])]] <- as.data.frame(t(xa_draws))
  }
}

fourth_analysis <- data.table::rbindlist(fourth_analysis_list, idcol = "usm") %>%
  separate_wider_delim(usm, ":", names = c("usm", "extra_param")) %>%
  pivot_longer(2 + 1 : 4, names_to = "parameter")
fourth_draws <- data.table::rbindlist(fourth_posterior_draws, idcol = "usm") %>%
  separate_wider_delim(usm, ":", names = c("usm", "extra_param")) %>%
  pivot_longer(2 + 1 : 4, names_to = "parameter")

saveRDS(fourth_xb, file = paste0(dir_path, "/AnalysisInputs/fourth_xb.rds"))
saveRDS(fourth_prior, file = paste0(dir_path, "/AnalysisInputs/fourth_prior.rds"))
saveRDS(fourth_analysis, file = paste0(dir_path, "/AnalysisInputs/fourth_analysis.rds"))
saveRDS(fourth_draws, file = paste0(dir_path, "/AnalysisInputs/fourth_draws.rds"))
rm(fourth_prior, fourth_analysis, fourth_draws, fourth_xb, fourth_truth, fourth_posterior_draws, fourth_params, fourth_param_set, fourth_analysis_list)
################################################################################
# Ensemble size variation.
# Prior can be just 1000 ensemble members run on the chosen parameter set.
# Will subset from that to get the ensemble size, result should be similar to running them separately and more inefficiently.
# Then posterior is run with the 1st truth again across varying ensemble sizes.
large_ens <- 1000
xb_large <- matrix(ncol = 3, nrow = large_ens)
colnames(xb_large) <- name_params[1 : 3]
for (i in 1 : 3) {
  if(names(params[i]) == "vlaimax") { # Since vlaimax is close to the edge of its valid range, want to avoid overshooting. It doesn't appear to cause much trouble when not far below the lower bound, but there do seem to be undesirable effects further away from the valid range.
    xb_large[, i] <- rnorm(large_ens, params[[i]], abs(params[[i]] * 0.1))
  } else {
    xb_large[, i] <- rnorm(large_ens, params[[i]], abs(params[[i]] * 0.2))
  }
}

large_ens_prior <- data.frame()
for (num_params_varied in c(3)) {
  print(paste0("Prior estimation for ", num_params_varied, " parameters varying, large ensemble."))
  stics_results <- cache_rds(run_ensemble(large_ens, xb_large[, 1 : num_params_varied], calib_usms, sim_options), file = paste0("ens_size_prior_", num_params_varied, "params"), dir = dir_path, hash = list(xb_large, calib_usms))
  tmp_res <- list()
  # Extract all members of the ensemble, label them as the prior for future use.
  for (i in 1 : large_ens) {
    tmp <- data.table::rbindlist(stics_results[[i]], use.names = TRUE, idcol = "usm")
    tmp$dist <- "Prior"
    tmp_res[[i]] <- tmp
  }
  tmp_prior <- data.table::rbindlist(tmp_res, use.names = TRUE, idcol = "ens")
  tmp_prior$varying <- num_params_varied
  large_ens_prior <- rbind(large_ens_prior, tmp_prior)
  write.table(t(xb_large[, 1 : num_params_varied]), paste0(dir_path, "/xb_large_", num_params_varied, ".dat"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
}

large_ens_prior <- pivot_wider(large_ens_prior, names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n)) %>%
  mutate(date = as.Date(Date), .keep = "unused", .after = usm) %>%
  mutate(crop = gsub("[Barley_1290long]", "", usm), .after = usm)
large_ens_prior$crop[large_ens_prior$crop == "s"] <- "Barley"

# Calibrating with varying ensemble sizes
large_ens_analysis_list <- list()
large_ens_posterior_draws <- list()
for (num_params_varied in c(3)) {
  print(paste0("Running 4DEnVar on ", num_params_varied, " parameters varying with varied ensemble sizes."))
  basic_truth <- synth_truth %>%
    filter(truth_num == "1", params_changed == num_params_varied)
  basic_prior <- large_ens_prior %>%
    filter(varying == num_params_varied)

  # Setting these as the initial observations dates being used for the synthetic truth, as these are approximately the frequency and spread of observations we actually have.
  yield_obs_dates <- c(as.Date(c("2020-9-11"), format = "%Y-%m-%d")) # , as.Date(c("2021-9-14"), format = "%Y-%m-%d")
  nee_obs_dates <- as.Date(c("2020-06-03", "2020-06-15", "2020-06-29", "2020-07-03", "2020-07-14", "2020-07-27", "2020-07-31", "2020-08-11", "2020-08-27", "2020-08-28")) # , "2021-06-28", "2021-07-22", "2021-08-30"
  lai_obs_dates <- as.Date(c("2020-06-04", "2020-06-18", "2020-07-01", "2020-07-16", "2020-07-27", "2020-08-28")) # , "2021-07-01", "2021-07-26", "2021-08-23"

  yield_obs <- basic_truth[which(basic_truth$date %in% yield_obs_dates), c("date", "crop", "mafruit_plant_1")]
  lai_obs <- basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "lai_n_plant_1")]
  nee_obs <-  basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "nee")]

  # Starting with a fixed 1% uncertainty.
  UNCERTAINTY_PROP <- 0.01

  yield_uncertainty <- yield_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(mafruit_plant_1) * UNCERTAINTY_PROP)

  lai_uncertainty <- lai_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(lai_n_plant_1) * UNCERTAINTY_PROP)

  nee_uncertainty <- nee_obs %>%
    group_by(crop) %>%
    summarise(uncertainty = mean(abs(nee)) * UNCERTAINTY_PROP) # Introducing the absolute value here as nee can be negative and I don't want to risk zero uncertainty. The overall effect should not be much as the negative values are usually much smaller than the positive values, but also this makes this slightly more uncertain, which reflects my opinion of the real measurements.

  yield_obs <- merge(yield_obs, yield_uncertainty)
  lai_obs <- merge(lai_obs, lai_uncertainty)
  nee_obs <- merge(nee_obs, nee_uncertainty)

  # Rename the column of the actual observation value for consistency and simple formatting for the function to handle
  colnames(yield_obs)[3] <- "value"
  colnames(lai_obs)[3] <- "value"
  colnames(nee_obs)[3] <- "value"

  obs <- list(yield = yield_obs, lai = lai_obs, nee = nee_obs)
  obs_names <- paste0(sort(names(obs)), sep = "", collapse = "")

  # Setting up the calibration input.
  for (ens_size in c(1000, 500, 250, 200, 100, 50)) {
  # for (ens_size in c(10, 5, 3, 2)) {
    gen_4DEnVar_input(calib_usms, obs, filter(basic_prior, ens <= ens_size), dir_path = dir_path, run_suffix = paste0("_", "ens_size_", ens_size), xb_large[1 : ens_size, ])
  }
  # Call 4DEnVar directly
  analysis_list <- list()
  posterior_draws <- list()
  all_costs <- list()
  for (name in calib_usms) {
    infix <- gsub("[Barley_1290long]", "", name)
    if (infix == "s") {
      infix <- "Barley"
    }
    print(infix)
    for (ens_size in c(1000, 500, 250, 200, 100, 50)) {
    # for (ens_size in c(10, 5, 3, 2)) {
      tmp <- system2(command = fourDEnVarPath,
                     args = c(paste0(dir_path,"/", "xb_large_", "ens_size_", ens_size, ".dat"),
                              paste0(dir_path, "/", c("hx", "y", "R", "hx_bar"), "_", infix, "_", obs_names, "_", "ens_size_", ens_size, ".dat")),
                     wait = TRUE,
                     stdout = TRUE
      )
      # Read from the output the analysis vector xa (the "best" estimate) and the posterior ensemble xa_draws.
      xa <- as.numeric(tmp[1 : 3])
      names(xa) <- name_params[1 : 3]
      large_ens_analysis_list[[paste0(name, ":", ens_size)]] <- as.data.frame(t(xa))
      xa_draws <- apply((str_split_fixed(tmp[(1 + 3) + 1 : 3], " ", ens_size)), 1:2, as.numeric)
      rownames(xa_draws) <- name_params[1 : 3]
      large_ens_posterior_draws[[paste0(name, ":", ens_size)]] <- as.data.frame(t(xa_draws)) %>% mutate(ens = row_number())
    }
  }
}


long_ensemble_size_analysis <- data.table::rbindlist(large_ens_analysis_list, idcol = "usm") %>%
  separate_wider_delim(usm, ":", names = c("usm", "ens_size")) %>%
  pivot_longer(2 + 1 : 3, names_to = "parameter")
long_ensemble_size_draws <- data.table::rbindlist(large_ens_posterior_draws, idcol = "usm") %>%
  separate_wider_delim(usm, ":", names = c("usm", "ens_size")) %>%
  pivot_longer(2 + 1 : 3, names_to = "parameter")
long_ensemble_size_analysis$ens_size <- factor(long_ensemble_size_analysis$ens_size, levels = c(1000, 500, 250, 200, 100, 50))
long_ensemble_size_draws$ens_size <- factor(long_ensemble_size_draws$ens_size, levels = c(1000, 500, 250, 200, 100, 50))

saveRDS(xb_large, file = paste0(dir_path, "/AnalysisInputs/twin_ens_size_xb.rds"))
saveRDS(long_ensemble_size_analysis, file = paste0(dir_path, "/AnalysisInputs/twin_ens_size_analysis.rds"))
saveRDS(long_ensemble_size_draws, file = paste0(dir_path, "/AnalysisInputs/twin_ens_size_draws.rds"))
rm(long_ensemble_size_analysis, long_ensemble_size_draws, large_ens_prior, large_ens_analysis_list, large_ens_posterior_draws, xb_large)
################################################################################
# Alternate truths, should be checked by running calibration on the full baseline twin experiment with the alternate truth set. Not planning to run the posteriors.
alt_truths_parameter_ens <- NULL
alt_truths_parameter_estimates <- NULL
for (truth_index in 1 : num_alt_truths) {
  for (num_params_varied in c(2:8)) {
    print(paste0("Running 4DEnVar on ", num_params_varied, " parameters varying with truth ", truth_index, "."))
    basic_truth <- synth_truth %>%
      filter(truth_num == truth_index, params_changed == num_params_varied)
    basic_prior <- twin_exp_prior %>%
      filter(varying == num_params_varied)

    # Setting these as the initial observations dates being used for the synthetic truth, as these are approximately the frequency and spread of observations we actually have.
    yield_obs_dates <- c(as.Date(c("2020-9-11"), format = "%Y-%m-%d")) # , as.Date(c("2021-9-14"), format = "%Y-%m-%d")
    nee_obs_dates <- as.Date(c("2020-06-03", "2020-06-15", "2020-06-29", "2020-07-03", "2020-07-14", "2020-07-27", "2020-07-31", "2020-08-11", "2020-08-27", "2020-08-28")) # , "2021-06-28", "2021-07-22", "2021-08-30"
    lai_obs_dates <- as.Date(c("2020-06-04", "2020-06-18", "2020-07-01", "2020-07-16", "2020-07-27", "2020-08-28")) # , "2021-07-01", "2021-07-26", "2021-08-23"

    yield_obs <- basic_truth[which(basic_truth$date %in% yield_obs_dates), c("date", "crop", "mafruit_plant_1")]
    lai_obs <- basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "lai_n_plant_1")]
    nee_obs <-  basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "nee")]

    # Starting with a fixed 1% uncertainty.
    UNCERTAINTY_PROP <- 0.01

    yield_uncertainty <- yield_obs %>%
      group_by(crop) %>%
      summarise(uncertainty = mean(mafruit_plant_1) * UNCERTAINTY_PROP)

    lai_uncertainty <- lai_obs %>%
      group_by(crop) %>%
      summarise(uncertainty = mean(lai_n_plant_1) * UNCERTAINTY_PROP)

    nee_uncertainty <- nee_obs %>%
      group_by(crop) %>%
      summarise(uncertainty = mean(abs(nee)) * UNCERTAINTY_PROP) # Introducing the absolute value here as nee can be negative and I don't want to risk zero uncertainty. The overall effect should not be much as the negative values are usually much smaller than the positive values, but also this makes this slightly more uncertain, which reflects my opinion of the real measurements.

    yield_obs <- merge(yield_obs, yield_uncertainty)
    lai_obs <- merge(lai_obs, lai_uncertainty)
    nee_obs <- merge(nee_obs, nee_uncertainty)

    # Rename the column of the actual observation value for consistency and simple formatting for the function to handle
    colnames(yield_obs)[3] <- "value"
    colnames(lai_obs)[3] <- "value"
    colnames(nee_obs)[3] <- "value"

    obs <- list(yield = yield_obs, lai = lai_obs, nee = nee_obs)
    obs_names <- paste0(sort(names(obs)), sep = "", collapse = "")

    gen_4DEnVar_input(calib_usms, obs, basic_prior, dir_path = dir_path, run_suffix = paste0("_varying_", num_params_varied, "_alt_truth_", truth_index))

    ################################################################################
    # Calling 4DEnVar directly
    analysis_list <- list()
    posterior_draws <- list()
    for (name in calib_usms) {
      infix <- gsub("[Barleylong_1290]", "", name)
      if (infix == "s") {
        infix <- "Barley"
      }
      print(infix)
      tmp <- system2(command = fourDEnVarPath,
                     args = c(paste0(dir_path, "/xb_", num_params_varied, ".dat"),
                              paste0(dir_path, "/", c("hx", "y", "R", "hx_bar"), "_", infix, "_", obs_names, "_varying_", num_params_varied, "_alt_truth_", truth_index, ".dat")),
                     wait = TRUE,
                     stdout = TRUE
      )
      # Read from the output the analysis vector xa (the "best" estimate) and the posterior ensemble xa_draws.
      xa <- as.numeric(tmp[1 : num_params_varied])
      names(xa) <- name_params[1 : num_params_varied]
      analysis_list[[name]] <- as.data.frame(t(xa))
      xa_draws <- apply((str_split_fixed(tmp[(1 + num_params_varied) + 1 : num_params_varied], " ", n_ens)), 1:2, as.numeric)
      rownames(xa_draws) <- name_params[1 : num_params_varied]
      posterior_draws[[name]] <- as.data.frame(t(xa_draws))
    }
    # Calculating sd and combining into a nicer format for usage.
    tmp_sd <- posterior_draws %>%
      data.table::rbindlist(use.names = TRUE, idcol = "usm") %>%
      pivot_longer(1 + 1 : all_of(num_params_varied), names_to = "parameter") %>%
      group_by(usm, parameter) %>%
      summarise(sd(value))
    tmp_analysis <- analysis_list %>%
      data.table::rbindlist(use.names = TRUE, idcol = "usm") %>%
      pivot_longer(1 + 1 : all_of(num_params_varied), names_to = "parameter") %>%
      merge(tmp_sd) %>%
      add_column(num_params_varied = num_params_varied, truth_used = as.factor(truth_index))

    tmp_param_ens <- posterior_draws %>%
      data.table::rbindlist(use.names = TRUE, idcol = "usm") %>%
      pivot_longer(1 + 1 : all_of(num_params_varied), names_to = "parameter") %>%
      add_column(num_params_varied = num_params_varied, truth_used = as.factor(truth_index))

    alt_truths_parameter_ens <- rbind(alt_truths_parameter_ens, tmp_param_ens)
    alt_truths_parameter_estimates <- rbind(alt_truths_parameter_estimates, tmp_analysis)
  }
}

saveRDS(alt_truths_parameter_ens, file = paste0(dir_path, "/AnalysisInputs/alt_truths_parameter_ens.rds"))
saveRDS(alt_truths_parameter_estimates, file = paste0(dir_path, "/AnalysisInputs/alt_truths_param_ests.rds"))
rm(alt_truths_parameter_ens, alt_truths_parameter_estimates, tmp_sd, tmp_analysis)
################################################################################
# Import observations
obs_data <- read_csv("~/Documents/TWINWIN-Intercrop/Data/TWINWINobs.csv") %>%
  filter(crop != "TG") # TG estimates are unusable due to known bugs in STICS
################################################################################
# TWINWIN calibration
# Create the prior for 2021 runs.
# combine with appropriate twin experiment prior for 2020 (reusable)
# calibrate with real measurements (running obs_vary). No need for separate estimate of the "best" combination, NEE + yield.
# estimate posteriors of both years.
extra_usms <- all_usms[which(!(all_usms %in% calib_usms))]
prior_2021 <- data.frame()
for (num_params_varied in c(2:8)) {
  print(paste0("Prior estimation for ", num_params_varied, " parameters varying."))
  stics_results <- cache_rds(run_ensemble(n_ens, xb[, 1 : num_params_varied], extra_usms, sim_options), file = paste0("prior_2021_", num_params_varied, "params"), dir = dir_path, hash = list(xb, extra_usms))
  tmp_res <- list()
  # Extract all members of the ensemble, label them as the prior for future use.
  for (i in 1 : n_ens) {
    tmp <- data.table::rbindlist(stics_results[[i]], use.names = TRUE, idcol = "usm")
    tmp$dist <- "Prior"
    tmp_res[[i]] <- tmp
  }
  tmp_prior <- data.table::rbindlist(tmp_res, use.names = TRUE, idcol = "ens")
  tmp_prior$varying <- num_params_varied
  prior_2021 <- rbind(prior_2021, tmp_prior)
  write.table(t(xb[, 1 : num_params_varied]), paste0(dir_path, "/xb_", num_params_varied, ".dat"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = " ")
}

prior_2021 <- pivot_wider(prior_2021, names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n)) %>%
  mutate(date = as.Date(Date), .keep = "unused", .after = usm) %>%
  mutate(crop = gsub("[Barley_1290long]", "", usm), .after = usm) %>%
  mutate(nee = CO2sol_plant_1 / 1000 - 0.48 * (44 / 12) * (dltams_n_plant_1 + dltams_n_plant_2))
prior_2021$crop[prior_2021$crop == "s"] <- "Barley"
prior_2021$nee[which(is.na(prior_2021$nee))] <- prior_2021$CO2sol_plant_1[which(is.na(prior_2021$nee))] / 1000 - 0.48 * (44 / 12) * (prior_2021$dltams_n_plant_1[which(is.na(prior_2021$nee))])
# Run calibration and posteriors
obs_sets <- c("laineeyield", "lainee", "laiyield", "lai", "neeyield", "nee", "yield")
for (num_params_varied in c(3)) {
  print(paste0("Running 4DEnVar on ", num_params_varied, " parameters varying, with varying observation streams."))
  # The twin experiment prior may be reused as a generic prior here as the twin experiment differences are in the observations used and how many parameters are varied at once.
  basic_prior <- twin_exp_prior %>%
    filter(varying == num_params_varied, usm %in% calib_usms)
  # Filter for calibration to observations during the 2020 barley growing season (plant until harvest)
  yield_obs <- obs_data %>%
    filter(obs == "yield", between(date, as.Date("2020-05-22"), as.Date("2020-09-11")))
  lai_obs <- obs_data %>%
    filter(obs == "GAI", between(date, as.Date("2020-05-22"), as.Date("2020-09-11")))
  # and also filter out positive NEE that is not able to be reproduced when calibrating only plant parameters.
  nee_obs <- obs_data %>%
    filter(obs == "NEE", between(date, as.Date("2020-05-22"), as.Date("2020-09-11")), value <= 0)

  for (obs_names in obs_sets) {
    use_lai <- str_detect(obs_names, "lai")
    use_nee <- str_detect(obs_names, "nee")
    use_yield <- str_detect(obs_names, "yield")
    if (all(use_lai, use_nee, use_yield)) {
      obs <- list(lai = lai_obs, nee = nee_obs, yield = yield_obs)
    } else if (use_lai & use_nee & !use_yield) {
      obs <- list(lai = lai_obs, nee = nee_obs)
    } else if (use_lai & !use_nee & use_yield) {
      obs <- list(lai = lai_obs, yield = yield_obs)
    } else if (use_lai & !use_nee & !use_yield) {
      obs <- list(lai = lai_obs)
    } else if (!use_lai & use_nee & use_yield) {
      obs <- list(nee = nee_obs, yield = yield_obs)
    } else if (!use_lai & use_nee & !use_yield) {
      obs <- list(nee = nee_obs)
    } else if (!use_lai & !use_nee & use_yield) {
      obs <- list(yield = yield_obs)
    } else {
      warning("You didn't enter a valid observation set to be used.")
    }
    gen_4DEnVar_input(calib_usms, obs, basic_prior, dir_path = dir_path, run_suffix = paste0("_varying_", num_params_varied))
  }
  ################################################################################
  # Calling 4DEnVar directly
  analysis_list <- list()
  posterior_draws <- list()
  for (name in calib_usms) {
    infix <- gsub("[Barley_1290long]", "", name)
    if (infix == "s") {
      infix <- "Barley"
    }
    print(paste0("4DEnVar: ", infix))
    for (obs_names in obs_sets) {
      tmp <- system2(command = fourDEnVarPath,
                     args = c(paste0(dir_path, "/xb_", num_params_varied, ".dat"),
                              paste0(dir_path, "/", c("hx", "y", "R", "hx_bar"), "_", infix, "_", obs_names, "_varying_", num_params_varied, ".dat")),
                     wait = TRUE,
                     stdout = TRUE
      )
      # Read from the output the analysis vector xa (the "best" estimate) and the posterior ensemble xa_draws.
      xa <- as.numeric(tmp[1 : num_params_varied])
      names(xa) <- name_params[1 : num_params_varied]
      analysis_list[[paste0(name, ":", obs_names)]] <- as.data.frame(t(xa))
      xa_draws <- apply((str_split_fixed(tmp[(1 + num_params_varied) + 1 : num_params_varied], " ", n_ens)), 1:2, as.numeric)
      rownames(xa_draws) <- name_params[1 : num_params_varied]
      posterior_draws[[paste0(name, ":", obs_names)]] <- as.data.frame(t(xa_draws), obs_names)
    }
  }

  long_obs_vary_analysis <- data.table::rbindlist(analysis_list, idcol = "usm") %>%
    separate_wider_delim(usm, ":", names = c("usm", "obs_set")) %>%
    pivot_longer(2 + 1 : all_of(num_params_varied), names_to = "parameter")
  long_obs_vary_draws <- data.table::rbindlist(posterior_draws, idcol = "usm") %>%
    separate_wider_delim(usm, ":", names = c("usm", "obs_set")) %>%
    pivot_longer(2 + 1 : all_of(num_params_varied), names_to = "parameter")

  post_est <- NULL
  for (obs_names in obs_sets) {
    post_est_run <- NULL
    for (name in calib_usms) {
      # First getting the infix used for this undersown species.
      infix <- gsub("[Barley_1290long]", "", name)
      if (infix == "s") {
        infix <- "Barley"
      }

      post_dist <- posterior_draws[[paste0(name, ":", obs_names)]]
      # Run the posterior ensemble estimation through Stics
      usms_run <- all_usms[str_detect(all_usms, infix)]
      tmp_post_est <- cache_rds(run_ensemble(n_ens, post_dist, usms_run, sim_options), file = paste0("/obs_varying_posterior_", infix, obs_names, "varying_", num_params_varied, "_params"), dir = dir_path, hash = list(xb, calib_usms, posterior_draws[[paste0(name, ":", obs_names)]]))
      tmp_post_res <- list()
      for (i in 1 : n_ens) {
        tmp_post <- data.table::rbindlist(tmp_post_est[[i]], use.names = TRUE, idcol = "usm")
        tmp_post_res[[i]] <- tmp_post
      }

      post_tmp <- data.table::rbindlist(tmp_post_res, use.names = TRUE, idcol = "ens")
      post_tmp$obs_calib <- obs_names
      post_tmp$usm_calib <- infix
      post_est_run <- rbind(post_est_run, post_tmp)
    }
    # Combining each parameter set run into one.
    post_est <- rbind(post_est, post_est_run)
  }
  warnings()
  post_est$dist <- "Posterior"
  post_est <- pivot_wider(post_est, names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n))
  colnames(post_est)[3] <- "date"
  post_est$date <- as.Date(post_est$date)
  post_est$year <- paste0("20", gsub("[A-Za-z_]", "", post_est$usm))
  post_est$nee <- post_est$CO2sol_plant_1 / 1000 - 0.48 * (44 / 12) * (post_est$dltams_n_plant_1 + post_est$dltams_n_plant_2)
  post_est$crop <- gsub("[Barley_1290long]", "", post_est$usm)
  post_est$crop[post_est$crop == "s"] <- "Barley"
  # Fix the barley single crop nee calculations
  post_est$nee[which(is.na(post_est$nee))] <- post_est$CO2sol_plant_1[which(is.na(post_est$nee))] / 1000 - 0.48 * (44 / 12) * (post_est$dltams_n_plant_1[which(is.na(post_est$nee))])
}

saveRDS(prior_2021, file = paste0(dir_path, "/AnalysisInputs/prior_2021.rds"))
saveRDS(long_obs_vary_analysis, file = paste0(dir_path, "/AnalysisInputs/obs_vary_analysis.rds"))
saveRDS(long_obs_vary_draws, file = paste0(dir_path, "/AnalysisInputs/obs_vary_draws.rds"))
saveRDS(post_est, file = paste0(dir_path, "/AnalysisInputs/TWINWIN_posterior.rds"))






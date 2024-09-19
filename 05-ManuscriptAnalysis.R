################################################################################
# Title: TWINWIN Analysis
# Author: Quentin Bell (quentin.bell@fmi.fi)
# Intention: This script takes the observation inputs and model runs for
# analysis and plotting, including all variations on twin experiments and the
# use of actual observations.
################################################################################
# Libraries
library(CroPlotR)
library(tidyverse)
library(stargazer)
library(data.table)
library(viridis)
################################################################################
# A few core options and set ups
setwd("~/Documents/TWINWIN-Intercrop/RunOutput/Analysis")
model_runs_dir <- "/home/bell/Documents/TWINWIN-Intercrop/RunOutput/ModelRuns/AnalysisInputs/"
manuscript_dir <- "Manuscript/"
supplement_dir <- "Manuscript/Supplementary/"
# exploration_dir <- "Exploration/"
theme_set(theme_classic())
################################################################################
# Functions
format_mean_sd <- function(x) {
  # This is a small function to group the mean and standard deviations for easy printing.
  return(sprintf("%.3f (%.3f)", mean(x), sd(x)))
}
# The following 2 functions are an attempt to simplify and automate the plotting a bit more, so that instead of copying and potentially modifying code I can instead call a function to plot it and have each changeable thing as an argument to that function.
# plot_lai_mean <- function(lai_est_mean, plot_years, plot_crops, plot_obs_calib, plot_dists, plot_usm_calib, plot_ens_size, lai_obs) {
#   plot_output <- lai_est_mean %>%
#     filter(between(date, as.Date(paste0(plot_years, "/05/20")), as.Date(paste0(as.numeric(plot_years), "/9/30"))) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
#     {
#       if (plot_usm_calib == "Self") {
#         filter(., usm_calib == crop | dist == "Prior")
#       } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
#     } %>%
#     {
#       if (!("Prior" %in% plot_dists)) {
#         filter(., dist != "Prior")
#       } else .
#     } %>%
#     ggplot() +
#     # scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep) +
#     geom_line(aes(date, LAI, colour = interaction(dist))) +
#     facet_wrap(vars(crop)) +
#     ggtitle(paste0("Average estimated LAI, ", plot_years, ", ", plot_usm_calib, " calibrated, Ensemble size: ", plot_ens_size)) +
#     labs(y = "LAI (m^2 m^-2)", x = "Date", fill = "95% Confidence Interval", colour = "Distribution Mean", shape = "Observations") +
#     geom_ribbon(aes(date, ymin = LAI_CI_min, ymax = LAI_CI_max, fill = interaction(dist)), alpha = 0.30) +
#     geom_point(data = lai_obs[lai_obs$crop %in% plot_crops, ], aes(x = date, y = value), size = 1) +
#     geom_errorbar(lai_obs[lai_obs$crop %in% plot_crops, ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty), width = 0.5) +
#     geom_line(data = basic_truth[basic_truth$crop %in% plot_crops, ], aes(date, lai_n_plant_1), colour = "black", alpha = 0.5) +
#     geom_vline(aes(xintercept = yield_obs_dates[which(format(yield_obs_dates, "%Y") == plot_years)]), linetype = "dashed") +
#     guides(colour = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
#     theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'))
#   return(plot_output)
# }
#
# plot_nee_mean <- function(nee_est_mean, plot_years, plot_crops, plot_obs_calib, plot_dists, plot_usm_calib, plot_ens_size) {
#   plot_output <- nee_est_mean %>%
#     filter(between(date, as.Date(paste0(plot_years, "/05/20")), as.Date(paste0(as.numeric(plot_years) + 1, "/05/20"))) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
#     {
#       if (plot_usm_calib == "Self") {
#         filter(., usm_calib == crop | dist == "Prior")
#       } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
#     } %>%
#     {
#       if (!("Prior" %in% plot_dists)) {
#         filter(., dist != "Prior")
#       } else .
#     } %>%
#     ggplot() +
#     scale_y_continuous(limits = c(0.47, -0.125), oob = scales::oob_keep) +
#     geom_line(aes(date, NEE, colour = interaction(dist))) +
#     geom_line(data = basic_truth[basic_truth$crop %in% plot_crops, ], aes(date, nee), colour = "black", alpha = 0.5) +
#     facet_wrap(vars(crop)) +
#     ggtitle(paste0("Average estimated NEE, ", plot_years, ", ", plot_usm_calib, " calibrated, Ensemble size: ", plot_ens_size)) +
#     labs(y = "NEE (tC ha^-1 d^-1)", x = "Date", fill = "95% Confidence Interval", colour = "Distribution Mean", shape = "Used in Calibration (2020)") +
#     geom_ribbon(aes(date, ymin = NEE - 1.96 * NEE_se, ymax = NEE + 1.96 * NEE_se, fill = interaction(dist)), alpha = 0.30) +
#     geom_point(data = nee_obs[nee_obs$crop %in% plot_crops, ], aes(x = date, y = value), size = 1) +
#     geom_errorbar(nee_obs[nee_obs$crop %in% plot_crops, ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty), width = 0.5) +
#     geom_hline(aes(yintercept = 0), linetype = "dashed", colour = "grey") +
#     geom_vline(aes(xintercept = yield_obs_dates[which(format(yield_obs_dates, "%Y") == plot_years)]), linetype = "dashed") +
#     guides(colour = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
#     theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'))
#   return(plot_output)
# }
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
params <- readRDS(paste0(model_runs_dir, "baseline_params.rds"))
name_params <- names(params)
################################################################################
# Twin experiment parameter selection: distributions 2-8 & 3+1; observation space results 3 params, 8 params; % error in parameter estimates; RMSE table
general_xb <- readRDS(paste0(model_runs_dir, "twin_xb.rds"))
general_prior <- readRDS(paste0(model_runs_dir, "twin_prior.rds")) %>%
  mutate(obs_calib = NA, usm_calib = NA) %>%
  rename(num_params_varied = varying)

twin_posterior <- readRDS(paste0(model_runs_dir, "twin_posterior.rds"))
twin_analysis <- readRDS(paste0(model_runs_dir, "twin_analysis.rds"))
twin_post_draws <- readRDS(paste0(model_runs_dir, "twin_posterior_draws.rds"))
synth_params <- readRDS(paste0(model_runs_dir, "synth_params.rds")) %>%
  rownames_to_column() %>%
  filter(str_detect(rowname, "truth_1_"))
synth_truth <- readRDS(paste0(model_runs_dir, "synth_truth.rds")) %>%
  filter(truth_num == "1")
n_ens <- dim(general_xb)[1]
# Note for the main twin experiment only truth 1 was used, so we filter to that set of parameter values and estimates

# Plotting parameter distributions
# plot_colours <- c("#F8766D", "#00BFC4", "black", "grey") # Original colour images
plot_colours <- c("#414487FF", "#7AD151FF", "black", "grey") # Greyscale and colourblind safe images.
print("Plotting Twin Experiment Distributions")
# Exploratory plots, not needed for manuscript
# for (num_params_varied in c(2:8)) {
#   plot_xb <- pivot_longer(as.data.frame(general_xb), 1 : num_params_varied, names_to = "parameter")
#   plot_params <- params[1 : num_params_varied] %>%
#     pivot_longer(everything(), names_to = "parameter")
#   plot_synth_params <- synth_params[num_params_varied, 1 + 1 : num_params_varied] %>%
#     pivot_longer(everything(), names_to = "parameter")
#   for (name in calib_usms) {
#     print(name)
#     dist_plotted <- twin_post_draws %>%
#       filter(usm == name, num_params == num_params_varied) %>%
#       group_by(parameter) %>%
#       ggplot(aes(x = value)) +
#       # geom_histogram(aes(x = value, fill = dist), bins = 20, position = "dodge") +
#       geom_density(aes(fill = "Posterior"), alpha = 0.5) +
#       geom_density(data = plot_xb, aes(fill = "Prior"), alpha = 0.5) +
#       geom_vline(data = twin_analysis[which(twin_analysis$usm == name & twin_analysis$num_params == num_params_varied), ], aes(xintercept = value, colour = "Posterior"), alpha = 1, linetype = "solid", linewidth = 1) +
#       geom_vline(data = plot_synth_params, aes(xintercept = value, colour = "Truth"), alpha = 1, linetype = "solid", linewidth = 1) +
#       geom_vline(data = plot_params, aes(xintercept = value, colour = "Prior"), alpha = 1, linetype = "solid", linewidth = 1) +
#       # stat_function(fun = dnorm, args = list(mean = prior_mean, sd = prior_mean * 0.1)) +
#       facet_wrap(~ parameter, scales = "free") +
#       scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#       scale_x_continuous(expand = c(0, 0)) +
#       scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
#       scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
#       # scale_colour_viridis_d() +
#       # scale_fill_viridis_d() +
#       ggtitle(paste0("Estimated Parameter Ensemble Distributions: ", name)) +
#       labs(y = "Density", x = "Parameter Value", fill = "Distribution", colour = "Distribution") +
#       # guides(fill = guide_legend(reverse = FALSE)) +
#       theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'))
#     # geom_line(data = plot_dists[plot_dists$dist == "prior", ], aes(y = dnorm(value, mean = tapply(value, parameter, mean)[PANEL], sd = tapply(value, parameter, sd)[PANEL])), colour = "blue")
#     if (inherits(try(ggplot_build(dist_plotted)), "try-error")) {
#       dist_plotted <- ggplot()
#       print(paste0("Plotting error with ", name))
#     }
#
#     dist_plotted
#     ggsave(paste0(exploration_dir, "twin_exp/ParameterDistributions_", name, "_", num_params_varied, "_params_", n_ens, "_ensemble.pdf"), units = "mm", width = 800, height = 420, limitsize = FALSE)
#     if (num_params_varied == 4 & name == "Barley_sole20_long") {
#       ggsave(paste0(manuscript_dir, "twin_exp_ParameterDistributions_", name, "_", num_params_varied, "_params_", n_ens, "_ensemble.pdf"), units = "mm", width = 800, height = 420, limitsize = FALSE)
#     }
#   }
# }

# Plot the 4 parameter plot necessary for the manuscript (but is this the correct one?)
# num_params_varied <- 4
# plot_xb <- pivot_longer(as.data.frame(general_xb), 1 : num_params_varied, names_to = "parameter")
# plot_params <- params[1 : num_params_varied] %>%
#   pivot_longer(everything(), names_to = "parameter")
# plot_synth_params <- synth_params[num_params_varied, 1 + 1 : num_params_varied] %>%
#   pivot_longer(everything(), names_to = "parameter")
# name <- "Barley_sole20_long"
# print(name)
# dist_plotted <- twin_post_draws %>%
#   filter(usm == name, num_params == num_params_varied) %>%
#   group_by(parameter) %>%
#   ggplot(aes(x = value)) +
#   # geom_histogram(aes(x = value, fill = dist), bins = 20, position = "dodge") +
#   geom_density(aes(fill = "Posterior", colour = "Posterior"), alpha = 0.5) +
#   geom_density(data = plot_xb, aes(fill = "Prior", colour = "Prior"), alpha = 0.5) +
#   geom_vline(data = twin_analysis[which(twin_analysis$usm == name & twin_analysis$num_params == num_params_varied), ], aes(xintercept = value, colour = "Posterior"), alpha = 1, linetype = "solid", linewidth = 1) +
#   geom_vline(data = plot_synth_params, aes(xintercept = value, colour = "Truth", fill = "Truth"), alpha = 1, linetype = "solid", linewidth = 1) +
#   geom_vline(data = plot_params, aes(xintercept = value, colour = "Prior"), alpha = 1, linetype = "solid", linewidth = 1) +
#   # stat_function(fun = dnorm, args = list(mean = prior_mean, sd = prior_mean * 0.1)) +
#   facet_wrap(~ parameter, scales = "free") +
#   scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
#   scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
#   # scale_colour_viridis_d() +
#   # scale_fill_viridis_d() +
#   ggtitle(paste0("Estimated Parameter Ensemble Distributions: Sole Barley")) +
#   labs(y = "Density", x = "Parameter Value", fill = "", colour = "") +
#   # guides(fill = guide_legend(reverse = FALSE)) +
#   theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom")
# # geom_line(data = plot_dists[plot_dists$dist == "prior", ], aes(y = dnorm(value, mean = tapply(value, parameter, mean)[PANEL], sd = tapply(value, parameter, sd)[PANEL])), colour = "blue")
# if (inherits(try(ggplot_build(dist_plotted)), "try-error")) {
#   dist_plotted <- ggplot()
#   print(paste0("Plotting error with ", name))
# }
#
# dist_plotted
#
# ggsave(paste0(manuscript_dir, "twin_exp_ParameterDistributions_", name, "_", num_params_varied, "_params_", n_ens, "_ensemble.pdf"), units = "mm", width = 800, height = 420, limitsize = FALSE)
#
# num_params_varied <- 4
# synth_params_all <- readRDS(paste0(model_runs_dir, "synth_params.rds")) %>%
#   rownames_to_column()
# plot_xb <- pivot_longer(as.data.frame(general_xb), 1 : num_params_varied, names_to = "parameter")
# plot_params <- params[1 : num_params_varied] %>%
#   pivot_longer(everything(), names_to = "parameter")
# name <- "Barley_sole20_long"
#
# for (truth in c(1:20)) {
#   synth_params_single <- synth_params_all %>%
#     filter(str_detect(rowname, paste0("truth_", truth, "_")))
#   plot_synth_params <- synth_params_single[num_params_varied, 1 + 1 : num_params_varied] %>%
#     pivot_longer(everything(), names_to = "parameter")
#
#   dist_plotted <- twin_post_draws %>%
#     filter(usm == name, num_params == num_params_varied) %>%
#     group_by(parameter) %>%
#     ggplot(aes(x = value)) +
#     # geom_histogram(aes(x = value, fill = dist), bins = 20, position = "dodge") +
#     geom_density(aes(fill = "Posterior"), alpha = 0.5) +
#     geom_density(data = plot_xb, aes(fill = "Prior"), alpha = 0.5) +
#     geom_vline(data = twin_analysis[which(twin_analysis$usm == name & twin_analysis$num_params == num_params_varied), ], aes(xintercept = value, colour = "Posterior"), alpha = 1, linetype = "solid", linewidth = 1) +
#     geom_vline(data = plot_synth_params, aes(xintercept = value, colour = "Truth"), alpha = 1, linetype = "solid", linewidth = 1) +
#     geom_vline(data = plot_params, aes(xintercept = value, colour = "Prior"), alpha = 1, linetype = "solid", linewidth = 1) +
#     # stat_function(fun = dnorm, args = list(mean = prior_mean, sd = prior_mean * 0.1)) +
#     facet_wrap(~ parameter, scales = "free") +
#     scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#     scale_x_continuous(expand = c(0, 0)) +
#     scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
#     scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
#     # scale_colour_viridis_d() +
#     # scale_fill_viridis_d() +
#     ggtitle(paste0("Estimated Parameter Ensemble Distributions: ", name)) +
#     labs(y = "Density", x = "Parameter Value", fill = "Distribution", colour = "Distribution") +
#     # guides(fill = guide_legend(reverse = FALSE)) +
#     theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'))
#   # geom_line(data = plot_dists[plot_dists$dist == "prior", ], aes(y = dnorm(value, mean = tapply(value, parameter, mean)[PANEL], sd = tapply(value, parameter, sd)[PANEL])), colour = "blue")
#   if (inherits(try(ggplot_build(dist_plotted)), "try-error")) {
#     dist_plotted <- ggplot()
#     print(paste0("Plotting error with ", name))
#   }
#
#   dist_plotted
#
#   ggsave(filename = paste0(exploration_dir, "twin_exp/vary_truth/ParameterDistributions_", name, "_", num_params_varied, "_params_", truth, "_truth_num_", n_ens, "_ensemble.pdf"),
#          units = "mm", width = 800, height = 420)
# }
# rm(dist_plotted, plot_params, plot_synth_params, plot_xb)
################################################################################
# Basic Twin Experiment Plotting Estimates
# Set up variables that are referred to in the loops that can allow for programmatic plotting with minimal changes
infixes <- gsub("[Barley_1290long]", "", calib_usms)
infixes[which(infixes == "s")] <- "Barley"
plot_years <- "2020"
plot_crops <- infixes
plot_obs_calib <- "laineeyield"
plot_dists <- factor(c("Prior", "Posterior"), levels = c("Prior", "Posterior", "Truth"))
plot_usm_calib <- "Self"
plot_ens_size <- n_ens

# Setting up the observation sets used
basic_truth <- synth_truth %>%
  filter(truth_num == "1") %>%
  rename(num_params_varied = params_changed)

# Setting these as the initial observations dates being used for the synthetic truth, as these are approximately the frequency and spread of observations we actually have.
yield_obs_dates <- c(as.Date(c("2020-9-11"), format = "%Y-%m-%d")) # , as.Date(c("2021-9-14"), format = "%Y-%m-%d")
nee_obs_dates <- as.Date(c("2020-06-03", "2020-06-15", "2020-06-29", "2020-07-03", "2020-07-14", "2020-07-27", "2020-07-31", "2020-08-11", "2020-08-27", "2020-08-28")) # , "2021-06-28", "2021-07-22", "2021-08-30"
lai_obs_dates <- as.Date(c("2020-06-04", "2020-06-18", "2020-07-01", "2020-07-16", "2020-07-27", "2020-08-28")) # , "2021-07-01", "2021-07-26", "2021-08-23"

yield_obs <- basic_truth[which(basic_truth$date %in% yield_obs_dates), c("date", "crop", "num_params_varied", "mafruit_plant_1")]
lai_obs <- basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "num_params_varied", "lai_n_plant_1")]
nee_obs <-  basic_truth[which(basic_truth$date %in% lai_obs_dates), c("date", "crop", "num_params_varied", "nee")]

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
rm(yield_uncertainty, lai_uncertainty, nee_uncertainty)
# Rename the column of the actual observation value for consistency and simple formatting for the function to handle
colnames(yield_obs)[4] <- c("value")
colnames(lai_obs)[4] <- c("value")
colnames(nee_obs)[4] <- c("value")
################################################################################
# Mean Plots
# LAI plot
twin_posterior$crop <- gsub("[Barley_1290long]", "", twin_posterior$usm)
twin_posterior$crop[twin_posterior$crop == "s"] <- "Barley"

plot_crops <- "Barley" # "Barley" "AC"     "RC"     "WC"     "FA"     "AA"     "IR"     "CI"
# LAI plot prep
lai_plot_df <- dplyr::bind_rows(general_prior[ , c("ens", "num_params_varied", "date", "crop", "dist", "lai_n_plant_1")], twin_posterior[ , c("ens", "num_params_varied", "date", "crop", "dist", "obs_calib", "usm_calib", "lai_n_plant_1")]) %>%
  group_by(date, crop, dist, obs_calib, usm_calib, num_params_varied) %>%
  summarise("LAI" = mean(lai_n_plant_1),
            "LAI_CI_min" = max(mean(lai_n_plant_1) - 1.96 * sd(lai_n_plant_1), 0),
            "LAI_CI_max" = mean(lai_n_plant_1) + 1.96 * sd(lai_n_plant_1)
  ) %>%
  filter(between(date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  }
# NEE plot prep
nee_plot_df <- dplyr::bind_rows(general_prior[ , c("ens", "num_params_varied", "date", "crop", "dist", "nee")], twin_posterior[ , c("ens", "num_params_varied", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")]) %>%
  group_by(date, crop, dist, obs_calib, usm_calib, num_params_varied) %>%
  summarise("NEE" = mean(nee),
            "NEE_se" = sd(nee)
  ) %>%
  filter(between(date, as.Date(paste0(plot_years, "/05/20")), as.Date(paste0(as.numeric(plot_years), "/12/12"))) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  }

# Making twin experiment LAI plots for supplementary materials.
for (plot_varying_params in c(3, 8)) {
  lai_plot <- lai_plot_df %>%
    filter(num_params_varied == plot_varying_params) %>%
    ggplot() +
    # scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep) +
    geom_line(aes(date, LAI, colour = interaction(dist))) +
    facet_wrap(vars(crop)) +
    ggtitle(paste0("Average estimated barley LAI, ", plot_years)) +
    labs(y = expression(paste("LAI (", m^{2}, m^{-2}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution Mean", shape = "Observations") +
    geom_ribbon(aes(date, ymin = LAI_CI_min, ymax = LAI_CI_max, fill = interaction(dist)), alpha = 0.5) +
    geom_point(data = lai_obs[lai_obs$crop %in% plot_crops & lai_obs$num_params_varied == plot_varying_params & between(lai_obs$date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates), ], aes(x = date, y = value, colour = "Truth"), size = 1) +
    geom_errorbar(lai_obs[lai_obs$crop %in% plot_crops & lai_obs$num_params_varied == plot_varying_params & between(lai_obs$date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates), ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Truth"), width = 0.5) +
    geom_line(data = basic_truth[basic_truth$crop %in% plot_crops & basic_truth$num_params_varied == plot_varying_params & between(basic_truth$date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates), ], aes(date, lai_n_plant_1, colour = "Truth"), alpha = 1) +
    geom_vline(aes(xintercept = yield_obs_dates[which(format(yield_obs_dates, "%Y") == plot_years)], colour = "Harvest"), linetype = "dashed") +
    guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = TRUE)) +
    theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
    scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth", "Harvest")) +
    scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth", "Harvest")) +
    # scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_x_date(expand = c(0, 0))

  lai_plot

  ggsave(filename = paste0("twin_LAI_plot_mean_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
         path = supplement_dir,
         units = "mm", width = 190, height = 105)

  nee_plot <- nee_plot_df %>%
    filter(num_params_varied == plot_varying_params) %>%
    filter(between(date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
    ggplot() +
    scale_y_continuous(limits = c(-0.47, 0.1), oob = scales::oob_keep) +
    geom_line(aes(date, NEE, colour = interaction(dist))) +
    geom_line(data = basic_truth[basic_truth$crop %in% plot_crops & basic_truth$num_params_varied == plot_varying_params & between(basic_truth$date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates), ], aes(date, nee), colour = "black", alpha = 0.5) +
    facet_wrap(vars(crop)) +
    ggtitle(paste0("Average estimated plot NEE, barley growing season ", plot_years)) +
    labs(y = expression(paste("NEE (t", CO[2], "", ha^{-1}, "", d^{-1}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution Mean", shape = "Used in Calibration (2020)") +
    geom_ribbon(aes(date, ymin = NEE - 1.96 * NEE_se, ymax = NEE + 1.96 * NEE_se, fill = interaction(dist)), alpha = 0.5) +
    geom_point(data = nee_obs[nee_obs$crop %in% plot_crops & nee_obs$num_params_varied == plot_varying_params, ], aes(x = date, y = value, colour = "Truth"), size = 1) +
    geom_errorbar(nee_obs[nee_obs$crop %in% plot_crops & nee_obs$num_params_varied == plot_varying_params, ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Truth"), width = 0.5) +
    geom_hline(aes(yintercept = 0), linetype = "dashed", colour = "grey") +
    geom_vline(aes(xintercept = yield_obs_dates[which(format(yield_obs_dates, "%Y") == plot_years)], colour = "Harvest"), linetype = "dashed") +
    guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
    theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
    scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth", "Harvest")) +
    scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth", "Harvest")) +
    scale_x_date(expand = c(0, 2), breaks = waiver(), labels = waiver())

  nee_plot

  ggsave(filename = paste0("twin_NEE_plot_mean_barley_season_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
         path = supplement_dir,
         units = "mm", width = 190, height = 105)
}

rm(lai_plot_df, nee_plot_df, lai_plot, nee_plot)




# Yield Table
plot_crops <- infixes
yield_table <- dplyr::bind_rows(general_prior[ , c("ens", "num_params_varied", "date", "crop", "dist", "mafruit_plant_1")], twin_posterior[ , c("ens", "num_params_varied", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1")]) %>%
  group_by(crop, dist, obs_calib, usm_calib, num_params_varied, ens) %>%
  summarise("Yield" = last(mafruit_plant_1)) %>%
  summarise("Yield_mean" = mean(Yield),
            "Yield_se" = sd(Yield)
            ) %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior") & num_params_varied %in% c(3, 8)) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  } %>%
  mutate(Yield_printing = sprintf("%.2f (%.2f)", Yield_mean, Yield_se)) %>%
  ungroup() %>%
  select(crop, dist, num_params_varied, Yield_printing)

yield_obs$Yield_printing <- sprintf("%.2f (%.2f)", yield_obs$value, yield_obs$uncertainty)
yield_obs %>%
  filter(num_params_varied %in% c(3, 8)) %>%
  select(crop, num_params_varied, Yield_printing) %>%
  mutate(dist = "Truth") %>%
  rbind(yield_table) %>%
  pivot_wider(names_from = num_params_varied, values_from = Yield_printing, names_prefix = "Yield, varying ") %>%
  arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Truth", "Prior", "Posterior"))) %>%
  rename(Crop = crop, Distribution = dist, "Yield, t ha-1 (std dev.) Varying 3 Parameters" = `Yield, varying 3`, "Yield, t ha-1 (std dev.) Varying 8 Parameters" = `Yield, varying 8`) %>%
  stargazer::stargazer(type = "text", summary = FALSE, rownames = FALSE, out = paste0(supplement_dir, "twin_yield_table_3+8_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, c(".tex", ".txt")), title = "Yield estimates for barley grown alone and with individual species of cover crop in 2020 twin experiments.", label = "tab:twinYieldEsts")

rm(yield_table)

# Creating a proportional correction table
prior_means <- stack(colMeans(general_xb))
names(prior_means) <- c("prior", "parameter")
params_synth <- stack(synth_params[8, ])
names(params_synth) <- c("synth", "parameter")

twin_analysis %>%
  rename(posterior = value) %>%
  merge(prior_means, by = "parameter", all.x = TRUE) %>%
  merge(params_synth, by = "parameter", all.x = TRUE) %>%
  mutate(synth = as.numeric(synth)) %>%
  mutate(prop_correction = 100 * (prior - posterior) / (prior - synth), perc_error_prior = 100 * (prior - synth) / synth, perc_error_post = 100 * (posterior - synth) / synth) %>%
  filter(num_params %in% c(3, 8), usm == "Barley_sole20_long") %>%
  mutate(across(c(4:6), \(x) signif(x, 2))) %>%
  mutate(prop_correction = round(prop_correction, 0), across(c(8:9), \(x) round(x, 1))) %>%
  arrange(factor(usm, levels = c("Barley_sole20_long", "Barley_AA20_long", "Barley_AC20_long", "Barley_CI20_long", "Barley_FA20_long", "Barley_IR20_long", "Barley_RC20_long", "Barley_TG20_long", "Barley_WC20_long")), num_params, tolower(parameter)) %>%
  select(-c(usm)) %>%
  relocate(synth, prior, .after = num_params) %>%
  relocate(prop_correction, .after = last_col()) %>%
  rename(Parameter = parameter, Truth = synth, Prior = prior, Posterior = posterior, "% Error Prior" = perc_error_prior, "% Error Posterior" = perc_error_post, "Proportional Correction" = prop_correction) %>%
  select(-c(num_params)) %>%
  stargazer::stargazer(type = "text", summary = FALSE, rownames = FALSE, out = paste0(manuscript_dir, "prop_correction_table_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, c(".tex", ".txt")), title = "Percentage Error in parameter estimates in the twin experiment. The upper section is the 3 parameter case, the lower has 8 parameters varying.", label = "tab:twinPercError", digits = NA)
# "# Parameters Varied" = num_params,
rm(twin_posterior, twin_analysis, twin_post_draws)

################################################################################
# Looking at the 3+1 results.
twin_fourth_prior <- readRDS(paste0(model_runs_dir, "fourth_prior.rds"))
twin_fourth_analysis <- readRDS(paste0(model_runs_dir, "fourth_analysis.rds"))
twin_fourth_draws <- readRDS(paste0(model_runs_dir, "fourth_draws.rds"))

# Mainly need to plot the parameter distributions here

rm(twin_fourth_prior, twin_fourth_analysis, twin_fourth_draws)
################################################################################
# twin experiment alternate truths: spread/mean of absolute % errors across truths; post. vs prior dists?; distribution of estimates by mean vs truths?

alt_truths_parameter_ens <- readRDS(paste0(model_runs_dir, "alt_truths_parameter_ens.rds")) %>%
  filter(num_params_varied == 4)
alt_truths_param_ests <- readRDS(paste0(model_runs_dir, "alt_truths_param_ests.rds")) %>%
  filter(num_params_varied == 4)
synth_params_all <- readRDS(paste0(model_runs_dir, "synth_params.rds")) %>%
  rownames_to_column() %>%
  separate_wider_delim(rowname, delim = "_", names = c(NA, "truth_num", NA, NA, "num_params"))

synth_params_4 <- synth_params_all %>%
  filter(num_params == 4) %>%
  select(truth_num, adens, efcroiveg, vitircarb, INNmin)


plot_ens_size <- n_ens
# Need to add back the prior mean, and the synthetic truth for each one, so that the plot can have on the x-axis the difference between the prior mean and the synthetic truth, and on the y axis the difference between the posterior mean and the synthetic truth. Need to consider how the uncertainties can be best communicated, is it worth including the prior uncertainty?

num_params_varied <- 4
name <- "Barley_sole20_long"
plot_xb <- pivot_longer(as.data.frame(general_xb), 1 : num_params_varied, names_to = "parameter")
plot_prior_params <- params[1 : num_params_varied] %>%
  pivot_longer(everything(), names_to = "parameter")
# for (truth in c(1:20)) {
#   plot_params <- alt_truths_param_ests %>%
#     filter(truth_used == truth) %>%
#     select(-c(truth_used, num_params_varied))
#
#   plot_post_draws <- alt_truths_parameter_ens %>%
#     filter(truth_used == truth, usm == name) %>%
#     select(-c(truth_used, num_params_varied))
#
#   plot_synth_params <- synth_params_4 %>%
#     filter(truth_num == truth) %>%
#     select(-c(truth_num)) %>%
#     pivot_longer(c(adens, efcroiveg, vitircarb, INNmin), names_to = "parameter")
#
#   dist_plotted <- plot_post_draws %>%
#     filter(usm == name) %>%
#     group_by(parameter) %>%
#     ggplot(aes(x = value)) +
#     # geom_histogram(aes(x = value, fill = dist), bins = 20, position = "dodge") +
#     geom_density(aes(fill = "Posterior", colour = "Posterior"), alpha = 0.5) +
#     geom_density(data = plot_xb, aes(fill = "Prior", colour = "Prior"), alpha = 0.5) +
#     geom_vline(data = plot_params[which(plot_params$usm == name), ], aes(xintercept = value, colour = "Posterior"), alpha = 1, linetype = "solid", linewidth = 1) +
#     geom_vline(data = plot_synth_params, aes(xintercept = value, colour = "Truth", fill = "Truth"), alpha = 1, linetype = "solid", linewidth = 1) +
#     geom_vline(data = plot_prior_params, aes(xintercept = value, colour = "Prior"), alpha = 1, linetype = "solid", linewidth = 1) +
#     # stat_function(fun = dnorm, args = list(mean = prior_mean, sd = prior_mean * 0.1)) +
#     facet_wrap(~ parameter, scales = "free") +
#     scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#     scale_x_continuous(expand = c(0, 0)) +
#     scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
#     scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
#     # scale_colour_viridis_d() +
#     # scale_fill_viridis_d() +
#     ggtitle(paste0("Estimated Parameter Ensemble Distributions: Sole Barley")) +
#     labs(y = "Density", x = "Parameter Value", fill = "", colour = "") +
#     # guides(fill = guide_legend(reverse = FALSE)) +
#     theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom")
#   # geom_line(data = plot_dists[plot_dists$dist == "prior", ], aes(y = dnorm(value, mean = tapply(value, parameter, mean)[PANEL], sd = tapply(value, parameter, sd)[PANEL])), colour = "blue")
#   if (inherits(try(ggplot_build(dist_plotted)), "try-error")) {
#     dist_plotted <- ggplot()
#     print(paste0("Plotting error with ", name, " truth ", truth))
#     next
#   }
#
#   dist_plotted
#   ggsave(paste0(exploration_dir, "twin_exp_ParameterDistributions_alt_truth_", truth, "_", name, "_", num_params_varied, "_params_", n_ens, "_ensemble.pdf"), units = "mm", width = 800, height = 420, limitsize = FALSE)
# }

truth <- 1
plot_params <- alt_truths_param_ests %>%
  filter(truth_used == truth) %>%
  select(-c(truth_used, num_params_varied))

plot_post_draws <- alt_truths_parameter_ens %>%
  filter(truth_used == truth, usm == name) %>%
  select(-c(truth_used, num_params_varied))

plot_synth_params <- synth_params_4 %>%
  filter(truth_num == truth) %>%
  select(-c(truth_num)) %>%
  pivot_longer(c(adens, efcroiveg, vitircarb, INNmin), names_to = "parameter")

dist_plotted <- plot_post_draws %>%
  filter(usm == name) %>%
  group_by(parameter) %>%
  ggplot(aes(x = value)) +
  # geom_histogram(aes(x = value, fill = dist), bins = 20, position = "dodge") +
  geom_density(aes(fill = "Posterior", colour = "Posterior"), alpha = 0.5) +
  geom_density(data = plot_xb, aes(fill = "Prior", colour = "Prior"), alpha = 0.5) +
  geom_vline(data = plot_params[which(plot_params$usm == name), ], aes(xintercept = value, colour = "Posterior"), alpha = 1, linetype = "solid", linewidth = 1) +
  geom_vline(data = plot_synth_params, aes(xintercept = value, colour = "Truth", fill = "Truth"), alpha = 1, linetype = "solid", linewidth = 1) +
  geom_vline(data = plot_prior_params, aes(xintercept = value, colour = "Prior"), alpha = 1, linetype = "solid", linewidth = 1) +
  # stat_function(fun = dnorm, args = list(mean = prior_mean, sd = prior_mean * 0.1)) +
  facet_wrap(~ parameter, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
  scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth")) +
  # scale_colour_viridis_d() +
  # scale_fill_viridis_d() +
  ggtitle(paste0("Estimated Parameter Ensemble Distributions: Sole Barley")) +
  labs(y = "Density", x = "Parameter Value", fill = "", colour = "") +
  # guides(fill = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom")
# geom_line(data = plot_dists[plot_dists$dist == "prior", ], aes(y = dnorm(value, mean = tapply(value, parameter, mean)[PANEL], sd = tapply(value, parameter, sd)[PANEL])), colour = "blue")
if (inherits(try(ggplot_build(dist_plotted)), "try-error")) {
  dist_plotted <- ggplot()
  print(paste0("Plotting error with ", name, " truth ", truth))
  next
}

dist_plotted
ggsave(paste0(manuscript_dir, "twin_ParameterDistributions_", name, "_", num_params_varied, "_params_", n_ens, "_ensemble.pdf"), units = "mm", width = 190, height = 100, limitsize = FALSE)

# tmp <- rand_params
# tmp$truth_num <- factor(1 : nrow(tmp))
# long_synth_truth <- pivot_longer(tmp, 1:8, names_to = c("parameter"), values_to = "true_value")
#
# long_prior_params <- pivot_longer(as.data.frame(xb), 1:8, names_to = "parameter") %>%
#   group_by(parameter) %>%
#   summarise(prior_mean = mean(value), prior_sd = sd(value), prior_sd_as_pct = prior_sd / abs(prior_mean) * 100)
#
# estimated_parameters %>%
#   group_by(num_params_varied, parameter, usm, truth_used) %>%
#   ggplot() +
#   geom_point(aes(x = value, y = truth_used, colour = usm, shape = parameter))
#
# tmp_calculating <- full_join(estimated_parameters, long_prior_params, by = join_by(parameter)) %>%
#   full_join(long_synth_truth, by = join_by(parameter, truth_used == truth_num)) %>%
#   na.omit()
# names(tmp_calculating) <- c("usm", "parameter", "est_value", "est_sd", "num_params_varied", "truth_used", "prior_mean", "prior_sd", "prior_sd_as_pct", "synthetic_truth")
#
#
#
# tmp_calculating$prior_diff_pct <- (tmp_calculating$prior_mean - tmp_calculating$synthetic_truth) / tmp_calculating$prior_mean * 100
# tmp_calculating$posterior_diff_pct <- (tmp_calculating$est_value - tmp_calculating$synthetic_truth) / tmp_calculating$prior_mean * 100
# tmp_calculating$post_sd_as_pct <- tmp_calculating$est_sd / abs(tmp_calculating$est_value) * 100
# tmp_calculating$prop_correction <- 100 * (tmp_calculating$prior_mean - tmp_calculating$est_value) / (tmp_calculating$prior_mean - tmp_calculating$synthetic_truth)
#
# COMP_PARAM_DIFFS <- FALSE
# if (COMP_PARAM_DIFFS) {
#   for (i in 4:8) {
#     param_diff_plot <- tmp_calculating %>%
#       filter(num_params_varied == i) %>%
#       group_by(parameter, usm, truth_used) %>%
#       ggplot() +
#       geom_point(aes(x = prior_diff_pct, y = posterior_diff_pct, colour = usm)) +
#       facet_wrap(vars(parameter), scales = "fixed") +
#       # facet_wrap_paginate(~ num_params_varied, ncol = 1, nrow = 1, scales = "free") +
#       geom_vline(aes(xintercept = prior_sd_as_pct, linetype = "Prior_sd"), alpha = 0.8) +
#       geom_vline(aes(xintercept = -prior_sd_as_pct, linetype = "Prior_sd"), alpha = 0.8) +
#       geom_hline(aes(yintercept = 0, linetype = "Posterior is at Truth")) +
#       ylim(-50, 50)
#     # geom_hline(aes(yintercept = post_sd_as_pct, colour = usm, linetype = "Post_sd"), alpha = 0.01)
#     # param_diff_plot + facet_wrap(vars(parameter, num_params_varied), scales = "free")
#     param_diff_plot
#     ggsave(filename = paste0("ParameterDifferences_", i, "_params_varied_", plot_ens_size, "_ensemble.pdf"),
#            path = results_path,
#            units = "mm", width = 190, height = 105)
#   }
# }

rm(alt_truths_param_ests, alt_truths_parameter_ens, plot_post_draws, plot_synth_params, plot_prior_params, plot_params, dist_plotted, synth_params_all, synth_params_4)
################################################################################
# TWINWIN calib
num_params_varied <- 3
prior_2021 <- readRDS(paste0(model_runs_dir, "prior_2021.rds")) %>%
  mutate(obs_calib = NA, usm_calib = NA) %>%
  filter(varying == num_params_varied) %>%
  rename(num_params_varied = varying)
obs_vary_analysis <- readRDS(paste0(model_runs_dir, "obs_vary_analysis.rds"))
obs_vary_draws <- readRDS(paste0(model_runs_dir, "obs_vary_draws.rds"))
twinwin_posterior <- readRDS(paste0(model_runs_dir, "TWINWIN_posterior.rds"))

# Plotting the distributions and analytical results.
print(paste0("Plotting ", num_params_varied))
plot_xb <- pivot_longer(as.data.frame(general_xb), 1 : all_of(num_params_varied), names_to = "parameter")
plot_params <- as.data.frame(t(params[1 : num_params_varied]))
colnames(plot_params) <- "value"
plot_params <- rownames_to_column(plot_params, var = "parameter")
# [1] "#F8766D" "#CD9600" "#7CAE00" "#00BE67" "#00BFC4" "#00A9FF" "#C77CFF" "#FF61CC"
# for (name in calib_usms) {
#   print(name)
#   dist_plotted <- long_obs_vary_draws %>%
#     filter(usm == name) %>%
#     group_by(parameter) %>%
#     ggplot(aes(x = value)) +
#     # geom_histogram(aes(x = value, fill = dist), bins = 20, position = "dodge") +
#     geom_density(aes(fill = obs_set), alpha = 0.3) +
#     geom_density(data = plot_xb, aes(fill = "Prior"), alpha = 0.5) +
#     geom_vline(data = long_obs_vary_analysis[long_obs_vary_analysis$usm == name, ], aes(xintercept = value, colour = obs_set), alpha = 1, linetype = "solid", linewidth = 1) +
#     geom_vline(data = plot_params, aes(xintercept = value, colour = "Prior"), alpha = 1, linetype = "solid", linewidth = 1) +
#     facet_wrap(~ usm + parameter, scales = "free") +
#     scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth", "laineeyield", "lainee", "laiyield", "lai", "neeyield", "nee", "yield")) +
#     scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Truth", "laineeyield", "lainee", "laiyield", "lai", "neeyield", "nee", "yield")) +
#     ggtitle("Estimated Parameter Ensemble Distributions, Varying Observation Types") +
#     labs(y = "Density", x = "Parameter Value", fill = "Distribution", colour = "Distribution") +
#     theme(text = element_text(size = 40), legend.key.size = unit(2, 'cm'))
#   if (inherits(try(ggplot_build(dist_plotted)), "try-error")) {
#     dist_plotted <- ggplot()
#     print(paste0("Plotting error with ", name))
#   }
#   dist_plotted
#   ggsave(paste0(exploration_dir, "ObsVary_ParameterDistributions_", name, "_", num_params_varied, "_params", ".pdf"), units = "mm", width = 1600, height = 840, limitsize = FALSE)
# }

# Comparison of calibrated distributions plots.
name <- "Barley_sole20_long"
print(name)
dist_plotted <- obs_vary_draws %>%
  filter(usm == name) %>%
  group_by(parameter) %>%
  ggplot(aes(x = value)) +
  # geom_histogram(aes(x = value, fill = dist), bins = 20, position = "dodge") +
  geom_density(aes(fill = obs_set), alpha = 0.3) +
  geom_density(data = plot_xb, aes(fill = "Prior"), alpha = 0.5) +
  geom_vline(data = obs_vary_analysis[obs_vary_analysis$usm == name, ], aes(xintercept = value, colour = obs_set), alpha = 1, linetype = "solid", linewidth = 1) +
  geom_vline(data = plot_params, aes(xintercept = value, colour = "Prior"), alpha = 1, linetype = "solid", linewidth = 1) +
  facet_wrap(~ parameter, scales = "free") +
  scale_colour_manual(values = viridis(10), breaks = c("Prior", "Posterior", "Truth", "laineeyield", "lainee", "laiyield", "lai", "neeyield", "nee", "yield")) +
  scale_fill_manual(values = viridis(10), breaks = c("Prior", "Posterior", "Truth", "laineeyield", "lainee", "laiyield", "lai", "neeyield", "nee", "yield")) +
  ggtitle("Estimated Parameter Ensemble Distributions of Sole Barley, Varying Observation Types") +
  labs(y = "Density", x = "Parameter Value", fill = "Distribution", colour = "Distribution") +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'))
if (inherits(try(ggplot_build(dist_plotted)), "try-error")) {
  dist_plotted <- ggplot()
  print(paste0("Plotting error with ", name))
}
dist_plotted
ggsave(paste0(manuscript_dir, "ObsVary_ParameterDistributions_", name, "_", num_params_varied, "_params", ".pdf"), units = "mm", width = 190, height = 100, limitsize = FALSE)


# Import observations
obs_data <- read_csv("~/Documents/TWINWIN-Intercrop/Data/TWINWINobs.csv") %>%
  filter(crop != "TG") # TG estimates are unusable due to known bugs in STICS

# Set up variables that are referred to in the loops that can allow for programmatic plotting with minimal changes
infixes <- gsub("[Barley_1290long]", "", calib_usms)
infixes[which(infixes == "s")] <- "Barley"
# plot_years <- "2020"
plot_crops <- infixes
# plot_obs_calib <- obs_names
plot_dists <- factor(c("Prior", "Posterior"), levels = c("Prior", "Posterior", "Truth"))
plot_usm_calib <- "Self"
plot_ens_size <- n_ens
obs_sets <- unique(twinwin_posterior$obs_calib)

plot_prior_2020 <- general_prior %>%
  filter(num_params_varied == 3)

# Exploratory plotting comparing different observation sets being used in calibration
# for (plot_years in c("2020", "2021")) {
#   # Filter/set the various inputs that change with the year being plotted.
#   yield_obs_dates <- obs_data %>%
#     filter(obs == "yield", format(date, "%Y") == plot_years) %>%
#     first() %>%
#     .$date
#   if (plot_years == "2020") {
#     lai_obs <- obs_data %>%
#       filter(obs == "GAI", between(date, as.Date("2020/05/22"), as.Date("2020/09/11")))
#     nee_obs <- obs_data %>%
#       filter(obs == "NEE", between(date, as.Date("2020/05/22"), as.Date("2021/05/22")))
#     plot_prior <- plot_prior_2020
#     plot_posterior <- twinwin_posterior %>%
#       filter(between(date, as.Date("2020/05/22"), as.Date("2021/05/22")))
#   } else {
#     lai_obs <- obs_data %>%
#       filter(obs == "GAI", between(date, as.Date("2021/05/25"), as.Date("2022/09/16")))
#     nee_obs <- obs_data %>%
#       filter(obs == "NEE", between(date, as.Date("2021/05/25"), as.Date("2022/05/25")))
#     plot_prior <- prior_2021
#     plot_posterior <- twinwin_posterior %>%
#       filter(between(date, as.Date("2021/05/25"), as.Date("2022/05/25")))
#   }
#
#   # Looping over all sets of observation calibrations.
#   for (plot_obs_calib in obs_sets) {
#     # Prepare a suitable combined data frame for plotting.
#     # LAI plot prep
#     lai_plot_df <- dplyr::bind_rows(plot_prior[ , c("ens", "date", "crop", "dist", "lai_n_plant_1")], plot_posterior[ , c("ens", "date", "crop", "dist", "obs_calib", "usm_calib", "lai_n_plant_1")]) %>%
#       group_by(date, crop, dist, obs_calib, usm_calib) %>%
#       summarise("LAI" = mean(lai_n_plant_1),
#                 "LAI_CI_min" = max(mean(lai_n_plant_1) - 1.96 * sd(lai_n_plant_1), 0),
#                 "LAI_CI_max" = mean(lai_n_plant_1) + 1.96 * sd(lai_n_plant_1)
#       ) %>%
#       filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
#       {
#         if (plot_usm_calib == "Self") {
#           filter(., usm_calib == crop | dist == "Prior")
#         } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
#       } %>%
#       {
#         if (!("Prior" %in% plot_dists)) {
#           filter(., dist != "Prior")
#         } else .
#       }
#     # NEE plot prep
#     nee_plot_df <- dplyr::bind_rows(plot_prior[ , c("ens", "date", "crop", "dist", "nee")], plot_posterior[ , c("ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")]) %>%
#       group_by(date, crop, dist, obs_calib, usm_calib) %>%
#       summarise("NEE" = mean(nee),
#                 "NEE_se" = sd(nee)
#       ) %>%
#       filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
#       {
#         if (plot_usm_calib == "Self") {
#           filter(., usm_calib == crop | dist == "Prior")
#         } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
#       } %>%
#       {
#         if (!("Prior" %in% plot_dists)) {
#           filter(., dist != "Prior")
#         } else .
#       }
#
#
#
#     ############# Mean Plots #############
#     # LAI plot
#     lai_plot <- lai_plot_df %>%
#       filter(date < yield_obs_dates) %>%
#       ggplot() +
#       # scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep) +
#       geom_line(aes(date, LAI, colour = interaction(dist))) +
#       facet_wrap(vars(crop)) +
#       ggtitle(paste0("Average estimated LAI, ", plot_years, ", ", plot_usm_calib, " calibrated")) +
#       labs(y = expression(paste("LAI (", m^{2}, m^{-2}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution Mean", shape = "Observations") +
#       geom_ribbon(aes(date, ymin = LAI_CI_min, ymax = LAI_CI_max, fill = interaction(dist)), alpha = 0.5) +
#       geom_point(data = lai_obs[lai_obs$crop %in% plot_crops, ], aes(x = date, y = value, colour = "Observation"), size = 1) +
#       geom_errorbar(lai_obs[lai_obs$crop %in% plot_crops, ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Observation"), width = 0.5) +
#       # geom_vline(aes(xintercept = yield_obs_dates[which(format(yield_obs_dates, "%Y") == plot_years)], colour = "Harvest"), linetype = "dashed") +
#       guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = TRUE)) +
#       theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
#       scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
#       scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
#       # scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#       scale_x_date(expand = c(0, 0))
#
#     if (inherits(try(ggplot_build(lai_plot)), "try-error")) {
#       lai_plot <- ggplot()
#       print(paste0("Plotting error with LAI plot of ", plot_years, plot_obs_calib))
#     } else{
#       lai_plot
#
#       ggsave(filename = paste0("ObsVary_LAI_plot_mean_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
#              path = paste0(exploration_dir, "ObsVary"),
#              units = "mm", width = 190, height = 105)
#     }
#
#     # NEE plot
#     nee_plot <- nee_plot_df %>%
#       ggplot() +
#       scale_y_continuous(limits = c(-0.47, 0.1), oob = scales::oob_keep) +
#       geom_line(aes(date, NEE, colour = interaction(dist))) +
#       geom_line(data = basic_truth[basic_truth$crop %in% plot_crops & basic_truth$num_params_varied == plot_varying_params, ], aes(date, nee), colour = "black", alpha = 0.5) +
#       facet_wrap(vars(crop)) +
#       ggtitle(paste0("Average estimated NEE, ", plot_years, ", ", plot_usm_calib, " calibrated")) +
#       labs(y = expression(paste("NEE (t", CO[2], "", ha^{-1}, "", d^{-1}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution Mean", shape = "Used in Calibration (2020)") +
#       geom_ribbon(aes(date, ymin = NEE - 1.96 * NEE_se, ymax = NEE + 1.96 * NEE_se, fill = interaction(dist)), alpha = 0.30) +
#       geom_point(data = nee_obs[nee_obs$crop %in% plot_crops, ], aes(x = date, y = value, colour = "Observation"), size = 1) +
#       geom_errorbar(nee_obs[nee_obs$crop %in% plot_crops, ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Observation"), width = 0.5) +
#       geom_hline(aes(yintercept = 0), linetype = "dashed", colour = "grey") +
#       geom_vline(aes(xintercept = yield_obs_dates, colour = "Harvest"), linetype = "dashed") +
#       guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
#       theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
#       scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
#       scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
#       scale_x_date(expand = c(0, 2), breaks = waiver(), labels = waiver())
#
#
#     if (inherits(try(ggplot_build(nee_plot)), "try-error")) {
#       nee_plot <- ggplot()
#       print(paste0("Plotting error with NEE plot of ", plot_years, plot_obs_calib))
#     } else{
#       nee_plot
#
#       ggsave(filename = paste0("ObsVary_NEE_plot_mean_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
#              path = paste0(exploration_dir, "ObsVary"),
#              units = "mm", width = 190, height = 105)
#     }
#
#
#
#   }
# }

plot_years <- "2020"
# Filter/set the various inputs that change with the year being plotted.
yield_obs_dates <- obs_data %>%
  filter(obs == "yield", format(date, "%Y") == plot_years) %>%
  first() %>%
  .$date
lai_obs <- obs_data %>%
  filter(obs == "GAI", between(date, as.Date("2020/05/22"), as.Date("2020/09/11")))
nee_obs <- obs_data %>%
  filter(obs == "NEE", between(date, as.Date("2020/05/22"), as.Date("2021/05/22")))
plot_prior <- plot_prior_2020
plot_posterior <- twinwin_posterior %>%
  filter(between(date, as.Date("2020/05/22"), as.Date("2021/05/22")))


# Plotting main manuscript results using the best calibration of gai+yield obs.
plot_obs_calib <- "laiyield"
# LAI plot prep
lai_plot_df <- dplyr::bind_rows(plot_prior[ , c("ens", "date", "crop", "dist", "lai_n_plant_1")], plot_posterior[ , c("ens", "date", "crop", "dist", "obs_calib", "usm_calib", "lai_n_plant_1")]) %>%
  group_by(date, crop, dist, obs_calib, usm_calib) %>%
  summarise("LAI" = mean(lai_n_plant_1),
            "LAI_CI_min" = max(mean(lai_n_plant_1) - 1.96 * sd(lai_n_plant_1), 0),
            "LAI_CI_max" = mean(lai_n_plant_1) + 1.96 * sd(lai_n_plant_1)
  ) %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  }
# NEE plot prep
nee_plot_df <- dplyr::bind_rows(plot_prior[ , c("ens", "date", "crop", "dist", "nee")], plot_posterior[ , c("ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")]) %>%
  group_by(date, crop, dist, obs_calib, usm_calib) %>%
  summarise("NEE" = mean(nee),
            "NEE_se" = sd(nee)
  ) %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  }



############# Manuscript Plots #############
# LAI plot
lai_plot <- lai_plot_df %>%
  filter(date < yield_obs_dates) %>%
  ggplot() +
  # scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep) +
  geom_line(aes(date, LAI, colour = interaction(dist))) +
  facet_wrap(vars(crop)) +
  ggtitle(paste0("Average estimated LAI, ", plot_years, ", ", plot_usm_calib, " calibrated")) +
  labs(y = expression(paste("LAI (", m^{2}, m^{-2}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution Mean", shape = "Observations") +
  geom_ribbon(aes(date, ymin = LAI_CI_min, ymax = LAI_CI_max, fill = interaction(dist)), alpha = 0.5) +
  geom_point(data = lai_obs[lai_obs$crop %in% plot_crops, ], aes(x = date, y = value, colour = "Observation"), size = 1) +
  geom_errorbar(lai_obs[lai_obs$crop %in% plot_crops, ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Observation"), width = 0.5) +
  # geom_vline(aes(xintercept = yield_obs_dates[which(format(yield_obs_dates, "%Y") == plot_years)], colour = "Harvest"), linetype = "dashed") +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = TRUE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
  scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  # scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_date(expand = c(0, 0))

if (inherits(try(ggplot_build(lai_plot)), "try-error")) {
  lai_plot <- ggplot()
  print(paste0("Plotting error with LAI plot of ", plot_years, plot_obs_calib))
} else{
  lai_plot

  ggsave(filename = paste0("ObsVary_LAI_plot_mean_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
         path = supplement_dir,
         units = "mm", width = 190, height = 105)
}

# NEE plot
nee_plot <- nee_plot_df %>%
  filter(between(date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  ggplot() +
  scale_y_continuous(limits = c(-0.47, 0.1), oob = scales::oob_keep) +
  geom_line(aes(date, NEE, colour = dist, linetype = dist)) +
  # geom_line(data = basic_truth[basic_truth$crop %in% plot_crops & basic_truth$num_params_varied == plot_varying_params, ], aes(date, nee), colour = "black", alpha = 0.5) +
  facet_wrap(vars(crop)) +
  ggtitle(paste0("Average estimated NEE, barley growing season ", plot_years, ", ", plot_usm_calib, " calibrated")) +
  labs(y = expression(paste("NEE (t", CO[2], "", ha^{-1}, "", d^{-1}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution", linetype = "Distribution", shape = "Used in Calibration (2020)") +
  geom_ribbon(aes(date, ymin = NEE - 1.96 * NEE_se, ymax = NEE + 1.96 * NEE_se, fill = dist), alpha = 0.5) +
  geom_point(data = nee_obs[nee_obs$crop %in% plot_crops & between(nee_obs$date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates), ], aes(x = date, y = value, colour = "Observation"), size = 1) +
  geom_errorbar(nee_obs[nee_obs$crop %in% plot_crops & between(nee_obs$date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates), ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Observation"), width = 0.5) +
  geom_hline(aes(yintercept = 0, linetype = "Observation"), colour = "grey") +
  geom_vline(aes(xintercept = yield_obs_dates, colour = "Harvest", linetype = "Harvest")) +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
  scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_linetype_manual(values = c(2, 1, 2, 2), breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_x_date(expand = c(0, 2), breaks = waiver(), labels = waiver())

if (inherits(try(ggplot_build(nee_plot)), "try-error")) {
  nee_plot <- ggplot()
  print(paste0("Plotting error with NEE plot of ", plot_years, plot_obs_calib))
} else{
  nee_plot

  ggsave(filename = paste0("ObsVary_NEE_plot_mean_barley_season_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
         path = manuscript_dir,
         units = "mm", width = 190, height = 105)
}


# NEE plot
nee_plot <- nee_plot_df %>%
  filter(between(date, yield_obs_dates, as.Date(paste0(plot_years, "/12/12"))) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  ggplot() +
  scale_y_continuous(limits = c(-0.47, 0.1), oob = scales::oob_keep) +
  geom_line(aes(date, NEE, colour = dist, linetype = dist)) +
  # geom_line(data = basic_truth[basic_truth$crop %in% plot_crops & basic_truth$num_params_varied == plot_varying_params, ], aes(date, nee), colour = "black", alpha = 0.5) +
  facet_wrap(vars(crop)) +
  ggtitle(paste0("Average estimated NEE, post-harvest ", plot_years, ", ", plot_usm_calib, " calibrated")) +
  labs(y = expression(paste("NEE (t", CO[2], "", ha^{-1}, "", d^{-1}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution", linetype = "Distribution", shape = "Used in Calibration (2020)") +
  geom_ribbon(aes(date, ymin = NEE - 1.96 * NEE_se, ymax = NEE + 1.96 * NEE_se, fill = dist), alpha = 0.5) +
  geom_point(data = nee_obs[nee_obs$crop %in% plot_crops & between(nee_obs$date, yield_obs_dates, as.Date(paste0(plot_years, "/12/12"))), ], aes(x = date, y = value, colour = "Observation"), size = 1) +
  geom_errorbar(nee_obs[nee_obs$crop %in% plot_crops & between(nee_obs$date, yield_obs_dates, as.Date(paste0(plot_years, "/12/12"))), ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Observation"), width = 0.5) +
  geom_hline(aes(yintercept = 0, linetype = "Observation"), colour = "grey") +
  geom_vline(aes(xintercept = yield_obs_dates, colour = "Harvest", linetype = "Harvest")) +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
  scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_linetype_manual(values = c(2, 1, 2, 2), breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_x_date(expand = c(0, 2), breaks = waiver(), labels = waiver())

if (inherits(try(ggplot_build(nee_plot)), "try-error")) {
  nee_plot <- ggplot()
  print(paste0("Plotting error with NEE plot of ", plot_years, plot_obs_calib))
} else{
  nee_plot

  ggsave(filename = paste0("ObsVary_NEE_plot_post_harvest_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
         path = manuscript_dir,
         units = "mm", width = 190, height = 105)
}


############# Manuscript posterior yield table #############
# Yield Table preparation
yield_obs <- obs_data %>%
  filter(obs == "yield") %>%
  mutate(year = format(date, "%Y"), dist = "Observation") %>%
  select(-c(date, obs))
yield_obs_table <- yield_obs %>%
  mutate(Yield_printing = sprintf("%.3f (%.3f)", value, uncertainty),) %>%
  select(year, crop, dist, Yield_printing)
# Splitting some of this data trimming down to reduce memory load
yield_prior_2020 <- plot_prior_2020 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise("Yield" = last(mafruit_plant_1)) %>%
  summarise("Yield_mean" = mean(Yield),
            "Yield_se" = sd(Yield)
  )
yield_prior_2021 <- prior_2021 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise("Yield" = last(mafruit_plant_1)) %>%
  summarise("Yield_mean" = mean(Yield),
            "Yield_se" = sd(Yield)
  )

yield_table <- twinwin_posterior %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise("Yield" = last(mafruit_plant_1)) %>%
  summarise("Yield_mean" = mean(Yield),
            "Yield_se" = sd(Yield)
  ) %>%
  bind_rows(yield_prior_2020) %>%
  bind_rows(yield_prior_2021)

rm(yield_prior_2020, yield_prior_2021)


yield_table %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  } %>%
  mutate(Yield_printing = sprintf("%.3f (%.3f)", Yield_mean, Yield_se)) %>%
  ungroup() %>%
  select(year, crop, dist, Yield_printing) %>%
  bind_rows(yield_obs_table) %>%
  pivot_wider(names_from = year, values_from = Yield_printing) %>%
  arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Observation", "Prior", "Posterior"))) %>%
  rename(Crop = crop, Distribution = dist, "Yield 2020, t ha-1 (std dev.)" = "2020", "Yield 2021, t ha-1 (std dev.)" = "2021") %>%
  stargazer::stargazer(type = "text", summary = FALSE, rownames = FALSE, out = paste0(manuscript_dir, "yield_table__", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, c(".tex", ".txt")))

############# Yield bar plot #############
yield_table %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  } %>%
  rename(value = Yield_mean, uncertainty = Yield_se) %>%
  select(-c(usm_calib, obs_calib)) %>%
  bind_rows(yield_obs) %>%
  arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Observation", "Prior", "Posterior"))) %>%
  ggplot(aes(x = interaction(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC"))), y = value, fill = factor(dist, levels = c("Observation", "Prior", "Posterior")))) +
  scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep, expand = c(0,0)) +
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.6) +
  geom_errorbar(aes(ymin = value - 1.96 * uncertainty, ymax = value + 1.96 * uncertainty), width = 0.2, position = position_dodge(0.9)) +
  geom_vline(aes(xintercept = 0, colour = "95% Confidence Interval", group = 1), width = 0.2,) +
  scale_fill_manual(values = plot_colours[c(3, 1, 2, 4)], breaks = c("Observation", "Prior", "Posterior")) +
  scale_colour_manual(values = "black", breaks = c("95% Confidence Interval")) +
  facet_wrap(vars(year), ncol = 1, axes = "all_x") +
  ggtitle(paste0("Yields, Observed and Estimated with LAI + Yield Self-Calibration")) +
  labs(y = expression(paste("Yield (t ", ha^{-1}, ")")), x = "Secondary Crop", fill = "Distribution", colour = "") +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom")

ggsave(filename = paste0("Yield_plot_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
       path = manuscript_dir,
       units = "mm", width = 190, height = 105)

rm(yield_table)

################# NEE bar plot #################
plot_prior_2020
prior_2021

# Getting the necessary data:
# Splitting some of this data trimming down to reduce memory load
nee_prior_2020 <- plot_prior_2020 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise(nee_sum = sum(nee)) %>%
  summarise("value" = mean(nee_sum),
            "uncertainty" = sd(nee_sum)
  )
nee_prior_2021 <- prior_2021 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise(nee_sum = sum(nee)) %>%
  summarise("value" = mean(nee_sum),
            "uncertainty" = sd(nee_sum)
  )

nee_table <- twinwin_posterior %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise(nee_sum = sum(nee)) %>%
  summarise("value" = mean(nee_sum),
            "uncertainty" = sd(nee_sum)
  ) %>%
  bind_rows(nee_prior_2020) %>%
  bind_rows(nee_prior_2021)

rm(nee_prior_2020, nee_prior_2021)

nee_table %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  } %>%
  # rename(value = Yield_mean, uncertainty = Yield_se) %>%
  select(-c(usm_calib, obs_calib)) %>%
  # bind_rows(yield_obs) %>%
  arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Observation", "Prior", "Posterior"))) %>%
  ggplot(aes(x = interaction(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC"))), y = value, fill = factor(dist, levels = c("Observation", "Prior", "Posterior")))) +
  # scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep, expand = c(0,0)) +
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.6) +
  geom_errorbar(aes(ymin = value - 1.96 * uncertainty, ymax = value + 1.96 * uncertainty), width = 0.2, position = position_dodge(0.9)) +
  geom_vline(aes(xintercept = 0, colour = "95% Confidence Interval", group = 1), width = 0.2,) +
  scale_fill_manual(values = plot_colours[c(3, 1, 2, 4)], breaks = c("Observation", "Prior", "Posterior")) +
  scale_colour_manual(values = "black", breaks = c("95% Confidence Interval")) +
  facet_wrap(vars(year), ncol = 1, axes = "all_x") +
  ggtitle(paste0("Estimated Net Ecosystem Exchange using LAI + Yield Self-Calibration")) +
  labs(y = expression(paste("NEE (t", CO[2], "", ha^{-1}, ")")), x = "Secondary Crop", fill = "Distribution", colour = "") +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom")

ggsave(filename = paste0("NEE_plot_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
       path = manuscript_dir,
       units = "mm", width = 190, height = 105)

rm(nee_table)
################################################################################
# Plotting nee+yield calibration results for supplementary materials
# Plotting main manuscript results using the best calibration of gai+yield obs.
plot_obs_calib <- "neeyield"
# LAI plot prep
lai_plot_df <- dplyr::bind_rows(plot_prior[ , c("ens", "date", "crop", "dist", "lai_n_plant_1")], plot_posterior[ , c("ens", "date", "crop", "dist", "obs_calib", "usm_calib", "lai_n_plant_1")]) %>%
  group_by(date, crop, dist, obs_calib, usm_calib) %>%
  summarise("LAI" = mean(lai_n_plant_1),
            "LAI_CI_min" = max(mean(lai_n_plant_1) - 1.96 * sd(lai_n_plant_1), 0),
            "LAI_CI_max" = mean(lai_n_plant_1) + 1.96 * sd(lai_n_plant_1)
  ) %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  }
# NEE plot prep
nee_plot_df <- dplyr::bind_rows(plot_prior[ , c("ens", "date", "crop", "dist", "nee")], plot_posterior[ , c("ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")]) %>%
  group_by(date, crop, dist, obs_calib, usm_calib) %>%
  summarise("NEE" = mean(nee),
            "NEE_se" = sd(nee)
  ) %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  }



############# Manuscript Plots #############
# LAI plot
lai_plot <- lai_plot_df %>%
  filter(date < yield_obs_dates) %>%
  ggplot() +
  # scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep) +
  geom_line(aes(date, LAI, colour = interaction(dist))) +
  facet_wrap(vars(crop)) +
  ggtitle(paste0("Average estimated LAI, ", plot_years, ", ", plot_usm_calib, " calibrated")) +
  labs(y = expression(paste("LAI (", m^{2}, m^{-2}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution Mean", shape = "Observations") +
  geom_ribbon(aes(date, ymin = LAI_CI_min, ymax = LAI_CI_max, fill = interaction(dist)), alpha = 0.5) +
  geom_point(data = lai_obs[lai_obs$crop %in% plot_crops, ], aes(x = date, y = value, colour = "Observation"), size = 1) +
  geom_errorbar(lai_obs[lai_obs$crop %in% plot_crops, ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Observation"), width = 0.5) +
  # geom_vline(aes(xintercept = yield_obs_dates[which(format(yield_obs_dates, "%Y") == plot_years)], colour = "Harvest"), linetype = "dashed") +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = TRUE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
  scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  # scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_date(expand = c(0, 0))

if (inherits(try(ggplot_build(lai_plot)), "try-error")) {
  lai_plot <- ggplot()
  print(paste0("Plotting error with LAI plot of ", plot_years, plot_obs_calib))
} else{
  lai_plot

  ggsave(filename = paste0("ObsVary_LAI_plot_mean_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
         path = supplement_dir,
         units = "mm", width = 190, height = 105)
}

# NEE plot
nee_plot <- nee_plot_df %>%
  filter(between(date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  ggplot() +
  scale_y_continuous(limits = c(-0.47, 0.1), oob = scales::oob_keep) +
  geom_line(aes(date, NEE, colour = dist, linetype = dist)) +
  # geom_line(data = basic_truth[basic_truth$crop %in% plot_crops & basic_truth$num_params_varied == plot_varying_params, ], aes(date, nee), colour = "black", alpha = 0.5) +
  facet_wrap(vars(crop)) +
  ggtitle(paste0("Average estimated NEE, barley growing season ", plot_years, ", ", plot_usm_calib, " calibrated")) +
  labs(y = expression(paste("NEE (t", CO[2], "", ha^{-1}, "", d^{-1}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution", linetype = "Distribution", shape = "Used in Calibration (2020)") +
  geom_ribbon(aes(date, ymin = NEE - 1.96 * NEE_se, ymax = NEE + 1.96 * NEE_se, fill = dist), alpha = 0.5) +
  geom_point(data = nee_obs[nee_obs$crop %in% plot_crops & between(nee_obs$date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates), ], aes(x = date, y = value, colour = "Observation"), size = 1) +
  geom_errorbar(nee_obs[nee_obs$crop %in% plot_crops & between(nee_obs$date, as.Date(paste0(plot_years, "/05/20")), yield_obs_dates), ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Observation"), width = 0.5) +
  geom_hline(aes(yintercept = 0, linetype = "Observation"), colour = "grey") +
  geom_vline(aes(xintercept = yield_obs_dates, colour = "Harvest", linetype = "Harvest")) +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
  scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_linetype_manual(values = c(2, 1, 2, 2), breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_x_date(expand = c(0, 2), breaks = waiver(), labels = waiver())

if (inherits(try(ggplot_build(nee_plot)), "try-error")) {
  nee_plot <- ggplot()
  print(paste0("Plotting error with NEE plot of ", plot_years, plot_obs_calib))
} else{
  nee_plot

  ggsave(filename = paste0("ObsVary_NEE_plot_mean_barley_season_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
         path = supplement_dir,
         units = "mm", width = 190, height = 105)
}


# NEE plot
nee_plot <- nee_plot_df %>%
  filter(between(date, yield_obs_dates, as.Date(paste0(plot_years, "/12/12"))) & crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  ggplot() +
  scale_y_continuous(limits = c(-0.47, 0.1), oob = scales::oob_keep) +
  geom_line(aes(date, NEE, colour = dist, linetype = dist)) +
  # geom_line(data = basic_truth[basic_truth$crop %in% plot_crops & basic_truth$num_params_varied == plot_varying_params, ], aes(date, nee), colour = "black", alpha = 0.5) +
  facet_wrap(vars(crop)) +
  ggtitle(paste0("Average estimated NEE, post-harvest ", plot_years, ", ", plot_usm_calib, " calibrated")) +
  labs(y = expression(paste("NEE (t", CO[2], "", ha^{-1}, "", d^{-1}, ")")), x = "Date", fill = "95% Confidence Interval", colour = "Distribution", linetype = "Distribution", shape = "Used in Calibration (2020)") +
  geom_ribbon(aes(date, ymin = NEE - 1.96 * NEE_se, ymax = NEE + 1.96 * NEE_se, fill = dist), alpha = 0.5) +
  geom_point(data = nee_obs[nee_obs$crop %in% plot_crops & between(nee_obs$date, yield_obs_dates, as.Date(paste0(plot_years, "/12/12"))), ], aes(x = date, y = value, colour = "Observation"), size = 1) +
  geom_errorbar(nee_obs[nee_obs$crop %in% plot_crops & between(nee_obs$date, yield_obs_dates, as.Date(paste0(plot_years, "/12/12"))), ], mapping = aes(x = date, ymax = value + 1.96 * uncertainty, ymin = value - 1.96 * uncertainty, colour = "Observation"), width = 0.5) +
  geom_hline(aes(yintercept = 0, linetype = "Observation"), colour = "grey") +
  geom_vline(aes(xintercept = yield_obs_dates, colour = "Harvest", linetype = "Harvest")) +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
  scale_colour_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_fill_manual(values = plot_colours, breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_linetype_manual(values = c(2, 1, 2, 2), breaks = c("Prior", "Posterior", "Observation", "Harvest")) +
  scale_x_date(expand = c(0, 2), breaks = waiver(), labels = waiver())

if (inherits(try(ggplot_build(nee_plot)), "try-error")) {
  nee_plot <- ggplot()
  print(paste0("Plotting error with NEE plot of ", plot_years, plot_obs_calib))
} else{
  nee_plot

  ggsave(filename = paste0("ObsVary_NEE_plot_post_harvest_", plot_varying_params, "_params_", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
         path = supplement_dir,
         units = "mm", width = 190, height = 105)
}


############# Manuscript posterior yield table #############
# Yield Table preparation
yield_obs <- obs_data %>%
  filter(obs == "yield") %>%
  mutate(year = format(date, "%Y"), dist = "Observation") %>%
  select(-c(date, obs))
yield_obs_table <- yield_obs %>%
  mutate(Yield_printing = sprintf("%.3f (%.3f)", value, uncertainty),) %>%
  select(year, crop, dist, Yield_printing)
# Splitting some of this data trimming down to reduce memory load
yield_prior_2020 <- plot_prior_2020 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise("Yield" = last(mafruit_plant_1)) %>%
  summarise("Yield_mean" = mean(Yield),
            "Yield_se" = sd(Yield)
  )
yield_prior_2021 <- prior_2021 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise("Yield" = last(mafruit_plant_1)) %>%
  summarise("Yield_mean" = mean(Yield),
            "Yield_se" = sd(Yield)
  )

yield_table <- twinwin_posterior %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise("Yield" = last(mafruit_plant_1)) %>%
  summarise("Yield_mean" = mean(Yield),
            "Yield_se" = sd(Yield)
  ) %>%
  bind_rows(yield_prior_2020) %>%
  bind_rows(yield_prior_2021)

rm(yield_prior_2020, yield_prior_2021)


yield_table %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  } %>%
  mutate(Yield_printing = sprintf("%.3f (%.3f)", Yield_mean, Yield_se)) %>%
  ungroup() %>%
  select(year, crop, dist, Yield_printing) %>%
  bind_rows(yield_obs_table) %>%
  pivot_wider(names_from = year, values_from = Yield_printing) %>%
  arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Observation", "Prior", "Posterior"))) %>%
  rename(Crop = crop, Distribution = dist, "Yield 2020, t ha-1 (std dev.)" = "2020", "Yield 2021, t ha-1 (std dev.)" = "2021") %>%
  stargazer::stargazer(type = "text", summary = FALSE, rownames = FALSE, out = paste0(supplement_dir, "yield_table__", plot_years, "_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, c(".tex", ".txt")))

############# Yield bar plot #############
yield_table %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  } %>%
  rename(value = Yield_mean, uncertainty = Yield_se) %>%
  select(-c(usm_calib, obs_calib)) %>%
  bind_rows(yield_obs) %>%
  arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Observation", "Prior", "Posterior"))) %>%
  ggplot(aes(x = interaction(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC"))), y = value, fill = factor(dist, levels = c("Observation", "Prior", "Posterior")))) +
  scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep, expand = c(0,0)) +
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.6) +
  geom_errorbar(aes(ymin = value - 1.96 * uncertainty, ymax = value + 1.96 * uncertainty), width = 0.2, position = position_dodge(0.9)) +
  geom_vline(aes(xintercept = 0, colour = "95% Confidence Interval", group = 1), width = 0.2,) +
  scale_fill_manual(values = plot_colours[c(3, 1, 2, 4)], breaks = c("Observation", "Prior", "Posterior")) +
  scale_colour_manual(values = "black", breaks = c("95% Confidence Interval")) +
  facet_wrap(vars(year), ncol = 1, axes = "all_x") +
  ggtitle(paste0("Yields, Observed and Estimated with LAI + Yield Self-Calibration")) +
  labs(y = expression(paste("Yield (t ", ha^{-1}, ")")), x = "Secondary Crop", fill = "Distribution", colour = "") +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom")

ggsave(filename = paste0("Yield_plot_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
       path = supplement_dir,
       units = "mm", width = 190, height = 105)

rm(yield_table)

################# NEE bar plot #################
plot_prior_2020
prior_2021

# Getting the necessary data:
# Splitting some of this data trimming down to reduce memory load
nee_prior_2020 <- plot_prior_2020 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise(nee_sum = sum(nee)) %>%
  summarise("value" = mean(nee_sum),
            "uncertainty" = sd(nee_sum)
  )
nee_prior_2021 <- prior_2021 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise(nee_sum = sum(nee)) %>%
  summarise("value" = mean(nee_sum),
            "uncertainty" = sd(nee_sum)
  )

nee_table <- twinwin_posterior %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  summarise(nee_sum = sum(nee)) %>%
  summarise("value" = mean(nee_sum),
            "uncertainty" = sd(nee_sum)
  ) %>%
  bind_rows(nee_prior_2020) %>%
  bind_rows(nee_prior_2021)

rm(nee_prior_2020, nee_prior_2021)

nee_table %>%
  filter(crop %in% plot_crops & (obs_calib %in% plot_obs_calib | dist == "Prior")) %>%
  {
    if (plot_usm_calib == "Self") {
      filter(., usm_calib == crop | dist == "Prior")
    } else filter(., usm_calib != "Barley" | crop == "Barley" | dist == "Prior")
  } %>%
  {
    if (!("Prior" %in% plot_dists)) {
      filter(., dist != "Prior")
    } else .
  } %>%
  # rename(value = Yield_mean, uncertainty = Yield_se) %>%
  select(-c(usm_calib, obs_calib)) %>%
  # bind_rows(yield_obs) %>%
  arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Observation", "Prior", "Posterior"))) %>%
  ggplot(aes(x = interaction(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC"))), y = value, fill = factor(dist, levels = c("Observation", "Prior", "Posterior")))) +
  # scale_y_continuous(limits = c(0, 4), oob = scales::oob_keep, expand = c(0,0)) +
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.6) +
  geom_errorbar(aes(ymin = value - 1.96 * uncertainty, ymax = value + 1.96 * uncertainty), width = 0.2, position = position_dodge(0.9)) +
  geom_vline(aes(xintercept = 0, colour = "95% Confidence Interval", group = 1), width = 0.2,) +
  scale_fill_manual(values = plot_colours[c(3, 1, 2, 4)], breaks = c("Observation", "Prior", "Posterior")) +
  scale_colour_manual(values = "black", breaks = c("95% Confidence Interval")) +
  facet_wrap(vars(year), ncol = 1, axes = "all_x") +
  ggtitle(paste0("Estimated Net Ecosystem Exchange using LAI + Yield Self-Calibration")) +
  labs(y = expression(paste("NEE (t", CO[2], "", ha^{-1}, ")")), x = "Secondary Crop", fill = "Distribution", colour = "") +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom")

ggsave(filename = paste0("NEE_plot_", paste0(plot_crops, collapse = "_"), "_", plot_obs_calib, "_", plot_ens_size, ".pdf"),
       path = supplement_dir,
       units = "mm", width = 190, height = 105)

rm(nee_table)
################################################################################
# # Create a table combining yield estimates and summed NEE estimates as our two key measures.
# # Calculate the net NEE for each ensemble member and then take the mean of those for the overall mean estimate, and the standard deviation for a measure of uncertainty.
# nee_est_sum <- est_results[ , c("ens", "usm", "year", "date", "crop", "dist", "obs_calib", "usm_calib", "nee")] %>%
#   filter((usm_calib != "Barley" | crop == "Barley" | dist == "Prior")) %>%
#   group_by(usm, crop, dist, obs_calib, usm_calib, ens) %>%
#   summarise("net_NEE_ens" = sum(nee)) %>%
#   group_by(usm, crop, dist, obs_calib, usm_calib) %>%
#   summarise("net_NEE" = mean(net_NEE_ens), "net_NEE_sd" = sd(net_NEE_ens)) %>%
#   arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Observation", "Prior", "Posterior")), factor(obs_calib, levels = c("Prior", "lai", "nee", "yield", "lainee", "laiyield", "neeyield", "laineeyield"))) %>%
#   mutate(year = paste0("20", gsub("[A-Za-z_]", "", usm))) %>%
#   pivot_wider(id_cols = c(crop, dist, obs_calib), names_from = year, values_from = c(net_NEE, net_NEE_sd), names_sep = " ")
#
# nee_est_sum$netNEE2020 <- sprintf("%.3f (%.3f)", nee_est_sum$`net_NEE 2020`, nee_est_sum$`net_NEE_sd 2020`)
# nee_est_sum$netNEE2021 <- sprintf("%.3f (%.3f)", nee_est_sum$`net_NEE 2021`, nee_est_sum$`net_NEE_sd 2021`)
#
# knitr::kable(nee_est_sum[, c(1:3, 8, 9)], "latex", digits = 3) %>%
#   cat(file = paste0(results_path, "ObsVary_net_nee_table_", num_params_varied, "_params_", "2020-2021", "_", paste0(plot_crops, collapse = "_"), "_obs_varying_", plot_ens_size, ".tex"))
# knitr::kable(nee_est_sum[, c(1:3, 8, 9)], "simple", digits = 3) %>%
#   cat(file = paste0(results_path, "ObsVary_net_nee_table_", num_params_varied, "_params_", "2020-2021", "_", paste0(plot_crops, collapse = "_"), "_obs_varying_", plot_ens_size, ".txt"))
# nee_est_sum[, c(1:3, 8, 9)]
#
# yield_nee_table <- merge(yield_table_2020, nee_est_sum[, c(1:3, 8, 9)], all = TRUE) %>%
#   arrange(factor(crop, levels = c("Barley + Herbicide", "Barley", "AA", "AC", "CI", "FA", "IR", "RC", "TG", "WC")), factor(dist, levels = c("Observation", "Prior", "Posterior")), factor(obs_calib, levels = c("lai", "nee", "yield", "lainee", "laiyield", "neeyield", "laineeyield")))
# yield_nee_table %>%
#   stargazer(type = "text", summary = FALSE, rownames = FALSE, out = paste0(results_path, "ObsVary_combined_yield_net_nee_table_", num_params_varied, "_params_", "2020-2021", "_", paste0(plot_crops, collapse = "_"), "_obs_varying_", plot_ens_size, c(".txt", ".tex")))
#
# best_yield_nee_table <- yield_nee_table %>%
#   filter(obs_calib == "laiyield" | dist %in% c("Prior", "Observation"))
# best_yield_nee_table[, c(1, 2, 4:7)] %>%
#   stargazer(type = "text", summary = FALSE, rownames = FALSE, out = paste0(results_path, "ObsVary_best_yield_net_nee_table_", num_params_varied, "_params_", "2020-2021", "_", paste0(plot_crops, collapse = "_"), "_obs_varying_", plot_ens_size, c(".txt", ".tex")))

################# Compare RMSEs of prior and posterior #################
# Getting the necessary data:
# Splitting some of this data trimming down to reduce memory load
rmse_prior_2020 <- plot_prior_2020 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1", "lai_n_plant_1", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  filter(date %in% obs_data$date)

rmse_prior_2021 <- prior_2021 %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1", "lai_n_plant_1", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  filter(date %in% obs_data$date)

rmse_table <- twinwin_posterior %>%
  mutate(year = paste0("20", gsub("[A-Z_a-z]", "", usm))) %>%
  select(c("year", "ens", "date", "crop", "dist", "obs_calib", "usm_calib", "mafruit_plant_1", "lai_n_plant_1", "nee")) %>%
  group_by(year, crop, dist, obs_calib, usm_calib, ens) %>%
  filter(date %in% obs_data$date) %>%
  bind_rows(rmse_prior_2020) %>%
  bind_rows(rmse_prior_2021) %>%
  rename(yield = mafruit_plant_1, GAI = lai_n_plant_1, NEE = nee) %>%
  pivot_longer(c(yield, GAI, NEE), names_to = "obs") %>%
  filter(obs == "NEE" & date %in% obs_data$date[which(obs_data$obs == "NEE")] |
           obs == "GAI" & date %in% obs_data$date[which(obs_data$obs == "GAI")] |
           obs == "yield" & date %in% obs_data$date[which(obs_data$obs == "yield")])

rm(rmse_prior_2020, rmse_prior_2021)

# Add the observations and calculate RMSEs, then take the mean across the ensemble
rmse_plot <- rmse_table %>%
  replace_na(list(obs_calib = "Prior", usm_calib = "Prior")) %>%
  inner_join(obs_data, by = join_by(date, obs, crop), suffix = c("", "_observed")) %>%
  group_by(crop, ens, obs_calib, usm_calib, year, obs) %>%
  summarise(RMSE = RMSE(value, value_observed)) %>%
  group_by(crop, obs_calib, usm_calib, year, obs) %>%
  summarise(mean_RMSE = mean(RMSE)) %>%
  mutate(usm_calib = case_when(
    usm_calib == "Prior" ~ "Prior",
    crop == "Barley" ~ "BarleySelf",
    crop == usm_calib ~ "Self",
    .default = "Barley"
  )) %>%
  group_by(obs_calib, usm_calib, year, obs) %>%
  summarise(RMSE = sum(mean_RMSE)) %>%
  pivot_wider(names_from = usm_calib, values_from = RMSE) %>%
  mutate(Barley = Barley + BarleySelf, Self = Self + BarleySelf) %>%
  group_by(year, obs) %>%
  fill(Prior) %>%
  na.omit() %>%
  select(-c(BarleySelf)) %>%
  pivot_longer(c(Barley, Self), names_to = "Type") %>%
  mutate(prop = (Prior - value)/Prior * 100) %>%
  mutate(obs_calib = str_replace(obs_calib, "lai", "L"), obs_calib = str_replace(obs_calib, "nee", "N"), obs_calib = str_replace(obs_calib, "yield", "Y"),
         obs = str_replace(obs, "GAI", "LAI (L)"), obs = str_replace(obs, "yield", "Yield (Y)"), obs = str_replace(obs, "NEE", "NEE (N)"))

prop_totals <- rmse_plot %>% group_by(obs_calib, Type, year) %>%
  summarise(total = sum(prop))


rmse_plot %>%
  ggplot() +
  geom_bar(aes(x = obs_calib, y = prop, fill = obs), stat = "identity",
           position = "stack", alpha = 0.9) +
  geom_bar(data = prop_totals,
           aes(x = obs_calib, y = total, fill = "Total"), width = .2, alpha = 0.9,
           stat = "identity") +
  geom_hline(yintercept = 0,  col = "black", lty = 1, size = 1) +
  facet_wrap(vars(year, Type), ncol = 2, axes = "all_x") +
  scale_fill_manual(values = viridis(5)[c(1, 4, 3, 2)], breaks = c("Total", "LAI (L)", "NEE (N)", "Yield (Y)")) +
  # scale_colour_manual(values = "black", breaks = c("95% Confidence Interval")) +
  ggtitle(paste0("Percentage improvement in RMSE across calibrations")) +
  labs(y = "Percentage Improvement", x = "Observation Sets Used in Calibration", fill = "", colour = "") +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom")


ggsave(filename = paste0("Prop_RMSE_plot_", paste0(plot_crops, collapse = "_"), "_", plot_ens_size, ".pdf"),
       path = manuscript_dir,
       units = "mm", width = 190, height = 105)

rmse_plot %>%
  ggplot() +
  geom_bar(aes(x = obs_calib, y = prop, fill = obs), stat = "identity",
           position = "stack", alpha = 0.9) +
  geom_bar(data = prop_totals,
           aes(x = obs_calib, y = total, fill = "Total"), width = .2, alpha = 0.9,
           stat = "identity") +
  geom_hline(yintercept = 0,  col = "black", lty = 1, size = 1) +
  facet_wrap(vars(year, Type), ncol = 2, axes = "all_x") +
  scale_fill_manual(values = viridis(5)[c(1, 4, 3, 2)], breaks = c("Total", "LAI (L)", "NEE (N)", "Yield (Y)")) +
  # scale_colour_manual(values = "black", breaks = c("95% Confidence Interval")) +
  ggtitle(paste0("Percentage improvement in RMSE across calibrations")) +
  labs(y = "Percentage Improvement (%)", x = "Observation Sets Used in Calibration", fill = "", colour = "") +
  guides(colour = guide_legend(reverse = FALSE), fill = guide_legend(reverse = FALSE), shape = guide_legend(reverse = FALSE)) +
  theme(text = element_text(size = 8), legend.key.size = unit(5, 'mm'), legend.position = "bottom") +
  coord_flip()


ggsave(filename = paste0("Prop_RMSE_plot_flipped_", paste0(plot_crops, collapse = "_"), "_", plot_ens_size, ".pdf"),
       path = manuscript_dir,
       units = "mm", width = 190, height = 105)

# # Looking at the normalised root mean square errors, to assess overall efficacy at hitting the calibration observations.
# lai_obs <- gai_file[which(gai_file$date > as.Date(paste0("2020", "/05/20")) & gai_file$date < as.Date(paste0("2022", "/05/20")) & gai_file$What == "Barley"), c("date", "crop", "GAI", "GAI_se")]
# nee_obs <- nee_file[which(nee_file$date>= as.Date(paste0("2020", "-05-20")) & nee_file$date <= as.Date(paste0("2022", "-05-20"))), ]
# names(nee_obs) <- c("date", "crop", "value", "sd")
# yield_obs <- yield_file[which(yield_file$year %in% c("2020", "2021")), c("crop", "Yield", "Yield_se")]
# yield_obs$date <- yield_obs_dates[1:2] # This shouldn't work this easily but it does, so I'm not making it foolproof and messing it up.
# names(yield_obs) <- c("crop", "value", "sd", "date")
#
# ens_lai_rmse <- merge(lai_est_ens[which(lai_est_ens$date %in% lai_obs$date), ], lai_obs, by = c("date", "crop")) %>%
#   group_by(crop, ens, obs_calib, usm_calib, year) %>%
#   summarise(LAI_RMSE = RMSE(lai_n_plant_1, GAI)) %>%
#   group_by(crop, obs_calib, usm_calib, year) %>%
#   summarise(ens_LAI_RMSE_mean = mean(LAI_RMSE), ens_LAI_RMSE_sd = sd(LAI_RMSE))
# lai_rmse <- merge(lai_est_mean[which(lai_est_mean$date %in% lai_obs$date), ], lai_obs, by = c("date", "crop")) %>%
#   group_by(crop, obs_calib, usm_calib, year) %>%
#   summarise(LAI_RMSE = RMSE(LAI, GAI)) %>%
#   merge(ens_lai_rmse)
#
# ens_nee_rmse <- merge(nee_est_ens[which(nee_est_ens$date %in% nee_obs$date), ], nee_obs, by = c("date", "crop")) %>%
#   group_by(crop, ens, obs_calib, usm_calib, year) %>%
#   summarise(NEE_RMSE = RMSE(nee, value)) %>%
#   group_by(crop, obs_calib, usm_calib, year) %>%
#   summarise(ens_NEE_RMSE_mean = mean(NEE_RMSE), ens_NEE_RMSE_sd = sd(NEE_RMSE))
# nee_rmse <- merge(nee_est_mean[which(nee_est_mean$date %in% nee_obs$date), ], nee_obs, by = c("date", "crop")) %>%
#   group_by(crop, obs_calib, usm_calib, year) %>%
#   summarise(NEE_RMSE = RMSE(NEE, value)) %>%
#   merge(ens_nee_rmse)
#
# ens_yield_rmse <- merge(yield_est_ens[which(yield_est_ens$date %in% yield_obs$date), ], yield_obs, by = c("date", "crop")) %>%
#   group_by(crop, ens, obs_calib, usm_calib, year) %>%
#   summarise(yield_RMSE = RMSE(mafruit_plant_1, value)) %>%
#   group_by(crop, obs_calib, usm_calib, year) %>%
#   summarise(ens_yield_RMSE_mean = mean(yield_RMSE), ens_yield_RMSE_sd = sd(yield_RMSE))
# yield_rmse <- merge(yield_est_mean[which(yield_est_mean$date %in% yield_obs$date), ], yield_obs, by = c("date", "crop")) %>%
#   group_by(crop, obs_calib, usm_calib, year) %>%
#   summarise(yield_RMSE = RMSE(Yield, value)) %>%
#   merge(ens_yield_rmse)
#
# comb_rmses <- merge(lai_rmse, nee_rmse) %>%
#   merge(yield_rmse) %>%
#   arrange(crop, usm_calib, factor(obs_calib, levels = c("lai", "nee", "yield", "lainee", "laiyield", "neeyield", "laineeyield")))
# comb_rmses$obs_calib[which(is.na(comb_rmses$obs_calib))] <- "Prior"
# comb_rmses$usm_calib[which(is.na(comb_rmses$usm_calib))] <- "Prior"
# comb_rmses$usm_calib[which(!(comb_rmses$usm_calib %in% c("Prior", "Barley")))] <- "Self"
# comb_rmses$total_RMSE <- comb_rmses$LAI_RMSE + comb_rmses$NEE_RMSE + comb_rmses$yield_RMSE
# stargazer(comb_rmses[, c(1:4, 14, 5, 8, 11)], type = "text", summary = FALSE, rownames = FALSE, out = paste0(results_path, "ObsVary_rmse_table_", num_params_varied, "_params_2020_", paste0(plot_crops, collapse = "_"), "_", "obs_varying", "_", plot_ens_size, c(".txt", ".tex")))
# # Looking at combined rmses across crops. Excluding TG as it doesn't work in 2020. Duplicating barley so it contributes both to self and barley specific calibration results equally.
# calc_tmp <- rbind(comb_rmses, mutate(comb_rmses[which(comb_rmses$crop == comb_rmses$usm_calib), ], usm_calib = "Self"))
# comb_rmses_by_calib <- summarise(filter(calc_tmp, !(crop %in% c("TG"))), yield = sum(yield_RMSE), lai = sum(LAI_RMSE), nee = sum(NEE_RMSE), total = sum(total_RMSE), .by = c(usm_calib, obs_calib))
# comb_rmses_by_calib_year <- summarise(filter(calc_tmp, !(crop %in% c("TG"))), yield = sum(yield_RMSE), lai = sum(LAI_RMSE), nee = sum(NEE_RMSE), total = sum(total_RMSE), .by = c(year, usm_calib, obs_calib))
#
# # ELEPHANT: Look at percentage differences in RMSEs of posteriors relative to the prior, potentially gives some proportional comparison to consider.
#
# prop_rmses_by_calib_year <- comb_rmses_by_calib_year %>%
#   mutate(across(c(yield, lai, nee), ~ifelse(obs_calib == "Prior", ., 100 * (last(.) - .)) / last(.)), .by = c(year)) %>%
#   mutate(total = yield + lai + nee, ) %>%
#   arrange(year, factor(usm_calib, levels = c("Self", "Barley")), factor(obs_calib, levels = c("lai", "nee", "yield", "lainee", "laiyield", "neeyield", "laineeyield"))) %>%
#   filter(usm_calib != "Prior") # Removing the prior as no longer needed.
# stargazer(prop_rmses_by_calib_year, type = "text", summary = FALSE, rownames = FALSE, out = paste0(results_path, "ObsVary_prop_rmse_table_", num_params_varied, "_params_", paste0(plot_crops, collapse = "_"), "_", "obs_varying", "_", plot_ens_size, c(".txt", ".tex")))
#
# prop_rmses_by_calib <- prop_rmses_by_calib_year %>%
#   summarise(across(c(yield, lai, nee, total), sum), .by = c(usm_calib, obs_calib)) %>%
#   arrange(factor(usm_calib, levels = c("Self", "Barley")), factor(obs_calib, levels = c("lai", "nee", "yield", "lainee", "laiyield", "neeyield", "laineeyield")))
# stargazer(prop_rmses_by_calib, type = "text", summary = FALSE, rownames = FALSE, out = paste0(results_path, "ObsVary_comb_prop_rmse_table_", num_params_varied, "_params_", paste0(plot_crops, collapse = "_"), "_", "obs_varying", "_", plot_ens_size, c(".txt", ".tex")))
#



rm(prior_2021, obs_vary_analysis, obs_vary_draws, twinwin_posterior)
################################################################################
# twin experiment ensemble size selection: parameter distribution plots (priors especially); analysis vectors of parameters (and spread?); RMSE?
# twin_ens_size_xb <- readRDS(paste0(model_runs_dir, "twin_ens_size_xb.rds"))
# twin_ens_size_analysis <- readRDS(paste0(model_runs_dir, "twin_ens_size_analysis.rds"))
# twin_ens_size_draws <- readRDS(paste0(model_runs_dir, "twin_ens_size_draws.rds"))
#
#
# # What is actually useful for this to show? The set of distributions was the main factor in choice, but the final analysis vectors were useful as well.
#
#
# rm(twin_ens_size_xb, twin_ens_size_analysis, twin_ens_size_draws)









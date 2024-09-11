################################################################################
# Title: Plant Parameter Sensitivity Analysis
# Author: Quentin Bell (quentin.bell@fmi.fi)
# Intention: This script imports all plant parameters for a specified crop variety, removes those that we do not believe to be likely variable or make no sense to vary for our situation, and then estimates the effect on yield when increasing or decreasing these parameters individually. The latest version not only varies parameters around the default value given, but also by drawing uniformly from the range of valid values specified in the JavaSTICS documentation.
################################################################################
# Honestly no idea if this is necessary, but better to set it just in case.
setwd("~/Documents/TWINWIN/RunData")
# Load necessary package, some function calls we have chosen to specify which package they are from specifically, so could probably do that for this too.
library(SticsOnR)
library(SticsRFiles)
library(rlist)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(CroPlotR)
library(data.table)

set.seed(1)

################################################################################
# Set up paths for calling Stics and directing it to the correct folders.
javastics <- "/home/bell/Documents/TWINWIN/Stics_v9.2"
workspace <- "/home/bell/Documents/TWINWIN/Stics_v9.2/TWINWIN_workspace"
output    <- "/home/bell/Documents/TWINWIN/Stics_v9.2/TWINWIN_output"
# Set options for stics_wrapper, where is the data (workspace) and where to find
# the Stics executables (from the base folder of Stics as given by javastics).
sim_options <- stics_wrapper_options(javastics = javastics, workspace = output)
################################################################################
# These horrible vectors of parameter names list all the parameters we are not varying in subcategories according to a reason why.
discuss <- c("abscission", "cgrainv0", "coefamflax", "coefdrpmat", "coefflodrp", "coeflaxsen", "coeflevamf", "coeflevdrp", "coefmshaut", "coefsenlan", "dlaimin", "hautbase", "inngrain1", "INNimin", "masecmeta", "nbjgerlim", "nlevlim1", "nlevlim2", "propjgermin", "sea", "sensanox", "sensiphot", "tcxstop", "temax", "tgelflo10", "tgelflo90", "tigefeuil", "udlaimax", "vigueurbat", "vitprophuile", "vitpropsucre")

exclude_code <- c("codazofruit", "codazorac", "codcalinflo", "codebeso", "codebfroid", "codedormance", "codefixpot", "codegdh", "codegdhdeb", "codgelveg", "codegermin", "codehypo", "codeindetermin", "codeINN", "codeintercept", "codeir", "codelaitr", "codelegume", "codemonocot", "codeperenne", "codephot", "codeplante", "codeplisoleN", "coderacine", "coderetflo", "codestrphot", "codetemp", "codetemprac", "codetransrad", "codetremp", "codevar", "codgelflo", "codgeljuv", "codgellev", "codlainet", "codtrophrac")

exclude_999_all <- c("jvcmini", "julvernal", "tfroid", "ampfroid", "stdordebour", "tdmindeb", "tdmaxdeb", "ifindorm", "q10", "idebdorm", "laiplantule", "masecplantule", "zracplantule", "dlaimaxbrut", "durviesupmax", "innsen", "rapsenturg", "phobasesen", "dltamsmaxsen", "dltamsminsen", "alphaphot", "tauxrecouvkmax", "tauxrecouvmax", "pentrecouv", "infrecouv", "vitircarbT",  "nboite", "allocfrmax", "afpf", "bfpf", "cfpf", "dfpf", "stdrpnou", "spfrmax", "spfrmin", "splaimax", "splaimin", "nbinflo", "inflomax", "pentinflores", "tmaxremp", "tminremp", "zlabour", "zpente", "zprlim", "minefnra", "minazorac", "maxazorac", "repracpermax", "repracpermin", "krepracperm", "repracseumax", "repracseumin", "krepracseu", "mouillabil", "stemflowmax", "kstemflow", "adilmax", "stlevdno", "stdnofno", "stfnofvino", "vitno",  "profnod",  "concNnodseuil", "concNrac0", "concNrac100", "tempnod1", "tempnod2", "tempnod3", "tempnod4", "fixmax", "fixmaxveg", "fixmaxgr", "jvc", "afruitpot", "dureefruit")

exclude_999_intercrop <- c("extin")

# These new_version_intercrop may or may not be just the intercropping ones, labelling them as such initially in any case.
exclude_999_new_version <- c("tdoptdeb", "propresP", "PropresPN", "Efremobil", "Propres", "tauxmortresP", "Parazoper", "ParazoTmorte", "inilai", "SurfApex", "SeuilMorTalle", "SigmaDisTalle", "VitReconsPeupl", "SeuilReconsPeupl", "MaxTalle", "SeuilLAIapex", "tigefeuilcoupe", "coefracoupe", "kdisrac", "alloperirac", "parazorac", "lvmax", "rapdia", "RTD", "propracfmax")

exclude_code_new_version <- c("codephot_part", "code_WangEngel", "code_acti_reserve", "codemontaison", "code_stress_root", "code_rootdeposition", "code_diff_root", "codetranspitalle", "codemortalracine", "codedisrac", "codedyntalle")

exclude_new_version_error <- c("irazomax")

exclude_999_alone <- c("adfol", "dfolbas", "dfolhaut",  "forme", "ktrou", "rapforme", "rsmin")


exclude_other <- c("nbfeuilplant", "nbfgellev", "nbVariete", "stoprac", "stflodrp")

exclude_character <- c("stadebbchamf", "stadebbchdebdes", "stadebbchdrp", "stadebbchfindorm", "stadebbchflo", "stadebbchger", "stadebbchlax", "stadebbchlev", "stadebbchmat", "stadebbchnou", "stadebbchplt", "stadebbchrec", "stadebbchsen")

# Parameters to exclude based on Toni's suggestions
exclude_toni <- c("adil", "bdilmax", "alphaco2", "belong", "celong", "contrdamax", "croirac", "debsenrac", "draclong", "durvieF", "elmax", "envfruit", "h2ofeuiljaune", "h2ofeuilverte", "h2ofrvert", "h2oreserve", "h2otigestruc", "Kmabs1", "Kmabs2", "kmax", "longsperac", "lvfront", "masecNmax", "nbjgrain", "Nmeta", "Nreserve", "parazofmorte", "phobase", "phosat", "phyllotherme", "potgermi", "psisto", "psiturg", "ratiodurvie", "remobres", "sensrsec", "tcmax", "tcmin", "tdebgel", "tdmax", "tdmin", "temin", "tempdeshyd", "teopt", "teoptbis", "tgeljuv10", "tgeljuv90", "tgellev10", "tgellev90", "tgelveg10", "tgelveg90", "tgmin", "tletale", "tustressmin")
################################################################################
param_ranges <- read.csv("/home/bell/Documents/TWINWIN/barley_sole_default.csv", header = TRUE, )
param_ranges <- param_ranges[, c(1, 4, 5)]
colnames(param_ranges) <- c("parameter", "min", "max")
################################################################################
# All the usms we are looping over for sensitivity analysis
all_usms <- c("Barley_AC20", "Barley_FA20", "Barley_IR20", "Barley_RC20", "Barley_WC20", "Barley_AA20", "Barley_CI20", "Barley_sole20", "Barley_AC21", "Barley_FA21", "Barley_IR21", "Barley_RC21", "Barley_WC21", "Barley_AA21", "Barley_CI21", "Barley_sole21")
# all_usms <- barley_usms
# sorted_params <- list()
# all_usms <- c("Barley_AC20", "Barley_FA20", "Barley_IR20", "Barley_RC20", "Barley_WC20", "Barley_AA20", "Barley_CI20", "Barley_sole20")
unif_draw <- runif(10)
# plot(unif_draw, unif_draw)
var_by <- c(0.05, 0.1, 0.15, 0.2, 0.3, 0.4)
sensitivity_df <- NULL
for (name_USM in all_usms) {
  # Extract the plant parameters from the input file. For two crop plots there will be a second plant parameter file "ficplt2.txt" corresponding to the secondary crop.
  plant <- get_plant_txt(file.path(paste0(output, "/", name_USM), "ficplt1.txt"))
  # Taking all parameters that are not in one of those vectors gives us the parameters we wish to keep and vary.

  # Barley alone
  if (str_detect(name_USM, "sole")) {
    params <- as.data.frame(plant[!(names(plant) %in% c(discuss, exclude_999_all, exclude_999_alone, exclude_999_intercrop, exclude_999_new_version, exclude_character, exclude_code, exclude_code_new_version, exclude_other, exclude_toni, exclude_new_version_error))])["cork", ]
  } else {
    params <- as.data.frame(plant[!(names(plant) %in% c(discuss, exclude_999_all, exclude_999_alone, exclude_999_intercrop, exclude_999_new_version, exclude_character, exclude_code, exclude_code_new_version, exclude_other, exclude_toni, exclude_new_version_error))])["cork", ]
  }

  name_params <- c("adens", "stlevamf", "efcroiveg", "stamflax", "stlevdrp", "dlaimax", "stpltger", "vlaimax")
  # Confirmed on 27/03/23 that the parameter values initially are identical for both intercropped and single cropped barley, so for this initial stage and calibration we can use the exact same input.
  # name <- all_usms[1]
  # Extract the plant parameters from the input file. For two crop plots there will be a second plant parameter file "ficplt2.txt" corresponding to the secondary crop.
  plant <- get_plant_txt(file.path(paste0(output, "/", name_USM), "ficplt1.txt"))
  # Extract default values of only our top 8, and choose only one variety (here cork, the other option currently is scarlett).
  # params <- as.data.frame(plant[names(plant) %in% name_params])["cork", ]
  n_dims <- length(params)
  # Reorder the parameters so that the most sensitive is the first and the least is the last in the vector
  # params <- params[order(factor(names(params), levels = name_params))]

  # names(params)[1] <- "alphaCO2" # Apparently this is misnamed if we want to modify it, but fine otherwise. Not needed as we cannot constrain it
  ################################################################################
  # Now to vary the parameters and run Stics to see the effect.
  # First a baseline
  # baseline_est <- stics_wrapper(model_options = sim_options, situation = name_USM,
  #                               var = c("mafruit", "lai_n", "dltams_n", "CO2sol", "masec_n", "MSexporte"))
  # length_exp <- length(which(baseline_est[[2]][[1]]$Plant == "plant_1"))
  # yield_baseline <- baseline_est[[2]][[1]]$mafruit[1 : length_exp]
  # lai_baseline <- baseline_est[[2]][[1]]$lai_n[1 : length_exp]
  # nee_baseline <- 0.48 * (44 / 12) * (baseline_est[[2]][[1]]$dltams_n[1 : length_exp]) - baseline_est[[2]][[1]]$CO2sol[1 : length_exp] / 1000
  # final_yield <- tail(yield_baseline, n = 1)
  # dates <- baseline_est[[2]][[1]]$Date[1 : length_exp]
#
#   sensitivity_df <- rbindlist(baseline_est$sim_list, idcol = "usm")
#   sensitivity_df$varied_by <- 1
#   sensitivity_df$parameter <- NA
#   sensitivity_df$param_value <- NA

  # Now loop over all the parameters, running stics and storing the yield in our list.
  for (i in 1 : length(params)) {
    # Store the parameter being varied in a named tibble, this can be safely multiplied by our scaling factor to vary it, but we cannot do that directly on a sublist of params.
    param <- tibble(params[[i]])
    names(param) <- names(params)[i]
    # This is redundant, but makes the plotting later so much easier and simpler to do
    baseline_param <- stics_wrapper(model_options = sim_options,
                                    param_values = param,
                                    situation = name_USM,
                                    var = c("mafruit", "lai_n", "dltams_n", "CO2sol", "masec_n", "MSexporte"))
    tmp_base <- rbindlist(baseline_param$sim_list, idcol = "usm")
    # tmp_base$varied_by <- 1
    tmp_base$param_value  <- param
    tmp_base$parameter <- names(param)
    sensitivity_df <- rbind(sensitivity_df, tmp_base)

    # Incorporating uniform parameter draws from the parameter ranges
    # param_draw <- unif_draw * (param_ranges[param_ranges$parameter == names(param), 3] - param_ranges[param_ranges$parameter == names(param), 2]) + param_ranges[param_ranges$parameter == names(param), 2]
    # for (val in param_draw) {
    #   names(val) <- names(param)
    #   stics_out <- stics_wrapper(model_options = sim_options,
    #                                     param_values = val,
    #                                     situation = name_USM,
    #                                     var = c("mafruit", "lai_n", "dltams_n", "CO2sol", "masec_n", "MSexporte")
    #   )
    #   tmp <- rbindlist(stics_out$sim_list, idcol = "usm")
    #   tmp$param_value  <- val[[1]]
    #   tmp$parameter <- names(param)
    #   sensitivity_df <- rbind(sensitivity_df, tmp)
    # }

    for (vary_amount in var_by) {
      tmp_pos <- NULL
      tmp_neg <- NULL
      # Checking that the increased parameter value being used is within the valid range
      if (dplyr::between((param * (1 + vary_amount))[[1]], param_ranges[param_ranges$parameter == names(param), 2], param_ranges[param_ranges$parameter == names(param), 3]) ) {
        # First calculate the effect of increasing the parameter by 10% on the yield
        stics_output_pos <- stics_wrapper(model_options = sim_options,
                                          param_values = param * (1 + vary_amount),
                                          situation = name_USM,
                                          var = c("mafruit", "lai_n", "dltams_n", "CO2sol", "masec_n", "MSexporte")
        )

        tmp_pos <- rbindlist(stics_output_pos$sim_list, idcol = "usm")
        # tmp_pos$varied_by <- (1 + vary_amount)
        tmp_pos$param_value  <- param * (1 + vary_amount)
      }
      if (dplyr::between((param * (1 - vary_amount))[[1]], param_ranges[param_ranges$parameter == names(param), 2], param_ranges[param_ranges$parameter == names(param), 3]) ) {
        # Now calculate the decreased parameter value's effect
        stics_output_neg <- stics_wrapper(model_options = sim_options,
                                          param_values = param * (1 - vary_amount),
                                          situation = name_USM,
                                          var = c("mafruit", "lai_n", "dltams_n", "CO2sol", "masec_n", "MSexporte")
        )
        tmp_neg <- rbindlist(stics_output_neg$sim_list, idcol = "usm")
        # tmp_neg$varied_by <- (1 - vary_amount)
        tmp_neg$param_value  <- param * (1 - vary_amount)
        # # This should actually be separate, and not included if the value is outside negative but not positive or vice versa.
        # tmp <- rbind(tmp_pos, tmp_neg)
        # tmp$parameter <- names(param)
        #
        # sensitivity_df <- rbind(sensitivity_df, tmp)
      }
      if (!(is.null(tmp_pos) & is.null(tmp_neg))) {
        tmp <- rbind(tmp_pos, tmp_neg)
        tmp$parameter <- names(param)

        sensitivity_df <- rbind(sensitivity_df, tmp)
      }
    }
  }
}


# Separately doing stflodrp as the zero default value gives no variation.
# param <- tibble(params[[32]])
# names(param) <- names(params)[32]
# for (name_USM in "Barley_WC20") {
#   for (stflodrp_value in c(10, 20, 40, 80, 160)) {
#     param[1] <- stflodrp_value
#     stics_stflodrp <- stics_wrapper(model_options = sim_options,
#                                       param_values = param,
#                                       situation = name_USM,
#                                       var = c("mafruit", "lai_n", "dltams_n", "CO2sol", "masec_n", "MSexporte")
#     )
#     tmp <- rbindlist(stics_stflodrp$sim_list, idcol = "usm")
#     tmp$param_value  <- stflodrp_value
#     tmp$parameter <- names(param)
#
#     sensitivity_df <- rbind(sensitivity_df, tmp)
#   }
# }

sensitivity_wide <- pivot_wider(unique(sensitivity_df), names_from = Plant, values_from = c(mafruit, lai_n, dltams_n, CO2sol, MSexporte, masec_n))

sensitivity_wide$nep <- 0.48 * (44 / 12) * sensitivity_wide$dltams_n_plant_1 - sensitivity_wide$CO2sol_plant_1 / 1000

sensitivity_wide$nep[!is.na(sensitivity_wide$dltams_n_plant_2)] <- sensitivity_wide$nep[!is.na(sensitivity_wide$dltams_n_plant_2)] + 0.48 * (44 / 12) * sensitivity_wide$dltams_n_plant_2[!is.na(sensitivity_wide$dltams_n_plant_2)]

sensitivity_wide$nep[is.na(sensitivity_wide$nep)]

################################################################################
# saveRDS(sensitivity_wide, "plant_parameter_sensitivity_estimates_barley_alone.RDS")
# sensitivity_wide <- readRDS("plant_parameter_sensitivity_estimates_barley_alone.RDS")
################################################################################
barley_default <- read.csv("/home/bell/Documents/TWINWIN/barley_sole_default.csv", header = TRUE, )
barley_default <- barley_default[, c(1, 3)]
names(barley_default) <- c("parameter", "default")
barley_default <- barley_default %>%
  filter(parameter %in% names(params))

theme_set(theme_classic())

for (name_USM in all_usms) {
  lai_mean_plots <- sensitivity_wide %>%
    filter(usm == name_USM) %>%
    summarise(lai_mean = mean(lai_n_plant_1), .by = c(parameter, param_value, usm)) %>%
    ggplot(aes(x = param_value, y = lai_mean)) + # , colour = usm
    geom_point() +
    # stat_smooth (geom = "line") +
    geom_vline(data = barley_default, aes(xintercept = default), linetype = "dashed") +
    facet_wrap_paginate(~ parameter, ncol = 1, nrow = 1, scales = "free") +
    # facet_wrap(vars(parameter), scales = "free")
    ggtitle(paste0("Sensitivity of mean LAI to one-at-a-time parameter variation, ", name_USM), subtitle = "Default barley (Cork) parameter value shown with dashed line.") +
    labs(y = "Mean LAI", x = "Parameter Value", colour = "") +
    # guides(override.aes = list(shape = c(NA, 16), linetype = c("solid", "blank"))) +
    theme(text = element_text(size = 20), legend.key.size = unit(1, 'cm'))

  lai_mean_plots + facet_wrap(vars(parameter), scales = "free")
  ggsave(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/LAI_sensitivity_free_y.png"), units = "mm", width = 400, height = 220)
  lai_mean_plots + facet_wrap(vars(parameter), scales = "free_x")
  ggsave(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/LAI_sensitivity_fixed_y.png"), units = "mm", width = 800, height = 420, limitsize = FALSE)


  pdf(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/plot_per_page_LAI_sensitivity.pdf"))
  print(lai_mean_plots + facet_wrap(vars(parameter), scales = "free"))
  for(i in 1 : n_pages(lai_mean_plots)){
    print(lai_mean_plots +
      facet_wrap_paginate(~ parameter, ncol = 1, nrow = 1, page = i, scales = "free"))
  }
  dev.off()


  yield_plots <- sensitivity_wide %>%
    filter(usm == name_USM) %>%
    summarise(yield = last(mafruit_plant_1), .by = c(parameter, param_value, usm)) %>%
    ggplot(aes(x = param_value, y = yield)) +
    geom_point() +
    stat_smooth (geom = "line", alpha = 0.6) +
    geom_vline(data = barley_default, aes(xintercept = default), linetype = "dashed") +
    facet_wrap_paginate(~ parameter, ncol = 1, nrow = 1, scales = "free") +
    ggtitle(paste0("Sensitivity of final yield to one-at-a-time parameter variation, ", name_USM), subtitle = "Default barley (Cork) parameter value shown with dashed line.") +
    labs(y = "Yield", x = "Parameter Value", colour = "")
    theme(text = element_text(size = 20), legend.key.size = unit(1, 'cm'))
    # facet_wrap(vars(parameter), scales = "free")
  yield_plots + facet_wrap(vars(parameter), scales = "free")
  ggsave(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/yield_sensitivity_free_y.png"), units = "mm", width = 400, height = 220)
  yield_plots + facet_wrap(vars(parameter), scales = "free_x")
  ggsave(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/yield_sensitivity_fixed_y.png"), units = "mm", width = 800, height = 420, limitsize = FALSE)

  pdf(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/plot_per_page_yield_sensitivity.pdf"))
  print(yield_plots + facet_wrap(vars(parameter), scales = "free"))
  for(i in 1 : n_pages(yield_plots)){
    print(yield_plots +
            facet_wrap_paginate(~ parameter, ncol = 1, nrow = 1, page = i, scales = "free"))
  }
  dev.off()

  # agb_plots <- sensitivity_wide %>%
  #   summarise(agb = last(masec_n_plant_1), .by = c(parameter, param_value, usm)) %>%
  #   ggplot(aes(x = param_value, y = agb, colour = usm)) +
  #   geom_point() +
  #   geom_smooth(se = FALSE) +
  #   geom_vline(data = barley_default, aes(xintercept = default), linetype = "dashed") +
  #   facet_wrap_paginate(~ parameter, ncol = 1, nrow = 1, scales = "free")
  #   # facet_wrap(vars(parameter), scales = "free")
  # agb_plots + facet_wrap(vars(parameter), scales = "free")
  # ggsave("/home/bell/Documents/TWINWIN/plots/sensitivity/Barley_WC20/AGB_sensitivity_free_y.png")
  # agb_plots + facet_wrap(vars(parameter), scales = "free_x")
  # ggsave("/home/bell/Documents/TWINWIN/plots/sensitivity/Barley_WC20/AGB_sensitivity_fixed_y.png")
  #
  #
  # pdf("/home/bell/Documents/TWINWIN/plots/sensitivity/Barley_WC20/plot_per_page/AGB_sensitivity.pdf")
  # print(agb_plots + facet_wrap(vars(parameter), scales = "free"))
  # for(i in 1 : n_pages(agb_plots)){
  #   print(agb_plots +
  #           facet_wrap_paginate(~ parameter, ncol = 1, nrow = 1, page = i, scales = "free"))
  # }
  # dev.off()

  nep_mean_plots <- sensitivity_wide %>%
    filter(usm == name_USM) %>%
    summarise(NEP_mean = mean(nep), .by = c(parameter, param_value, usm)) %>%
    ggplot(aes(x = param_value, y = NEP_mean)) +
    geom_point() +
    stat_smooth (geom = "line", alpha = 0.6) +
    geom_vline(data = barley_default, aes(xintercept = default), linetype = "dashed") +
    facet_wrap_paginate(~ parameter, ncol = 1, nrow = 1, scales = "free") +
    ggtitle(paste0("Sensitivity of mean NEP to one-at-a-time parameter variation, ", name_USM), subtitle = "Default barley (Cork) parameter value shown with dashed line.") +
    labs(y = "Mean NEP", x = "Parameter Value", colour = "")
  theme(text = element_text(size = 20), legend.key.size = unit(1, 'cm'))
    # facet_wrap(vars(parameter), scales = "free")
  nep_mean_plots + facet_wrap(vars(parameter), scales = "free")
  ggsave(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/NEP_sensitivity_free_y.png"), units = "mm", width = 400, height = 220)
  nep_mean_plots + facet_wrap(vars(parameter), scales = "free_x")
  ggsave(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/NEP_sensitivity_fixed_y.png"), units = "mm", width = 800, height = 420, limitsize = FALSE)

  pdf(paste0("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/", name_USM, "/plot_per_page_NEP_sensitivity.pdf"))
  print(nep_mean_plots + facet_wrap(vars(parameter), scales = "free"))
  for(i in 1 : n_pages(nep_mean_plots)){
    print(nep_mean_plots +
            facet_wrap_paginate(~ parameter, ncol = 1, nrow = 1, page = i, scales = "free"))
  }
  dev.off()
}
################################################################################
# Look at taking the default value of the observation as a baseline to then normalise the differences and allow plotting of the three variables on one plot.
sensitivity_wide %>%
  summarise(lai_mean = mean(lai_n_plant_1), NEP_mean = mean(nep), yield = last(mafruit_plant_1), .by = c(parameter, param_value, usm))


###############################################################################
sensitivity_deviations <- sensitivity_wide %>%
  mutate(lai_baseline = first(lai_n_plant_1), yield_baseline = first(mafruit_plant_1),agb_baseline = first(masec_n_plant_1), nep_baseline = first(nep), .by = c(parameter, usm, Date))

sensitivity_nrmse <- sensitivity_deviations %>%
  summarise(lai_nrmse = nRMSE(lai_n_plant_1, lai_baseline), nep_nrmse = nRMSE(nep, nep_baseline), yield_nrmse = nRMSE(mafruit_plant_1, yield_baseline), agb_nrmse = nRMSE(masec_n_plant_1, agb_baseline), net_nrmse = lai_nrmse + nep_nrmse + yield_nrmse + agb_nrmse, .by = c(parameter, param_value, usm))

sensitivity_mean_nrmse <- sensitivity_nrmse %>%
  summarise(lai_mean = mean(lai_nrmse), nep_mean = mean(nep_nrmse), yield_mean = mean(yield_nrmse), agb_mean = mean(agb_nrmse), net_mean_total = lai_mean + nep_mean + yield_mean + agb_mean, .by = c(parameter, usm))

overall_sensitivity <- sensitivity_mean_nrmse %>%
  summarise(mean = mean(net_mean_total), .by = parameter)
sens_2020 <- sensitivity_mean_nrmse %>%
  filter(str_detect(usm, "20")) %>%
  summarise(mean = mean(net_mean_total), .by = parameter)
sens_2021 <-sensitivity_mean_nrmse %>%
  filter(str_detect(usm, "21")) %>%
  summarise(mean = mean(net_mean_total), .by = parameter)

sensitivity_mean_nrmse %>%
  ggplot() +
  geom_point(aes(x = net_mean_total, y = parameter, colour = usm)) +
  scale_y_discrete(limits = rev) +
  geom_point(data = filter(overall_sensitivity, parameter != "dlaimax"), aes(x = mean, y = parameter), colour = "black", shape = 4, size = 3) +
  ggtitle("Sum of Mean nRMSEs", subtitle = "x is the mean of all USMs")

ggsave("/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/Overall_nRMSEs.png")

#
# sensitivity_mean_nrmse %>%
#   filter(parameter != "dlaimax") %>% # , str_detect(usm, "sole")
#   ggplot() +
#   geom_point(aes(x = net_mean_total, y = parameter, colour = usm)) +
#   scale_y_discrete(limits = rev) +
#   ggtitle("Sum of Mean nRMSEs, Excluding dlaimax", subtitle = "x is the mean of all USMs") +
#   geom_point(data = filter(overall_sensitivity, parameter != "dlaimax"), aes(x = mean, y = parameter), colour = "black", shape = 4, size = 3)
#
# ggsave("/home/bell/Documents/TWINWIN/plots/sensitivity/sum_mean_nRMSEs_no_dlaimax.png")

print(arrange(overall_sensitivity, desc(mean)), n = 39)

write.table(arrange(overall_sensitivity, desc(mean)), file = "/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/Overall_nRMSEs.csv", row.names = FALSE, quote = FALSE, sep = ",")

relative_sens <- merge(sens_2020, sens_2021, by = "parameter", suffixes = c(".2020", ".2021"))

write.table(arrange(relative_sens, desc(mean.2020)), file = "/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/Overall_nRMSEs_sorted_2020.csv", row.names = FALSE, quote = FALSE, sep = ",")
write.table(arrange(relative_sens, desc(mean.2021)), file = "/home/bell/Documents/TWINWIN/plots/sensitivity/2020Sensitivity/Overall_nRMSEs_sorted_2021.csv", row.names = FALSE, quote = FALSE, sep = ",")

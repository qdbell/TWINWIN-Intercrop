# Testing Interactive Shiny plotting
setwd("~/Documents/TWINWIN-Intercrop")
library(shiny)
library(SticsOnR)
library(SticsRFiles)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
theme_set(theme_classic())
################################################################################
# Stics Options Set Up
################################################################################
# Path to the folder containing Stics and workspace/example folders.
javastics <- "/home/bell/Documents/TWINWIN-Intercrop/Stics_v9.2"
# Path to the workspace containing our weather data and crop initialisations.
workspace <- "/home/bell/Documents/TWINWIN-Intercrop/Stics_v9.2/TWINWIN_workspace"
# Path to store simulation results in, while the previous directories must
# exist, this may be created automatically?
output    <- "/home/bell/Documents/TWINWIN-Intercrop/Stics_v9.2/TWINWIN_output"
# Set options for stics_wrapper, where is the data (workspace) and where to find
# the Stics executables (from the base folder of Stics as given by javastics).
# CORRECTION: SticsOnR states that this should be the workspace folder, but it
# is actually the out_dir folder from SticsRFiles::gen_usms_xml2txt(), running
# stics_wrapper with the workspace folder leads to no Stics directories being
# found as they were created in another directory.
sim_options <- stics_wrapper_options(javastics = javastics, workspace = output, verbose = FALSE, time_display = FALSE)

# usm <- "Barley_sole20"
# name_params <- c("stpltger", "adens", "vlaimax", "efcroiveg", "stlevamf", "dlaimax", "stamflax", "stlevdrp")

# baseline <- stics_wrapper(model_options = sim_options,
#                           situation = usm,
#                           var = c("mafruit", "lai_n",
#                                   "dltams_n", "CO2sol", "MSexporte", "masec_n"))
################################################################################
# These correspond roughly with the actual dates and frequencies of observation for our real data, and can be used to look at how the parameter variations affect the estimated variables at the time of observation.
nep_obs_dates <- as.POSIXct(c("2020-06-03", "2020-06-15", "2020-06-29", "2020-07-03", "2020-07-14", "2020-07-27", "2020-07-31", "2020-08-11", "2020-08-27", "2021-06-28", "2021-07-22", "2021-08-30", "2022-06-28", "2022-07-22", "2022-08-30"))
lai_obs_dates <- as.POSIXct(c("2020-06-04", "2020-06-18", "2020-07-01", "2020-07-16", "2020-07-27", "2020-08-28", "2021-07-01", "2021-07-26", "2021-08-23", "2022-07-01", "2022-07-26", "2022-08-23"))
################################################################################
# Shiny ui
ui <- fluidPage(
  # verticalLayout(
    sidebarLayout(
      # titlePanel("Varying parameters manually"),
      sidebarPanel(
        sliderInput("adens", "Parameter Value of adens (default is -0.6)", 1.4 * -0.6, 0.6 * -0.6, value = -0.6, step = 0.01),
        sliderInput("dlaimax", "Parameter Value of dlaimax (default is 0.00044)", 0.6 * 0.00044, 1.4 * 0.00044, value = 0.00044, step = 0.00001),
        sliderInput("efcroiveg", "Parameter Value of efcroiveg (default is 4.7)", 0.6 * 4.7, 1.4 * 4.7, value = 4.7, step = 0.01),
        # sliderInput("stamflax", "Parameter Value of stamflax (default is 240)", 0.6 * 240, 1.4 * 240, value = 240, step = 1),
        # sliderInput("stlevamf", "Parameter Value of stlevamf (default is 380)", 0.6 * 380, 1.4 * 380, value = 380, step = 1),
        sliderInput("stlevdrp", "Parameter Value of stlevdrp (default is 650)", 0.6 * 650, 1.4 * 650, value = 650, step = 1),
        # sliderInput("stpltger", "Parameter Value of stpltger (default 50)", 0.6 * 50, 1.4 * 50, value = 50, step = 0.5),
        sliderInput("vlaimax", "Parameter Value of vlaimax (default is 1.8)", 1.5, 2.5, value = 1.8, step = 0.01),
        sliderInput("INNmin", "Parameter Value of INNmin (default is 0.36)", 0.6 * 0.36, 1.4 * 0.36, value = 0.36, step = 0.01),
        # sliderInput("pentlaimax", "Parameter Value of pentlaimax (default is 5.5)", 0.6 * 5.5, 1.4 * 5.5, value = 5.5, step = 0.1),
        # sliderInput("Vmax2", "Parameter Value of Vmax2 (default is 0.05)", 0.6 * 0.05, 1.4 * 0.05, value = 0.05, step = 0.001),
        sliderInput("vitircarb", "Parameter Value of vitircarb (default is 0.0107)", 0.6 * 0.0107, 1.4 * 0.0107, value = 0.0107, step = 0.0001),
        # sliderInput("stressdev", "Parameter Value of stressdev (default is 0.22)", 0.6 * 0.22, 1.4 * 0.22, value = 0.22, step = 0.01),
        sliderInput("efcroirepro", "Parameter Value of efcroirepro (default is 4.7)", 0.6 * 4.7, 1.4 * 4.7, value = 4.7, step = 0.1),
        checkboxGroupInput("which_obs", label = h4("When were observations taken"),
                           choices = list("LAI" = "lai", "NEP" = "nep")),
        selectInput("crop2", label = h4("Select field set up"),
                    choices = list("Sole Barley" = "sole",
                                   "Barley with alfalfa" = "AA", "Barley with alsike clover" = "AC",
                                   "Barley with chicory" = "CI", "Barley with tall fescue" = "FA",
                                   "Barley with italian ryegrass" = "IR", "Barley with red clover" = "RC",
                                   "Barley with timothy grass" = "TG", "Barley with white clover" = "WC")),
        radioButtons("year", label = h4("Simulation Year"),
                     choices = list("2020" = 20, "2021" = 21))
      ),
      mainPanel(
        plotOutput("plot1"),
        textOutput("meanlai"),
        plotOutput("plot2"),
        textOutput("meannep"),
        textOutput("yield")
      )
    )
    # plotOutput("plot1"),
    # textOutput("meanlai"),
    # plotOutput("plot2"),
    # textOutput("meannep"),
    # textOutput("yield"),
    # wellPanel(
    #   sliderInput("adens", "Parameter Value of adens (default is -0.6)", 1.4 * -0.6, 0.6 * -0.6, value = -0.6, step = 0.01),
    #   sliderInput("dlaimax", "Parameter Value of dlaimax (default is 0.00044)", 0.6 * 0.00044, 1.4 * 0.00044, value = 0.00044, step = 0.00001),
    #   sliderInput("efcroiveg", "Parameter Value of efcroiveg (default is 4.7)", 0.6 * 4.7, 1.4 * 4.7, value = 4.7, step = 0.01),
    #   sliderInput("stamflax", "Parameter Value of stamflax (default is 240)", 0.6 * 240, 1.4 * 240, value = 240, step = 1),
    #   sliderInput("stlevamf", "Parameter Value of stlevamf (default is 380)", 0.6 * 380, 1.4 * 380, value = 380, step = 1),
    #   sliderInput("stlevdrp", "Parameter Value of stlevdrp (default is 650)", 0.6 * 650, 1.4 * 650, value = 650, step = 1),
    #   sliderInput("stpltger", "Parameter Value of stpltger (default 50)", 0.6 * 50, 1.4 * 50, value = 50, step = 0.5),
    #   sliderInput("vlaimax", "Parameter Value of vlaimax (default is 1.8)", 0.6 * 1.8, 1.4 * 1.8, value = 1.8, step = 0.01)
    # )
    # wellPanel(
    #   fluidRow(
    #     column(6,
    #            sliderInput("adens", "Parameter Value of adens (default is -0.6)", 1.4 * -0.6, 0.6 * -0.6, value = -0.6, step = 0.01),
    #            sliderInput("dlaimax", "Parameter Value of dlaimax (default is 0.00044)", 0.6 * 0.00044, 1.4 * 0.00044, value = 0.00044, step = 0.00001),
    #            sliderInput("efcroiveg", "Parameter Value of efcroiveg (default is 4.7)", 0.6 * 4.7, 1.4 * 4.7, value = 4.7, step = 0.01),
    #            sliderInput("stamflax", "Parameter Value of stamflax (default is 240)", 0.6 * 240, 1.4 * 240, value = 240, step = 1)
    #     ),
    #     column(6,
    #            sliderInput("stlevamf", "Parameter Value of stlevamf (default is 380)", 0.6 * 380, 1.4 * 380, value = 380, step = 1),
    #            sliderInput("stlevdrp", "Parameter Value of stlevdrp (default is 650)", 0.6 * 650, 1.4 * 650, value = 650, step = 1),
    #            sliderInput("stpltger", "Parameter Value of stpltger (default 50)", 0.6 * 50, 1.4 * 50, value = 50, step = 0.5),
    #            sliderInput("vlaimax", "Parameter Value of vlaimax (default is 1.8)", 0.6 * 1.8, 1.4 * 1.8, value = 1.8, step = 0.01)
    #     )
    #   )
    # )
  # )
)

################################################################################
# Shiny server
server <- function(input, output) {

  usm <- reactive({
    paste0("Barley_", input$crop2, input$year, "_long")
  })

  baseline <- reactive({
    stics_wrapper(model_options = sim_options,
                  situation = usm(),
                  var = c("mafruit", "lai_n", "dltams_n",
                          "CO2sol", "MSexporte", "masec_n")
                  ) %>%
      pluck("sim_list", 1) %>%
      pivot_wider(names_from = Plant, values_from = c("mafruit", "lai_n", "dltams_n",
                                                      "CO2sol", "MSexporte", "masec_n")) %>%
      mutate(nep = {if(str_detect(usm(), "sole")) {0.48 * (44 / 12) * dltams_n_plant_1 - CO2sol_plant_1 / 1000} else {0.48 * (44 / 12) * (dltams_n_plant_1 + dltams_n_plant_2) - CO2sol_plant_1 / 1000}})
  })

  stics_output <- reactive({
    stics_wrapper(model_options = sim_options,
                  param_values = c("adens" = input$adens,
                                   "dlaimax" = input$dlaimax,
                                   "efcroiveg" = input$efcroiveg,
                                   # "stamflax" = input$stamflax,
                                   # "stlevamf" = input$stlevamf,
                                   "stlevdrp" = input$stlevdrp,
                                   # "stpltger" = input$stpltger,
                                   "vlaimax" = input$vlaimax,
                                   "INNmin" = input$INNmin,
                                   # "pentlaimax" = input$pentlaimax,
                                   # "Vmax2" = input$Vmax2,
                                   "vitircarb" = input$vitircarb,
                                   # "stressdev" = input$stressdev,
                                   "efcroirepro" = input$efcroirepro
                                   ),
                  situation = usm(),
                  var = c("mafruit", "lai_n", "dltams_n",
                          "CO2sol", "MSexporte", "masec_n")
                  ) %>%
      pluck("sim_list", 1) %>%
      pivot_wider(names_from = Plant, values_from = c("mafruit", "lai_n", "dltams_n",
                                                      "CO2sol", "MSexporte", "masec_n")) %>%
      mutate(nep = {if(str_detect(usm(), "sole")) {0.48 * (44 / 12) * dltams_n_plant_1 - CO2sol_plant_1 / 1000} else {0.48 * (44 / 12) * (dltams_n_plant_1 + dltams_n_plant_2) - CO2sol_plant_1 / 1000}})

  })



  output$plot1 <- renderPlot({
    stics_output() %>%
      ggplot() +
        scale_y_continuous(limits = c(0, 3), oob = scales::oob_keep) +
        geom_line(aes(Date, lai_n_plant_1, colour = "Sliders")) +
        ggtitle(paste0("Estimated LAI ", usm())) +
        labs(y = "LAI (m^2 m^-2)", x = "Date", colour = "Parameter Set") +
        theme(text = element_text(size = 20), legend.key.size = unit(1, 'cm')) +
        geom_line(data = baseline(), aes(Date, lai_n_plant_1, colour = "Baseline")) +
        {if("lai" %in% input$which_obs) geom_vline(xintercept = lai_obs_dates, linetype = "dashed", colour = "black")}
  })
  output$plot2 <- renderPlot({
    ggplot(stics_output()) +
      scale_y_continuous(limits = c(-0.125, 0.5), oob = scales::oob_keep) +
      geom_line(aes(Date, nep, colour = "Sliders")) +
      ggtitle(paste0("Estimated NEP ", usm())) +
      labs(y = "NEP (t ha^-1 d^-1)", x = "Date", colour = "Parameter Set") +
      theme(text = element_text(size = 20), legend.key.size = unit(1, 'cm')) +
      geom_line(data = baseline(), aes(Date, nep, colour = "Baseline")) +
      geom_hline(aes(yintercept = 0), linetype = "dashed", colour = "grey") +
      {if("nep" %in% input$which_obs) geom_vline(xintercept = nep_obs_dates, linetype = "dashed", colour = "black")}
  })
  output$yield <- renderText({
    paste0("Yield is ", round(tail(stics_output()$mafruit_plant_1, 1), 3), " tonnes per hectare. Baseline yield is ", round(tail(baseline()$mafruit_plant_1, 1), 3), " tonnes per hectare. Difference: ", round(tail(stics_output()$mafruit_plant_1, 1) - tail(baseline()$mafruit_plant_1, 1), 3))
  })
  output$meanlai <- renderText({
    paste0("Mean LAI is ", round(mean(stics_output()$lai_n_plant_1), 3), ". Baseline mean LAI is ", round(mean(baseline()$lai_n_plant_1), 3), ". Difference: ", round(mean(stics_output()$lai_n_plant_1) - mean(baseline()$lai_n_plant_1), 3))
  })
  output$meannep <- renderText({
    paste0("Mean NEP is ", round(mean(stics_output()$nep), 4), ". Baseline mean NEP is ", round(mean(baseline()$nep), 4), ". Difference: ", round(mean(stics_output()$nep) - mean(baseline()$nep), 4))
  })
}
################################################################################
# Run app
shinyApp(ui = ui, server = server)




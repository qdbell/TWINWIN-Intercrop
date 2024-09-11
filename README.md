# TWINWIN-Intercrop

To replicate the results of the manuscript TITLE it is sufficient to run 01.DataImport.R, having downloaded the data provided at METIS-LINK, then 04-ModelRuns.R, with Stics_v9.2 installed and 4DEnVar_engine downloaded, and finally 05-ManuscriptAnalysis.R. 

Note that all libraries required are loaded at the top of each script, and within scripts paths will need to be changed by the user, and sub-directories may need to be created, and more changes may be necessary for those on non-linux systems. Of particular note, this version of STICS requires the use of absolute paths.

02-PlantParameterSensitivity.R and 03-InteractiveParameterSensitivity.R are included as part of the exploration procedure followed in selecting the parameters we calibrated or attempted to calibrate. They are not necessary for reproducing the results presented in TITLE.
# TWINWIN-Intercrop

To replicate the results of Bell et al. *Calibrating primary crop parameters to capture undersown species impacts* it is sufficient to run 01-DataImport.R, then 04-ModelRuns.R, and finally 05-ManuscriptAnalysis.R, provided the necessary data and other software has been downloaded from the following sources:
- Data: INSERT METIS LINK
- [4DEnVar](https://github.com/qdbell/4DEnVar_engine) (version used for this research, more up to date versions may be found at https://github.com/tquaife/4DEnVar_engine)
- [STICS v9.2](https://stics.inrae.fr/eng/download) (Specifically, JavaSTICS-1.41-stics-9.2.zip)

Note that all libraries required are loaded at the top of each script, and within scripts paths will need to be changed by the user, and sub-directories may need to be created, and more changes may be necessary for those on non-linux systems. Of particular note, this version of STICS requires the use of absolute paths.

02-PlantParameterSensitivity.R and 03-InteractiveParameterSensitivity.R are included as part of the exploration procedure followed in selecting the parameters we calibrated. They are not necessary for reproducing the results presented in Bell et al. *Calibrating primary crop parameters to capture undersown species impacts*.

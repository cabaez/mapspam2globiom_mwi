# Initialization ---------------------------------------------------------------

# NOTE doParallel does not seem to work, modified code to run normally on the server.
# Also add gams path and setwd to scratch path.
# Added additional code to save the full ssp results in GDX.

#library(doParallel)
#registerDoParallel(cores = 16)

# Create folders
griddir <- file.path(getwd(), "grid")

folderlist <- c("grid", "GDX", "Logfiles", "Restart",
                "../Figures/Sans-serif", "../Figures/Times",
                "../Article/Tables")
for (folderext in folderlist) {
  folder <- file.path(getwd(), folderext)
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
}

# Strings for GAMS calls
GAMScalls <- c("gams MAIDADSEstimation.gms  lo=2 s=Restart/MAIDADS                     gdx=GDX/MAIDADS_Estimation  --ZeroBeta=0 lf=Logfiles/MAIDADSEstimation.log",
               "gams MAIDADSEstimation.gms  lo=2 s=Restart/MAIDADS0                    gdx=GDX/MAIDADS_Estimation0 --ZeroBeta=1 lf=Logfiles/MAIDADSEstimation0.log",
               "gams MAIDADSProjections.gms lo=2                    r=Restart/MAIDADS0 gdx=GDX/Projections                      lf=Logfiles/MAIDADSProjections.log",
               "gams MAIDADSSimulation.gms  lo=2                                                                                lf=Logfiles/MAIDADSSimulation.log",
               "gams BOOTSTRAPLRTest.gms    lo=2                    r=Restart/MAIDADS0 gdx=GDX/LR                               lf=Logfiles/BOOTSTRAPLRTest.log")

# Main GAMS calls --------------------------------------------------------------
# Estimations
#foreach (i = 1:2) %dopar% system(GAMScalls[i])
for (i in 1:2) {system(GAMScalls[i])}
# Simulations, projections and LR test
system(GAMScalls[5], wait = FALSE)
foreach (i = 3:4) %dopar% system(GAMScalls[i])

# Run nase MAIDADSProjections for each SSP with output to gdx
for (ssp_id in 1:5) {
  system(paste0("gams MAIDADSProjections.gms lo=0 r=Restart/MAIDADS0 --SSP=", ssp_id,
                  " gdx=GDX/Projections_ssp", ssp_id))}

# 10-fold cross-validation -----------------------------------------------------
foreach (i = 1:10) %dopar%
  system(paste0("gams MAIDADSEstimation.gms lo=2 gdx=GDX/MAIDADS_EstimationCV",
                i, " --ZeroBeta=1 --gTest=", i,
                " lf=Logfiles/MAIDADSEstimationCV", i, ".log"))

# Bootstrap estimations --------------------------------------------------------
unlink(file.path(griddir, "grid"), recursive = TRUE)
dir.create(file.path(griddir, "grid"))
system(paste0("C:/GAMS/win64/24.9/gams BOOTSTRAPCI.gms lo=2 r=Restart/MAIDADS  s=Restart/BOOTSTRAPCI  lf=Logfiles/BOOTSTRAPCI.log  gdir=",
              normalizePath(file.path(griddir, "grid"))))
unlink(file.path(griddir, "grid0"), recursive = TRUE)
dir.create(file.path(griddir, "grid0"))
system(paste0("C:/GAMS/win64/24.9/gams BOOTSTRAPCI.gms lo=2 r=Restart/MAIDADS0 s=Restart/BOOTSTRAPCI0 lf=Logfiles/BOOTSTRAPCI0.log gdir=",
              normalizePath(file.path(griddir, "grid0"))))

# Wait 30 minutes for the bootstrap estimations to finish
Sys.sleep(30*60)

# Collect bootstrap estimations
system(paste0("C:/GAMS/win64/24.9/gams CollectBootstrapCI.gms lo=3 r=Restart/BOOTSTRAPCI  gdx=GDX/BootstrapedMAIDADS  gdir=",
              normalizePath(file.path(griddir, "grid"))))
system(paste0("C:/GAMS/win64/24.9/gams CollectBootstrapCI.gms lo=3 r=Restart/BOOTSTRAPCI0 gdx=GDX/BootstrapedMAIDADS0 gdir=",
              normalizePath(file.path(griddir, "grid0"))))

# Bootstrap simulations and projections ----------------------------------------
file.remove(paste0("GDX/",
                   list.files(path = "GDX",
                              pattern = glob2rx("MAIDADS_Simulation*.gdx"))))
for (iterBS in 0:2000) {
  system(paste0("C:/GAMS/win64/24.9/gams MAIDADSSimulation.gms lo=0 --BS=", iterBS))}
for (iSSP in c("", paste0("--SSP=", 1:5))) {
  for (iterBS in 0:2000)
      system(paste0("C:/GAMS/win64/24.9/gams MAIDADSProjections.gms lo=0 r=Restart/MAIDADS0 ",
                    iSSP, " --BS=", iterBS))}

# Call R Programs to process results -------------------------------------------
source("DescriptiveStat.R")
source("BootstrapMAIDADS.R")
source("Elasticities.R")
source("Figures.R")
source("Projections.R")

#Sarah is doing more Git testing
#Bridget is testing push/pull functions

#Set working directory
myWD<-"C:/R_Projects/lakePowellCH4" #set to wherever your scripts and files are saved

# Load libraries and functions
source("C:/R_Projects/lakePowellCH4/scriptsAndRmd/masterLibrary.R")

# Read raw data
source("C:/R_Projects/lakePowellCH4/scriptsAndRmd/readLgrLP.R") # Reads in raw LGR data
#source("ohio2016/scriptsAndRmd/readChem.R") # Merges with eqAreaData
#source("ohio2016/scriptsAndRmd/readChl.R") # Merges with eqAreaData

# Calculate derived quantities:

# Merge chamber time with excel spreadsheet, 10min, 
source("C:/R_Projects/lakePowellCH4/scriptsAndRmd/plotCleanLgrLP10min.R") # keeps nominal chamber deployment dt as 10-min
source("C:/R_Projects/lakePowellCH4/scriptsAndRmd/calculateEmissionsLP10.R") # Merges with eqAreaData for 10-min chamber dt

#then make plots with "plotsLP.R"
#can replicate the back of the envelope ("BOE") whole lake emission estimate using 
#the "wholeLakeBOEcalc.R" script

#3-min scripts now defunct
#source("C:/R_Projects/lakePowellCH4/scriptsAndRmd/plotCleanLgrLP3min.R")
#source("C:/R_Projects/lakePowellCH4/scriptsAndRmd/calculateEmissionsLP3.R") # Merges with eqAreaData, 6min, for the three 3-min sub-deployment version of the data




# # grts calculations
# source("ohio2016/scriptsAndRmd/grtsWgtAdj.R") # Merges with eqAreaData, 2s
# source("ohio2016/scriptsAndRmd/grtsMeanVariance.R") # 20s
# 
# # Data analysis
# source("ohio2016/scriptsAndRmd/descRes.R")
# source("ohio2016/scriptsAndRmd/exploratoryPlots.R")

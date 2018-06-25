
# Load libraries and functions
source("C:/R_Projects/lakePowell/scriptsAndRmd/masterLibrary.R")

# Read raw data
source("C:/R_Projects/lakePowell/scriptsAndRmd/readLgrLP.R") # Reads in raw LGR data
#source("ohio2016/scriptsAndRmd/readChem.R") # Merges with eqAreaData
#source("ohio2016/scriptsAndRmd/readChl.R") # Merges with eqAreaData

# Calculate derived quantities
source("C:/R_Projects/lakePowell/scriptsAndRmd/plotCleanLgrLP.R") # Merges chamber time with excel spreadsheet, 10min
source("C:/R_Projects/lakePowell/scriptsAndRmd/calculateEmissionsLP.R") # Merges with eqAreaData, 6min

#then make plots with "plotsLP.R"


# grts calculations
source("ohio2016/scriptsAndRmd/grtsWgtAdj.R") # Merges with eqAreaData, 2s
source("ohio2016/scriptsAndRmd/grtsMeanVariance.R") # 20s

# Data analysis
source("ohio2016/scriptsAndRmd/descRes.R")
source("ohio2016/scriptsAndRmd/exploratoryPlots.R")

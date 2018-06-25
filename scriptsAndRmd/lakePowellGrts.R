
# UNSTRATIFIED, EQUAL PROBABILITY GRTS DESIGN
    ## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
          ## MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED
            ## NOMINAL MAINSITES = 15
            ## NOMINAL OVER SAMPLE = 20
          ## CHANGE THE ZOOM FACTOR ON LINE 179
          ## FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME

# LAKE Powell Mock GRTS DESIGN
# SAMPLE LAKE FOR WEEK 1 OF STUDY, MAY 30 - JUNE 3
# HUGE LAKE (520 km^2) WITH three main tributaries and a lot of side canyons. 
# However, since executing sampling at GRTS-determined sites is not feasible, I'm simply 
# running this code for formatting/data flow/curiosity purposes.
# WILL USE AN UNSTRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/lakePowell2017/"

# READ/PLOT SHAPEFILE ---------
lakePowellEqArea <- readOGR(dsn = paste(rootDir, "lakePowell", sep=""), # Could use read.shape from spsurvey package
                           layer = "lakePowellEqArea")  # shapefile name
#lakePowellEqArea<-read.shape(filename="lakePowellEqArea")


plot(lakePowellEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------  
attLakePowell <- read.dbf(filename = paste(rootDir, "lakePowell/LakePowellEqArea", sep=""))



# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list. We want 48 rows for data at Lake Powell: 45 chamber deployments
#                         + 3 independent sites for funnel deployments 
lakePowellDsgn <- list(None=list(panel=c(mainSites=30), # unstratified, therefore 1 list
                            seltype="Equal",
                            over = 18)) # Equal probability, have been using 20 for oversample sites


# Execute survey design  
lakePowellSitesEqArea <- grts(design=lakePowellDsgn,
                   DesignID="U", # U for unstratified
                   type.frame="area",
                   src.frame="shapefile",
                   in.shape=paste(rootDir, "lakePowell/lakePowellEqArea", sep=""),
                   att.frame=attLakePowell,
                   shapefile=TRUE,
                   prjfilename = paste(rootDir, "lakePowell/lakePowellEqArea", sep=""),
                   out.shape=paste(rootDir, "lakePowell/lakePowellSitesEqArea", sep=""))

# new feature added 14 June 2016
# Set up for mutating the actonSitesEqArea file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
lakePowellSitesEqArea <- readOGR(dsn = paste(rootDir, "lakePowell", sep=""), # Could use read.shape from spsurvey package
                                 layer = "lakePowellSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
lakePowellSitesEqArea@data <- mutate(lakePowellSitesEqArea@data, 
                                deplyDt = "",    # adding all of these colums to the 
                                deplyTm = "",    # shape file to be filled in the field
                                chmStTm = "",  # tried to enter them in the order they will be filled
                                chm_vol = "",
                                bbblngO = "",                                
                                wtrDpth = "", 
                                smDpthS = "",
                                Tmp_C_S = "",
                                DOPrc_S = "",
                                DO__L_S = "",
                                SpCn__S = "",
                                pH_S = "",
                                ORP_S = "",
                                TrNTU_S = "",
                                chla_S = "",
                                smDpthD = "",
                                Tmp_C_D = "",
                                DOPrc_D = "",
                                DO__L_D = "",
                                SpCn__D = "",
                                pH_D = "",
                                ORP_D = "",
                                TrNTU_D = "",
                                chla_D = "",
                                ArExtnrs = "",
                                DG_Extn = "",
                                H2O_vol = "",
                                HeVol = "",
                                BrPrssr = "",
                                RtrvDat = "",
                                RtrvTim = "",
                                TtTrpVl = "",
                                TrapExtn = "",
                                Notes = "",
                                LatSamp = "",
                                LongSmp = ""
)


# re-write this mutated file, will keep the equal area projection
writeOGR(obj = lakePowellSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "lakePowell", sep=""), 
         layer = "lakePowellSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# Print the initial six lines of the survey design ------------
head(lakePowellSitesEqArea@data)


# Print the survey design summary
summary(lakePowellSitesEqArea)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
lakePowellEqArea84 <- spTransform(x = lakePowellEqArea, 
                             CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = lakePowellEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "lakePowell", sep=""),  
         layer = "lakePowellEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
lakePowellEqArea84@data$id = rownames(lakePowellEqArea84@data)
lakePowellEqArea84.f <- fortify(lakePowellEqArea84, region="id")  # fortify polygon for ggmap/ggplot
lakePowellEqArea84.f <- merge(lakePowellEqArea84.f, lakePowellEqArea84@data, 
                         by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
lakePowellSitesPlot <- readOGR(dsn = paste(rootDir, "lakePowell", sep=""),  
                          layer = "lakePowellSitesEqArea")  # shapefile created with grts function
lakePowellSites84 <- spTransform(x = lakePowellSitesPlot, #reproject
                            CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
lakePowellSites84@data <- mutate(lakePowellSites84@data, 
                            long=coordinates(lakePowellSites84)[,1], # add long to @data slot
                            lat=coordinates(lakePowellSites84)[,2]) # add lat to @data slot
                            
# write out table of overdraw sites for reference in field
write.table(filter(lakePowellSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "lakePowell/lakePowellOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")
            

# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = lakePowellSites84, 
         dsn = paste(rootDir, "lakePowell", sep=""),  
         layer = "lakePowellSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
lakePowellSites84ListByPanel <- split(lakePowellSites84, # split preserves class, outputs a list
                                 f= lakePowellSites84@data$panel)
# to look at the list (optional):
lakePowellSites84ListByPanel[[1]]@data        #should be the main sites
lakePowellSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = lakePowellSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "lakePowell", sep=""),  
         layer = "lakePowellSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = lakePowellSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "lakePowell", sep=""), 
         layer = "lakePowellSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(lakePowellEqArea)
points(lakePowellSitesEqArea$xcoord, lakePowellSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=lakePowellSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.4) # f is zoom.  Large #, less zoom. tweak for each lake.  
lakePowellSat <- get_map(location = bbox,
                    color = "color",
                    source = "google",
                    maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(lakePowellSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=lakePowellEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(lakePowellSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(lakePowellSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Lake Powell")

ggsave(filename=paste(rootDir, "lakePowellmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(lakePowellSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=lakePowellEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=lakePowellEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(lakePowellSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(lakePowellSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Lake Powell")

ggsave(filename=paste(rootDir, "lakePowellOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(lakePowellSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=lakePowellEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=lakePowellSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=lakePowellSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Lake Powell")

ggsave(filename=paste(rootDir, "lakePowellAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")




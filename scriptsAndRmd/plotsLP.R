
#Investigation of uncertainty from:
  # 1. Regression models
  # 2. Within-site variability
  # 3. Between-site variability
#stacked dot chart of CH4 diffusive fluxes like the lake comparison chart, but for sites

lakePowellData10$dSite<-(orderSite(lakePowellData10, "ch4.d"))

ggplot(lakePowellData10,
       aes(ch4.drate.mg.h*24, dSite)) +
  geom_point(alpha=0.3) +
  xlab(expression(CH[4]~diffusive~emission~rate~(mg~ CH[4]~ m^{-2}~ d^{-1}))) +
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  scale_x_log10()+
  geom_errorbarh(aes(x=(ch4.drate.mg.h*24),xmin=(ch4.drate.L*24), xmax=(ch4.drate.U*24)), 
                 size=0.5)+
  theme_bw()+
  geom_point(data=lakePowellData10, aes(ch4.trate.mg.h*24, site, color="green"))

#mean diffusive emissions by site:
 lakePowellBySite<-lakePowellData10 %>%
    dplyr::group_by(site) %>%
    dplyr::summarize(ch4.drate.sd = sd(ch4.drate.mg.h, na.rm=TRUE),
                     ch4.drate.mg.h = (mean(ch4.drate.mg.h, na.rm=TRUE)),
                     ch4.drate.n = n(),
                      ch4.U = ch4.drate.mg.h + ch4.drate.sd/sqrt(ch4.drate.n),
                      ch4.L = ch4.drate.mg.h - ch4.drate.sd/sqrt(ch4.drate.n)) 
#for CO2: 
 lakePowellData10$dSite<-(orderSite(lakePowellData10, "co2.d"))
 
 ggplot(lakePowellData10,
        aes(co2.drate.mg.h*24, dSite)) +
   geom_point(alpha=0.5, aes(color="red")) +
   xlab(expression(CO[2]~diffusive~emission~rate~(mg~ CO[2]~ m^{-2}~ d^{-1}))) +
   theme(axis.title.y = element_blank(), # Eliminate x-axis title
         plot.title = element_text(hjust = 0.5)) +  #plot title justify center
   #scale_x_log10()+
   geom_errorbarh(aes(x=(co2.drate.mg.h*24),xmin=(co2.drate.L*24), xmax=(co2.drate.U*24), color="red"), 
                  size=0.5)+
   theme_bw()+
   theme(legend.position = "none")
   geom_point(data=lakePowellData10, aes(ch4.trate.mg.h*24, site, color="green"))
 
 #mean diffusive emissions by site:
 lakePowellBySite<-lakePowellData10 %>%
   dplyr::group_by(site) %>%
   dplyr::summarize(ch4.drate.sd = sd(ch4.drate.mg.h, na.rm=TRUE),
                    ch4.drate.mg.h = (mean(ch4.drate.mg.h, na.rm=TRUE)),
                    ch4.drate.n = n(),
                    ch4.U = ch4.drate.mg.h + ch4.drate.sd/sqrt(ch4.drate.n),
                    ch4.L = ch4.drate.mg.h - ch4.drate.sd/sqrt(ch4.drate.n),
                    co2.drate.sd = sd(co2.drate.mg.h, na.rm=TRUE),
                    co2.drate.mg.h = (mean(co2.drate.mg.h, na.rm=TRUE)),
                    co2.drate.n = n(),
                    co2.U = co2.drate.mg.h + co2.drate.sd/sqrt(co2.drate.n),
                    co2.L = co2.drate.mg.h - co2.drate.sd/sqrt(co2.drate.n)) 

lakePowellBySite<-data.frame(lakePowellBySite)   
lakePowellBySite$dSite<-orderSite(lakePowellBySite, "ch4.d")
lakePowellBySite$dSiteCo2<-orderSite(lakePowellBySite, "co2.d")

ggplot(lakePowellBySite,
       aes(ch4.drate.mg.h*24, dSite)) +
  geom_point(alpha=0.3) +
  xlab(expression(CH[4]~diffusive~emission~rate~(mg~ CH[4]~ m^{-2}~ d^{-1}))) +
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  scale_x_log10()+
  geom_errorbarh(aes(x=(ch4.drate.mg.h*24),xmin=(ch4.L*24), xmax=(ch4.U*24)), 
                 size=0.5)+
  theme_bw()
#CO2
ggplot(lakePowellBySite,
       aes(co2.drate.mg.h*24, dSiteCo2)) +
  geom_point(alpha=0.5, aes(color="red")) +
  xlab(expression(CO[2]~diffusive~emission~rate~(mg~ CO[2]~ m^{-2}~ d^{-1}))) +
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  #scale_x_log10()+
  geom_errorbarh(aes(color="red", x=(co2.drate.mg.h*24),xmin=(co2.L*24), xmax=(co2.U*24)), 
                 size=0.5)+
  theme_bw()+
  theme(legend.position = "none")

#ebullitive emissions
lakePowellData10$eSite<-orderSite(lakePowellData10, "ch4.e")
eb.plot<-ggplot(filter(lakePowellEb.m, variable == "CH4.funnel.flux" | variable == "ch4.erate"),
                aes(value*24, site)) +
  geom_point(alpha=0.5, aes(shape = variable), size=2) +
  #geom_point(lakePowellData10, aes(CH4.funnel.flux*24, eSite))+
  xlab(expression(CH[4]~ebullitive~emission~rate~(mg~ CH[4]~ m^{-2}~ d^{-1}))) +
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  scale_x_log10()+
  theme_bw()

eb.plot+geom_point(data=lakePowellFunnel, aes(CH4.funnel.flux, Site)) 
####################################################----
##### MAPS #########################################
####################################################
# Read and project spatial points dataframe for plotting
powellSitesPlot <- readOGR(dsn = "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/lakePowell2017", 
                          layer = "lakePowellEqAreaData")  # shapefile with data

powellSites84 <- spTransform(x = powellSitesPlot, #reproject
                            CRS("+proj=longlat +datum=WGS84")) # projection for google maps

powellSites84@data <- mutate(powellSites84@data, 
                            long=coordinates(powellSites84)[,1], # add long to @data slot
                            lat=coordinates(powellSites84)[,2]) # add lat to @data slot

# Get ggmap
bbox <- make_bbox(data=powellSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.4) # f is zoom.  Large #, less zoom. tweak for each lake.  
powellSat <- get_map(location = bbox,
                    color = "color",
                    source = "google",
                    maptype = "satellite")

ggmap(powellSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_point(data=(powellSites84@data), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#FF00FF") + # specify color to be consistent across maps
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  geom_text(data=powellSites84@data, #only main sites
            aes(label=site, x=long, y=lat),
            hjust=1.2, vjust=0, size=3.5, color = "#330066", fontface = "bold") +      #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Lake Powell Sample Sites")

ggsave('C:/R_Projects/lakePowell/siteMap.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=7,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

#high emitting diffusive sites
ggmap(powellSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_point(data=powellSites84@data,
    # data=filter(powellSites84@data, site=="Hite Canyon" | site == "Alcove" | site== "SJR Inlet"
    #                      | site =="Escalante Inflow" | site =="Garces Island" | site=="Sheep Canyon"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#FF00FF") + # specify color to be consistent across maps
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  geom_text(data=filter(powellSites84@data, site=="Hite Canyon" | site == "Alcove" | site== "SJR Inlet"
                        | site =="Escalante Inflow" | site =="Garces Island" | site=="Sheep Canyon"), #only main sites
             #only sites with diffusive fluxes > 24 /d
            aes(label=site, x=long, y=lat),
            hjust=1.2, vjust=0, size=3.5, color = "#330066", fontface = "bold") +      #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Lake Powell Sample Sites With Diffusive Emissions >24 mg CH4 m-2 d-1")

#sites where ebullition was observed
ggmap(powellSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_point(data=(filter(powellSites84@data, site=="Camp2" | site == "Alcove" | site== "SJR Inlet"
                          | site =="Colorado Inlet" | site =="Garces Island" | site == "Escalante Inflow"
                          | site == "wahweap" | site == "camp1" | site == "Camp 3" | site == "Camp 4")), #ebullition sites
             aes(x=long, y=lat),
             size = 2, color = "#FF00FF") + # specify color to be consistent across maps
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  geom_text(data=(filter(powellSites84@data, site=="Camp2" | site == "Alcove" | site== "SJR Inlet"
                         | site =="Colorado Inlet" | site =="Garces Island" | site == "Escalante Inflow"
                         | site == "wahweap" | site == "camp1" | site == "Camp 3" | site == "Camp 4")), #ebullition sites
            aes(label=site, x=long, y=lat),
            hjust=1.2, vjust=0, size=3.5, color = "#330066", fontface = "bold") +      #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Lake Powell Sample Sites where Ebullition was Observed/Measured")

##Map showing which sites had CO2 diffusive uptake vs. emission
powellSites84@data <- merge(powellSites84@data, # add emission to point shp
                            select(lakePowellBySite, ch4.drate.mg.h,co2.drate.mg.h, site))

powellSites84@data$CO2.sign<-ifelse(powellSites84@data$co2.drate.mg.h<0,
                                    "uptake",
                                    "emission")

co2.signP<-ggmap(powellSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  #scale_color_manual(values = c("#00CC66", "#CC33FF", "#999999"))+
  geom_point(data=(powellSites84@data), #only main sites
             aes(x=long, y=lat, color=CO2.sign),
             size = 2) + # specify color to be consistent across maps
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  geom_text(data=powellSites84@data, #only main sites
            aes(label=site, x=long, y=lat, color=CO2.sign),
            hjust=1.2, vjust=0, size=3.5, fontface = "bold") +      #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Lake Powell Sample Sites")

co2.signP
co2.signP+scale_fill_manual(values = c("#FF00FF", "#330066", "#999999"))






######Ch4 Diffusive Emission Rate

powellEqArea84 <- readOGR(dsn = "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/lakePowell2017/lakePowell", # get polygon
                       layer = "lakePowellEqArea84",  # shapefile name
                       verbose = FALSE)  #keep from reporting progress

#powellSitesPlot@data <- merge(powellSitesPlot@data, # add emission to point shp
#                             select(lakePowellBySite, ch4DiffMean, site))
powellSites84@data <- merge(powellSites84@data, # add emission to point shp
                              select(lakePowellBySite, ch4.drate.mg.h,co2.drate.mg.h, site))

# load external script (this is the scatterplot3d function, but changed a bit with to add more user control over style)
source(paste("C:/R_Projects/mulitResSurvey", "/ohio2016/scriptsAndRmd/scatterplot3d_edit.R", sep = ""))

# grab coordinates from polygon (in order to plot as a line on the xy plane of 3D plot)
coords = fortify(powellEqArea84)
coords <- subset(coords, !hole)



##########################################################################
### Total CH4 emission rate
with(powellSites84@data, {
  ch4_plot <- scatterplot3d_edit(x=coords$long, y=coords$lat, z=rep(0,length(coords$long)),   # x y and z data, respectively
                                                           color="#282830",                               # color of lake outline
                                                           fill = "light blue",                                 # polygon fill color
                                                           lwd=1.0,                                       # line width of lake outline
                                                           type="l",                                      # plot type - "l" = line, "h" = the lollipop style with a line  connecting to the horizontal plane 
                                                           angle=40,                                      # angle aspect of plot (-40)
                                                           scale.y=0.5,                                   # scale y axis (increase by 175% (1.75))
                                                           xlab="Latitude",                                       # x-axis label (left blank here to reposition later)
                                                           ylab="",                                       # y-axis label
                                                           cex.axis = 0.7, 
                                                           cex.lab = 1.1,
                                                           mar = c(5,6,4,2)+0.1,
                                                           zlab=expression(ln(1+CH[4]~emission~rate)~(mg~ CH[4]~ m^{-2}~ hr^{-1})),                      # z-axis label
                                                           zlim=range(0,2),                            # range of z data shown - I grabbed the max from previously plotting the methane data here. Set so when I add the points, the z-axis fits that data. 
                                                           labs.line.z=-0.5,                              # adjustment of the z axis tick labels in margin lines
                                                           n.breaks.x=6,                                  # Number of breaks for the axis. If x.pretty = TRUE, this is just a suggestion. It may do more or less. If left blank, defaults to par("lab")[1] value
                                                           x.pretty=FALSE,                                # Whether or not to let the function create pretty breaks (default to TRUE if unspecified)
                                                           x.sig=4,                                       # When x.pretty=FALSE, can use this to set the sig. digits displayed for x axis tick labels
                                                           
                                                           lty.axis=1,                                    # the line type of the axes
                                                           axis=TRUE,                                     # wheather to include axes
                                                           tick.marks = TRUE,
                                                           label.tick.marks = TRUE,   #whether or not to include tick mark labels for all three axes
                                                           x.ticklabs=NULL,
                                                           y.ticklabs=NULL,
                                                           
                                                           grid=c('xy','xz','yz'),                        # which grids to show - added to edited version of scatterplot3d fx
                                                           lty.grid.xy = 1,                               # grid line type  
                                                           lty.grid.xz = 3,
                                                           lty.grid.yz = 3,
                                                           lwd.grid.xy = 1,                               # line width of grid 
                                                           lwd.grid.xz = 1,
                                                           lwd.grid.yz = 1,
                                                           box=FALSE                                      # get rid of the box around the plot
                                                           
)

# reposition & add y-axis label
dims <- par("usr")                        # format of 'dims' is vector of: [xmin,xmax,ymin,ymax]
x <- dims[2] - 0.05*diff(dims[1:2])       # define x position of label
y <- dims[3] + 0.1*diff(dims[3:4])       # define y position of label
text(x,y,"Longitude", srt=55)             # add label.  srt sets angle

# add dam label
dims <- par("usr")                        # format of 'dims' is vector of: [xmin,xmax,ymin,ymax]
x.d <- dims[1] + 0.17*diff(dims[1:2])       # define x position of label
y.d <- dims[3] + 0.1*diff(dims[3:4])        # define y position of label
text(x.d, y.d, "Dam")             # add label.  srt sets angle

# add inflow label
#dims <- par("usr")                        # format of 'dims' is vector of: [xmin,xmax,ymin,ymax]
#x.i <- dims[1] + 0.17*diff(dims[1:2])       # define x position of label
#y.i <- dims[3] + 0.25*diff(dims[3:4])        # define y position of label
#text(x.i, y.i, "Inflow")                       # add label. 


# add the lollipop points
ch4_plot$points3d(x=long, y=lat, z=as.numeric(as.character(log1p(ch4DiffMean))), # x y and z data
                  col="#282830",                      # color of lines connecting points to lake outline
                  lwd=1.5,                            # line width of lines connecting points to lake outline
                  pch=21,                             # type of point to plot
                  bg="#FF0000",                       # fill color of points
                  type="h",                           # plot type = lines to the horizontal plane
                  cex=1 #(vRateBest/500) + 0.5          # scaling of point sizes - this will need to be adjusted for each variable
)
})
  


#stacked dot chart of CH4 diffusive fluxes like the lake comparison chart, but for sites
ggplot(lakePowellData3,
       aes(ch4.drate.mg.h, site)) +
  geom_point(alpha=0.3) +
  xlab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5))   #plot title justify center
#same as above, but with negative values filtered and with a log scale
#stacked dot chart of CH4 diffusive fluxes like the lake comparison chart, but for sites
ggplot(filter(lakePowellData3, ch4.drate.mg.h>0),
       aes(ch4.drate.mg.h*24, site)) +
  #geom_point(alpha=0.3) +
  xlab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ d^{-1}))) +
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5)) +  #plot title justify center
  scale_x_log10()+
  geom_point(data=lakePowellData10, 
                aes(ch4.drate.mg.h*24, site, color="10"), alpha=0.6)
  
ggsave('C:/R_Projects/lakePowell/ch4diffusion.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=7,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

#histogram of flux magnitude occurances
ggplot(lakePowellData, aes(ch4.drate.mg.h*24))+
  geom_histogram(binwidth=5)+

ggsave('C:/R_Projects/lakePowell/ch4diffusionHist.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=7,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

#stacked dot chart of CO2 diffusive fluxes
ggplot(lakePowellData,
       aes(co2.drate.mg.h, site)) +
  geom_point(alpha=0.3) +
  xlab(expression(CO[2]~emission~rate~(mg~ CO[2]~ m^{-2}~ hr^{-1}))) +
  theme(axis.title.y = element_blank(), # Eliminate x-axis title
        plot.title = element_text(hjust = 0.5))   #plot title justify center

ggsave('C:/R_Projects/lakePowell/co2diffusion.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=7,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

#histogram of CO2 fluxes
ggplot(lakePowellData, aes(co2.drate.mg.h))+
  geom_histogram(binwidth=10)
ggsave('C:/R_Projects/lakePowell/co2diffusionHist.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=7,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

write.table(lakePowellData, 
            file="C:/R_Projects/lakePowell/powellDiffusiveFluxes.csv",
            sep=",",
            row.names=FALSE)

#facet plot of CH4 diffusive emissions by site and by 3-min timeperiod within 10-min deployment
#for the 20 low-emitting sites (<5 mg CH4 m-2 d-1)
ggplot(filter(lakePowellData3, site != "Alcove",
              site != "Hite Canyon",
              #site != "Camp 3",
              #site != "Camp2",
              site != "Colorado Inlet", 
              site != "Sheep Canyon",
              site != "Escalante Inflow",
              site != "SJR Inlet",
              site != "Garces Island"),
  aes(timeperiod, ch4.drate.mg.h))+
  geom_point(alpha=0.5, aes(color=rep))+
  facet_wrap(~site)
 
# ylab("CH4 Diffusive Flux (mg CH4 m-2 d-1)")

ggplot(filter(lakePowellData, site == c("Alcove","Hite Canyon","Colorado Inlet", 
              "Sheep Canyon", "Escalante Inflow", "SJR Inlet", "Garces Island")),
ggplot(lakePowellData, aes(timeperiod, ch4.drate.mg.h))+
  geom_point(alpha=0.5, aes(color=rep))+
  facet_wrap(~site)

ggplot(filter(lakePowellData, site == c("SJR Inlet")),
       aes(timeperiod, ch4.drate.mg.h))+
  geom_point(alpha=0.5, aes(color=rep))
  facet_wrap(~site)


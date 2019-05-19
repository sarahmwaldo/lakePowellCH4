#####################
## this script estimates littoral tributary areas at different lake elevations
## and then relates these estimates to estimates of total C flux and energy generation
## started 5/19/2019
#######################

bathy<-read.csv(paste(myWD, "input/lp-predam-topo-multi-values-surf-area-vol-1128-to-1058m.csv", sep="/")) 
head(bathy)

bathy_by_elev<-bathy %>%
  group_by(elevation) %>%
  summarise(x2d_area=sum(X2d_area))

elevations_m<-c(1128,1113,1098,1083,1109)
#max capacities taken from spreadsheet that Lucas sent me
maxcap_MWhperAF<-c(0.526276,0.481421,0.436567,0.391712,0.469521)
#calculate max power generation based on 8.5 million acre feet
MWh_y<-maxcap_MWhperAF*8500000
#surface areas in acres are taken from Powe_ele-area_live.dat excel file from Heather Patno
totalSA_acres<-c(161354,125504,96652,72997,117145)
totalSA_km2<-totalSA_acres*0.00404686

a<-bathy_by_elev[5,2]-bathy_by_elev[4,2]
b<-bathy_by_elev[4,2]-bathy_by_elev[3,2]
c<-bathy_by_elev[3,2]-bathy_by_elev[2,2]
d<-bathy_by_elev[2,2]-bathy_by_elev[1,2]
#from older spreadsheet that Tom Gushue sent 
e<-33162391

trib_lit_SA_m2<-c(42000761,33753536,27924903,18509832,33162391)
trib_lit_SA_km2<-trib_lit_SA_m2/1000000

upscaling<-cbind(trib_lit_SA_km2,totalSA_km2,MWh_y,elevations_m)
upscaling<-data.frame(upscaling)
upscaling$mainSA<-upscaling$totalSA_km2-upscaling$trib_lit_SA_km2
upscaling$CH4kgyr<-upscaling$trib_lit_SA_km2*255.2*365+upscaling$mainSA*2.1*365
upscaling$CO2kgyr<-upscaling$totalSA_km2*234*365
upscaling$CO2ekgyr<-upscaling$CH4kgyr*34+upscaling$CO2kgyr
upscaling$kgCO2eMWh<-upscaling$CO2ekgyr/upscaling$MWh_y
upscaling$percent_littoral<-upscaling$trib_lit_SA_km2/upscaling$totalSA_km2*100


upscaling_plot<-upscaling %>%
  ggplot(aes(x=elevations_m,y=kgCO2eMWh))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  ylab(expression("Maximum Capacity kg CO"[2]*"-eq MWh"^"-1"*""))+
  xlab(expression("Lake Powell Elevation (m)"))
upscaling_plot

ggsave("output/upscaling_sceenarios_plot.tiff",upscaling_plot,height=3.25,width=3.25,units='in',dpi=600,compression="lzw")


## need to add a point for the actual July data points





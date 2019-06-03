#####################
## this script estimates littoral tributary areas at different lake elevations
## and then relates these estimates to estimates of total C flux and energy generation
## started 5/19/2019
#######################

library(dplyr)
library(ggplot2)

#bathy<-read.csv(paste(myWD, "input/lp-predam-topo-multi-values-surf-area-vol-1128-to-1058m.csv", sep="/")) 
bathy<-read.csv(paste(myWD, "input/lp-predam-topo-multi-values-surf-area-vol-5m-increments.csv", sep="/"))
head(bathy)

bathy_by_elev<-bathy %>%
  filter(!is.na(X2d_area))%>%
  group_by(elevation) %>%
  summarise(x2d_area=sum(X2d_area))

#elevations_m<-c(1128,1113,1098,1083,1109)
elevations_m<-c(1125,1120,1115,1110,1105,1100,1095,1090,1085,1080,1075,1070,1065,1109)
elevations_f<-elevations_m*3.28084
#max capacities taken from spreadsheet that Lucas sent me
#maxcap_MWhperAF<-c(0.526276,0.481421,0.436567,0.391712,0.469521)
maxcap_MWhperAF<-c(0.517946,0.502933,0.487954,0.472908,0.457895,0.442883,0.42787,0.412871,0.397845,0.382839,0.36782,0.352807,0.337785,0.469521)
#calculate max power generation based on 8.5 million acre feet
MWh_y<-maxcap_MWhperAF*8500000
#surface areas in acres are taken from Powe_ele-area_live.dat excel file from Heather Patno
#totalSA_acres<-c(161354,125504,96652,72997,117145)
totalSA_acres<-c(153897,141591,129679,119146,109142,100078,91742,83778,75849,68654,62143,56108,50583,117145)
totalSA_km2<-totalSA_acres*0.00404686

a<-bathy_by_elev[14,2]-bathy_by_elev[13,2]
b<-bathy_by_elev[13,2]-bathy_by_elev[12,2]
c<-bathy_by_elev[12,2]-bathy_by_elev[11,2]
d<-bathy_by_elev[11,2]-bathy_by_elev[10,2]
e<-bathy_by_elev[10,2]-bathy_by_elev[9,2]
f<-bathy_by_elev[9,2]-bathy_by_elev[8,2]
g<-bathy_by_elev[8,2]-bathy_by_elev[7,2]
h<-bathy_by_elev[7,2]-bathy_by_elev[6,2]
i<-bathy_by_elev[6,2]-bathy_by_elev[5,2]
j<-bathy_by_elev[5,2]-bathy_by_elev[4,2]
k<-bathy_by_elev[4,2]-bathy_by_elev[3,2]
l<-bathy_by_elev[3,2]-bathy_by_elev[2,2]
m<-bathy_by_elev[2,2]-bathy_by_elev[1,2]
#from older spreadsheet that Tom Gushue sent 
n<-33162391

trib_lit_SA_m2<-c(15560363,14053523,12268707,11952939,12003428,11987123,10929861,8803166,7632404,7480452,6394541,5807385,5372036)
trib_lit_SA_km2<-trib_lit_SA_m2/1000000

upscaling<-cbind(trib_lit_SA_km2,totalSA_km2,MWh_y,elevations_m)
upscaling<-data.frame(upscaling)
upscaling$mainSA<-upscaling$totalSA_km2-upscaling$trib_lit_SA_km2
upscaling$CH4kgyr<-upscaling$trib_lit_SA_km2*255.2*365+upscaling$mainSA*2.1*365
upscaling$CO2kgyr<-upscaling$totalSA_km2*234*365
upscaling$CO2ekgyr<-upscaling$CH4kgyr*34+upscaling$CO2kgyr
upscaling$kgCO2eMWh<-upscaling$CO2ekgyr/upscaling$MWh_y
upscaling$percent_littoral<-upscaling$trib_lit_SA_km2/upscaling$totalSA_km2*100
upscaling$type<-c("Theoretical Conditions","Theoretical Conditions","Theoretical Conditions","Theoretical Conditions",
                  "Theoretical Conditions","Theoretical Conditions","Theoretical Conditions","Theoretical Conditions",
                  "Theoretical Conditions","Theoretical Conditions","Theoretical Conditions","Theoretical Conditions",
                  "Theoretical Conditions","July 2017")

upscaling_plot<-upscaling %>%
  ggplot(aes(x=elevations_m,y=kgCO2eMWh))+
  geom_point(aes(color=type))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        legend.title=element_blank(),legend.position=c(0.35,0.8))+
  ylab(expression(atop("Energy Production Emission Factor",paste("kg CO"[2]*"-eq MWh"^"-1"*""))))+
  xlab(expression("Lake Powell Elevation (masl)"))
upscaling_plot

ggsave("output/upscaling_sceenarios_plot.tiff",upscaling_plot,height=3.25,width=3.25,units='in',dpi=600,compression="lzw")
ggsave("output/upscaling_scenarios_plot.jpeg",upscaling_plot,height=3.25,width=3.25,units='in',dpi=600)

## need to add a point for the actual July data points





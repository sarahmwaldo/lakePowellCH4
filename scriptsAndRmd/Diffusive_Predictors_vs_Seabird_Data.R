###########################################################
########## Lake Powell GHG Data from July 2017 ############
########## August 2018 Analysis                   ############
###########################################################


#call seabird data exported from access database and sort it-- I think Nick has yet to process the inflow stations?
seabird<-read.csv(paste(myWD, "input/tblProfiles_July2017_Query.csv", sep="/"))

seabirdsurface<-seabird %>%
  filter(Depth==1)

seabird

chlaseabird<-seabird %>%
  group_by(Date,Station.ID) %>%
  summarise(Chl=max(Chl))

#call the diffusive flux data that Sarah put toether
diffusive<-read.csv(paste(myWD, "input/powellDiffusiveFluxes.csv", sep="/")) 
head(diffusive)

#join diffusive fluxes with surface seabird data
surface_seabird_diffusive<-right_join(seabirdsurface,diffusive,by="Station.ID")
head(surface_seabird_diffusive)

#join diffusive fluxes with chlorophyll maximum data
chlamax_seabird_diffusive<-right_join(chlaseabird,diffusive,by="Station.ID")
head(chlamax_seabird_diffusive)

## look at potential predictors of GHG flux

co2ph<-surface_seabird_diffusive %>%
  mutate(co2=co2.drate.mg.h*24)%>%
  ggplot(aes(x=pH,y=co2))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Diffusive CO2 Emission mg m-2 d-1")))+
  xlab(expression(paste("pH")))
co2ph

co2Chl<-surface_seabird_diffusive %>%
  mutate(co2=co2.drate.mg.h*24)%>%
  ggplot(aes(x=Chl,y=co2))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Diffusive CO2 Emission mg m-2 d-1")))+
  xlab(expression(paste("Chlorophyll a")))
co2Chl

ch4dist<-surface_seabird_diffusive %>%
  mutate(ch4=ch4.drate.mg.h*24)%>%
  ggplot(aes(x=Distance,y=ch4))+
  facet_wrap(~River_arm)+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Diffusive CH4 Emission mg m-2 d-1")))+
  xlab(expression(paste("Distance Upstream from Dam")))
ch4dist

ch4Chl<-surface_seabird_diffusive %>%
  mutate(ch4=as.numeric(ch4.drate.mg.h)*24)%>%
  ggplot(aes(x=Chl,y=ch4))+
  geom_point()+
  geom_text(aes(label=Station.ID,hjust=0.2,vjust=0.1))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Log Diffusive CH4 Emission mg m-2 d-1")))+
  xlab(expression(paste("Log Chlorophyll a")))+
  scale_y_log10()+
  scale_x_log10()
ch4Chl

a<-lm(as.numeric(surface_seabird_diffusive$ch4.drate.mg.h)~surface_seabird_diffusive$Chl)
summary(a)

ch4Chlmax<-chlamax_seabird_diffusive %>%
  mutate(ch4=as.numeric(ch4.drate.mg.h)*24)%>%
  ggplot(aes(x=Chl,y=ch4))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Log Diffusive CH4 Emission mg m-2 d-1")))+
  xlab(expression(paste("Log Chlorophyll a")))+
  scale_y_log10()+
  scale_x_log10()
ch4Chlmax

b<-lm(as.numeric(chlamax_seabird_diffusive$ch4.drate.mg.h)~chlamax_seabird_diffusive$Chl)
summary(b)

ch4temp<-surface_seabird_diffusive %>%
  mutate(ch4=as.numeric(ch4.drate.mg.h)*24)%>%
  ggplot(aes(x=T,y=ch4))+
  geom_point()+
  geom_text(aes(label=Station.ID,hjust=0.2,vjust=0.1))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Log Diffusive CH4 Emission mg m-2 d-1")))+
  xlab(expression(paste("Log Surface Water Temperature")))+
  scale_y_log10()+
  scale_x_log10()
ch4temp

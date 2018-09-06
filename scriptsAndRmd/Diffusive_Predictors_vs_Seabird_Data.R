###########################################################
########## Lake Powell GHG Data from July 2017 ############
########## August 2018 Analysis                   ############
###########################################################


#call seabird data exported from access database and sort it-- I think Nick has yet to process the inflow stations?
seabird<-read.csv(paste(myWD, "input/tblProfiles_July2017_Query.csv", sep="/"))

seabirdsurface<-seabird %>%
  filter(Depth==1)

chlaseabird<-seabird %>%
  group_by(Date,Station.ID) %>%
  summarise(Chl=max(Chl))

domin_seabird<-seabird %>%
  group_by(Date,Station.ID) %>%
  summarise(DO=min(DO))

depthmax_seabird <- seabird %>%
  group_by(Date,Station.ID) %>%
  summarise(Depth=max(Depth))

#call the diffusive flux data that Sarah put toether
diffusive<-read.csv(paste(myWD, "input/powellDiffusiveFluxes.csv", sep="/")) 
head(diffusive)

#join diffusive fluxes with surface seabird data
surface_seabird_diffusive<-inner_join(seabirdsurface,lakePowellData10,by="Station.ID")
# head(surface_seabird_diffusive)
# str(surface_seabird_diffusive)
# surface_seabird_diffusive$ch4.drate.mg.h
# surface_seabird_diffusive$ch4.drate.mg.h.<-as.numeric(as.character(surface_seabird_diffusive$ch4.drate.mg.h))

#join diffusive fluxes with chlorophyll maximum data
chlamax_seabird_diffusive<-inner_join(chlaseabird,diffusive,by="Station.ID")
head(chlamax_seabird_diffusive)

domin_seabird_diffusive<-inner_join(diffusive,domin_seabird,by="Station.ID")
head(domin_seabird_diffusive)

depthmax_seabird_diffusive<-inner_join(diffusive,depthmax_seabird,by="Station.ID")
head(depthmax_seabird_diffusive)

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

aa<-lm(surface_seabird_diffusive$co2.drate.mg.h~surface_seabird_diffusive$pH)
summary(aa)

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

#when joined, ch4.drate.mg.h became a factor. convert to numeric:
surface_seabird_diffusive$ch4.drate.mg.h<-as.numeric(as.character(surface_seabird_diffusive$ch4.drate.mg.h))

ch4dist<-filter(surface_seabird_diffusive, River_arm != "") %>%
  mutate(ch4=ch4.drate.mg.h*24)%>%
  ggplot(aes(x=Distance,y=ch4))+
  facet_grid(.~River_arm)+
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

c<-lm(as.numeric(surface_seabird_diffusive$ch4.drate.mg.h)~surface_seabird_diffusive$T)
summary(c)

d<-lm(as.numeric(surface_seabird_diffusive$ch4.drate.mg.h)~surface_seabird_diffusive$Cond)
summary(d)

ch4DO<-domin_seabird_diffusive %>%
  mutate(ch4=as.numeric(ch4.drate.mg.h)*24)%>%
  ggplot(aes(x=DO,y=ch4))+
  geom_point()+
  geom_text(aes(label=Station.ID,hjust=0.2,vjust=0.1))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Diffusive CH4 Emission mg m-2 d-1")))+
  xlab(expression(paste("Minimum Water Column DO")))
  #scale_y_log10()+
  #scale_x_log10()
ch4DO

e<-lm(as.numeric(domin_seabird_diffusive$ch4.drate.mg.h)~domin_seabird_diffusive$DO)
summary(e)

ch4depth<-depthmax_seabird_diffusive %>%
  mutate(ch4=as.numeric(ch4.drate.mg.h)*24)%>%
  ggplot(aes(x=Depth,y=ch4))+
  geom_point()+
  geom_text(aes(label=Station.ID,hjust=0.2,vjust=0.1))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Diffusive CH4 Emission mg m-2 d-1")))+
  xlab(expression(paste("Maximum Depth")))
#scale_y_log10()+
#scale_x_log10()
ch4depth

f<-lm(as.numeric(depthmax_seabird_diffusive$ch4.drate.mg.h)~depthmax_seabird_diffusive$Depth)
summary(f)

co2depth<-depthmax_seabird_diffusive %>%
  mutate(co2=co2.drate.mg.h*24)%>%
  ggplot(aes(x=Depth,y=co2))+
  geom_point()+
  geom_text(aes(label=Station.ID,hjust=0.2,vjust=0.1))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Diffusive Co2 Emission mg m-2 d-1")))+
  xlab(expression(paste("Maximum Depth")))
#scale_y_log10()+
#scale_x_log10()
co2depth
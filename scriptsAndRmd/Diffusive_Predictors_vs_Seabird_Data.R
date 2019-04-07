###########################################################
########## Lake Powell GHG Data from July 2017 ############
########## August 2018 Analysis                   ############
###########################################################


#call seabird data exported from access database and sort it-- I think Nick has yet to process the inflow stations?
seabird<-read.csv(paste(myWD, "input/tblProfiles_July2017_Query.csv", sep="/"))
depths<-read.csv(paste(myWD,"input/SiteDepths.csv", sep="/"))

seabird$Station.ID<-gsub("LPSJR767","LPSJRINF",seabird$Station.ID)
seabird$Station.ID<-gsub("LPCR2880","LPCRINF",seabird$Station.ID)
seabird$Station.ID<-gsub("LPESC334","LPESCINF",seabird$Station.ID)

seabirdsurface<-seabird %>%
  group_by(Station.ID)%>%
  filter(Depth==min(Depth))

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
#just changed this to pull in the output file, but if you want to compare with what we were looking at previously
#you can change it back to the diffusive fluxes in the input folder
diffusive<-read.csv(paste(myWD, "output/lakePowellFluxes20190322.csv", sep="/")) 
head(diffusive)

#join diffusive fluxes with surface seabird data
diffusive$Station.ID<-as.character(diffusive$Station.ID)

surface_seabird_diffusive<-inner_join(seabirdsurface,diffusive,by="Station.ID")
# head(surface_seabird_diffusive)
# str(surface_seabird_diffusive)
# surface_seabird_diffusive$ch4.drate.mg.h
# surface_seabird_diffusive$ch4.drate.mg.h.<-as.numeric(as.character(surface_seabird_diffusive$ch4.drate.mg.h))

#join diffusive fluxes with chlorophyll maximum data
chlamax_seabird_diffusive<-inner_join(chlaseabird,diffusive,by="Station.ID")
head(chlamax_seabird_diffusive)

domin_seabird_diffusive<-inner_join(diffusive,domin_seabird,by="Station.ID")
head(domin_seabird_diffusive)

depthmax_seabird_diffusive<-inner_join(diffusive,depths,by="Station.ID")
head(depthmax_seabird_diffusive)

## look at potential predictors of GHG flux

#what is the difference between a trate and a drate?
co2ph<-surface_seabird_diffusive %>%
  mutate(co2=CO2.trate.best*24)%>%
  ggplot(aes(x=pH,y=co2))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        plot.margin= unit(c(0,0,0,1.5),"cm"))+
  ylab(expression(paste("Diffusive CO2 Emission mg m-2 d-1")))+
  xlab(expression(paste("pH")))
co2ph

aa<-lm(surface_seabird_diffusive$CO2.trate.best~surface_seabird_diffusive$pH)
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

#surface_seabird_diffusive$Distance<-c(2.4,15.5,45.3,90.5,100.1,)
#ch4dist<-surface_seabird_diffusive %>% 
  #mutate(ch4=ch4.drate.mg.h*24)%>%
  #ggplot(aes(x=Distance,y=ch4))+
  #facet_grid(.~River_arm)+
  #geom_point()+
  #theme_bw()+
  #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"),
        #plot.margin= unit(c(0,0,0,1.5),"cm"))+
  #ylab(expression(paste("Diffusive CH4 Emission mg m-2 d-1")))+
  #xlab(expression(paste("Distance Upstream from Dam")))
#ch4dist

ch4Chl<-surface_seabird_diffusive %>%
  mutate(ch4=as.numeric(CH4.trate.best)*24)%>%
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

a<-lm(as.numeric(surface_seabird_diffusive$CH4.trate.best)~surface_seabird_diffusive$Chl)
summary(a)

ch4Chlmax<-chlamax_seabird_diffusive %>%
  mutate(ch4=as.numeric(CH4.trate.best)*24)%>%
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

b<-lm(as.numeric(chlamax_seabird_diffusive$CH4.trate.best)~chlamax_seabird_diffusive$Chl)
summary(b)

ch4temp<-surface_seabird_diffusive %>%
  mutate(ch4=as.numeric(CH4.trate.best)*24)%>%
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

c<-lm(as.numeric(surface_seabird_diffusive$CH4.trate.best)~surface_seabird_diffusive$T)
summary(c)

d<-lm(as.numeric(surface_seabird_diffusive$CH4.trate.best)~surface_seabird_diffusive$Cond)
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

e<-lm(as.numeric(domin_seabird_diffusive$CH4.trate.best)~domin_seabird_diffusive$DO)
summary(e)

ch4depth<-depthmax_seabird_diffusive %>%
  mutate(ch4=as.numeric(CH4.trate.best)*24)%>%
  group_by(Station.ID)%>%
  summarise(ch4=mean(ch4),Depth=mean(Depth))%>%
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

f<-lm(as.numeric(depthmax_seabird_diffusive$CH4.trate.best)~depthmax_seabird_diffusive$Depth)
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

#### Now Load Nutrient/Ion Data and Make a Master Datafile that Combines all the Predictor Information
chemistry<-read.csv(paste(myWD, "input/July_2017_nutrients_for_GHG.csv", sep="/")) 
head(chemistry)
chemsum<-chemistry %>%
  filter(Depth<2)

chemsurface<-chemistry %>%
  group_by(Station.ID)%>%
  filter(Depth==min(Depth))%>%
  select(Station.ID, HCO3, Ca)
colnames(chemsurface)<-c("Station.ID","SurfaceHCO3","SurfaceCa")

depthmax_seabird$Depthmax<-depthmax_seabird$Depth
depthmax<-data.frame(depthmax_seabird$Station.ID,depthmax_seabird$Depthmax)
colnames(depthmax)<-c("Station.ID","depthmax")

chlamax<-data.frame(chlamax_seabird_diffusive$Chl,chlamax_seabird_diffusive$Station.ID)
colnames(chlamax)<-c("chlamax","Station.ID")

seabirdbottom<-seabird %>%
  group_by(Station.ID)%>%
  filter(Depth==max(Depth))%>%
  select(Station.ID,Depth,T,Cond,DO,pH,ORP,Turb,Chl)
colnames(seabirdbottom)<-c("Station.ID","BottomDepth","BottomTemp","BottomCond","BottomDO","BottompH","BottomORP","BottomTurb","BottomChl")

seabirdsurfaceformaster<-seabirdsurface %>%
  select(Station.ID,Chl,T,pH)
colnames(seabirdsurfaceformaster)<-c("Station.ID","SurfaceChl","SurfaceTemp","SurfacepH")

master<-left_join(surface_seabird_diffusive,chemsum,by="Station.ID")
master<-left_join(master,depths,by="Station.ID")
master<-left_join(master, chlamax,by="Station.ID")
master<-left_join(master, seabirdbottom, by="Station.ID")
master<-left_join(master, chemsurface,by="Station.ID")


write.table(master, 
            file=paste(myWD, "output/master.csv", sep="/"),
            sep=",",
            row.names=FALSE)

#### Now try modeling the fluxes using glm

library(glmm)



#pull out subset of master list that has chemistry and seabird data
mastersub<- master %>%
  filter(!is.na(Ca)) %>%
  mutate(sqrtCH4=1/sqrt(CH4.trate.best))

  

## test for normality

shapiro.test(mastersub$sqrtCH4)
shapiro.test(mastersub$CO2.trate.best)



#Single Predictors of CH4
summary(glm(mastersub$sqrtCH4~mastersub$SurfaceChl)) #45
summary(glm(mastersub$sqrtCH4~mastersub$Depth.y)) #35
summary(glm(mastersub$sqrtCH4~mastersub$Total.P..mg.L)) #46
summary(glm(mastersub$sqrtCH4~mastersub$SO4)) #57
summary(glm(mastersub$sqrtCH4~mastersub$BottomTemp)) #37
summary(glm(mastersub$sqrtCH4~mastersub$BottomCond)) #44
summary(glm(mastersub$sqrtCH4~mastersub$BottomTurb)) #35
summary(glm(mastersub$sqrtCH4~mastersub$Total.N.mg.L)) #50

## So let's say our best model is TP, now we try adding variables
summary(glm(mastersub$sqrtCH4~mastersub$Total.P..mg.L+mastersub$SurfaceChl)) #40
summary(glm(mastersub$sqrtCH4~mastersub$Total.P..mg.L+mastersub$BottomTemp)) #39
summary(glm(mastersub$sqrtCH4~mastersub$Total.P..mg.L+mastersub$Depth.y)) #34
summary(glm(mastersub$sqrtCH4~mastersub$Total.P..mg.L+mastersub$BottomCond)) #38
summary(glm(mastersub$sqrtCH4~mastersub$Total.P..mg.L+mastersub$BottomTurb)) #36.6
summary(glm(mastersub$sqrtCH4~mastersub$Total.P..mg.L+mastersub$Total.N.mg.L)) #47
# Total Nitrogen is uninformative

#Interactions

summary(glm(mastersub$sqrtCH4~mastersub$BottomTemp*mastersub$Total.P..mg.L)) #39
summary(glm(mastersub$sqrtCH4~mastersub$BottomTemp*mastersub$BottomTurb)) #36.7
summary(glm(mastersub$sqrtCH4~mastersub$BottomTemp*mastersub$BottomCond)) #36
summary(glm(mastersub$sqrtCH4~mastersub$BottomTemp*mastersub$Depth.y)) #34
summary(glm(mastersub$sqrtCH4~mastersub$BottomCond*mastersub$Total.P..mg.L)) #39.5
summary(glm(mastersub$sqrtCH4~mastersub$BottomCond*mastersub$BottomTemp)) #36
summary(glm(mastersub$sqrtCH4~mastersub$Depth.y*mastersub$Total.P..mg.L)) #35

#BEST CH4 MODEL additive with Depth and TP as predictors


#Single Predictors of CO2
summary(glm(mastersub$CO2.trate.best~mastersub$SurfacepH)) #103
summary(glm(mastersub$CO2.trate.best~mastersub$Depth.y)) #115
summary(glm(mastersub$CO2.trate.best~mastersub$Total.P..mg.L)) #115
summary(glm(mastersub$CO2.trate.best~mastersub$SurfaceCa)) #103
summary(glm(mastersub$CO2.trate.best~mastersub$SurfaceTemp)) #115
summary(glm(mastersub$CO2.trate.best~mastersub$SurfaceChl)) #114
summary(glm(mastersub$CO2.trate.best~mastersub$BottomTurb)) #114
summary(glm(mastersub$CO2.trate.best~mastersub$Total.N.mg.L)) #113
summary(glm(mastersub$CO2.trate.best~mastersub$SurfaceHCO3)) #109.6

## So let's say our best model is pH
summary(glm(mastersub$CO2.trate.best~mastersub$SurfacepH+mastersub$SurfaceCa)) #102
#calcium is uninformative once surface pH is considered
summary(glm(mastersub$CO2.trate.best~mastersub$SurfacepH+mastersub$SurfaceTemp))#104
#water temp is also uninformative
summary(glm(mastersub$CO2.trate.best~mastersub$SurfacepH+mastersub$SurfaceChl))#105
#surface chlorophyll is also uninformative
summary(glm(mastersub$CO2.trate.best~mastersub$SurfacepH+mastersub$Depth.y))#105
#depth also uninformative
summary(glm(mastersub$CO2.trate.best~mastersub$SurfacepH+mastersub$SurfaceHCO3))#99.5
#just barely improves the model

#Interactions

summary(glm(mastersub$CO2.trate.best~mastersub$SurfacepH*mastersub$HCO3)) #98.9

#BEST CO2 MODEL IS ADDITIVE WITH pH and bicarbonate concentration as predictors

# Now get master table organized to summarize the two main regions used for upscaling

Littoral_Tributary<-c("Alcove","Colorado Inlet","Sheep Canyon","Escalante Inflow","Hite","Camp 3","Camp 4","Garces Island","SJR Inlet")

master$region<-ifelse(master$site.x %in% Littoral_Tributary,"tributary","main")

summary_table<-master %>%
  group_by(region)%>%
  summarise(
            Depth=mean(Depth,na.rm=TRUE), 
            SurfacepH=mean(pH,na.rm=TRUE),
            BottomDO=mean(BottomDO,na.rm=TRUE),
            BottomTemp=mean(BottomTemp,na.rm=TRUE),
            Total.P=mean(Total.P..mg.L,na.rm=TRUE),
            BottomCond=mean(BottomCond,na.rm=TRUE),
            SurfaceChl=mean(Chl,na.rm=TRUE),
            SurfaceHCO3=mean(SurfaceHCO3,na.rm=TRUE),
            SurfaceCa=mean(SurfaceCa,na.rm=TRUE),
            SurfaceTemp=mean(T,na.rm=TRUE),
            Total.N=mean(Total.N.mg.L,na.rm=TRUE),
            SO4=mean(SO4,na.rm=TRUE),
            BottomTurb=mean(BottomTurb,na.rm=TRUE))

#need to make a separate table for this b/c not all sites had seabird/chemical data associated with them
summary_rates<-diffusive %>%
  mutate(region=ifelse(site %in% Littoral_Tributary,"tributary","main"))%>%
  group_by(region)%>%
  summarise(CH4trate.mean=mean(CH4trate.mean,na.rm=TRUE),
            CO2trate.mean=mean(CO2trate.mean,na.rm=TRUE),
            meanCH4.funnel=mean(meanCH4.funnel,na.rm=TRUE),
            CH4drate.mean=mean(CH4drate.mean,na.rm=TRUE),
            CH4erate.mean=mean(CH4erate.mean,na.rm=TRUE))

summary_rate_error<- diffusive %>%
  mutate(region=ifelse(site %in% Littoral_Tributary,"tributary","main"))%>%
  mutate(CH4trate.log=log(CH4trate.mean))%>%
  mutate(CH4drate.log=log(CH4drate.mean))%>%
  mutate(CH4erate.log=log(CH4erate.mean+1))%>%
  mutate(meanCH4.funnel.log=log(meanCH4.funnel+1))%>%
  group_by(region)%>%
  summarise(CH4trate.sd=sd(CH4trate.log,na.rm=TRUE),
            CH4trate.na=(sum(is.na(CH4trate.log))),
            CH4trate.log=log(mean(CH4trate.mean,na.rm=TRUE)),
            
            meanCH4.funnel.sd=sd(meanCH4.funnel.log,na.rm=TRUE),
            meanCH4.funnel.na=(sum(is.na(meanCH4.funnel.log))),
            meanCH4.funnel.log=log(mean(meanCH4.funnel,na.rm=TRUE)),
            
            CH4drate.sd=sd(CH4drate.log,na.rm=TRUE),
            CH4drate.na=(sum(is.na(CH4drate.log))),
            CH4drate.log=log(mean(CH4drate.mean,na.rm=TRUE)),
            
            CH4erate.sd=sd(CH4erate.log,na.rm=TRUE),
            CH4erate.na=(sum(is.na(CH4erate.log))),
            CH4erate.log=log(mean(CH4erate.mean,na.rm=TRUE)))

Error_tributary<-summary_rate_error %>%
  filter(region=="tributary")%>%
  mutate(CH4trate.se=CH4trate.sd/sqrt(8-CH4trate.na))%>%
  mutate(CH4trate.upper=exp(CH4trate.se+CH4trate.log))%>%
  mutate(CH4trate.lower=exp(CH4trate.log-CH4trate.se))%>%
  mutate(meanCH4.funnel.se=meanCH4.funnel.sd/sqrt(8-meanCH4.funnel.na))%>%
  mutate(meanCH4.funnel.upper=exp(meanCH4.funnel.log+meanCH4.funnel.se))%>%
  mutate(meanCH4.funnel.lower=exp(meanCH4.funnel.log-meanCH4.funnel.se))%>%
  mutate(CH4drate.se=CH4drate.sd/sqrt(8-CH4drate.na))%>%
  mutate(CH4drate.upper=exp(CH4drate.log+CH4drate.se))%>%
  mutate(CH4drate.lower=exp(CH4drate.log-CH4drate.se))%>%
  mutate(CH4erate.se=CH4erate.sd/sqrt(8-CH4erate.na))%>%
  mutate(CH4erate.upper=exp(CH4erate.log+CH4erate.se))%>%
  mutate(CH4erate.lower=exp(CH4erate.log-CH4erate.se))

Error_main<-summary_rate_error %>%
  filter(region=="main")%>%
  mutate(CH4trate.se=CH4trate.sd/sqrt(22-CH4trate.na))%>%
  mutate(CH4trate.upper=exp(CH4trate.se+CH4trate.log))%>%
  mutate(CH4trate.lower=exp(CH4trate.log-CH4trate.se))%>%
  mutate(meanCH4.funnel.se=meanCH4.funnel.sd/sqrt(22-meanCH4.funnel.na))%>%
  mutate(meanCH4.funnel.upper=exp(meanCH4.funnel.log+meanCH4.funnel.se))%>%
  mutate(meanCH4.funnel.lower=exp(meanCH4.funnel.log-meanCH4.funnel.se))%>%
  mutate(CH4drate.se=CH4drate.sd/sqrt(22-CH4drate.na))%>%
  mutate(CH4drate.upper=exp(CH4drate.log+CH4drate.se))%>%
  mutate(CH4drate.lower=exp(CH4drate.log-CH4drate.se))%>%
  mutate(CH4erate.se=CH4erate.sd/sqrt(22-CH4erate.na))%>%
  mutate(CH4erate.upper=exp(CH4erate.log+CH4erate.se))%>%
  mutate(CH4erate.lower=exp(CH4erate.log-CH4erate.se))

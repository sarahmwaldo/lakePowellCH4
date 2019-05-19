####################################
#### Calculate Average Emissions and Error
#### Script started 5/19/19
###################################

master<-read.csv(paste(myWD, "output/lakePowellFluxes20190502.csv", sep="/")) 
head(master)

Littoral_Tributary<-c("Alcove","Colorado Inlet","Sheep Canyon","Escalante Inflow","Hite Canyon","Camp 3","Garces Island","SJR Inlet")

master$region<-ifelse(master$site %in% Littoral_Tributary,"tributary","main")

# rates are in units of mg CH4 m-2 h-1-- converting to per day fluxes here
# emission rates calculated starting on line 190 of calculate emissions lp10 script
summary_rates<-master %>%
  group_by(region)%>%
  summarise(CH4trate.mean=mean(CH4trate.mean,na.rm=TRUE)*24,
            CO2trate.mean=mean(CO2trate.mean,na.rm=TRUE)*24,
            meanCH4.funnel=mean(meanCH4.funnel,na.rm=TRUE)*24,
            CH4drate.mean=mean(CH4drate.mean,na.rm=TRUE)*24,
            CH4erate.mean=mean(CH4erate.mean,na.rm=TRUE)*24,
            CO2trate.mean=mean(CO2trate.mean,na.rm=TRUE)*24)

summary_rate_error<- diffusive %>%
  mutate(region=ifelse(site %in% Littoral_Tributary,"tributary","main"))%>%
  mutate(CH4trate.log=log(CH4trate.mean))%>%
  mutate(CH4drate.log=log(CH4drate.mean))%>%
  mutate(CH4erate.log=log(CH4erate.mean+1))%>%
  mutate(meanCH4.funnel.log=log(meanCH4.funnel+1))%>%
  group_by(region)%>%
  summarise(CH4trate.sd=sd(CH4trate.log,na.rm=T),
            CH4trate.na=(sum(is.na(CH4trate.log))),
            CH4trate.log=mean(CH4trate.log,na.rm=T),
            
            meanCH4.funnel.sd=sd(meanCH4.funnel.log,na.rm=T),
            meanCH4.funnel.na=(sum(is.na(meanCH4.funnel.log))),
            meanCH4.funnel.log=mean(meanCH4.funnel.log,na.rm=T),
            
            CH4drate.sd=sd(CH4drate.log,na.rm=T),
            CH4drate.na=(sum(is.na(CH4drate.log))),
            CH4drate.log=mean(CH4drate.log,na.rm=T),
            
            CH4erate.sd=sd(CH4erate.log,na.rm=T),
            CH4erate.na=(sum(is.na(CH4erate.log))),
            CH4erate.log=mean(CH4erate.log,na.rm=T))

#these tables are generating fluxes in units of per hour, so transform them here
Error_tributary<-summary_rate_error %>%
  filter(region=="tributary")%>%
  mutate(CH4trate.se=CH4trate.sd/sqrt(8-CH4trate.na))%>%
  mutate(CH4trate.upper=24*exp(CH4trate.se+CH4trate.log))%>%
  mutate(CH4trate.lower=24*exp(CH4trate.log-CH4trate.se))%>%
  mutate(meanCH4.funnel.se=meanCH4.funnel.sd/sqrt(8-meanCH4.funnel.na))%>%
  mutate(meanCH4.funnel.upper=24*exp(meanCH4.funnel.log+meanCH4.funnel.se))%>%
  mutate(meanCH4.funnel.lower=24*exp(meanCH4.funnel.log-meanCH4.funnel.se))%>%
  mutate(CH4drate.se=CH4drate.sd/sqrt(8-CH4drate.na))%>%
  mutate(CH4drate.upper=24*exp(CH4drate.log+CH4drate.se))%>%
  mutate(CH4drate.lower=24*exp(CH4drate.log-CH4drate.se))%>%
  mutate(CH4erate.se=CH4erate.sd/sqrt(8-CH4erate.na))%>%
  mutate(CH4erate.upper=24*exp(CH4erate.log+CH4erate.se))%>%
  mutate(CH4erate.lower=24*exp(CH4erate.log-CH4erate.se))

Error_main<-summary_rate_error %>%
  filter(region=="main")%>%
  mutate(CH4trate.se=CH4trate.sd/sqrt(22-CH4trate.na))%>%
  mutate(CH4trate.upper=24*exp(CH4trate.se+CH4trate.log))%>%
  mutate(CH4trate.lower=24*exp(CH4trate.log-CH4trate.se))%>%
  mutate(meanCH4.funnel.se=meanCH4.funnel.sd/sqrt(22-meanCH4.funnel.na))%>%
  mutate(meanCH4.funnel.upper=24*exp(meanCH4.funnel.log+meanCH4.funnel.se))%>%
  mutate(meanCH4.funnel.lower=24*exp(meanCH4.funnel.log-meanCH4.funnel.se))%>%
  mutate(CH4drate.se=CH4drate.sd/sqrt(22-CH4drate.na))%>%
  mutate(CH4drate.upper=24*exp(CH4drate.log+CH4drate.se))%>%
  mutate(CH4drate.lower=24*exp(CH4drate.log-CH4drate.se))%>%
  mutate(CH4erate.se=CH4erate.sd/sqrt(22-CH4erate.na))%>%
  mutate(CH4erate.upper=24*exp(CH4erate.log+CH4erate.se))%>%
  mutate(CH4erate.lower=24*exp(CH4erate.log-CH4erate.se))

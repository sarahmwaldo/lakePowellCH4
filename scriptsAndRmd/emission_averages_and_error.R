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
  summarise(CH4trate.mean2=mean(CH4trate.mean,na.rm=TRUE)*24,
            CO2trate.mean2=mean(CO2trate.mean,na.rm=TRUE)*24,
            meanCH4.funnel2=mean(meanCH4.funnel,na.rm=TRUE)*24,
            CH4drate.mean2=mean(CH4drate.mean,na.rm=TRUE)*24,
            CH4erate.mean2=mean(CH4erate.mean,na.rm=TRUE)*24,
            CH4trate.na=(sum(is.na(CH4trate.mean))),
            CH4trate.sd=sd(CH4trate.mean,na.rm=T),
            CO2trate.na=(sum(is.na(CO2trate.mean))),
            CO2trate.sd=sd(CO2trate.mean,na.rm=T),
            CH4drate.na=(sum(is.na(CH4drate.mean))),
            CH4drate.sd=sd(CH4drate.mean,na.rm=T),
            CH4erate.na=(sum(is.na(CH4erate.mean))),
            CH4erate.sd=sd(CH4erate.mean,na.rm=T),
            CH4funnel.na=(sum(is.na(meanCH4.funnel))),
            CH4funnel.sd=sd(meanCH4.funnel,na.rm=T)
            )

## I'm hashing out this code which was looking at log means-- you can use control shift C for that!
#summary_rate_error<- master %>%
  #mutate(CH4trate.log=log(CH4trate.mean))%>%
  #mutate(CH4drate.log=log(CH4drate.mean))%>%
  #mutate(CH4erate.log=log(CH4erate.mean+1))%>%
  #mutate(CO2trate.log=log(CO2trate.mean+38))%>%
  #mutate(meanCH4.funnel.log=log(meanCH4.funnel+1))%>%
  #group_by(region)%>%
  #summarise(CH4trate.sd=sd(CH4trate.log,na.rm=T),
            #CH4trate.na=(sum(is.na(CH4trate.log))),
            #CH4trate.log=mean(CH4trate.log,na.rm=T),
            
            #meanCH4.funnel.sd=sd(meanCH4.funnel.log,na.rm=T),
            #meanCH4.funnel.na=(sum(is.na(meanCH4.funnel.log))),
            #meanCH4.funnel.log=mean(meanCH4.funnel.log,na.rm=T),
            
            # CH4drate.sd=sd(CH4drate.log,na.rm=T),
            # CH4drate.na=(sum(is.na(CH4drate.log))),
            # CH4drate.log=mean(CH4drate.log,na.rm=T),
            # 
            # CH4erate.sd=sd(CH4erate.log,na.rm=T),
            # CH4erate.na=(sum(is.na(CH4erate.log))),
            # CH4erate.log=mean(CH4erate.log,na.rm=T),
            # 
            # CO2trate.sd=sd(CO2trate.log,na.rm=T),
            # CO2trate.na=(sum(is.na(CO2trate.log))),
            # CO2trate.log=mean(CO2trate.log,na.rm=T))

#these tables are generating fluxes in units of per hour, so transform them here
# Error_tributary<-summary_rate_error %>%
#   filter(region=="tributary")%>%
#   mutate(CH4trate.se=CH4trate.sd/sqrt(8-CH4trate.na))%>%
#   mutate(CH4trate.upper=24*exp(CH4trate.se+CH4trate.log))%>%
#   mutate(CH4trate.lower=24*exp(CH4trate.log-CH4trate.se))%>%
#   mutate(CH4trate.logmean=24*exp(CH4trate.log))%>%
#   mutate(meanCH4.funnel.se=meanCH4.funnel.sd/sqrt(8-meanCH4.funnel.na))%>%
#   mutate(meanCH4.funnel.upper=24*exp(meanCH4.funnel.log+meanCH4.funnel.se-1))%>%
#   mutate(meanCH4.funnel.lower=24*exp(meanCH4.funnel.log-meanCH4.funnel.se-1))
#   mutate(CH4drate.se= CH4drate.sd/sqrt(8-CH4drate.na))%>%
#   mutate(CH4drate.upper=24*exp(CH4drate.log+CH4drate.se))%>%
#   mutate(CH4drate.lower=24*exp(CH4drate.log-CH4drate.se))%>%
#   mutate(CH4erate.se=CH4erate.sd/sqrt(8-CH4erate.na))%>%
#   mutate(CH4erate.upper=24*exp(CH4erate.log+CH4erate.se-1))%>%
#   mutate(CH4erate.lower=24*exp(CH4erate.log-CH4erate.se-1))%>%
#   mutate(CO2trate.se=CO2trate.sd/sqrt(8-CO2trate.na))%>%
#   mutate(CO2trate.upper=24*exp(CO2trate.log+CO2trate.se-38))%>%
#   mutate(CO2trate.lower=24*exp(CO2trate.log-CO2trate.se-38))
# 
# Error_main<-summary_rate_error %>%
#   filter(region=="main")%>%
#   mutate(CH4trate.se=CH4trate.sd/sqrt(22-CH4trate.na))%>%
#   mutate(CH4trate.upper=24*exp(CH4trate.se+CH4trate.log))%>%
#   mutate(CH4trate.lower=24*exp(CH4trate.log-CH4trate.se))%>%
#   mutate(meanCH4.funnel.se=meanCH4.funnel.sd/sqrt(22-meanCH4.funnel.na))%>%
#   mutate(meanCH4.funnel.upper=24*exp(meanCH4.funnel.log+meanCH4.funnel.se))%>%
#   mutate(meanCH4.funnel.lower=24*exp(meanCH4.funnel.log-meanCH4.funnel.se))%>%
#   mutate(CH4drate.se=CH4drate.sd/sqrt(22-CH4drate.na))%>%
#   mutate(CH4drate.upper=24*exp(CH4drate.log+CH4drate.se))%>%
#   mutate(CH4drate.lower=24*exp(CH4drate.log-CH4drate.se))%>%
#   mutate(CH4erate.se=CH4erate.sd/sqrt(22-CH4erate.na))%>%
#   mutate(CH4erate.upper=24*exp(CH4erate.log+CH4erate.se))%>%
#   mutate(CH4erate.lower=24*exp(CH4erate.log-CH4erate.se))




# EMISSION RATE CALCULATIONS--------------------
# STEP 1:  CALCULATE EMISSION RATE VIA LINEAR AND NONLINEAR REGRESSION
#          FOR SITES WHERE PERIODS OF LINEAR ACCUMULATION ARE INDICATED 
# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OF NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# STEP 3: MERGE WITH eqAreaData

lakePowellData10<-filter(lakePowellData10, site!="SJR Inlet End", #SJR inlet end was recorded for the lat/long, but isn't a separate observation
                             rep!=4) #Camp 3, rep 4 is the measurement taken in the morning after the chamber was left on the water all night. 
                                     #There is a negative methane regression, indicating uptake, but this is an artifact. This is the only rep=4 in the dataset. 


# STEP 1: LINEAR AND NONLINEAR REGRESSION
n <- length(lakePowellData10$diffStartTime)
temp <- rep(NA, n)

# Dataframe to hold results
OUT <- data.frame(site = temp, dt = temp,
                  ch4.diff.max=temp, #Sarah added on 6/14/17; making histogram of max ch4 levels measured by LGR
                  ch4.lm.slope = temp, ch4.lm.drate.mg.h = temp, ch4.lm.aic = temp, ch4.lm.r2 = temp, ch4.lm.pval = temp,
                  ch4.ex.aic = temp, ch4.ex.r2 = temp, ch4.ex.slope = temp, ch4.ex.drate.mg.h = temp, ch4.ex.k=temp, 
                  co2.lm.slope = temp, co2.lm.drate.mg.h = temp, co2.lm.aic = temp, co2.lm.r2 = temp, co2.lm.pval = temp,
                  co2.ex.aic = temp, co2.ex.r2 = temp, co2.ex.slope = temp, co2.ex.k = temp, co2.ex.drate.mg.h = temp,
                  ch4.trate.mg.h = temp, co2.trate.mg.h = temp, 
                  ch4.lm.slope.err = temp, co2.lm.slope.err = temp, ch4.ex.slope.err = temp, co2.ex.slope.err = temp,
                  ch4.ex.res.err = temp, ch4.lm.res.err = temp, co2.ex.res.err = temp, co2.lm.res.err = temp)




# Remove data not recorded during deployment
gga.model <-gga

pdf(paste(myWD, "output/curveFits10.pdf", sep="/"), paper = "a4r")
start.time <- Sys.time()
for (i in 1:length(lakePowellData10$diffStartTime)) {  # For each unique site
  site.i <- lakePowellData10$site[i]  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
  #lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
  OUT[i,"site"] <- site.i
  #OUT[i,"Lake_Name"] <- lake.i  
  # Need chamber volume from eqAreaData.
  chmVol.L.i <- lakePowellData10$chm_vol[i] 
  


  data.i.ch4 <- filter(gga.model,  # extract data
                       RDateTime >= lakePowellData10$diffStartTime[i], # based on diff start time
                       RDateTime <= lakePowellData10$diffEndTime[i])%>%  # based on diff end time
                select(CH4._ppm, GasT_C, RDateTime)  # Pull out data of interest

  data.i.ch4$elapTime<-(data.i.ch4$RDateTime-data.i.ch4$RDateTime[1])
  data.i.ch4$chmVol.L<-lakePowellData10$chm_vol[i]
  
  #for total emission calc
  data.i.ch4.t <- filter(gga.model,  # extract data
                       RDateTime >= lakePowellData10$totStartTime[i], # based on diff start time
                       RDateTime <= lakePowellData10$totEndTime[i])%>%  # based on diff end time
    select(CH4._ppm, GasT_C, RDateTime)  # Pull out data of interest
  
  data.i.ch4.t$elapTime<-(data.i.ch4.t$RDateTime-data.i.ch4.t$RDateTime[1])
  data.i.ch4.t$chmVol.L<-lakePowellData10$chm_vol[i]
  
  OUT[i, "ch4.diff.max"]<-max(data.i.ch4$CH4._ppm, na.rm=TRUE) #maximum CH4 mixing ratio measured during the chamber deployment time
  OUT[i, "elap.time"]<-dt.i
  
  data.i.co2 <- filter(gga.model,  # extract data
                       RDateTime >= lakePowellData10$diffStartTime[i], # based on diff start time
                       RDateTime <= lakePowellData10$diffEndTime[i])%>% # based on diff end time
    select(CO2._ppm, GasT_C, RDateTime)  # Pull out data of interest
 
   data.i.co2$elapTime<-(data.i.co2$RDateTime-data.i.co2$RDateTime[1])
  data.i.co2$chmVol.L<-lakePowellData10$chm_vol[i]
  
  #for total emission calc
  data.i.co2.t <- filter(gga.model,  # extract data
                       RDateTime >= lakePowellData10$totStartTime[i], # based on diff start time
                       RDateTime <= lakePowellData10$totEndTime[i])%>% # based on diff end time
    select(CO2._ppm, GasT_C, RDateTime)  # Pull out data of interest
  
  data.i.co2.t$elapTime<-(data.i.co2.t$RDateTime-data.i.co2.t$RDateTime[1])
  data.i.co2.t$chmVol.L<-lakePowellData10$chm_vol[i]
  
  # Are there data available to run the model?
  co2.indicator <- length(data.i.co2$CO2._ppm) == 0
  ch4.indicator <- length(data.i.ch4$CH4._ppm) == 0
  
  # Data needed for emission rate calcs.  Same #'s for CO2 and CH4.  Arbitrarily pulled from CO2.  
  temp.i <- if (co2.indicator) mean(data.i.ch4$GasT_C, na.rm = TRUE) else (mean(data.i.co2$GasT_C, na.rm = TRUE))  # GGA measured temp
  volume.i <- if (co2.indicator) unique(data.i.ch4[!is.na(data.i.ch4$chmVol.L), "chmVol.L"]) else
    unique(data.i.co2[!is.na(data.i.co2$chmVol.L), "chmVol.L"])# Dome "volume" offset -- in fact a height
  #needed for total emission rate calc
  d.ch4.i<- (-1*diff(data.i.ch4.t$CH4._ppm[end(data.i.ch4.t$CH4._ppm)])) #last CH4 value - first CH4 value
  d.co2.i<- (-1*diff(data.i.co2.t$CO2._ppm[end(data.i.co2.t$CO2._ppm)])) #last CO2 value - first CO2 value
  dt.i<- (-1*diff(data.i.ch4.t$elapTime[end(data.i.ch4.t$elapTime)])) #delta t in seconds
  
  # lm
  lm.ch4.i <- try(lm(data.i.ch4$CH4._ppm ~ data.i.ch4$elapTime), silent = TRUE)  # suppress warning if fails 
  lm.co2.i <- try(lm(data.i.co2$CO2._ppm ~ data.i.co2$elapTime), silent = TRUE)  # linear regression
  
  # lm slopes
  slope.ch4.i <- if(ch4.indicator) NA else (as.numeric(coef(lm.ch4.i)[2]))  # lm slope: ppm s-1 
  slope.ch4.i.err <- if(ch4.indicator) NA else (as.numeric(summary(lm.ch4.i)$coefficients[2,2])/slope.ch4.i)  # lm slope fractional error
  slope.co2.i <- if(co2.indicator) NA else (as.numeric(coef(lm.co2.i)[2]))   # lm slope: ppm s-1
  slope.co2.i.err <- if(co2.indicator) NA else (as.numeric(summary(lm.co2.i)$coefficients[2,2])/slope.co2.i)  # lm slope fractional error
  OUT[i, c("ch4.lm.slope", "ch4.lm.slope.err", "co2.lm.slope", "co2.lm.slope.err")] <- c(slope.ch4.i, slope.ch4.i.err, slope.co2.i, slope.co2.i.err)
  
  #lm intercepts for plot
  intercept.ch4.i<- if(ch4.indicator) NA else (as.numeric(coef(lm.ch4.i)[1]))
  intercept.co2.i<- if(co2.indicator) NA else (as.numeric(coef(lm.co2.i)[1]))
  
  # lm p-values
  fstat.ch4 <- if(ch4.indicator) rep(NA,3) else summary(lm.ch4.i)$fstatistic
  fstat.co2 <- if(co2.indicator) rep(NA,3) else summary(lm.co2.i)$fstatistic
  OUT[i, c("ch4.lm.pval")]  <- pf(fstat.ch4[1], fstat.ch4[2], fstat.ch4[3], lower.tail = FALSE)
  OUT[i, c("co2.lm.pval")]  <- pf(fstat.co2[1], fstat.co2[2], fstat.co2[3], lower.tail = FALSE)
  
  # lm r2 values
  OUT[i, c("ch4.lm.r2")]  <- if(ch4.indicator) NA else summary(lm.ch4.i)["r.squared"]
  OUT[i, c("co2.lm.r2")]  <- if(co2.indicator) NA else summary(lm.co2.i)["r.squared"]
  
  # lm AIC values
  OUT[i, c("ch4.lm.aic")] <- if(ch4.indicator) NA else AIC(lm.ch4.i)
  OUT[i, c("co2.lm.aic")] <- if(co2.indicator) NA else AIC(lm.co2.i)
  
  #lm residual standard errors:
  OUT[i, c("ch4.lm.res.err")] <- if(ch4.indicator) NA else sigma(lm.ch4.i)
  OUT[i, c("co2.lm.res.err")] <- if(co2.indicator) NA else sigma(lm.co2.i)
  
  # Exponential Model
  cmax.ch4 <- data.i.ch4$CH4._ppm[max(which(!is.na(data.i.ch4$CH4._ppm)))]  # cmax = final CH4
  c.initial.ch4 <- data.i.ch4$CH4._ppm[min(which(!is.na(data.i.ch4$CH4._ppm)))]  # initial CH4 
  exp.ch4.i <-try(nlsLM(CH4._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                       # data = data.i.ch4, start=list(cmax=cmax.ch4, b=cmax.ch4-c.initial.ch4, k=.03)), #SW 6/28/2018. b = C_air in Kroon
                        data = data.i.ch4, start=list(cmax=cmax.ch4, b=c.initial.ch4, k=.03)),
                  silent = TRUE) 

  cmax.co2 <- data.i.co2$CO2._ppm[max(which(!is.na(data.i.co2$CO2._ppm)))]  # cmax = final CO2
  c.initial.co2 <- data.i.co2$CO2._ppm[min(which(!is.na(data.i.co2$CO2._ppm)))]  # initial CO2   
  exp.co2.i <-try(nlsLM(CO2._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.i.co2, start=list(cmax=505, b= 400, k=0.004)),
                  silent=TRUE) 
  # Ex r2
  rss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else sum(residuals(exp.ch4.i)^2)
  tss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else 
    sum((data.i.ch4$CH4._ppm - mean(data.i.ch4$CH4._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  OUT[i, "ch4.ex.r2"] = 1 - rss.ch4.i/tss.ch4.i
  
  rss.co2.i <- if(class(exp.co2.i) == "try-error") NA else sum(residuals(exp.co2.i)^2)
  tss.co2.i <- if(class(exp.co2.i) == "try-error") NA else 
    sum((data.i.co2$CO2._ppm - mean(data.i.co2$CO2._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  OUT[i, "co2.ex.r2"] = 1 - rss.co2.i/tss.co2.i
  
  # Ex AIC
  OUT[i, "ch4.ex.aic"] = if(class(exp.ch4.i) == "try-error") NA else AIC(exp.ch4.i)
  OUT[i, "co2.ex.aic"] = if(class(exp.co2.i) == "try-error") NA else AIC(exp.co2.i)
  #lm residual standard errors:
  OUT[i, c("ch4.ex.res.err")] <- if(class(exp.ch4.i) == "try-error") NA else sigma(exp.ch4.i)
  OUT[i, c("co2.ex.res.err")] <- if(class(exp.co2.i) == "try-error") NA else sigma(exp.co2.i)
  
  # Ex slope
  coef.exp.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else coef(exp.ch4.i)
  err.exp.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else summary(exp.ch4.i)$coefficients[,2]
  OUT[i, "ch4.ex.slope"] = if(class(exp.ch4.i) == "try-error") NA else 
    coef.exp.ch4.i["k"]*(coef.exp.ch4.i["cmax"]-coef.exp.ch4.i["b"])  # ppm s-1
  OUT[i, "ch4.ex.slope.err"] = if(class(exp.ch4.i) == "try-error") NA else 
    sqrt((err.exp.ch4.i["k"]/coef.exp.ch4.i["k"])^2+
           (sqrt(err.exp.ch4.i["cmax"]^2+err.exp.ch4.i["b"]^2)/(coef.exp.ch4.i["cmax"]+coef.exp.ch4.i["b"]))^2)  # fractional error
  
  coef.exp.co2.i <- if(class(exp.co2.i) == "try-error") NA else coef(exp.co2.i)   
  err.exp.co2.i <- if(class(exp.co2.i) == "try-error") NA else summary(exp.co2.i)$coefficients[,2]
  OUT[i, "co2.ex.slope"] = if(class(exp.co2.i) == "try-error") NA else 
    coef.exp.co2.i["k"]*(coef.exp.co2.i["cmax"]-coef.exp.co2.i["b"])  # ppm s-1
  OUT[i, "co2.ex.slope.err"] = if(class(exp.ch4.i) == "try-error") NA else 
    sqrt((err.exp.co2.i["k"]/coef.exp.co2.i["k"])^2+
           (sqrt(err.exp.co2.i["cmax"]^2+err.exp.co2.i["b"]^2)/(coef.exp.co2.i["cmax"]+coef.exp.co2.i["b"]))^2)  # fractional error
  
  #Ex k  
  OUT[i, "ch4.ex.k"] = if(class(exp.ch4.i) == "try-error") NA else 
    coef.exp.ch4.i["k"]
  OUT[i, "co2.ex.k"] = if(class(exp.co2.i) == "try-error") NA else 
    coef.exp.co2.i["k"]
  
  # Emission rate.  Assumes atmospheric pressure of 1 atm.
  # Converting from parts per million to umole cross out.  No conversion factor necessary. Dome area = 0.2 m2
  ###########units: ~~~~~~~~~~~ m ~~~ mm ~~~~ m/mm ~ m^2 ~ atm ~ ppm/s ~~~~~~~~~ m^3atm K-1mol-1 ~~~~~~ K ~~~~~~~~~~ m^2         
  ch4.lm.drate.i.umol.s <- ((((0.2-(volume.i*0.001))*0.2) * 1 * slope.ch4.i) / (0.000082057 * (temp.i + 273.15))) / 0.2 #umol CH4 m-2 s-1
  OUT[i, "ch4.lm.drate.mg.h"] = if (length(ch4.lm.drate.i.umol.s) == 0)  # throws error if no data
    NA else
      ch4.lm.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.lm.drate.i.umol.s <- ((((0.2-(volume.i*0.001))*0.2) * 1 * slope.co2.i) / (0.000082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.lm.drate.mg.h"] =  if (length(co2.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.lm.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  ch4.ex.drate.i.umol.s <- ((((0.2-(volume.i*0.001))*0.2) * 1 * OUT[i, "ch4.ex.slope"]) / (0.000082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  OUT[i, "ch4.ex.drate.mg.h"] = if (length(ch4.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      ch4.ex.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.ex.drate.i.umol.s <- ((((0.2-(volume.i*0.001))*0.2) * 1 * OUT[i, "co2.ex.slope"]) / (0.000082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.ex.drate.mg.h"] =  if (length(co2.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.ex.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  #total emission rates from dGas/dt
  ch4.trate.i.umol.s <- ((((0.2-(volume.i*0.001))*0.2) * 1 * (d.ch4.i/as.numeric(dt.i))) / (0.000082057 * (temp.i + 273.15))) / 0.2 #umol CH4 m-2 s-1
  OUT[i, "ch4.trate.mg.h"] = if (length(ch4.trate.i.umol.s) == 0)  # throws error if no data
    NA else
      ch4.trate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.trate.i.umol.s <- ((((0.2-(volume.i*0.001))*0.2) * 1 * (d.co2.i/as.numeric(dt.i))) / (0.000082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.trate.mg.h"] =  if (length(co2.trate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.trate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  
  # Plots
  # CH4 first
  ch4.ex.pred <- try(  
    data.frame(ch4.pred = predict(exp.ch4.i, newdata = data.i.ch4), # pred values from exponential model
               elapTime = data.i.ch4$elapTime),
    silent = TRUE)
  
  ch4.title <- paste(OUT[i, "site"], # plot title
                     "ex.r2=",
                     round(OUT[i, "ch4.ex.r2"], 3),
                     "ex.AIC=",
                     round(OUT[i, "ch4.ex.aic"],2),
                     "ex.rate=",
                     round(OUT[i, "ch4.ex.drate.mg.h"], 2),  
                     "\n lm.r2=",
                     round(OUT[i, "ch4.lm.r2"],2),
                     "lm.AIC=",
                     round(OUT[i, "ch4.lm.aic"],2),
                     "lm.rate=",
                     round(OUT[i, "ch4.lm.drate.mg.h"], 2),
                     "\n tot.rate=",
                     round(OUT[i, "ch4.trate.mg.h"], 2),
                     "ex.frac.err",
                     round(OUT[i, "ch4.ex.slope.err"], 2),
                     "lm.frac.err",
                     round(OUT[i, "ch4.lm.slope.err"], 3),
                     sep=" ")
  p.ch4 <- ggplot(data.i.ch4, aes(as.numeric(elapTime), CH4._ppm)) + 
    geom_point() +
    xlab("Seconds") +
    ggtitle(ch4.title) +
    stat_smooth(method = "lm", se=TRUE)
  if (class(exp.ch4.i) == "try-error") p.ch4 else  # if exp model worked, add exp line
    p.ch4 <- (p.ch4 + geom_line(data=ch4.ex.pred, aes(as.numeric(elapTime), ch4.pred), color = "red")+
                geom_abline(intercept = intercept.ch4.i, 
                            slope = (coef.exp.ch4.i["k"]*(coef.exp.ch4.i["cmax"]-coef.exp.ch4.i["b"])), color="green", size = 1)+
                geom_abline(intercept = intercept.ch4.i+OUT$ch4.ex.res.err[i], 
                            slope = (coef.exp.ch4.i["k"]*(coef.exp.ch4.i["cmax"]-coef.exp.ch4.i["b"])), color="green", size = 0.5, linetype = 2)+
                geom_abline(intercept = intercept.ch4.i-OUT$ch4.ex.res.err[i], 
                            slope = (coef.exp.ch4.i["k"]*(coef.exp.ch4.i["cmax"]-coef.exp.ch4.i["b"])), color="green", size = 0.5, linetype = 2))+
    theme_bw()
    
              
    
  print(p.ch4)
  
  
  # CO2 models
  co2.ex.pred <- try(
    data.frame(co2.pred = predict(exp.co2.i, newdata = data.i.co2),  # pred data from exp model
               elapTime = data.i.co2$elapTime),
    silent=TRUE)
  
  co2.title <- paste(OUT[i, "site"], # plot title
                     "ex.r2=",
                     round(OUT[i, "co2.ex.r2"], 3),
                     "ex.AIC=",
                     round(OUT[i, "co2.ex.aic"],2),
                     "ex.rate=",
                     round(OUT[i, "co2.ex.drate.mg.h"], 2),                    
                     "\n lm.r2=",
                     round(OUT[i, "co2.lm.r2"],2),
                     "lm.AIC=",
                     round(OUT[i, "co2.lm.aic"],2),
                     "lm.rate=",
                     round(OUT[i, "co2.lm.drate.mg.h"], 2),
                     "tot.rate=",
                     round(OUT[i, "co2.trate.mg.h"], 2),
                     sep=" ")
  p.co2 <- ggplot(data.i.co2, aes(as.numeric(elapTime), CO2._ppm)) + 
    geom_point() +
    xlab("Seconds") +
    ggtitle(co2.title) +
    stat_smooth(method = "lm", se=TRUE)
  if (class(exp.co2.i) == "try-error") p.co2 else  # if exp model worked, add exp line
    p.co2 <- (p.co2 + geom_line(data=co2.ex.pred, aes(as.numeric(elapTime), co2.pred), color = "red")+
                geom_abline(intercept = intercept.co2.i, 
                            slope = (coef.exp.co2.i["k"]*(coef.exp.co2.i["cmax"]-coef.exp.co2.i["b"])), color="green", size = 1)+
                geom_abline(intercept = intercept.co2.i+OUT$co2.ex.res.err[i], 
                            slope = (coef.exp.co2.i["k"]*(coef.exp.co2.i["cmax"]-coef.exp.co2.i["b"])), color="green", size = 0.5, linetype = 2)+
                geom_abline(intercept = intercept.co2.i-OUT$co2.ex.res.err[i], 
                            slope = (coef.exp.co2.i["k"]*(coef.exp.co2.i["cmax"]-coef.exp.co2.i["b"])), color="green", size = 0.5, linetype = 2))+
    theme_bw()
    
  print(p.co2)
  grid.arrange(p.ch4, p.co2, ncol = 2)
}  
dev.off()
start.time;Sys.time() 

#############################################################333------
###############################################################33
#Error in mutate_impl(.data, dots) : incorrect number of dimensions
#So let's debug by trying to do one thing at a time:

# data.ch4.test <- filter(gga.model,  # extract data
#                      RDateTime >= lakePowellData10$diffStartTime[1], # based on diff start time
#                      RDateTime <= lakePowellData10$diffEndTime[1]) # based on diff end time
# 
# data.ch4.test$elapTime<-(data.ch4.test$RDateTime-data.ch4.test$RDateTime[1])  %>%
#   
#   mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds). 
#          chmVol.L = chmVol.L.i[1,1]) %>%  # subscripting needed to remove name
#   select(CH4._ppm, elapTime, GasT_C, chmVol.L)  # Pull out data of interest
# 
# OUT[i, "ch4.diff.max"]<-max(data.i.ch4$CH4._ppm, na.rm=TRUE) #maximum CH4 mixing ratio measured during the chamber deployment time
# 



############################################333
  ########################################### --------
  
  

# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OR NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# Choose best rate.  
#SW added 6/29/2018: use AIC if difference in AIC scores are >10%, otherwise, use linear model
OUT <- mutate(OUT, 
              co2.best.model = ifelse(abs((co2.lm.aic-co2.ex.aic)/((co2.lm.aic+co2.ex.aic)/2))>0.1 & co2.lm.aic > co2.ex.aic | is.na(co2.ex.aic), 
                                      "exponential", "linear"),
              co2.drate.mg.h.best = ifelse(co2.best.model == "linear",
                                           co2.lm.drate.mg.h, co2.ex.drate.mg.h),
              ch4.best.model = ifelse(abs((ch4.lm.aic-ch4.ex.aic)/((ch4.lm.aic+ch4.ex.aic)/2))>0.1 & ch4.lm.aic > ch4.ex.aic | is.na(ch4.ex.aic), 
                                      "exponential","linear"),
              ch4.drate.mg.h.best = ifelse(ch4.best.model == "linear",
                                           ch4.lm.drate.mg.h, ch4.ex.drate.mg.h)) 
# Inspect r2 and fractional error of slope
#co2:
plot(with(OUT,ifelse(co2.best.model == "linear", co2.lm.r2, co2.ex.r2)))  # CO2: some low ones to investigate
plot(with(OUT, ifelse(co2.best.model == "linear", abs(co2.lm.slope.err), abs(co2.ex.slope.err))))
#ch4:
plot(with(OUT,ifelse(ch4.best.model == "linear", ch4.lm.r2, ch4.ex.r2)))  # CH4:  some low ones to investigate
plot(with(OUT, ifelse(ch4.best.model == "linear", ch4.lm.slope.err, ch4.ex.slope.err)))

# If r2 of best model < 0.8 for CO2, 0.9 for CH4, then set to NA
OUT <- mutate(OUT, 
              co2.drate.mg.h.best = ifelse((co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic)) & co2.lm.r2 < 0.8, # if ex is best, but r2<0.9
                                                NA, # then NA
                                           ifelse((co2.ex.aic < co2.lm.aic) & co2.ex.r2 < 0.8, # if lm is best, but r2<0.9
                                                  NA, # the NA
                                                  co2.drate.mg.h.best)), # otherwise assume value defined above
                                                  
              ch4.drate.mg.h.best = ifelse((ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic)) & ch4.lm.r2 < 0.9, # if ex is best, but r2<0.9
                                           NA, # then NA
                                           ifelse((ch4.ex.aic < ch4.lm.aic) & ch4.ex.r2 < 0.9, # if lm is best, but r2<0.9
                                                  NA, # the NA
                                                  ch4.drate.mg.h.best))) # otherwise assume value defined above

# Inspect r2 after scrubbing r2<0.9
plot(with(OUT[!is.na(OUT$co2.drate.mg.h.best),], 
          ifelse(co2.best.model == "linear", co2.lm.r2, co2.ex.r2)))  # CO2: all > 0.9
plot(with(OUT[!is.na(OUT$co2.drate.mg.h.best),], 
          ifelse(co2.best.model == "linear", abs(co2.lm.slope.err), abs(co2.ex.slope.err))))  # CO2: all > 0.9
          #3 with fractional error above 0.5
plot(with(OUT[!is.na(OUT$ch4.drate.mg.h.best),], 
          ifelse(ch4.best.model == "linear", ch4.lm.r2, ch4.ex.r2)))  # CH4: all > 0.9
plot(with(OUT[!is.na(OUT$ch4.drate.mg.h.best),], 
          ifelse(ch4.best.model == "linear", ch4.lm.slope.err, ch4.ex.slope.err))) 
          #2 with fractional error above 0.5
ggplot(OUT, aes(ch4.drate.mg.h.best*24*12/16))+ #mgC m-2 d-1
         geom_histogram(binwidth = 0.2)+
  xlim(0, 10)


# STEP 3: MERGE DIFFUSION RATES WITH eqAreaData

lakePowellData10<-mutate(lakePowellData10,
                         ch4.drate.mg.h = OUT$ch4.drate.mg.h.best,
                         co2.drate.mg.h = OUT$co2.drate.mg.h.best,
                         ch4.trate.mg.h = OUT$ch4.trate.mg.h,
                         co2.trate.mg.h = OUT$co2.trate.mg.h,
                         ch4.lm.slope.err = OUT$ch4.lm.slope.err,
                         ch4.ex.slope.err = OUT$ch4.ex.slope.err,
                         co2.lm.slope.err = OUT$co2.lm.slope.err,
                         co2.ex.slope.err = OUT$co2.ex.slope.err,
                         ch4.slope.err = ifelse(OUT$ch4.best.model == "linear", ch4.lm.slope.err, ch4.ex.slope.err),
                         co2.slope.err = ifelse(OUT$co2.best.model == "linear", abs(co2.lm.slope.err), abs(co2.ex.slope.err)),
                         elap.time = OUT$elap.time)

lakePowellData10$site<-ifelse(lakePowellData10$site == "camp1", "Camp 1", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "Camp2", "Camp 2", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "sjr confluence", "SJR Confluence", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "oak canyon", "Oak Canyon", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "navajo", "Navajo", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "wahweap", "Wahweap", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "crossing of the fathers", "Crossing of the Fathers", lakePowellData10$site)

###total (aka to determine ebullition) at the following sites:
#Camp2, Alcove, SJR Inlet, Colorado Inlet,  
#Sheep Canyon, Escalante Inflow, and Garces Island
lakePowellData10<-mutate(lakePowellData10,
                         ch4.drate.U=ch4.drate.mg.h*(1+ch4.slope.err),
                         ch4.drate.L=ch4.drate.mg.h*(1-ch4.slope.err),
                         co2.drate.U=co2.drate.mg.h*(1+co2.slope.err),
                         co2.drate.L=co2.drate.mg.h*(1-co2.slope.err),
                         ch4.erate = ifelse(site =="Camp 2" | site== "Alcove" |site == "SJR Inlet" | site== "Colorado Inlet" | 
                                              site =="Sheep Canyon" |site == "Escalante Inflow"|site== "Garces Island", 
                                            (ch4.trate.mg.h - ch4.drate.mg.h),
                                            NA),
                         co2.erate = ifelse(site =="Camp 2" | site== "Alcove" |site == "SJR Inlet" | site== "Colorado Inlet" | 
                                              site =="Sheep Canyon" |site == "Escalante Inflow"|site== "Garces Island", 
                                            (co2.trate.mg.h - co2.drate.mg.h),
                                            NA))

OUT10<-OUT

# # CALCULATE EBULLITION RATE FROM FUNNELS------------------
#  From chamber data, 7 sites: Camp2, Alcove, SJR Inlet, Colorado Inlet,  
#                              Sheep Canyon, Escalante Inflow, and Garces Island

lakePowellFunnel<-read_xlsx(paste(myWD, "input/ebFluxCalcsPowell.xlsx", sep="/"))
funMerge2<-data.frame(select(lakePowellFunnel, Site, CH4.funnel.flux, CO2.funnel.flux, N2O.funnel.flux))%>%
  filter(!is.na(funMerge2$CH4.funnel.flux))
funMerge2$site<-funMerge2$Site

lakePowellData10<-left_join(lakePowellData10, funMerge2, by="site")

#want to melt/cast this so I have one column of ebullitive emissions, one column
#assigning whether it was a chamber or funnel measurement

lakePowellEb<-select(lakePowellData10, site, CH4.funnel.flux, ch4.erate, CO2.funnel.flux, co2.erate)

lakePowellEb.m <- reshape2::melt(lakePowellEb, id.vars =  "site") %>% # melt, converts exetainer code to factor
  filter(!is.na(value))  # remove NAs
#simplify names
lakePowellEb.m$variable<-ifelse(lakePowellEb.m$variable=="ch4.erate",
                                lakePowellEb.m$variable=="CH4.chamber.flux",
                                lakePowellEb.m$variable == lakePowellEb.m$variable)

###############################################################################
############BACK OF ENVELOPE WHOLE LAKE CALC###################################
################################################################################

# Break lake into tributary and open water sections, based on overlap in where we saw high diffusive emissions and ebullition: 
#   Colorado arm to Hite Canyon : 26 km^2
      # Colorado Inlet
      # Sheep Canyon
      # Camp 3
      # Hite Canyon
# SJR arm to Alcove: 13.5 km^2
      # SJR Inlet
      # Alcove
# Escalante arm to Garces Island: 2.7 km^2
      # Escalante Inflow
      # Garces Island 
# Total lake surface area from the GIS Albers’s 84 projection shapefile I have: 520 km^2 - (26 + 13.5 + 2.7) = 477.8 km^2
      # Colorado Arm: Scorup, Good Hope Bay, Knowles, Moki, Bullfrog, Lake Canyon, Camp 4
      # Escalante Arm: Davis Gulch, LPESC030
      # SJR Arm: Upper Piute, Camp 2, LPSJR193, LPSJR070
      # Below the confluences: LPCR1169, SJR Confluence, Oak Canyon, Crossing of the Fathers, Navajo, Camp 1, Wahweap

##############Mean diffusive rate for tributary areas: -----

filter(lakePowellData10, site=="Colorado Inlet" | site == "Sheep Canyon" | site == "Camp 3" | site == "Hite Canyon"| site == "SJR Inlet"|site == "Alcove" | site == "Escalante Inflow" | site == "Garces Island") %>%
  summarize(mean.ch4 = mean(ch4.drate.mg.h, na.rm=TRUE),
            sd.ch4 = sd(ch4.drate.mg.h, na.rm=TRUE),
            n.ch4 = n(),
            fe.ch4 = sd.ch4/sqrt(n.ch4)/mean.ch4,
            mean.co2 = mean(co2.drate.mg.h, na.rm=TRUE),
            sd.co2 = sd(co2.drate.mg.h, na.rm=TRUE),
            n.co2 = n(),
            fe.co2 = sd.co2/sqrt(n.co2)/mean.co2)
# mean.ch4   sd.ch4 n.ch4    fe.ch4 mean.co2   sd.co2 n.co2   fe.co2
# 2.182499 3.402686    16 0.3897696 28.05389 65.96109    16 0.587807

#2.18 mg CH4 m-2 h-1 * 1000^2 m^2/km^2 * 42.2 km^2 * 24 hr/d * 90 d/ summer season
2.18*1000*1000*42.2*24*90 *10^-9 #10^-9 to convert from mg to g to kg to Mg = 198.7+/-77.29 Mg CH4 per summer 

################Mean diffusive rate for open water:
filter(lakePowellData10, site!="Colorado Inlet",
       site != "Sheep Canyon",
       site != "Camp 3", site != "Hite Canyon", site != "SJR Inlet",
       site != "Alcove", site != "Escalante Inflow", site != "Garces Island") %>%
  summarize(mean.ch4 = mean(ch4.drate.mg.h, na.rm=TRUE),
            sd.ch4 = sd(ch4.drate.mg.h, na.rm=TRUE),
            n.ch4 = n(),
            fe.ch4 = sd.ch4/sqrt(n.ch4)/mean.ch4,
            mean.co2 = mean(co2.drate.mg.h, na.rm=TRUE),
            sd.co2 = sd(co2.drate.mg.h, na.rm=TRUE),
            n.co2 = n(),
            fe.co2 = sd.co2/sqrt(n.co2)/mean.co2)
#   mean.ch4    sd.ch4 n.ch4    fe.ch4 mean.co2   sd.co2 n.co2    fe.co2
# 0.08837668 0.1238105    31 0.2516164 10.13487 27.25131    31 0.4829345

#0.08837668 mg CH4 m-2 h-1 * 1000^2 m^2/km^2 * 477.8 km^2 * 24 hr/d * 90 d/ summer season
0.08837668*1000*1000*477.8*24*90 *10^-9 #10^-9 to convert from mg to g to kg to Mg = 91.2+/- 22.8 Mg CH4 per summer 


#############Mean ebullitive rate for trib areas: -------
filter(lakePowellEb.m, site=="Colorado Inlet" |site=="Sheep Canyon" |site== "Camp 3" |site=="SJR Inlet" |site=="Alcove"
       |site=="Escalante Inflow" |site=="Garces Island" & (variable == "CH4.funnel.flux" | variable == "ch4.erate")) %>%
  summarize(mean.ch4 = mean(value, na.rm=TRUE),
            sd.ch4 = sd(value, na.rm=TRUE),
            n.ch4 = n(),
            fe.ch4 = sd.ch4/sqrt(n.ch4)/mean.ch4)
# mean.ch4   sd.ch4 n.ch4   fe.ch4
# 1.400959 21.33385    29 2.827775
#but, these are measurements from 7 of the 8 "tributary" areas -- we didn't see ebullition at Hite Canyon. 
1.4*7/8 #=1.225
#1.225 mg CH4 m-2 h-1 * 1000^2 m^2/km^2 * 42.2 km^2 * 24 hr/d * 90 d/ summer season
1.225*1000*1000*42.2*24*90 *10^-9 #10^-9 to convert from mg to g to kg to Mg = 112 Mg CH4 per summer  -- need to juj the Escalante calc before honing in on the se



#############Mean ebullitive rate for open water areas:
filter(lakePowellEb.m, site!="Colorado Inlet", site!="Sheep Canyon", site!= "Camp 3", site!="SJR Inlet", site!="Alcove",
       site!="Escalante Inflow", site!="Garces Island", site!= "Camp 4" & (variable == "CH4.funnel.flux" | variable == "ch4.erate")) %>%
  summarize(mean.ch4 = mean(value, na.rm=TRUE),
            sd.ch4 = sd(value, na.rm=TRUE),
            n.ch4 = n(),
            fe.ch4 = sd.ch4/sqrt(n.ch4)/mean.ch4)
# With Camp 4:
# mean.ch4   sd.ch4 n.ch4    fe.ch4
# 1.392925 3.342951    14 0.6414138

# Without Camp 4:
#  mean.ch4     sd.ch4 n.ch4    fe.ch4
# 0.0780283 0.06545584    12 0.2421618
#but there are 20 open water sites, and we only have measurements from 3 of them: Wahweap, Camp 2, and Camp 4
  #With Camp 4:
1.3929*3/20 #=0.2089
  #without Camp 4:
0.0780283*2/19 #= 0.008213505
  
  #with Camp 4:
#0.2089 mg CH4 m-2 h-1 * 1000^2 m^2/km^2 * 477.8 km^2 * 24 hr/d * 90 d/ summer season
0.2089*1000*1000*477.8*24*90 *10^-9 #10^-9 to convert from mg to g to kg to Mg = 215.6 +/- 138 Mg CH4 per summer 
  #without Camp 4:
0.008213505*1000*1000*477.8*24*90 *10^-9 #=8.4767
#Total sum of CH4 emissions:
  #with Camp 4:
198.7+91.2+128+215.6 # = 633.5 Mg CH4 per summer 
  #without Camp 4:
198.7+91.2+128+8.4767 #=426.38 Mg CH4 per summer
 

##############CO2####################################
lakePowellBySite$co2Section<-c("SJR Uptake", "CO Uptake", "Dam Emission", 
                               "SJR Uptake", "CO Emission", "CO Uptake",
                               "CO Emission", "NA", "Dam Emission",
                               "Esc Emission", "Esc Emission", "Esc Emission",
                               "Net Zero", "CO Emission", "CO Uptake",
                               "CO Uptake", "Net Zero", "Net Zero", 
                               "Net Zero", "SJR Uptake", "CO Uptake",
                               "Dam Emission", "Net Zero", "Net Zero", 
                               "CO Emission", "Net Zero", "SJR Emission",
                               "SJR Uptake", "Dam Emission"
                               )


filter(lakePowellBySite, co2Section == "Dam Emission")%>%
       # site=="Crossing of the Fathers" | site=="Camp 1" |
        #  site == "Navajo" | site=="Wahweap")%>%
  summarize(mean.co2 = mean(co2.drate.mg.h, na.rm=TRUE),
            sd.co2 = sd(co2.drate.mg.h, na.rm=TRUE),
            n.co2 = n(),
            fe.co2 = sd.co2/sqrt(n.co2)/mean.co2)

#  mean.co2   sd.co2 n.co2     fe.co2
# 34.46831 4.165377     4 0.06042329

filter(lakePowellBySite, co2Section == "SJR Uptake")%>%
  # site=="Crossing of the Fathers" | site=="Camp 1" |
  #  site == "Navajo" | site=="Wahweap")%>%
  summarize(mean.co2 = mean(co2.drate.mg.h, na.rm=TRUE),
            sd.co2 = sd(co2.drate.mg.h, na.rm=TRUE),
            n.co2 = n(),
            fe.co2 = sd.co2/sqrt(n.co2)/mean.co2)

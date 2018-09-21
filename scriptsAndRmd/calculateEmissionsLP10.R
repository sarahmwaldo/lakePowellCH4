


# EMISSION RATE CALCULATIONS--------------------
# STEP 1:  CALCULATE EMISSION RATE VIA LINEAR AND NONLINEAR REGRESSION
#          FOR SITES WHERE PERIODS OF LINEAR ACCUMULATION ARE INDICATED 
# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OF NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# STEP 3: MERGE WITH eqAreaData

#Bridget is testing GitHub
lakePowellData10.orig<-lakePowellData10
lakePowellData10<-filter(lakePowellData10.orig, site!="SJR Inlet End", #SJR inlet end was recorded for the lat/long, but isn't a separate observation
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
  chmVol.i <- lakePowellData10$chm_vol[i] 
  


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
  
  OUT[i, "elap.time"]<-dt.i
  
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



############################################333
  ########################################### --------
  
  

# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OR NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# Choose best rate.  
#SW added 6/29/2018: use AIC only if difference in AIC scores are >10%, otherwise, use linear model
OUT <- mutate(OUT, 
              co2.best.model = ifelse(abs((co2.lm.aic-co2.ex.aic)/((co2.lm.aic+co2.ex.aic)/2))>0.1 & co2.lm.aic > co2.ex.aic | is.na(co2.ex.aic), 
                                      "exponential", "linear"),
              co2.drate.mg.h.best = ifelse(co2.best.model == "linear",
                                           co2.lm.drate.mg.h, co2.ex.drate.mg.h),
              ch4.best.model = ifelse(abs((ch4.lm.aic-ch4.ex.aic)/((ch4.lm.aic+ch4.ex.aic)/2))>0.1 & ch4.lm.aic > ch4.ex.aic | is.na(ch4.ex.aic), 
                                      "exponential","linear"),
              ch4.drate.mg.h.best = ifelse(ch4.best.model == "linear",
                                           ch4.lm.drate.mg.h, ch4.ex.drate.mg.h)) 
#9/7/18: this may be making a bigger difference in the final emission values than I realized. look at the data with using AIC
OUT <- mutate(OUT, 
              co2.best.model = ifelse(co2.lm.aic > co2.ex.aic | is.na(co2.ex.aic), 
                                      "exponential", "linear"),
              co2.drate.mg.h.best = ifelse(co2.best.model == "linear",
                                           co2.lm.drate.mg.h, co2.ex.drate.mg.h),
              ch4.best.model = ifelse(ch4.lm.aic > ch4.ex.aic | is.na(ch4.ex.aic), 
                                      "exponential","linear"),
              ch4.drate.mg.h.best = ifelse(ch4.best.model == "linear",
                                           ch4.lm.drate.mg.h, ch4.ex.drate.mg.h)) 

# Inspect r2 and fractional error of slope
  #CO2 R2
plot(with(OUT,ifelse(co2.best.model == "linear", co2.lm.r2, co2.ex.r2)))  # CO2: some low ones to investigate
  #CO2 fractional error:
plot(with(OUT, ifelse(co2.best.model == "linear", abs(co2.lm.slope.err), abs(co2.ex.slope.err))))
  #ch4 R2:
plot(with(OUT,ifelse(ch4.best.model == "linear", ch4.lm.r2, ch4.ex.r2)))  # CH4:  some low ones to investigate
  #ch4 fractional error:
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
plot(with(OUT[!is.na(OUT$co2.drate.mg.h.best),], #R2
          ifelse(co2.best.model == "linear", co2.lm.r2, co2.ex.r2)))  # CO2: all > 0.9
plot(with(OUT[!is.na(OUT$co2.drate.mg.h.best),], #fractional error
          ifelse(co2.best.model == "linear", abs(co2.lm.slope.err), abs(co2.ex.slope.err))))  # CO2: all > 0.9
          #1 with fractional error >>0.2
plot(with(OUT[!is.na(OUT$ch4.drate.mg.h.best),], #R2
          ifelse(ch4.best.model == "linear", ch4.lm.r2, ch4.ex.r2)))  # CH4: all > 0.9
plot(with(OUT[!is.na(OUT$ch4.drate.mg.h.best),], #fractional error
          ifelse(ch4.best.model == "linear", ch4.lm.slope.err, ch4.ex.slope.err))) 
          #2 with fractional error above 0.1
#histogram of the methane diffusive emission rate in units of mg C m-2 d-1
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
#edit inconsistencies in site names:
lakePowellData10$site<-ifelse(lakePowellData10$site == "camp1", "Camp 1", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "Camp2", "Camp 2", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "sjr confluence", "SJR Confluence", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "oak canyon", "Oak Canyon", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "navajo", "Navajo", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "wahweap", "Wahweap", lakePowellData10$site)
lakePowellData10$site<-ifelse(lakePowellData10$site == "crossing of the fathers", "Crossing of the Fathers", lakePowellData10$site)

lakePowellData10$Station.ID<-c("LPCR1","LPCR1","LPCR0024","LPCR0024","LPCR0155","LPCR0453","LPCR0905","LPCR1001","LPSJR070","LPSJR193",
                               "LPCR2","LPCR2","LPCR2","LPSJR431","LPSJR530","LPSJR530","LPSJRINF","LPCR3","LPCR3","LPCR3","LPCR3",
                               "LPCR3","LPCR3","LPCRINF","LPCRINF","LPCRINF2","LPCRsheep","LPCR2387","LPCR2255","LPCR2085","LPCR1933",
                               "LPCR1799","LPCR1679","LPCR1587","LPCR4","LPCR4","LPESCINF","LPESC273","LPESC119","LPESC030","LPCR1169",
                               "LPCR0024","LPCR0453","LPCRsheep")
#check Station.ID and site match
translationKey<-select(lakePowellData10, site, Station.ID)
  

###total (aka to determine ebullition) at the following sites:
#Camp2, Alcove, SJR Inlet, Colorado Inlet,  
#Sheep Canyon, Escalante Inflow, and Garces Island
#calculate ebullitive emissions as total - diffusive
#while we're mutating the data frame, lets calculate upper (".U") and lower (".L") estimates for the diffusive rate to be used in plotting the error bars 
lakePowellData10<-mutate(lakePowellData10,
                         ch4.erate = ifelse(site =="Camp 2" | site== "Alcove" |site == "SJR Inlet" | site== "Colorado Inlet" | 
                                              site =="Sheep Canyon" |site == "Escalante Inflow"|site== "Garces Island", 
                                            (ch4.trate.mg.h - ch4.drate.mg.h),
                                            0),
                         co2.erate = ifelse(site =="Camp 2" | site== "Alcove" |site == "SJR Inlet" | site== "Colorado Inlet" | 
                                              site =="Sheep Canyon" |site == "Escalante Inflow"|site== "Garces Island", 
                                            (co2.trate.mg.h - co2.drate.mg.h),
                                            0),
                         ch4.drate.U=ch4.drate.mg.h*(1+ch4.slope.err),
                         ch4.drate.L=ch4.drate.mg.h*(1-ch4.slope.err),
                         co2.drate.U=co2.drate.mg.h*(1+co2.slope.err),
                         co2.drate.L=co2.drate.mg.h*(1-co2.slope.err))

OUT10<-OUT

# # CALCULATE EBULLITION RATE FROM FUNNELS------------------
#  From chamber data, 7 sites: Camp2, Alcove, SJR Inlet, Colorado Inlet,  
#                              Sheep Canyon, Escalante Inflow, and Garces Island

lakePowellFunnel<-read_xlsx(paste(myWD, "input/ebFluxCalcsPowell.xlsx", sep="/"))
lakePowellFunnel$site<-lakePowellFunnel$Site
lakePowellFunnel<-(select(lakePowellFunnel, site, CH4.funnel.flux, CO2.funnel.flux, N2O.funnel.flux))%>%
  filter(!is.na(lakePowellFunnel$CH4.funnel.flux))


#add station ID's to this data frame, assigning LPCR5 and LPCR6 to WAH01 and WAH02, respectively
lakePowellFunnel$Station.ID<-c("LPCR4", "LPCR3", "LPCR2", 
                               "LPCR0024", "LPCR0024", "LPCR1")
lakePowellFunnel<-select(lakePowellFunnel, -site)
#summarise with mean values for each site

lakePowellFunnelsum<-lakePowellFunnel %>%
  group_by(Station.ID)%>%
  summarise_all(mean)

lakePowellData10sum<-lakePowellData10 %>% #this step eliminates everything that can't be averaged
  group_by(Station.ID)%>%
  summarise_all(mean)

#merge with the main data frame 
lakePowellData10<-merge(lakePowellData10sum, lakePowellFunnelsum, 
                            by.x="Station.ID", by.y="Station.ID", all=TRUE)
#unique(lakePowellData10test$site)

#rename so naming makes more sense in the melted dataframe
lakePowellData10<-mutate(lakePowellData10,
                         CH4.chamber.eb=ch4.erate,
                         CH4.funnel.eb=CH4.funnel.flux,
                         CO2.chamber.eb=co2.erate,
                         CO2.funnel.eb=CO2.funnel.flux)
#####9/7/2018 -- we don't want to do this SW----
# #want to melt/cast this so I have one column of ebullitive emissions, one column
# #assigning whether it was a chamber or funnel measurement
# lakePowellMethEb<-select(lakePowellData10, RDateTime, CH4.funnel.eb, 
#                      CH4.chamber.eb)
# lakePowellCO2Eb<-select(lakePowellData10, RDateTime, CO2.funnel.eb, CO2.chamber.eb)
# 
# lakePowellMethEb.m <- reshape2::melt(lakePowellMethEb, id.vars =  "RDateTime") %>% # melt, converts measurement code to factor
#   filter(!is.na(value))  # remove NAs
# lakePowellCO2Eb.m <- reshape2::melt(lakePowellCO2Eb, id.vars =  "RDateTime") %>% # melt, converts measurement code to factor
#   filter(!is.na(value))  # remove NAs
# #prep for meerging with lakePowellData10
# lakePowellMethEb.m<-mutate(lakePowellMethEb.m,
#                        methEbMethod = variable,
#                        methEbullition = value)
# lakePowellCO2Eb.m<-mutate(lakePowellCO2Eb.m,
#                            co2ebMethod = variable,
#                            co2Ebullition = value)
# lakePowellData10t3<-left_join(lakePowellData10, lakePowellMethEb.m, by="RDateTime")
# lakePowellData10<-left_join(lakePowellData10, lakePowellCO2Eb.m, by="RDateTime")

#it doesn't look like any fluxes are popping up in the column "CH4.chamber.eb"
# lakePowellData10<-lakePowellData10 %>%
#   mutate(MethEbullition=ifelse(CH4.funnel.flux>0,CH4.funnel.flux,0))
#####------

<<<<<<< HEAD
lakePowellData10$site<-translationKey[match(lakePowellData10$Station.ID, translationKey$Station.ID), 1]
#select(lakePowellData10, Station.ID, site)

lakePowellDataSubset<-select(lakePowellData10, Station.ID, site, lat, long, RDateTime, 
||||||| merged common ancestors
lakePowellDataSubset<-select(lakePowellData10, Station.ID, site, rep, lat, long, RDateTime, 
=======
lakePowellData10$site<-translationKey[match(lakePowellData10$Station.ID, translationKey$Station.ID), 1]
select(lakePowellData10, Station.ID, site)

lakePowellDataSubset<-select(lakePowellData10, Station.ID, site, rep, lat, long, RDateTime, 
>>>>>>> 34c6a6b19037902c1fa6beb121f9fef3e869f0c4
                             ch4.drate.mg.h, co2.drate.mg.h, ch4.trate.mg.h, co2.trate.mg.h, 
                             ch4.slope.err, co2.slope.err, ch4.drate.U, ch4.drate.L, co2.drate.U, co2.drate.L,
                             CH4.chamber.eb, CH4.funnel.eb, CO2.chamber.eb, CO2.funnel.eb)
#Now make vectors of CH4 and CO2 ebullition best number for each site
#then we can calculate total emissions for each site

  #the only site with BOTH chamber and funnel eb msmts is Camp 2, 
  #otherwise, we can pick from which msmt is available:
lakePowellDataSubset$CH4.eb.best<-ifelse(
  is.na(lakePowellDataSubset$CH4.funnel.eb),
  lakePowellDataSubset$CH4.chamber.eb, #value if funnel eb is NA
  lakePowellDataSubset$CH4.funnel.eb) #value if it is a number
                                                       
lakePowellDataSubset$CO2.eb.best<-ifelse(
  is.na(lakePowellDataSubset$CO2.funnel.eb),
  lakePowellDataSubset$CO2.chamber.eb, #value if funnel eb is NA
  lakePowellDataSubset$CO2.funnel.eb) #value if it is a number         

#Do we trust the funnel numbers more than the chamber numbers?
#If not, we should average the measurements for Camp 2
camp2co2<-mean(1.071453, 0.018879, 13.13)
camp2ch4<-mean(0.1698103, 0.141869, 0.0322)

lakePowellDataSubset<-mutate(lakePowellDataSubset,
                             CH4.eb.best = replace(CH4.eb.best, 
                                                   site == "Camp 2",
                                                   camp2ch4),
                             CO2.eb.best = replace(CO2.eb.best, 
                                                   site == "Camp 2",
                                                   camp2co2))
###now we can calculate total emissions:


lakePowellDataSubset<-lakePowellDataSubset%>%
  rowwise()%>%
  mutate(CH4.trate.best = sum(ch4.drate.mg.h, CH4.eb.best, na.rm = TRUE),
         CO2.trate.best = sum(co2.drate.mg.h, CO2.eb.best, na.rm = TRUE))

#Now make a vector that just has the depth for each site added into the lakePowellData1

write.table(lakePowellDataSubset, 
            file=paste(myWD, "output/lakePowellFluxes.csv", sep="/"),
            sep=",",
            row.names=FALSE)






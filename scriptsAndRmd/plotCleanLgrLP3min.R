# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#1. INSPECT INSTANCES OF NA IN GGA
# Time/date stamp first
#filter(gga, is.na(RDateTime))
# there are a bunch of NA for this field, likely related to corrupt LGR files.  Will just strip out for now.
gga <- filter(gga, !is.na(RDateTime))  # strip out missing RDateTime.  They complicate functions below.


#2. LOAD THE EXCEL FILE THAT HAS SITE NAME, LAT, LONG, DEPLOYMENT DATE, CHAMBER START TIME, AND CHAMBER VOLUME
lakePowellData3<-read.csv(paste(myWD, "input/dataEntryQC3min.csv", sep="/"), #omitted two chamber deployments when the fan wasn't on
                         header=TRUE,
                         colClasses=c(rep("character",4), "numeric", "numeric", "character", "character", "integer", "character"))
#convert the deployment date, chmStTm columns to an RDateTime column:
lakePowellData3$RDateTime <- as.POSIXct(paste(lakePowellData3$deplyDt, lakePowellData3$chmStTm, sep=""),
                              format="%m/%d/%Y%H:%M:%S",
                              tz = "UTC")  # POSIXct
#head(lakePowellData3$RDateTime)
#str(lakePowellData3)
#tail(lakePowellData3)

lakePowellData3$startTime<-lakePowellData3$RDateTime 
lakePowellData3$endTime<-lakePowellData3$RDateTime+(60*3)  #chamber deployments were nominally 10 minutes; breaking them into 3-min chunks


#3. UPDATE DEPLOYMENT AND RETRIEVAL TIMES 

lakePowellData3$diffStartTime<-lakePowellData3$startTime
lakePowellData3$diffEndTime<-lakePowellData3$endTime




### 3-min adjustments: ----
lakePowellData3$diffEndTime[4]<-as.POSIXct("2017-07-17 23:19:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[9]<-as.POSIXct("2017-07-18 11:14:40", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[11]<-as.POSIXct("2017-07-18 11:31:15", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[18]<-as.POSIXct("2017-07-18 14:41:40", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[30]<-as.POSIXct("2017-07-18 21:46:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[35]<-as.POSIXct("2017-07-19 10:12:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[43]<-as.POSIXct("2017-07-19 12:15:25", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[44]<-as.POSIXct("2017-07-19 12:20:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[49]<-as.POSIXct("2017-07-19 14:48:15", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[55]<-as.POSIXct("2017-07-19 22:47:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[61]<-as.POSIXct("2017-07-20 09:43:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[72]<-as.POSIXct("2017-07-20 12:24:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[73]<-as.POSIXct("2017-07-20 12:26:15", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[74]<-as.POSIXct("2017-07-20 12:29:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[80]<-as.POSIXct("2017-07-20 14:05:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[83]<-as.POSIXct("2017-07-20 15:30:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[86]<-as.POSIXct("2017-07-20 16:14:20", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[92]<-as.POSIXct("2017-07-20 18:52:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[97]<-as.POSIXct("2017-07-20 21:30:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[105]<-as.POSIXct("2017-07-21 09:48:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[105]<-as.POSIXct("2017-07-21 09:48:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[109]<-as.POSIXct("2017-07-21 13:43:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[113]<-as.POSIXct("2017-07-21 14:39:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData3$diffEndTime[118]<-as.POSIXct("2017-07-21 16:54:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")

 
####----

#4. PLOT CO2 AND CH4 PROFILES FOR USE IN DIFFUSIVE FLUX CALCULATIONS

###### ----------------

pdf(paste(myWD, "output/ggaProfileLPDiffFluxQc3.pdf", sep="/"), paper = "a4r") # landscape orientation

for (i in 1:length(lakePowellData3$chmStTm)) {  # each chamber deployment
  
  data.i <- filter(gga, RDateTime>(lakePowellData3$diffStartTime[i]-30), (RDateTime<lakePowellData3$diffEndTime[i]+30)) %>%  # Pull out GGA data chunk
    select(-GasT_C) # No need to plot gas temperature
  RDate.i <- unique(data.i$RDate)  # for panel title
  site.i <- (lakePowellData3$site[i])
  
  plot.i <- ggplot(data.i,  aes(x = RDateTime, y = CH4._ppm)) + 
    geom_point() +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData3$diffStartTime[i]))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData3$diffEndTime[i]))) +
    scale_x_datetime(labels=date_format("%H:%M")) +
    ggtitle(paste(site.i, RDate.i)) +
    theme(axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 11))
  
  plot.ii <- ggplot(data.i,  aes(x = RDateTime, y = CO2._ppm)) + 
    geom_point() +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData3$diffStartTime[i]))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData3$diffEndTime[i]))) +
    scale_x_datetime(labels=date_format("%H:%M")) +
    ggtitle(paste(site.i)) +
    theme(axis.text.x = element_text(size = 7))
  
  grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
}


dev.off()











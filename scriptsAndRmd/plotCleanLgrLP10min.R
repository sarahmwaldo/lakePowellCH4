# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#1. INSPECT INSTANCES OF NA IN GGA
# Time/date stamp first
#filter(gga, is.na(RDateTime))
# there are a bunch of NA for this field, likely related to corrupt LGR files.  Will just strip out for now.
gga <- filter(gga, !is.na(RDateTime))  # strip out missing RDateTime.  They complicate functions below.


#2. LOAD THE EXCEL FILE THAT HAS SITE NAME, LAT, LONG, DEPLOYMENT DATE, CHAMBER START TIME, AND CHAMBER VOLUME
lakePowellData10<-read.csv(paste(myWD, "input/dataEntryQCV2.csv", sep="/"), #omitted two chamber deployments when the fan wasn't on
                         header=TRUE,
                         colClasses=c(rep("character",1), "numeric","numeric", "numeric", "character", "character", "integer"))
#convert the deployment date, chmStTm columns to an RDateTime column:
lakePowellData10$RDateTime <- as.POSIXct(paste(lakePowellData10$deplyDt, lakePowellData10$chmStTm, sep=""),
                              format="%m/%d/%Y%H:%M:%S",
                              tz = "UTC")  # POSIXct
#head(lakePowellData10$RDateTime)
#str(lakePowellData10)
#tail(lakePowellData10)

lakePowellData10$startTime<-lakePowellData10$RDateTime 
lakePowellData10$endTime<-lakePowellData10$RDateTime+(60*10)  #chamber deployments were nominally 10 minutes; breaking them into 3-min chunks


#3. UPDATE DEPLOYMENT AND RETRIEVAL TIMES 

#diffusion:
lakePowellData10$diffStartTime<-lakePowellData10$startTime
lakePowellData10$diffEndTime<-lakePowellData10$endTime
#####10-min diffusive flux adjustments: ----
lakePowellData10$diffStartTime[1]<-as.POSIXct("2017-07-17 23:02:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[2]<-as.POSIXct("2017-07-17 23:18:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[2]<-as.POSIXct("2017-07-17 23:22:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[3]<-as.POSIXct("2017-07-18 11:05:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[3]<-as.POSIXct("2017-07-18 11:09:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[4]<-as.POSIXct("2017-07-18 11:27:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[4]<-as.POSIXct("2017-07-18 11:31:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[5]<-as.POSIXct("2017-07-18 13:03:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[5]<-as.POSIXct("2017-07-18 13:08:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[6]<-as.POSIXct("2017-07-18 14:33:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[6]<-as.POSIXct("2017-07-18 14:37:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[7]<-as.POSIXct("2017-07-18 18:53:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[7]<-as.POSIXct("2017-07-18 19:01:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[8]<-as.POSIXct("2017-07-18 20:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[9]<-as.POSIXct("2017-07-18 20:48:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[9]<-as.POSIXct("2017-07-18 20:52:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[10]<-as.POSIXct("2017-07-18 21:37:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[11]<-as.POSIXct("2017-07-19 09:55:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC") # Camp 2
lakePowellData10$diffEndTime[11]<-as.POSIXct("2017-07-19 09:58:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[12]<-as.POSIXct("2017-07-19 10:07:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC") # Camp 2
lakePowellData10$diffEndTime[12]<-as.POSIXct("2017-07-19 10:12:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[13]<-as.POSIXct("2017-07-19 10:20:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")  # Camp 2
lakePowellData10$diffEndTime[13]<-as.POSIXct("2017-07-19 10:25:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[14]<-as.POSIXct("2017-07-19 11:29:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[15]<-as.POSIXct("2017-07-19 12:14:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[15]<-as.POSIXct("2017-07-19 12:15:15", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[16]<-as.POSIXct("2017-07-19 12:23:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[16]<-as.POSIXct("2017-07-19 12:26:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[17]<-as.POSIXct("2017-07-19 14:40:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[17]<-as.POSIXct("2017-07-19 14:44:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[18]<-as.POSIXct("2017-07-19 14:44:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[18]<-as.POSIXct("2017-07-19 14:48:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[19]<-as.POSIXct("2017-07-19 22:25:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC") # Camp 3
lakePowellData10$diffStartTime[20]<-as.POSIXct("2017-07-19 22:37:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC") # Camp 3
lakePowellData10$diffStartTime[21]<-as.POSIXct("2017-07-19 22:53:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC") # Camp 3
lakePowellData10$diffEndTime[22]<-as.POSIXct("2017-07-20 09:42:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[23]<-as.POSIXct("2017-07-20 09:45:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[26]<-as.POSIXct("2017-07-20 12:21:00" , format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[26]<-as.POSIXct("2017-07-20 12:24:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[27]<-as.POSIXct("2017-07-20 12:31:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[28]<-as.POSIXct("2017-07-20 12:31:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[28]<-as.POSIXct("2017-07-20 12:37:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[29]<-as.POSIXct("2017-07-20 13:56:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[29]<-as.POSIXct("2017-07-20 13:59:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[30]<-as.POSIXct("2017-07-20 15:20:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[31]<-as.POSIXct("2017-07-20 16:06:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[32]<-as.POSIXct("2017-07-20 17:14:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[33]<-as.POSIXct("2017-07-20 18:43:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[34]<-as.POSIXct("2017-07-20 19:30:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[34]<-as.POSIXct("2017-07-20 19:33:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[35]<-as.POSIXct("2017-07-20 21:21:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[35]<-as.POSIXct("2017-07-20 21:28:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[36]<-as.POSIXct("2017-07-20 22:28:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[36]<-as.POSIXct("2017-07-20 22:32:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[38]<-as.POSIXct("2017-07-21 09:40:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[39]<-as.POSIXct("2017-07-21 12:21:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[40]<-as.POSIXct("2017-07-21 13:42:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[40]<-as.POSIXct("2017-07-21 13:43:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[41]<-as.POSIXct("2017-07-21 14:30:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[42]<-as.POSIXct("2017-07-21 16:20:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[42]<-as.POSIXct("2017-07-21 16:24:50", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffStartTime[43]<-as.POSIXct("2017-07-21 16:46:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[44]<-as.POSIXct("2017-07-18 11:13:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[45]<-as.POSIXct("2017-07-18 14:41:50", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$diffEndTime[46]<-as.POSIXct("2017-07-20 14:04:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")


###total (aka to determine ebullition) at the following sites:
#Camp2, Alcove, SJR Inlet, Colorado Inlet,  
#Sheep Canyon, Escalante Inflow, and Garces Island
lakePowellData10$totStartTime<-lakePowellData10$diffStartTime
lakePowellData10$totEndTime<-lakePowellData10$diffEndTime

#Camp 2:
lakePowellData10$totEndTime[11]<-lakePowellData10$endTime[11]
lakePowellData10$totEndTime[12]<-lakePowellData10$endTime[12]
lakePowellData10$totEndTime[13]<-lakePowellData10$endTime[13]
#Alcove:
lakePowellData10$totEndTime[15]<-as.POSIXct("2017-07-19 12:20:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$totEndTime[16]<-as.POSIXct("2017-07-19 12:29:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
#SJR Inlet:
lakePowellData10$totEndTime[17]<-as.POSIXct("2017-07-19 14:49:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
#Colorado Inlet:
lakePowellData10$totStartTime[26]<-as.POSIXct("2017-07-20 12:25:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
lakePowellData10$totEndTime[26]<-as.POSIXct("2017-07-20 12:27:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
#Sheep Canyon:
lakePowellData10$totEndTime[29]<-lakePowellData10$endTime[29]
#Escalante Inflow:
lakePowellData10$totEndTime[39]<-as.POSIXct("2017-07-21 12:45:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
#Garces Island:
lakePowellData10$totEndTime[40]<-as.POSIXct("2017-07-21 13:46:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")



# ----

###6/27/2018: SW added red vlines that indicated the start and end points
###           for the total emission calculation

pdf(paste(myWD, "output/ggaProfileLPDiffFluxQc10.pdf", sep="/"), paper = "a4r") # landscape orientation

for (i in 1:length(lakePowellData10$chmStTm)) {  # each chamber deployment
  
  data.i <- filter(gga, RDateTime>(lakePowellData10$diffStartTime[i]-30), (RDateTime<lakePowellData10$totEndTime[i]+30)) %>%  # Pull out GGA data chunk
    select(-GasT_C) # No need to plot gas temperature
  RDate.i <- unique(data.i$RDate)  # for panel title
  site.i <- (lakePowellData10$site[i])
  
  plot.i <- ggplot(data.i,  aes(x = RDateTime, y = CH4._ppm)) + 
    geom_point() +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData10$diffStartTime[i]))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData10$diffEndTime[i]))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData10$totStartTime[i]), color="total")) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData10$totEndTime[i]), color="total")) +
    scale_x_datetime(labels=date_format("%H:%M")) +
    ggtitle(paste(site.i, RDate.i)) +
    theme(axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 11))
  
  plot.ii <- ggplot(data.i,  aes(x = RDateTime, y = CO2._ppm)) + 
    geom_point() +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData10$diffStartTime[i]))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData10$diffEndTime[i]))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData10$totStartTime[i]), color="total")) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(lakePowellData10$totEndTime[i]), color="total")) +
    scale_x_datetime(labels=date_format("%H:%M")) +
    ggtitle(paste(site.i)) +
    theme(axis.text.x = element_text(size = 7))
  
  grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
}


dev.off()











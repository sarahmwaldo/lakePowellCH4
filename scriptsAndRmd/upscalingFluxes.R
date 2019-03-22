###############################################################################
############ Upscaling to whole-lake ###################################
####################  2019 03 22    ############################################################
######################################################################################
# Break lake into littoral and open water sections,
# based on knowledge of CH4 emissions ~ pH, phosphorus
# Ends up being sites <15 m deep, representing a lake surface area of 33.2 km2
    # Colorado Inlet, Sheep Canyon, Camp 3, Hite Canyon, SJR Inlet, Alcove, 
    # Escalante Inflow, and Garces Island

# Other sites represent the rest of the lake, 467 - 33.2 km2, or 433.8 km2
    # Scorup, Good Hope Bay, Knowles, Moki, Bullfrog, Lake Canyon, Camp 4
    # Davis Gulch, LPESC030
    # Upper Piute, Camp 2, LPSJR193, LPSJR070
    # LPCR1169, SJR Confluence, Oak Canyon, Crossing of the Fathers, Navajo, Camp 1, Wahweap

##############Mean diffusive rate for tributary areas: -----

LP.littoral<-filter(lakePowellMeans, site=="Colorado Inlet" | site == "Sheep Canyon" | 
                      site == "Camp 3" | site == "Hite Canyon"| 
                      site == "SJR Inlet"|site == "Alcove" | site == "Escalante Inflow" | 
                      site == "Garces Island")
summary(LP.littoral)

LP.openWater<-filter(lakePowellMeans, site!="Colorado Inlet",
                     site != "Sheep Canyon",
                     site != "Camp 3", site != "Hite Canyon", site != "SJR Inlet",
                     site != "Alcove", site != "Escalante Inflow", site != "Garces Island")
summary(LP.openWater)

summary(lakePowellMeans)


#####Need to hard code the emission rate calcs for table 3



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
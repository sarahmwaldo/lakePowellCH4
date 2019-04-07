###############################################################################
############ Upscaling to whole-lake ###################################
####################  2019 03 22    ############################################################
######################################################################################
# Break lake into littoral and open water sections,
# based on knowledge of CH4 emissions ~ pH, phosphorus
# Ends up being sites <15 m deep, representing a lake surface area of 33.2 km2
    # Colorado Inlet, Sheep Canyon, Camp 3, Hite Canyon, SJR Inlet, Alcove, 
    # Escalante Inflow, and Garces Island

# Other sites represent the rest of the lake, 467-33.2 km2, or 433.8 km2
    # Scorup, Good Hope Bay, Knowles, Moki, Bullfrog, Lake Canyon, Camp 4
    # Davis Gulch, LPESC030
    # Upper Piute, Camp 2, LPSJR193, LPSJR070
    # LPCR1169, SJR Confluence, Oak Canyon, Crossing of the Fathers, Navajo, Camp 1, Wahweap

###If starting with this code:
lakePowellMeans<-read.csv(paste(myWD, "output/lakePowellFluxes20190322.csv", sep="/"))

##############Mean diffusive rate for tributary areas: -----

LP.littoral<-filter(lakePowellMeans, site=="Colorado Inlet" | site == "Sheep Canyon" | 
                      site == "Camp 3" | site == "Hite Canyon"| 
                      site == "SJR Inlet"|site == "Alcove" | site == "Escalante Inflow" | 
                      site == "Garces Island")
summary(LP.littoral)
ggplot(LP.littoral, aes(CH4trate.mean))+
  geom_histogram()

LP.littoral.summary<-LP.littoral%>%
  summarize(meanCH4lit = mean(CH4trate.mean),
            sdCH4lit = sd(log(CH4trate.mean)), #log is the natural log transformation, since the msmts are log-normally distributed
            nCH4lit = n(),
            seCH4lit = sdCH4lit/sqrt(nCH4lit),
            CH4lower = exp(log(meanCH4lit)-seCH4lit),
            CH4upper = exp(log(meanCH4lit)+seCH4lit),
            fracErrlit.L = abs(meanCH4lit-CH4lower)/meanCH4lit*100,
            fracErrlit.U = abs(meanCH4lit-CH4upper)/meanCH4lit*100)
            
meanCH4lit<-mean(LP.littoral$CH4trate.mean) #mean CH4 emission rate from littoral sites in mg CH4 m-2 hr-1
#convert from mg CH4 to kg CH4; m2 to km2; then upscaling to total littoral area (33.2 km2); convert from hr-1 to y-1
totCH4.lit.kg.y<-meanCH4lit/10^6*1000^2*33.2*24*365 #units: kg CH4 y-1 
ErrCH4L.lit.kg.y<-LP.littoral.summary$fracErrlit.L/100*totCH4.lit.kg.y
ErrCH4U.lit.kg.y<-LP.littoral.summary$fracErrlit.U/100*totCH4.lit.kg.y


LP.openWater<-filter(lakePowellMeans, site!="Colorado Inlet",
                     site != "Sheep Canyon",
                     site != "Camp 3", site != "Hite Canyon", site != "SJR Inlet",
                     site != "Alcove", site != "Escalante Inflow", site != "Garces Island")
summary(LP.openWater)
ggplot(LP.openWater, aes(log(CH4trate.mean)))+
  geom_histogram(bins=20)

LP.openWater.summary<-LP.openWater%>%
  summarize(meanCH4ow = mean(CH4trate.mean),
            sdCH4ow = sd(log(CH4trate.mean)), #log is the natural log transformation, since the msmts are log-normally distributed
            nCH4ow = n(),
            seCH4ow = sdCH4ow/sqrt(nCH4ow),
            CH4lower = exp(log(meanCH4ow)-seCH4ow),
            CH4upper = exp(log(meanCH4ow)+seCH4ow),
            fracErrOw.L = abs(meanCH4ow-CH4lower)/meanCH4ow*100,
            fracErrOw.U = abs(meanCH4ow-CH4upper)/meanCH4ow*100)

meanCH4ow<-mean(LP.openWater$CH4trate.mean) #mean CH4 emission rate from open water sites in mg CH4 m-2 hr-1
#convert from mg CH4 to kg CH4; m2 to km2; then upscaling to total littoral area (33.2 km2); convert from hr-1 to y-1
totCH4.ow.kg.y<-meanCH4ow/10^6*1000^2*(467-33.2)*24*365 #units: kg CH4 y-1 
ErrCH4L.ow.kg.y<-LP.openWater.summary$fracErrOw.L/100*totCH4.ow.kg.y
ErrCH4U.ow.kg.y<-LP.openWater.summary$fracErrOw.U/100*totCH4.ow.kg.y


totCH4.powell.kg.y<-sum(totCH4.lit.kg.y, totCH4.ow.kg.y)
totCH4errL.powell.kg.y<-sqrt(sum(ErrCH4L.lit.kg.y^2, ErrCH4L.ow.kg.y^2))
totCH4errU.powell.kg.y<-sqrt(sum(ErrCH4U.lit.kg.y^2, ErrCH4U.ow.kg.y^2))

totFracErrL<-totCH4errL.powell.kg.y/totCH4.powell.kg.y*100 #as a percent, 42%
totFracErrU<-totCH4errU.powell.kg.y/totCH4.powell.kg.y*100 #as a percent, 79%

totCH4.powell.g.m2.y<-totCH4.powell.kg.y/(467*10^6)*1000 #units: g CH4 m-2 y-1
powell.areal.ch4.co2e<-totCH4.powell.g.m2.y*34 #converting from gCH4 to CO2e, for units of g CO2e m-2 y-1
powell.areal.ch4.co2e #239.5
powell.areal.ch4.co2e.L<-powell.areal.ch4.co2e-(powell.areal.ch4.co2e*totFracErrL/100) #lower range: 137.6
powell.areal.ch4.co2e.U<-powell.areal.ch4.co2e+(powell.areal.ch4.co2e*totFracErrU/100) #upper range: 429.75

###############CO2##################################

ggplot(lakePowellMeans, aes(CO2trate.mean))+
  geom_histogram()

#using the whole lake as one category
LP.CO2.summary<-lakePowellMeans%>%
  summarize(meanCO2 = mean(CO2trate.mean, na.rm=TRUE), #mg CO2 m-2 hr-1
            sdCO2 = (sd(CO2trate.mean, na.rm=TRUE)), #no log transformation because 1) co2 fluxes are almost normally distributed and 2) negative values
            seCO2 = sdCO2/sqrt(30),
            fracErrCO2 = seCO2/meanCO2*100)

powell.areal.CO2.g.m2.y<-LP.CO2.summary$meanCO2*24*365/1000
powell.areal.CO2.g.m2.y #85.45 g CO2-e m-2 y-1
powell.areal.CO2err.g.m2.y<-totCO2.g.m2.y*LP.CO2.summary$fracErrCO2/100
powell.areal.CO2err.g.m2.y #+/- 47.8
powell.areal.CO2.L<-powell.areal.CO2.g.m2.y-powell.areal.CO2err.g.m2.y #37.6
powell.areal.CO2.U<-powell.areal.CO2.g.m2.y+powell.areal.CO2err.g.m2.y #133.2



totalArealEmissions<-sum(totCO2.g.m2.y, powell.areal.ch4.co2e)
totalArealEmissions #324.95
totArealErrorL<-sqrt(sum((powell.areal.ch4.co2e*totFracErrL/100)^2, powell.areal.CO2err.g.m2.y^2))
totFracErrL<-totArealErrorL/totalArealEmissions*100 #34%
totArealErrorU<-sqrt(sum((powell.areal.ch4.co2e*totFracErrU/100)^2, powell.areal.CO2err.g.m2.y^2))
totFracErrU<-totArealErrorU/totalArealEmissions*100 #60%
totArealCO2eL<-totalArealEmissions-totArealErrorL #212.4
totArealCO2eU<-totalArealEmissions+totArealErrorU #521.1
#####In units of kg CO2-e MWh-1
#converting from g CO2-e m-2 y-1 to kg CO2e MWh-1
# power production number: 5,196,113 MWh y-1, based on avg generation from 1978 - 1999
# area: 467 km2

hydroPower<-5196113 #MWh yr-1
areaPowell<-467 #km2
area_elec<-areaPowell*10^6/hydroPower #m2 y per MWh

powell.ef.ch4.co2e<-powell.areal.ch4.co2e*area_elec/1000 #kg CO2-e MWh-1
powell.ef.ch4.co2e #21.52
powell.ef.ch4.co2eL<-powell.ef.ch4.co2e-(powell.ef.ch4.co2e*totFracErrL/100)
powell.ef.ch4.co2eU<-powell.ef.ch4.co2e+(powell.ef.ch4.co2e*totFracErrU/100)


powell.ef.co2.co2e<-powell.areal.CO2.g.m2.y*area_elec/1000
powell.ef.co2.co2e #7.68
powell.ef.err.co2.co2e<-powell.ef.co2.co2e*LP.CO2.summary$fracErrCO2/100
powell.ef.err.co2.co2e
powell.ef.co2.co2e.L<-powell.ef.co2.co2e-powell.ef.err.co2.co2e
powell.ef.co2.co2e.U<-powell.ef.co2.co2e+powell.ef.err.co2.co2e

totEF.emissions<-sum(powell.ef.ch4.co2e, powell.ef.co2.co2e)
totEF.emissions #29.2
totEF.errL<-totEF.emissions-sqrt(sum((powell.ef.ch4.co2e*totFracErrL/100)^2, powell.ef.err.co2.co2e^2))
totEF.errU<-totEF.emissions+sqrt(sum((powell.ef.ch4.co2e*totFracErrU/100)^2, powell.ef.err.co2.co2e^2))

# filter(lakePowellData10, site=="Colorado Inlet" | site == "Sheep Canyon" | site == "Camp 3" | site == "Hite Canyon"| site == "SJR Inlet"|site == "Alcove" | site == "Escalante Inflow" | site == "Garces Island") %>%
#   summarize(mean.ch4 = mean(ch4.drate.mg.h, na.rm=TRUE),
#             sd.ch4 = sd(ch4.drate.mg.h, na.rm=TRUE),
#             n.ch4 = n(),
#             fe.ch4 = sd.ch4/sqrt(n.ch4)/mean.ch4,
#             mean.co2 = mean(co2.drate.mg.h, na.rm=TRUE),
#             sd.co2 = sd(co2.drate.mg.h, na.rm=TRUE),
#             n.co2 = n(),
#             fe.co2 = sd.co2/sqrt(n.co2)/mean.co2)
# # mean.ch4   sd.ch4 n.ch4    fe.ch4 mean.co2   sd.co2 n.co2   fe.co2
# # 2.182499 3.402686    16 0.3897696 28.05389 65.96109    16 0.587807
# 
# #2.18 mg CH4 m-2 h-1 * 1000^2 m^2/km^2 * 42.2 km^2 * 24 hr/d * 90 d/ summer season
# 2.18*1000*1000*42.2*24*90 *10^-9 #10^-9 to convert from mg to g to kg to Mg = 198.7+/-77.29 Mg CH4 per summer 
# 
# ################Mean diffusive rate for open water:
# filter(lakePowellData10, site!="Colorado Inlet",
#        site != "Sheep Canyon",
#        site != "Camp 3", site != "Hite Canyon", site != "SJR Inlet",
#        site != "Alcove", site != "Escalante Inflow", site != "Garces Island") %>%
#   summarize(mean.ch4 = mean(ch4.drate.mg.h, na.rm=TRUE),
#             sd.ch4 = sd(ch4.drate.mg.h, na.rm=TRUE),
#             n.ch4 = n(),
#             fe.ch4 = sd.ch4/sqrt(n.ch4)/mean.ch4,
#             mean.co2 = mean(co2.drate.mg.h, na.rm=TRUE),
#             sd.co2 = sd(co2.drate.mg.h, na.rm=TRUE),
#             n.co2 = n(),
#             fe.co2 = sd.co2/sqrt(n.co2)/mean.co2)
# #   mean.ch4    sd.ch4 n.ch4    fe.ch4 mean.co2   sd.co2 n.co2    fe.co2
# # 0.08837668 0.1238105    31 0.2516164 10.13487 27.25131    31 0.4829345
# 
# #0.08837668 mg CH4 m-2 h-1 * 1000^2 m^2/km^2 * 477.8 km^2 * 24 hr/d * 90 d/ summer season
# 0.08837668*1000*1000*477.8*24*90 *10^-9 #10^-9 to convert from mg to g to kg to Mg = 91.2+/- 22.8 Mg CH4 per summer 
# 
# 
# #############Mean ebullitive rate for trib areas: -------
# filter(lakePowellEb.m, site=="Colorado Inlet" |site=="Sheep Canyon" |site== "Camp 3" |site=="SJR Inlet" |site=="Alcove"
#        |site=="Escalante Inflow" |site=="Garces Island" & (variable == "CH4.funnel.flux" | variable == "ch4.erate")) %>%
#   summarize(mean.ch4 = mean(value, na.rm=TRUE),
#             sd.ch4 = sd(value, na.rm=TRUE),
#             n.ch4 = n(),
#             fe.ch4 = sd.ch4/sqrt(n.ch4)/mean.ch4)
# # mean.ch4   sd.ch4 n.ch4   fe.ch4
# # 1.400959 21.33385    29 2.827775
# #but, these are measurements from 7 of the 8 "tributary" areas -- we didn't see ebullition at Hite Canyon. 
# 1.4*7/8 #=1.225
# #1.225 mg CH4 m-2 h-1 * 1000^2 m^2/km^2 * 42.2 km^2 * 24 hr/d * 90 d/ summer season
# 1.225*1000*1000*42.2*24*90 *10^-9 #10^-9 to convert from mg to g to kg to Mg = 112 Mg CH4 per summer  -- need to juj the Escalante calc before honing in on the se
# 
# 
# 
# #############Mean ebullitive rate for open water areas:
# filter(lakePowellEb.m, site!="Colorado Inlet", site!="Sheep Canyon", site!= "Camp 3", site!="SJR Inlet", site!="Alcove",
#        site!="Escalante Inflow", site!="Garces Island", site!= "Camp 4" & (variable == "CH4.funnel.flux" | variable == "ch4.erate")) %>%
#   summarize(mean.ch4 = mean(value, na.rm=TRUE),
#             sd.ch4 = sd(value, na.rm=TRUE),
#             n.ch4 = n(),
#             fe.ch4 = sd.ch4/sqrt(n.ch4)/mean.ch4)
# # With Camp 4:
# # mean.ch4   sd.ch4 n.ch4    fe.ch4
# # 1.392925 3.342951    14 0.6414138
# 
# # Without Camp 4:
# #  mean.ch4     sd.ch4 n.ch4    fe.ch4
# # 0.0780283 0.06545584    12 0.2421618
# #but there are 20 open water sites, and we only have measurements from 3 of them: Wahweap, Camp 2, and Camp 4
# #With Camp 4:
# 1.3929*3/20 #=0.2089
# #without Camp 4:
# 0.0780283*2/19 #= 0.008213505
# 
# #with Camp 4:
# #0.2089 mg CH4 m-2 h-1 * 1000^2 m^2/km^2 * 477.8 km^2 * 24 hr/d * 90 d/ summer season
# 0.2089*1000*1000*477.8*24*90 *10^-9 #10^-9 to convert from mg to g to kg to Mg = 215.6 +/- 138 Mg CH4 per summer 
# #without Camp 4:
# 0.008213505*1000*1000*477.8*24*90 *10^-9 #=8.4767
# #Total sum of CH4 emissions:
# #with Camp 4:
# 198.7+91.2+128+215.6 # = 633.5 Mg CH4 per summer 
# #without Camp 4:
# 198.7+91.2+128+8.4767 #=426.38 Mg CH4 per summer
# 
# 
# ##############CO2####################################
# lakePowellBySite$co2Section<-c("SJR Uptake", "CO Uptake", "Dam Emission", 
#                                "SJR Uptake", "CO Emission", "CO Uptake",
#                                "CO Emission", "NA", "Dam Emission",
#                                "Esc Emission", "Esc Emission", "Esc Emission",
#                                "Net Zero", "CO Emission", "CO Uptake",
#                                "CO Uptake", "Net Zero", "Net Zero", 
#                                "Net Zero", "SJR Uptake", "CO Uptake",
#                                "Dam Emission", "Net Zero", "Net Zero", 
#                                "CO Emission", "Net Zero", "SJR Emission",
#                                "SJR Uptake", "Dam Emission"
# )
# 
# 
# filter(lakePowellBySite, co2Section == "Dam Emission")%>%
#   # site=="Crossing of the Fathers" | site=="Camp 1" |
#   #  site == "Navajo" | site=="Wahweap")%>%
#   summarize(mean.co2 = mean(co2.drate.mg.h, na.rm=TRUE),
#             sd.co2 = sd(co2.drate.mg.h, na.rm=TRUE),
#             n.co2 = n(),
#             fe.co2 = sd.co2/sqrt(n.co2)/mean.co2)
# 
# #  mean.co2   sd.co2 n.co2     fe.co2
# # 34.46831 4.165377     4 0.06042329
# 
# filter(lakePowellBySite, co2Section == "SJR Uptake")%>%
#   # site=="Crossing of the Fathers" | site=="Camp 1" |
#   #  site == "Navajo" | site=="Wahweap")%>%
#   summarize(mean.co2 = mean(co2.drate.mg.h, na.rm=TRUE),
#             sd.co2 = sd(co2.drate.mg.h, na.rm=TRUE),
#             n.co2 = n(),
#             fe.co2 = sd.co2/sqrt(n.co2)/mean.co2)
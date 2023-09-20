#-------------------------------------------------------------------------------
# Script Name: regressions.R
# Author: NYZ
# Description: overall regressions using binary DV. Yields tex results tables.
#-------------------------------------------------------------------------------


#Import libraries
library(stargazer)
library(ROCR)
library(zoo)
library(MASS)
library(coefplot)


### BINARY DV - LOGISTIC REGRESSION ###

#With controls
overall.logit.base <- glm(BinaryPiracyIncident ~ GDPpc + Population + Unemployment + Military + Trade, data = master_naomit, family = "binomial")

overall.logit.lit <- glm(BinaryPiracyIncident ~ FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                           OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_naomit, family = "binomial")

overall.logit.total <- glm(BinaryPiracyIncident ~ TotalViolentIncidents + TotalCasualties + MajorShippingLane + Monsoon + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                             OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_naomit, family = "binomial")

overall.logit.coastal <- glm(BinaryPiracyIncident ~ CoastalIncidents + CoastalCasualties + MajorShippingLane + Monsoon + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                               OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_naomit, family = "binomial")

overall.logit.inland <- glm(BinaryPiracyIncident ~ InlandIncidents + InlandCasualties + MajorShippingLane + Monsoon + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                              OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_naomit, family = "binomial")

overall.logit.totaldvlag <- glm(BinaryPiracyIncident ~ TotalViolentIncidents + BinaryPiracyLag + MajorShippingLane + Monsoon + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                  OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_dvlag_naomit, family = "binomial")

stargazer(overall.logit.base, overall.logit.lit, overall.logit.total, overall.logit.coastal, overall.logit.inland, overall.logit.totaldvlag, type = "latex", out = "RegNAomit.tex", column.sep.width = "-15pt", no.space = TRUE)

coefplot(overall.logit.totaldvlag)


#No controls
overall.nocontrol.1lagtotal <- glm(BinaryPiracyIncident ~ BinaryPiracyLag + TotalViolentIncidents + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                  OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_dvlag_imputed, family = "binomial")

overall.nocontrol.1laginland <- glm(BinaryPiracyIncident ~ BinaryPiracyLag + InlandIncidents + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                      OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_dvlag_imputed, family = "binomial")


overall.nocontrol.2lag <- glm(BinaryPiracyIncident ~ BinaryPiracyLag + CoastalCasualties + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                       OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master2lag_dvlag_imputed, family = "binomial")


overall.nocontrol.6lag <- glm(BinaryPiracyIncident ~ BinaryPiracyLag + CoastalCasualties + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                       OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master6lag_dvlag_imputed, family = "binomial")


overall.nocontrol.12lag <- glm(BinaryPiracyIncident ~ BinaryPiracyLag + CoastalCasualties + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                        OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master12lag_dvlag_imputed, family = "binomial")

stargazer(overall.nocontrol.1lagtotal, overall.nocontrol.1laginland, overall.nocontrol.2lag, overall.nocontrol.6lag, overall.nocontrol.12lag, type = "latex", out = "RegNoControl.tex")



### EVENT COUNT DV - POISSION REGRESSION ###

#With controls
overall.poisson.base <- glm(CountPiracyIncidents ~ GDPpc + Population + Unemployment + Military + Trade, data = master_imputed, family = "poisson")

overall.poisson.lit <- glm(CountPiracyIncidents ~ FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                             OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_imputed, family = "poisson")

overall.poisson.total <- glm(CountPiracyIncidents ~ TotalViolentIncidents + TotalCasualties + MajorShippingLane + Monsoon + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                               OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_imputed, family = "poisson")

overall.poisson.coastal <- glm(CountPiracyIncidents ~ CoastalIncidents + CoastalCasualties + MajorShippingLane + Monsoon + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                 OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_imputed, family = "poisson")

overall.poisson.inland <- glm(CountPiracyIncidents ~ InlandIncidents + InlandCasualties + MajorShippingLane + Monsoon + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_imputed, family = "poisson")

overall.poisson.dvlag <- glm(CountPiracyIncidents ~ InlandIncidents + CountPiracyLag + MajorShippingLane + Monsoon + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                               OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_dvlag_imputed, family = "poisson")

stargazer(overall.poisson.base, overall.poisson.lit, overall.poisson.total, overall.poisson.coastal, overall.poisson.inland, overall.poisson.dvlag, type = "latex", out = "PoissonReg.tex")


#No controls
nocontrol.poisson.1lag <- glm(CountPiracyIncidents ~ CountPiracyLag + TotalViolentIncidents + InlandIncidents + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master_dvlag_imputed, family = "poisson")

nocontrol.poisson.2lag <- glm(CountPiracyIncidents ~ CountPiracyLag + CoastalCasualties + InlandIncidents + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master2lag_dvlag_imputed, family = "poisson")


nocontrol.poisson.6lag <- glm(CountPiracyIncidents ~ CountPiracyLag + CoastalCasualties + InlandIncidents + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master6lag_dvlag_imputed, family = "poisson")


nocontrol.poisson.12lag <- glm(CountPiracyIncidents ~ CountPiracyLag + CoastalCasualties + InlandIncidents + FishStocks + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + Chokepoints +
                                 OngoingCivilWar + StateFragility + StateReach + GDPpc + Population + Unemployment + Military + Trade, data = master12lag_dvlag_imputed, family = "poisson")

stargazer(nocontrol.poisson.1lag, nocontrol.poisson.2lag, nocontrol.poisson.6lag, nocontrol.poisson.12lag, type = "latex", out = "PoissonRegNoControl.tex")

#code that creates life cycle coefficients for electricity sipply per year per scenario. 
#Aggregated from detailed data obtained from Michaja Pehl

require(data.table)
require(magclass)
require(tidyverse)

setwd("C:/Users/Lucia/Documents/Arbeit/PIK/Paper/Paper_MA/LaTeX")

#REMIND data
rem <- read.report(c("data/REMIND_generic_Base.mif",
                     "data/REMIND_generic_Budg600.mif"),
                   as.list = FALSE)
rem <- rem[,getYears(rem)<="y2050",] #select years up to 2050
rem <- rem[,getYears(rem)>="y2010",] #select years above 2010
#select variables
var <- c("SE|Electricity (EJ/yr)",
         "SE|Electricity|Biomass|w/ CCS (EJ/yr)",
         "SE|Electricity|Biomass|w/o CCS (EJ/yr)",
         "SE|Electricity|Coal|w/ CCS (EJ/yr)",
         "SE|Electricity|Coal|w/o CCS (EJ/yr)",
         "SE|Electricity|Gas|w/ CCS (EJ/yr)",
         "SE|Electricity|Gas|w/o CCS (EJ/yr)",
         "SE|Electricity|Nuclear (EJ/yr)",
         "SE|Electricity|Solar|CSP (EJ/yr)",
         "SE|Electricity|Solar|PV (EJ/yr)",
         "SE|Electricity|Wind (EJ/yr)")

rem_tech <- rem[,,var]

share_bio_ccs = rem[,,"SE|Electricity|Biomass|w/ CCS (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_bio = rem[,,"SE|Electricity|Biomass|w/o CCS (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_coal_ccs = rem[,,"SE|Electricity|Coal|w/ CCS (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_coal = rem[,,"SE|Electricity|Coal|w/o CCS (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_gas_ccs = rem[,,"SE|Electricity|Gas|w/ CCS (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_gas = rem[,,"SE|Electricity|Gas|w/o CCS (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_nuc = rem[,,"SE|Electricity|Nuclear (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_csp = rem[,,"SE|Electricity|Solar|CSP (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_pv = rem[,,"SE|Electricity|Solar|PV (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]
share_wind = rem[,,"SE|Electricity|Wind (EJ/yr)"]/ rem[,,"SE|Electricity (EJ/yr)"]

share = mbind(share_bio_ccs, share_bio, share_coal_ccs, share_coal, share_gas_ccs, share_gas, share_nuc, share_csp, share_wind, share_pv)
share = collapseNames(share,collapsedim = 4)
getNames(share,dim=3) <- gsub(" (EJ/yr)","",getNames(share,dim=3),fixed = TRUE)
#base data for COEFFICIENTS
data = fread("data/lca_remind.csv")
colnames(data)[7] <- "carbon_gCO2eq_kWh"
colnames(data)[6] <- "El_EJ_yr"
colnames(data)[5] <- "emission_type"
#take out direct emissions from combustion
data =subset(data, emission_type != "direct fossil CO2 (non-CCS)")
data =subset(data, emission_type != "direct fossil CO2 (CCS)")

#aggregate emissions per technology per year per scenario per region
agg=setkey(setDT(data), 
           region, period, scenario, technology)[, 
                                                 list(cof=sum(carbon_gCO2eq_kWh)), 
                                                 by=list(region, period, scenario, technology)]
#convert to magpie object and make sure the two objects have the same dimensions
agg$region = gsub("World", "GLO", agg$region)
agg$scenario = gsub("ADV_WP5_Base", "Base.REMIND", agg$scenario)
agg$scenario = gsub("ADV_WP5_P240_FullTech", "Budg600.REMIND", agg$scenario)
agg$technology = paste0("SE|Electricity|", agg$technology)

coeff = as.magpie(agg)
getSets(coeff) = c("region",  "year" ,  "scenario", "model",  "variable")

#add missing variables
coeff_bio_css = coeff[,,"Base.REMIND.SE|Electricity|Biomass|w/o CCS"]*0
getNames(coeff_bio_css,dim=3) = gsub("w/o","w/",getNames(coeff_bio_css,dim=3),fixed = TRUE)

coeff_gas_css = coeff[,,"Base.REMIND.SE|Electricity|Gas|w/o CCS"]*0
getNames(coeff_gas_css,dim=3) = gsub("w/o","w/",getNames(coeff_gas_css,dim=3),fixed = TRUE)

coeff_coal_css = coeff[,,"Base.REMIND.SE|Electricity|Coal|w/o CCS"]*0
getNames(coeff_coal_css,dim=3) = gsub("w/o","w/",getNames(coeff_coal_css,dim=3),fixed = TRUE)

coeff = mbind(coeff, coeff_bio_css, coeff_gas_css, coeff_coal_css)


coeff = coeff["EUR",,]
share = share["EUR",,]
#shares*coeff = total values per technologies
tot = (coeff*share)/1000
getNames(tot, dim=3) = paste(getNames(tot, dim=3), "(t_CO2eq/MWh)")

#sum up values for a certain year to get final value
tot = as.data.frame(tot)
tot[is.na(tot)] = 0
fin=setkey(setDT(tot), 
          Year, Data1)[, list(total=sum(Value)), 
                                                 by=list(Year, Data1)]
write.csv(fin, "results_int/final_lca_coeff_600.csv", row.names=FALSE)




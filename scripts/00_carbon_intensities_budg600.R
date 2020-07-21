#this is the main script to greate ggplots and data to compare emission 
#profiles of different technologies

require("magclass") 
require(data.table)
require(quitte)
require(luplot)
library(mip)
library(ggpubr)
library(gridExtra)
library(grid)
library(viridis)
library(cowplot)

#vignette("magclass")
#
#
#get carbon intensities from remind
setwd("C:/Users/Lucia/Documents/Arbeit/PIK/Paper/Paper_MA/LaTeX")

up=read.csv(file="results_int/final_lca_coeff_600.csv", head=T)
ext = data.frame(Year = c(2055, 2055, 2060, 2060, 2070, 2070, 2080, 2080, 2090, 2090, 2100, 2100),
                  Data1 = c("Base", "Budg600", "Base", "Budg600", "Base",  "Budg600", "Base", "Budg600",  "Base", "Budg600", "Base",  "Budg600"),
                 total =c(0,0,0,0,0,0,0,0,0,0,0,0))
ext2 = data.frame(Year=2005,
                  Data1=c("Base", "Budg600"),
                  total=c(0,0))
up = rbind(ext2, up, ext)
rem <- read.report(c("data/REMIND_generic_Base.mif",
                     "data/REMIND_generic_Budg600.mif"),
                   as.list = FALSE)

rem <- rem[,getYears(rem)<="y2100",] #select years up to 2100

var_carbon <- c("Emi|CO2|Energy|Supply|Electricity|Gross (Mt CO2/yr)",
               "FE|+|Electricity (EJ/yr)")

rem_carbon <- rem[,,var_carbon]
CI <- rem_carbon[,,"Emi|CO2|Energy|Supply|Electricity|Gross (Mt CO2/yr)"] / rem_carbon[,,"FE|+|Electricity (EJ/yr)"]
CI <- collapseNames(CI,collapsedim = 4)                   
getNames(CI,dim=3) <- gsub("Emi|CO2|Energy|Supply|Electricity","Carbon_Intensity",getNames(CI,dim=3),fixed = TRUE)
getNames(CI,dim=3) <- gsub("(Mt CO2/yr)","(Mt CO2/EJ)",getNames(CI,dim=3),fixed = TRUE)

rem_carbon <- mbind(rem_carbon,CI)

CI2 <- rem_carbon[,,"Carbon_Intensity|Gross (Mt CO2/EJ)"]*(1000000/277777777.78)
getNames(CI2,dim=3) <- gsub("(Mt CO2/EJ)","(t_CO2/MWh)",getNames(CI2,dim=3),fixed = TRUE)

rem_carbon <- mbind(rem_carbon,CI2)

rem_carbon_e <- rem_carbon["EUR",,"Carbon_Intensity|Gross (t_CO2/MWh)"]

#format upstream emissions
up = as.magpie(up)
getSets(up) = c("region", "year", "scenario")
getRegions(up) = "EUR"

rem_carbon_e = rem_carbon_e + up
getNames(rem_carbon_e,dim=3) <- gsub("t_CO2","t_CO2eq",getNames(rem_carbon_e,dim=3),fixed = TRUE)

ci = as.quitte(rem_carbon_e)

write.csv(ci, "results_int/ci_600.csv", row.names=FALSE)

#initialise empty tables with all variables needed
f = data.frame(year = c(seq(from=2005, to=2055, by=5), seq(from=2060, to=2100,  by=10)),
               Base.System_Expansion=rep(0, each=16), 
               Budg600.System_Expansion=rep(0, each=16),
               Base.Elektrolyser = rep(0, each=16),
               Budg600.Elektrolyser = rep(0, each=16),
               Base.Electricity=rep(0, each=16),
               Budg600.Electricity=rep(0, each=16),
               #Base.Heat_el=rep(0, each=16), 
               #Budg1300.Heat_el=rep(0, each=16), 
               Base.Refinery = rep(0, each=16),
               Budg600.Refinery = rep(0, each=16),
               Base.By_Products = rep(0, each=16),
               Budg600.By_Products = rep(0, each=16),
               Base.Heat=rep(0, each=16), 
               Budg600.Heat=rep(0, each=16),
               Base.Methane = rep(0, each=16),
               Budg600.Methane = rep(0, each=16),
               Base.captured_Carbon = rep(0, each=16),
               Budg600.captured_Carbon = rep(0, each=16),
                stringsAsFactors=FALSE)

#convert to magclass object
f = as.magpie(f)
#change region
getRegions(f) = "EUR"
#add scenario dimension
getSets(f) = c(getSets(f), "Scenario")

#define all tables we need
#ef has the unit co2/ target unit
#demand tables have the unit target unit/unit ethylene
#target units can be energy or mass dependend on flow
#final unit should be tco2/ t ethylene, sometimes kg/kg is still inputet, as it is basicall the same
ef = f
demand_sc = f
demand_eh = f
demand_ocm = f

#fill emission factor table
ef[,,"Electricity"] = rem_carbon_e[,,"Carbon_Intensity|Gross (t_CO2eq/MWh)"]
ef[,,"Heat"] = 2.6 #kg/kg meth, taken from old cost caluclations, based on stochiometric combustion 
#-> maybe better IPCC
#ef[,,"Heat_el"] = rem_carbon_e[,,"Carbon_Intensity|Gross (kg CO2/MWh)"]
ef[,,"System_Expansion"] =  rem_carbon_e[,,"Carbon_Intensity|Gross (t_CO2eq/MWh)"]
ef[,,"Refinery"] = 0.535 #GaBi, per ton naphtha
ef[,,"Methane"] = 2.6 #kg/kg meth, taken from old cost caluclations, based on stochiometric combustion 
#-> maybe better IPCC
ef[,,"By_Products"] = 0.623 #sum already, taken from excel, needs to be 1 in the other tables!
ef[,,"Elektrolyser"] = rem_carbon_e[,,"Carbon_Intensity|Gross (t_CO2eq/MWh)"]
ef[,,"captured_Carbon"] = (-1 + 1.47*rem_carbon_e[,,"Carbon_Intensity|Gross (t_CO2eq/MWh)"]) 
#captured co2 plus energy demand per co2 captured. (electricity + heat - waste_heat (normalized to co2))

#fill demand steam cracker, normalized to ethylene
demand_sc[,,"Electricity"] = 0.21 #MWh/kg
demand_sc[,,"Heat"] = 0.488 #kg meth/kg eth
demand_sc[,,"System_Expansion"] = 3.39 #MWH/kg th
demand_sc[,,"Refinery"] = 3.58 # kg naphtha / kg ethylene, Gabi
demand_sc[,,"Methane"] = 0.0571 #kg meth/kg eth
demand_sc[,,"By_Products"] = NA
demand_sc[,,"Elektrolyser"] = NA
demand_sc[,,"captured_Carbon"] = NA

emi_sc_rem=demand_sc*ef


#fill demand electric steam cracker
demand_eh[,,"Electricity"] = 0.21 + 6.78 #MWh/kg
demand_eh[,,"Heat"] = NA #kg/kg
demand_eh[,,"System_Expansion"] = NA #MWH/kg
demand_eh[,,"Refinery"] = 3.58 # kg naphtha / kg ethylene
demand_eh[,,"Methane"] = 0.545 #kg/kg
demand_eh[,,"By_Products"] = NA
demand_eh[,,"Elektrolyser"] = NA
demand_eh[,,"captured_Carbon"] = NA

emi_eh_rem=demand_eh*ef

#fill demand ocm
demand_ocm[,,"Electricity"] = 1.1 #MWh/kg
demand_ocm[,,"Heat"] = NA #kg/kg
demand_ocm[,,"System_Expansion"] =  2.462 #MWH/kg 
demand_ocm[,,"Refinery"] = NA 
demand_ocm[,,"Methane"] = 0.183 #kg/kg
demand_ocm[,,"By_Products"] = 1
demand_ocm[,,"Elektrolyser"] = 32.2 #MWH/kg
demand_ocm[,,"captured_Carbon"] = 4.64 #kg/kg ethylene

emi_ocm_rem=demand_ocm*ef
#main script for caluclating costs and generating plots for base configuration of ocm

require(magclass) 
require(data.table)
require(luplot)
require(quitte)
require(tidyr)
require(ggpubr)
library(mip)
library(RColorBrewer)

wd = "C:/Users/Lucia/Documents/Arbeit/PIK_MA/Paper/Paper_MA/LaTeX"
setwd(wd)
#_______________________________________________
#GET COSTS DATA
rem <- read.report(c("data/REMIND_generic_Base.mif",
                     "data/REMIND_generic_Budg600.mif",
                     "data/REMIND_generic_Budg1300.mif"),
                   as.list = FALSE)

rem <- rem[,getYears(rem)<="y2100",] #select years up to 2100


var_costs = c("Price|Carbon (US$2005/t CO2)",
              "Price|Final Energy|Electricity|Industry (US$2005/GJ)",
              "Price|Oil|World Market (US$2005/GJ)",
              "Price|Secondary Energy|Gases (US$2005/GJ)")
rem_costs <- rem[,,var_costs]

#naphtha price model: 0.02 = carbon intensity of crude oil in ton/GJ (IPCC accounting scheme)
#42.3 = NCV of crude oil in GJ/t (IPCC accounting scheme)
#0.9 conversion to eur
#0.413 = upstream emission associated with refinery in mass/mass
naphtha = (1.3038*0.9*(rem_costs[,,"Price|Oil|World Market (US$2005/GJ)"]*42.3) - 8.601) + 0.9*rem_costs[,,"Price|Carbon (US$2005/t CO2)"]*0.413 
naphtha = collapseNames(naphtha, collapsedim=4)
getNames(naphtha, dim=3) = gsub("Oil|World Market", "Naphtha", getNames(naphtha,dim=3),fixed = TRUE)
getNames(naphtha, dim=3) = gsub("(US$2005/GJ)", "(US$2005/t)", getNames(naphtha,dim=3),fixed = TRUE)
rem_costs = mbind(naphtha, rem_costs)

#naphtha without carbon price
naphtha_wocc = 1.3038*42.3*0.9*rem_costs[,,"Price|Oil|World Market (US$2005/GJ)"]  - 8.601
getNames(naphtha_wocc, dim=3) = gsub("Oil|World Market", "Naphtha| w/o carbon price", getNames(naphtha_wocc,dim=3),fixed = TRUE)
getNames(naphtha_wocc, dim=3) = gsub("(US$2005/GJ)", "(US$2005/t)", getNames(naphtha_wocc,dim=3),fixed = TRUE)
rem_costs = mbind(naphtha_wocc, rem_costs)

#naphtha carbon price
carbon_n = naphtha - naphtha_wocc
carbon_n = collapseNames(carbon_n, collapsedim=4)
getNames(carbon_n, dim=3) = gsub("Naphtha", "Carbon_n", getNames(carbon_n, dim=3), fixed=TRUE)
getNames(carbon_n, dim=3) = gsub("(US$2005/t)", "(US$2005/t Naphtha)", getNames(carbon_n, dim=3), fixed=TRUE)
rem_costs = mbind(carbon_n, rem_costs)

#ethylene carbon price
#3.14 is the emission factor of ethylene, based on carbon content of ethylene and co2
#see annex1, tab A1 for values
carbon_e = rem_costs[,,"Price|Carbon (US$2005/t CO2)"]*3.14
getNames(carbon_e, dim=3) = gsub("Carbon", "Carbon_e", getNames(carbon_e, dim=3), fixed=TRUE)
getNames(carbon_e, dim=3) = gsub("(US$2005/t)", "(US$2005/t Ethylene)", getNames(carbon_e, dim=3), fixed=TRUE)
rem_costs = mbind(carbon_e, rem_costs)

#methane carbon price
#2.75 is the emission factor for methane, based on carbon content of ethylene and co2
#see annex1, tab A1 for values
carbon_m = rem_costs[,,"Price|Carbon (US$2005/t CO2)"]*2.75
getNames(carbon_m, dim=3) = gsub("Carbon", "Carbon_m", getNames(carbon_m, dim=3), fixed=TRUE)
getNames(carbon_m, dim=3) = gsub("(US$2005/t)", "(US$2005/t Meth)", getNames(carbon_m, dim=3), fixed=TRUE)
rem_costs = mbind(carbon_m, rem_costs)

rem_costs = add_columns(rem_costs, addnm = c("Fixed Costs"), dim=3.3)
rem_costs[,,"Fixed Costs"] = 1
#add capex
##older version with seperate capex and opec
### rem_costs = add_columns(rem_costs, addnm = c("Capex", "OM", "Capex Elektrolyser", "OM Elektrolyser"), dim=3.3)
# rem_costs[,,"Capex"] = 1
# rem_costs[,,"OM"] = 1
# rem_costs[,,"Capex Elektrolyser"] = 1
# rem_costs[,,"OM Elektrolyser"] = 1

#extract values for europe
rem_costs_e = rem_costs["EUR",,]

# remove oil price
rem_costs_e <- rem_costs_e[,,"Price|Oil|World Market (US$2005/GJ)",invert=TRUE]
#remove carbon price as per ton CO2
rem_costs_e <- rem_costs_e[,,"Price|Carbon (US$2005/t CO2)",invert=TRUE]
#remove naphtha including carbon price 
rem_costs_e <- rem_costs_e[,,"Price|Naphtha (US$2005/t)",invert=TRUE]




prices <- rem_costs_e
getNames(prices) <- gsub("Price\\|","",getNames(prices))
getNames(prices) <- gsub(" \\(.*\\)$","",getNames(prices))
prices[,"y2005", "Naphtha| w/o carbon price"] = 0.000001
prices[,"y2005", "Secondary Energy|Gases"] = 0.000001

#create object with conversion tables
#unit of price table is USD/ target unit, wth exception of fixed costs
#unit of conversion tables is target unit/t ethylene
#target unit can be mass or energy depending on flow
#final unit shuld be eur/ t ethylene
conversion = prices * 0
conversion[,,"Fixed Costs"] = 1


#older version with seperate capex and opec
# conversion[,,"Capex"] = 1
# conversion[,,"OM"] = 1
# conversion[,,"Capex Elektrolyser"] = 1
# conversion[,,"OM Elektrolyser"] = 1

conversion_sc = conversion
conversion_ocm = conversion
#Calculate capex and opex values to be in the unit EUR/t ethylene
r = 0.07
a = 30
CRF = (r*((1+r)^a))/(-1+(1+r)^a)

cf_sc = 1000000
C_sc = 1334944800
O_sc = 76.42
fc_sc = (C_sc *CRF + O_sc)/cf_sc # eur/t ethylene

cf_ocm = 148280
C_ocm = 188903764
O_ocm = 63.75
fc_ocm = (C_ocm *CRF + O_ocm)/cf_ocm # eur/ t ethylene


cf_cc = 1000000
C_cc = C_cc = c(16, 16, 16, 16, 16, 16, 16, 2, 2, 2, 2, 2, 2, 2, 2, 2 )*100000000*0.9
O_cc = 0
fc_cc = 3.64*(C_cc *CRF + O_cc)/cf_cc #eur/ t ethylene


cf_h2 = 148280
C_h2 = 500*1000*c(1000, 1000, 1000, 1000, 1000, 750, 750, 750, 750, 750, 750, 750, 750, 750, 750, 750) 
O_h2 = 178
fc_h2 = (C_h2 *CRF + O_h2)/cf_h2 #eur/t ethylene

fc_ocm=fc_ocm+fc_cc+fc_h2
#SC
#insert conversion factors
conversion_sc[,,"Final Energy|Electricity|Industry"] = 0.21*0.9/0.2778 
conversion_sc[,,"Naphtha| w/o carbon price"] = 3.578*0.9
#conversion_sc[,,"Naphtha"] = NA
conversion_sc[,,"Secondary Energy|Gases"] = -1.89*0.9*0.5/0.2778 #only methane
conversion_sc[,,"Carbon_n"] = 3.578*0.9
conversion_sc[,,"Fixed Costs"] = fc_sc
#conversion_sc[,,"Fixed Costs EL"] = NA
#conversion_sc[,,"Fixed Costs CC"] = NA
conversion_sc[,,"Carbon_e"] = 1
conversion_sc[,,"Carbon_m"] = 0.488 # only combusted in product, for electricty production it is included in SE price
conversion_sc[,,"Base.REMIND.Carbon_e"] = NA
conversion_sc[,,"Base.REMIND.Carbon_m"] = NA
conversion_sc[,,"Base.REMIND.Carbon_n"] = NA
#transform to electrified version to not have to change to many values
conversion_eh = conversion_sc
#multiply to get demand
costs_sc_rem = conversion_sc*prices
By_products = -0.6416*(costs_sc_rem[,,"Naphtha| w/o carbon price"] + costs_sc_rem[,,"Final Energy|Electricity|Industry"]) 
By_products = collapseNames(By_products, collapsedim=4)
getNames(By_products, dim=3) = gsub("Naphtha| w/o carbon price", "By-Products", getNames(By_products,dim=3),fixed = TRUE)
costs_sc_rem = mbind(costs_sc_rem, By_products)
getNames(costs_sc_rem) = paste(getNames(costs_sc_rem), "(EUR/ t Ethylene)")

#electrictrified steam cracking
conversion_eh[,,"Final Energy|Electricity|Industry"] = 6.99*0.9/0.2778 
conversion_eh[,,"Secondary Energy|Gases"] = -7.57*0.5*0.9/0.2778 #only methane
conversion_eh[,,"Carbon_m"] = NA # only combusted in product, for electricty production it is included in SE price
#multiply to get demand
costs_eh_rem = conversion_eh*prices
costs_eh_rem = mbind(costs_eh_rem, By_products)
getNames(costs_eh_rem) = paste(getNames(costs_eh_rem), "(EUR/ t Ethylene)")

#OCM
conversion_ocm[,,"Final Energy|Electricity|Industry"] = (1.10 + 33.1 + 1.765)*0.9/0.2778 
conversion_ocm[,,"Secondary Energy|Gases"] = -2.86*0.5*0.9/0.2778
conversion_ocm[,,"Base.REMIND.Fixed Costs"] = fc_ocm 
conversion_ocm[,,"Budg1300.REMIND.Fixed Costs"] = fc_ocm
conversion_ocm[,,"Budg600.REMIND.Fixed Costs"] = fc_ocm



conversion_ocm[,,"Naphtha| w/o carbon price"] = NA
#conversion_ocm[,,"Naphtha"] = NA
conversion_ocm[,,"Carbon_n"] = NA
conversion_ocm[,,"Carbon_e"] = NA
conversion_ocm[,,"Carbon_m"] = NA


costs_ocm_rem = conversion_ocm*prices
getNames(costs_ocm_rem) = paste(getNames(costs_ocm_rem), "(EUR/ t Ethylene)")

#'FLEXIBILISATION'
#'
##OCM flex full electric
cf_h2_ff = 148280
C_h2_ff = 2*500*1000*c(2100, 2100, 2100, 2100, 2100, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000) 
O_h2_ff = 178*2
fc_h2_ff = (C_h2_ff *CRF + O_h2_ff)/cf_h2_ff #eur/t ethylene

fc_ocm_ff=fc_ocm+fc_cc+fc_h2_ff

ocm_ff = conversion_ocm
ocm_ff[,,"Base.REMIND.Fixed Costs"] = fc_ocm_ff
ocm_ff[,,"Budg1300.REMIND.Fixed Costs"] = fc_ocm_ff
ocm_ff[,,"Base.REMIND.Final Energy|Electricity|Industry"] = (1.10 + 33.1*0.7 + 1.765)*0.9/0.2778
ocm_ff[,,"Budg1300.REMIND.Final Energy|Electricity|Industry"] = (1.10 + 33.1*c(0.7, 0.7, 0.7, 0.7, 0.7, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63) + 1.765)*0.9/0.2778
ocm_ff[,,"Budg600.REMIND.Final Energy|Electricity|Industry"] = (1.10 + 33.1*c(0.7, 0.7, 0.7, 0.7, 0.7, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63, 0.63) + 1.765)*0.9/0.2778


costs_ff_rem = ocm_ff*prices
getNames(costs_ff_rem) = paste(getNames(costs_ff_rem), "(EUR/ t Ethylene)")

#OCM flex full electric 4cap
cf_h2_ff4 = 148280
C_h2_ff4 = 4*500*1000*c(2100, 2100, 2100, 2100, 2100, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000) 
O_h2_ff4 = 178*4
fc_h2_ff4 = (C_h2_ff4 *CRF + O_h2)/cf_h2_ff4 #eur/t ethylene

fc_ocm_ff4=fc_ocm+fc_cc+fc_h2_ff4

ocm_ff4 = conversion_ocm
ocm_ff4[,,"Base.REMIND.Fixed Costs"] = fc_ocm_ff4
ocm_ff4[,,"Budg1300.REMIND.Fixed Costs"] = fc_ocm_ff4
ocm_ff4[,,"Base.REMIND.Final Energy|Electricity|Industry"] = (1.10 + 33.1*0.5 + 1.765)*0.9/0.2778
ocm_ff4[,,"Budg1300.REMIND.Final Energy|Electricity|Industry"] = (1.10 + 33.1*c(0.5, 0.5, 0.5, 0.5, 0.5, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3) + 1.765)*0.9/0.2778
ocm_ff4[,,"Budg600.REMIND.Final Energy|Electricity|Industry"] = (1.10 + 33.1*c(0.5, 0.5, 0.5, 0.5, 0.5, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3) + 1.765)*0.9/0.2778


costs_ff4_rem = ocm_ff4*prices
getNames(costs_ff4_rem) = paste(getNames(costs_ff4_rem), "(EUR/ t Ethylene)")


#############################################################################################
## Cascade CEA Model - Combination Interventions
## Determine the proximal stratgies for PSA analyses to estimate proportions being most CE
## ATTENTION: TWO OPTIONS TO CHOOSE FROM, Option 2 tackles potential issue with CS stratgies,
##            should be consistent to the module:
## Last updated: Feb 20, 2020
############################################################################################
rm(list=ls())
library(rstudioapi)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
all.cities <- c("ATL", "BAL", "LA", "MIA", "NYC", "SEA")

## SELECT city ##
CITY <- select.list(all.cities, multiple = FALSE,
                    title = 'Select city', graphics = FALSE)

npr <- 10        # select number of proximal strategies

combination.list <- readRDS("Combination/Combination.list.rds")

frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
comparator       <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$comparator

outcome.comb    <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref     <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))

####################################################################################################
#### OPTION 1 (currently applied): according to absolute NMB, however, might run into problem if OCIS is cost-saving while ICER > 100K compared with comparator
outcome.comb      <- cbind(outcome.comb, index = 1:nrow(outcome.comb))
all_comb          <- outcome.comb[-c(ocis, comparator), c("QALYs.sum", "costs.total.sum", "index")]
all_comb.mx       <- all_comb
all_comb.mx[ , 1] <- all_comb.mx[ , 1] - outcome.ref["QALYs.sum"]
all_comb.mx[ , 2] <- all_comb.mx[ , 2] - outcome.ref["costs.total.sum"]

nmb               <- 100000 * all_comb.mx[ , 1] - all_comb.mx[ , 2]
all_comb.mx       <- cbind(all_comb.mx, nmb)

PSA.sts           <- tail(all_comb.mx[order(all_comb.mx[,"nmb"]), ], n = npr)
saveRDS(PSA.sts[ , "index"], paste0("Combination/PSA-ProximalStrategies-", CITY, ".rds"))


####################################################################################################
#### OPTION 2 (solve the aformentioned problem): according to the distance to the NMB of OCIS:
####          make sure to select the corresponding one in "CascadeCEA-Interventions-3-Analysis-PSA-ProportionMostCE.R"
outcome.comb      <- cbind(outcome.comb, index = 1:nrow(outcome.comb))
all_comb          <- outcome.comb[ , c("QALYs.sum", "costs.total.sum", "index")]
all_comb.mx       <- all_comb
all_comb.mx[ , 1] <- all_comb.mx[ , 1] - outcome.ref["QALYs.sum"]
all_comb.mx[ , 2] <- all_comb.mx[ , 2] - outcome.ref["costs.total.sum"]

nmb               <- 100000 * all_comb.mx[ , 1] - all_comb.mx[ , 2]
nmb.diff          <- abs(nmb - nmb[ocis])
all_comb.mx       <- cbind(all_comb.mx, nmb, nmb.diff)
all_comb.mx       <- all_comb.mx[-c(ocis, comparator), ]
PSA.sts           <- tail(all_comb.mx[order(all_comb.mx[,"nmb.diff"]), ], n = npr)
saveRDS(PSA.sts[ , "index"], paste0("Combination/PSA-ProximalStrategies-", CITY, ".rds"))


#############################################################################
####### Print the included interventions in the proximal combination ########
#############################################################################
source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")
proximal <- PSA.sts[ , "index"]
for (i in 1:length(frontier)){
  print(paste0("####COMBINATION-", proximal[i], ":"))
  print(interventions[combination.list[[frontier[i]]]])
}

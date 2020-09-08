#############################################################################################
## Cascade CEA Model - Combination Interventions
## Estimate proportions of the selected combinations being most CE
## ATTENTION: This is corresponding to OPTION 1 (currently applied) in "CascadeCEA-Interventions-3-Analysis-DetermineProximalStrategies.R"
## Last updated: Feb 20, 2020
############################################################################################
rm(list=ls())
library(rstudioapi)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

## SELECT city ##
CITY <- select.list(all.cities, multiple = FALSE,
                    title = 'Select city', graphics = FALSE)

## LOAD the stratgies
combination.list <- readRDS("Combination/Combination.list.rds")
frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
comparator       <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$comparator

## Select cost-effectiveness threshold
CEthreshold <- 100000

combination.list <- readRDS("Combination/Combination.list.rds")

## LOAD reference case
outcome.SA.ref   <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))
ref.matrix       <- matrix(0, nrow = 2000, ncol =2)
ref.matrix[ ,1]  <- unlist(lapply(outcome.SA.ref, "[[", 1))
ref.matrix[ ,2]  <- unlist(lapply(outcome.SA.ref, "[[", 2))

## LOAD proximal stratgies
proximal.ind <- as.numeric(readRDS(paste0("Combination/PSA-ProximalStrategies-", CITY, ".rds")))
PSA.comparison.mx <- matrix(0, nrow = 2000, ncol = length(proximal.ind) + 2)

for (pp in 1:length(proximal.ind)){
  PSA.outcome <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", proximal.ind[pp], "(Proximal).rds"))[ , c("QALYs.sum", "costs.total.sum")]
  PSA.outcome[ , 1]          <- PSA.outcome[ , 1] - ref.matrix[ ,1]
  PSA.outcome[ , 2]          <- PSA.outcome[ , 2] - ref.matrix[ ,2]
  PSA.comparison.mx[ , pp+2] <- PSA.outcome[ , 1] * CEthreshold - PSA.outcome[ , 2]
}

## LOAD comparator stratgy
if (exists("comparator")){
  comparator.outcome <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", comparator, "(Comparator).rds"))[ , c("QALYs.sum", "costs.total.sum")]
  comparator.outcome[ , 1] <- comparator.outcome[ , 1] - ref.matrix[ ,1]
  comparator.outcome[ , 2] <- comparator.outcome[ , 2] - ref.matrix[ ,2]
  PSA.comparison.mx[ , 2]  <- comparator.outcome[ , 1] * CEthreshold - comparator.outcome[ , 2]
} else {
  PSA.comparison.mx[ , 2]  <- 0
}

## LOAD OCIS
ocis.outcome            <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", ocis, "(OCIS).rds"))
ocis.outcome.mx         <- ocis.outcome[ , c("QALYs.sum", "costs.total.sum")]
ocis.outcome.mx[ , 1]   <- ocis.outcome.mx[ , 1] - ref.matrix[ ,1]
ocis.outcome.mx[ , 2]   <- ocis.outcome.mx[ , 2] - ref.matrix[ ,2]
PSA.comparison.mx[ , 1] <- ocis.outcome.mx[ , 1] * CEthreshold - ocis.outcome.mx[ , 2]

colnames(PSA.comparison.mx) <- c("OCIS", "Comparator", as.character(PSA.comb.ind))

table(colnames(PSA.comparison.mx)[apply(PSA.comparison.mx, 1, which.max)])
# interventions[unlist(combination_list[22905])]    #print the combination compositions

#Need to manually paste the results into excel

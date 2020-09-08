#############################################################################
## Cascade CEA Model - Combination Interventions
## Produce the CEA table comparing the OCIS with the comparator, with 95% CI
## Last update: March 16, 2020
#############################################################################
############################################################################################
rm(list=ls())
library(rstudioapi)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

cea.table           <- matrix(0, ncol = 11, nrow = 6)
colnames(cea.table) <- c("IncrementalCost_pe", "IncrementalCost_low", "IncrementalCost_up",
                         "IncrementalQALY_pe", "IncrementalQALY_low", "IncrementalQALY_up",
                         "ICER_pe", "ICER_low", "ICER_up", "%cost-effective", "%cost-saving")
rownames(cea.table) <- c("Atlanta", "Baltimore", "Los Angeles", "Miami", "New York City", "Seattle")
CEthreshold         <- 100000
city.name.list      <- c("Atlanta", "Baltimore", "Los Angeles", "Miami", "New York City", "Seattle")
combination.list    <- readRDS("Combination/Combination.list.rds")


for (ww in 1:6){
  CITY       <- all.cities[ww]
  CITY.name  <- city.name.list[ww]
  frontier   <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
  ocis       <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
  comparator <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$comparator

  #Derministic results
  outcome.comb   <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
  comp.matrix    <- outcome.comb[c(comparator, ocis) , c("QALYs.sum", "costs.total.sum")]

  #PSA results
  ocis.mx         <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", ocis,"(OCIS).rds"))[ , c("QALYs.sum", "costs.total.sum")]
  comparator.mx   <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", comparator,"(Comparator).rds"))[ , c("QALYs.sum", "costs.total.sum")]
  outcome.SA.ref  <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))
  ref.matrix      <- matrix(0, nrow = 2000, ncol =2)
  ref.matrix[ ,1] <- unlist(lapply(outcome.SA.ref, "[[", 1))
  ref.matrix[ ,2] <- unlist(lapply(outcome.SA.ref, "[[", 2))

  ####in comparison to next best ####
  quantile((ocis.mx[ , "costs.total.sum"] - ref.matrix[ ,2]), c(0.025, 0.975)) / 1000000
  quantile((ocis.mx[ , "QALYs.sum"] - ref.matrix[ ,1]), c(0.025, 0.975))

  cost.col <- 2; qaly.col <-1

  cea.table[CITY.name, "IncrementalCost_pe"] <- comp.matrix[2, cost.col] - comp.matrix[1, cost.col]
  cea.table[CITY.name, c("IncrementalCost_low", "IncrementalCost_up")] <- quantile((ocis.mx[ , "costs.total.sum"] - comparator.mx[ , "costs.total.sum"]), c(0.025, 0.975))
  cea.table[CITY.name, "IncrementalQALY_pe"] <- comp.matrix[2, qaly.col] - comp.matrix[1, qaly.col]
  cea.table[CITY.name, c("IncrementalQALY_low", "IncrementalQALY_up")] <- quantile((ocis.mx[ , "QALYs.sum"] - comparator.mx[ , "QALYs.sum"]), c(0.025, 0.975))
  cea.table[CITY.name, "ICER_pe"] <- (comp.matrix[2, cost.col] - comp.matrix[1, cost.col]) / (comp.matrix[2, qaly.col] - comp.matrix[1, qaly.col])
  cea.table[CITY.name, c("ICER_low", "ICER_up")] <- quantile((ocis.mx[ , "costs.total.sum"] - comparator.mx[ , "costs.total.sum"]) /
                                                               (ocis.mx[ , "QALYs.sum"] - comparator.mx[ , "QALYs.sum"]), c(0.025, 0.975))
  cea.table[CITY.name, "%cost-effective"] <- sum(((ocis.mx[ , "costs.total.sum"] - comparator.mx[ , "costs.total.sum"]) /
                                                    (ocis.mx[ , "QALYs.sum"] - comparator.mx[ , "QALYs.sum"])) < CEthreshold) / 2000   # %cost-effective
  cea.table[CITY.name, "%cost-saving"]    <- length(which(ocis.mx[ , "costs.total.sum"] - ref.matrix[ , cost.col] < 0)) / 2000   # %cost-saving
}

write.csv(cea.table,'Outputs/CEA Table/CEATable(range).csv')

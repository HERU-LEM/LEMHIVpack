# Run model and accumulate outcomes (deterministic)
# This module allows for city selection, loads list of intervention combinations, loads ordinary differential equation functions,
# analysis scenario (deterministic or sensitivity analysis), and loads input parameters and comparators.
# It also provides code for running parallel estimations.

#############################################################################
# 1. SET directory and workspace
#############################################################################

rm(list=ls())
library(rstudioapi)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

# SELECT city ##
CITY <- select.list(all.cities, multiple = FALSE,
                        title = 'Select city', graphics = FALSE)
# ww <- 1; CITY <- all.cities[[ww]] # Otherwise you can set city by its index

## LOAD list of all combinations, interventions indexed by number in each combination
# if the list does not exist, source("CascadeCEA-Combination-0-Setup-combination.list.R")
combination.list <- readRDS("Combination/Combination.list.rds")

## LOAD ODE function
#source("ModelCoreModules/CascadeCEA-Model-0-Function-ode_model-Combination.R") # Function automatically loaded with LEMHIVpack

## LOAD analysis scenario
case = "DM"  # DM for deterministic, SA for sensitivity analysis

## LOAD all input parameters and comparators
source("01_Setup/CascadeCEA-Interventions-1-LoadParameterWorkspace-Combination.R")

total.comb <- length(combination.list)

tic("Model run")

outcome.comb.mx <- matrix(0, nrow = total.comb, ncol = 44)    ##Initialize combination outcome matrix (to save results)

outcome.comb.mx <- foreach(cc=1:total.comb, .combine=rbind, .export = export.int.model.names
) %dopar% {
  comb.eva.func(input.parameters = all.params, current.int = interventions[combination.list[[cc]]])
}

future:::ClusterRegistry("stop")   ##Stop parallel estimations and free cores

colnames(outcome.comb.mx)        <- rep("FILL", 44)
colnames(outcome.comb.mx)[1:6]   <- c("Infections.total-20Y", "SuscPY-over20Y", "Infections.total-10Y", "SuscPY-over10Y", "Infections.total-5Y", "SuscPY-over5Y")
colnames(outcome.comb.mx)[7:32]  <- paste0("Year", c(2015:2040))
colnames(outcome.comb.mx)[33:44] <- c("QALYs.sum", "costs.total.sum", "costs.hru.sum", "costs.art.sum", "costs.art.ini.sum",
                                      "costs.oat.sum", "costs.prep.sum", "costs.prep.tests.sum", "costs.test.sum", "int.costs.sum",
                                      "int.impl.costs.sum", "int.sust.costs.sum")

saveRDS(outcome.comb.mx,   paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))

toc()

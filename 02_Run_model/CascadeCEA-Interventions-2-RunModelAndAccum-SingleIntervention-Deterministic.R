# Run deterministic model for single interventions
# This module allows users to run deterministic and PSA for optimal combination implementation strategy at documented scale
# PREREQUISITE: run documented and determine the production function first

#############################################################################
# 1. SET directpry and workspace
#############################################################################
rm(list=ls())
library(rstudioapi)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("01_setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

# SELECT city ##
CITY <- select.list(all.cities, multiple = FALSE,
                        title = 'Select city', graphics = FALSE)
# ww <- 6; CITY <- all.cities[[ww]] # Otherwise you can set city by its index

## LOAD list of all combinations, interventions indexed by number in each combination
# if the list does not exist, source("CascadeCEA-Combination-0-Setup-combination.list.R")
combination.list <- readRDS("Combination/Combination.list.rds")

## LOAD ODE function
#source("01_setup/CascadeCEA-Model-0-Function-ode_model-Combination.R")

## LOAD analysis scenario
case = "DM"  # DM for deterministic, SA for sensitivity analysis

## LOAD all input parameters and comparators
source("01_setup/CascadeCEA-Interventions-1-LoadParameterWorkspace-Combination.R")

total.int <- length(interventions)-1
outcome.int.mx <- matrix(0, nrow = total.int, ncol = 44)    ##Initialize intervention outcome matrix (to save results)

for (cc in 1:total.int){
  outcome.int.mx[cc, ] <- comb.eva.func(input.parameters = all.params, current.int = interventions[cc])
}

colnames(outcome.int.mx)        <- rep("FILL", 44)
colnames(outcome.int.mx)[1:6]   <- c("Infections.total-20Y", "SuscPY-over20Y", "Infections.total-10Y", "SuscPY-over10Y", "Infections.total-5Y", "SuscPY-over5Y")
colnames(outcome.int.mx)[7:32]  <- paste0("Year", c(2015:2040))
colnames(outcome.int.mx)[33:44] <- c("QALYs.sum", "costs.total.sum", "costs.hru.sum", "costs.art.sum", "costs.art.ini.sum",
                                      "costs.oat.sum", "costs.prep.sum", "costs.prep.tests.sum", "costs.test.sum", "int.costs.sum",
                                      "int.impl.costs.sum", "int.sust.costs.sum")

outcome.int.mx <- as.data.frame(outcome.int.mx)
outcome.int.mx$Intervention  <- interventions[-total.int]

saveRDS(outcome.int.mx,   paste0("SingleIntervention/Outcome-Single-Intervention-", CITY, "-DM.rds"))

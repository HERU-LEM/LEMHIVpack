# Run model and accumulate outcomes (deterministic and PSA, for optimal combination implmentation strategy identifed at documented scale)
# This module allows users to run deterministic and PSA for optimal combination implementation strategy at documented scale
# PREREQUISITE: run documented and determine the production function first

#############################################################################
# 1. SET directory and workspace
#############################################################################

rm(list=ls())
library(rstudioapi)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("01_setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

# SELECT city ##
CITY <- select.list(all.cities, multiple = FALSE,
                        title = 'Select city', graphics = FALSE)
# ww <- 1; CITY <- all.cities[[ww]] # Otherwise you can set city by its index

## LOAD list of all combinations, interventions indexed by number in each combination
# if unavailable, source("CascadeCEA-Combination-0-Setup-combination.list.R")
combination.list <- readRDS("Combination/Combination.list.rds")

## LOAD ODE function
#source("01_setup/CascadeCEA-Model-0-Function-ode_model-Combination.R")

#Load OCIS, if unavailable, execute Part 2 of: "CascadeCEA-Interventions-3-Analysis-ProductionFunction.R"
ocis        <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
current.int <- interventions[combination.list[[ocis]]]

#######################
#### Deterministic ####
#######################
## LOAD analysis scenario
case  = "DM"  # DM for deterministic, SA for sensitivity analysis
int.scale = "ideal"  # This variable exists only for ideal scale

## LOAD all input parameters and comparators
source("01_setup/CascadeCEA-Interventions-1-LoadParameterWorkspace-Combination.R")


## RUN the deterministic model
outcome.dm.mx <- matrix(0, nrow = 2, ncol = 44)    ##Initialize intervention outcome matrix (to save results)
colnames(outcome.dm.mx) <- rep("FILL", 44)
colnames(outcome.dm.mx)[1:6]   <- c("Infections.total-20Y", "SuscPY-over20Y", "Infections.total-10Y", "SuscPY-over10Y", "Infections.total-5Y", "SuscPY-over5Y")
colnames(outcome.dm.mx)[7:32]  <- paste0("Year", c(2015:2040))
colnames(outcome.dm.mx)[33:44] <- c("QALYs.sum", "costs.total.sum", "costs.hru.sum", "costs.art.sum", "costs.art.ini.sum",
                                    "costs.oat.sum", "costs.prep.sum", "costs.prep.tests.sum", "costs.test.sum", "int.costs.sum",
                                    "int.impl.costs.sum", "int.sust.costs.sum")
outcome.dm.mx[2, ]      <- comb.eva.func(input.parameters = all.params, current.int = current.int)
outcome.dm.mx[1, 1:6]   <- readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-infections.rds"))$out.inf
outcome.dm.mx[1, 7:32]  <- readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-infections.rds"))$out.inf.yr
outcome.dm.mx[1, 33:44] <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))

saveRDS(outcome.dm.mx, paste0("Combination/Ideal/Outcome-refvsOCIS-", CITY, "-ideal.rds"))

#######################
######## PSA ##########
#######################
## LOAD analysis scenario
case = "SA"  # DM for deterministic, SA for sensitivity analysis
int.scale = "ideal"  # This variable exists only for ideal scale
param.sets  <- 2000

## LOAD all input parameters and comparators
source("01_setup/CascadeCEA-Interventions-1-LoadParameterWorkspace-Combination.R")

## Executing PSA analyses
outcome.ideal.SA.mx <- matrix(0, nrow = param.sets, ncol = 44)

for (cc in 1:param.sets){
  print(paste0("Running PSA for parameter set: ", cc))

  ##Load input data
  input.parameters = all.params.list[[cc]]
  All.Costs.ls     = All.Costs.ls.list[[cc]]
  StateQALYs       = State.QALYs.ls.list[[cc]]
  Int.Baseline.ls  = Int.Baseline.ls.list[[cc]]
  Int.Eff.ls       = Int.Eff.ls.list[[cc]]

  ##SSP, BUP and PrEP intervention grwoth needs to be adjusted for PSA according to the random baseline values to reach the ideal 90% scale
  pwid.pop <- c(52177, 60027, 70205, 23560, 94956, 23059)[which(CITY == all.cities)]
  Int.Scale.ls$IntScale.SSP  <- ((input.parameters$d * 12 * pwid.pop * 0.9 - input.parameters$v.ssp)/input.parameters$v.ssp + 1)^(1/18)-1
  Int.Scale.ls$IntScale.BUP  <- ((0.9 * sum(input.parameters$pop.pwid) * 0.727 *0.8 - sum(Int.Baseline.ls$IntBaseline.BUP))/sum(Int.Baseline.ls$IntBaseline.BUP)+1)^(1/18)-1
  Int.Scale.ls$IntScale.MET  <- ((0.9 * sum(input.parameters$pop.pwid) * 0.727 *0.2 - sum(Int.Baseline.ls$IntBaseline.MET))/sum(Int.Baseline.ls$IntBaseline.MET)+1)^(1/18)-1
  Int.Scale.ls$IntScale.prep <- ((sum(input.parameters$msm.h.scep)*0.9 - Int.Baseline.ls$IntBaseline.prep)/Int.Baseline.ls$IntBaseline.prep + 1)^(1/18)-1

  outcome.ideal.SA.mx[cc, ] = comb.eva.func(input.parameters = input.parameters, current.int = current.int)
}

colnames(outcome.ideal.SA.mx)        <- rep("FILL", 44)
colnames(outcome.ideal.SA.mx)[1:6]   <- c("Infections.total-20Y", "SuscPY-over20Y", "Infections.total-10Y", "SuscPY-over10Y", "Infections.total-5Y", "SuscPY-over5Y")
colnames(outcome.ideal.SA.mx)[7:32]  <- paste0("Year", c(2015:2040))
colnames(outcome.ideal.SA.mx)[33:44] <- c("QALYs.sum", "costs.total.sum", "costs.hru.sum", "costs.art.sum", "costs.art.ini.sum",
                                          "costs.oat.sum", "costs.prep.sum", "costs.prep.tests.sum", "costs.test.sum", "int.costs.sum",
                                          "int.impl.costs.sum", "int.sust.costs.sum")

saveRDS(outcome.ideal.SA.mx,   paste0("Combination/Ideal/Outcome-OCIS-", CITY, "-PSA-", ocis,"-ideal.rds"))

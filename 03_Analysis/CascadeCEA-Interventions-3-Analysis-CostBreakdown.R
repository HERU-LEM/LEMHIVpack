#############################################################################################
## Cascade CEA Model - Combination Interventions (Core)
## Derive the disaggregated cost estimates of selected OCIS for each city
## Last updated: March 15, 2020
############################################################################################

rm(list=ls())
library(rstudioapi)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")
## LOAD ODE function
#source("R/CascadeCEA-Model-0-Function-ode_model-Combination.R")

## SELECT city ##
CITY <- select.list(all.cities, multiple = FALSE,
                    title = 'Select city', graphics = FALSE)
# ww= 1; CITY <- all.cities[ww]

## LOAD inputs and ntervention ##
case             <- "DM"  # DM for deterministic, SA for sensitivity analysis
load.status.quo  <- TRUE
sums.only        <- FALSE
Discounting      <- 0.03
combination.list <- readRDS("Combination/Combination.list.rds")
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
source("01_Setup/CascadeCEA-Interventions-1-LoadParameterWorkspace-Combination.R")
current.int = interventions[combination.list[[ocis]]]

## RUN the model ##
out.ls.int <- intervention.model.combination(input.parameters = all.params,
                                             current.int = current.int)

out.ls.accum.int <- accum.outcomes.combination(input.parameters = all.params,
                                               pop.in           = out.ls.int[, 1:19, ],
                                               current.int      = current.int,
                                               sums.only        = sums.only)

saveRDS(out.ls.accum.int, paste0("Outputs/CostBreakdown/CostBreakdown-", CITY, "-Strategy-", ocis,"-documented.rds"))

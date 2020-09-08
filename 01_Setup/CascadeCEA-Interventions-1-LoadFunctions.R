##############################################
# Redundant when installing LEMpackHIV
##############################################
source("Scripts/CascadeCEA-Interventions-0-Function-intervention.model.combination.R")
# source("Scripts/CascadeCEA-Interventions-0-Function-intervention.model.sa.R")
source("Scripts/CascadeCEA-Interventions-0-Function-scale.interventions.combination.R")
# source("Scripts/CascadeCEA-Interventions-0-Function-true.false.interventions.R")
source("Scripts/CascadeCEA-Interventions-0-Function-true.false.interventions.combination.R")
source("Scripts/CascadeCEA-Interventions-0-Function-set.scale.race.gender.R")
source("Scripts/CascadeCEA-Interventions-0-Function-set.scale.risk.race.gender.R")
# source("Scripts/CascadeCEA-Interventions-0-Function-accumulate.outcomes.scenarios.R")
source("Scripts/CascadeCEA-Interventions-0-Function-accumulate.outcomes.combination.R")
# source("Scripts/CascadeCEA-Interventions-0-Function-accumulate.outcomes.sa.R")
# source("Scripts/CascadeCEA-Interventions-0-Function-accumulate.outcomes.R")
source("Scripts/CascadeCEA-Interventions-0-Function-time.period.infections.R")
source("Scripts/CascadeCEA-Interventions-0-Output-write.excel.R")
source("Scripts/CascadeCEA-Interventions-0-Function-comb.eva.func.R")

source("ModelCoreModules/CascadeCEA-Model-0-Function-ode.list.R")        #ode equations for HET and MSM
source("ModelCoreModules/CascadeCEA-Model-0-Function-ode.list.OAT.R")    #ode equations for PWID and MWID on OAT
source("ModelCoreModules/CascadeCEA-Model-0-Function-ode.list.offOAT.R") #ode equations for PWID and MWID not on OAT
source("ModelCoreModules/CascadeCEA-Model-0-Function-FoI.R")

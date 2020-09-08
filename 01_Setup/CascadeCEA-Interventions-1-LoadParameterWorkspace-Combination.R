source("01_Setup/CascadeCEA-Model-1-Module-Data.input.R")

### GET CALIBRATED PARAMETER SETS
if (case == "DM"){     #For the case of deterministic analysis based on the mean value of calibrationed parameters
  Int.Scale.ls   <- setNames(lapply(ls(pattern="^IntScale."), get), ls(pattern="^IntScale."))
  Int.Eff.ls     <- setNames(lapply(ls(pattern="^IntEff."), get), ls(pattern="^IntEff."))
  Int.Costs.ls   <- setNames(lapply(ls(pattern="^IntCosts."), get), ls(pattern="^IntCosts."))
  State.Costs.ls <- setNames(lapply(ls(pattern="^StateCosts."), get), ls(pattern="^StateCosts."))
  All.Costs.ls   <- c(Int.Costs.ls, State.Costs.ls)
  if(file.exists(paste0("Inputs/AllParams-Combination-DM-", CITY, ".rds"))){
    all.params   <- readRDS(paste0("Inputs/AllParams-Combination-DM-", CITY, ".rds"))
  } else if (!file.exists(paste0("Inputs/AllParams-Combination-DM-", CITY, ".rds"))){
    calib.params.ls <- readRDS(paste0("Inputs/Calib-Outputs-", CITY, ".rds"))   # Calibrated values for free parameter
    calib.params    <- calib.params.ls[[3]]
    calpar$pe = rowMeans(calib.params)
    if (CITY == "BAL"){
      calpar$pe[5] = 0.77  # Calibrated values for this parameter is bimodally distributed in Baltimore
    }
    source ("01_Setup/CascadeCEA-Model-0-Parameter.update.R")  # Update free parameters with the mean calibrated values
    all.params <- vparameters
    rm(calib.params.ls, calib.params)
    saveRDS(all.params, paste0("Inputs/AllParams-Combination-DM-", CITY, ".rds"))
  }
  all.params$move.pop.adj = TRUE     # adjust the population size of corresponding compartment at the end of each time step to maintain designated proportions in the given health state
  ## These are move.pop.adj trigger levels
  all.params$I.PLHIV.adj.level = 0.05
  all.params$D.PLHIV.adj.level = 0.05
  all.params$ART.PLHIV.adj.level = 0.95
  all.params$move.start = 1
  all.params$move.end = 10000
} else if (case == "SA"){
  Int.Scale.ls        <- setNames(lapply(ls(pattern="^IntScale."), get), ls(pattern="^IntScale."))
  All.Costs.ls.list   <- readRDS(paste0("PSA/", CITY, "-All.Costs.rds"))
  State.QALYs.ls.list <- readRDS(paste0("PSA/", CITY, "-StateQALYs.rds"))
  Int.Baseline.ls.list<- readRDS(paste0("PSA/", CITY, "-IntBaseline.rds"))
  Int.Eff.ls.list     <- readRDS(paste0("PSA/", CITY, "-IntEff.rds"))
  if(file.exists(paste0("Inputs/AllParams-Combination-SA-", CITY, ".rds"))){
    all.params.list   <- readRDS(paste0("Inputs/AllParams-Combination-SA-", CITY, ".rds"))
  } else if (!file.exists(paste0("Inputs/AllParams-Combination-SA-", CITY, ".rds"))){
    all.params.list   <- readRDS(paste0("PSA/", CITY, "-vparameters.rds"))
    saveRDS(all.params.list, paste0("Inputs/AllParams-Combination-SA-", CITY, ".rds"))
  }
}


#############################################################################
# 2. LOAD CITY COMPARATOR (Status Quo)
#############################################################################
# STATUS QUO (NAMING OF READ IN FILES TO BE UPDATED)
if (case == "DM"){
  if(file.exists(paste0("Inputs/Combination-DM-", CITY, "-refcase.rds"))){    # all population list of each compartment at each step
    ## Load reference case
    refcase.ls <- readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase.rds"))
    if(file.exists(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds"))){  # CEA outcomes of deterministic case
      refcase.cea.utcomes <- readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds"))
    } else if(!file.exists(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds"))){
      warning("Reference case outcomes data are missing")
    }
    if(file.exists(paste0("Inputs/Combination-DM-", CITY, "-refcase-infections.rds"))){ # Incidence outcomes of deterministic case
      refcase.infs <- readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-infections.rds"))
    } else if(!file.exists(paste0("Inputs/Combination-DM-", CITY, "-refcase-infections.rds"))){
      warning("Reference case infections data are missing")
    }
  } else if (!file.exists(paste0("Inputs/Combination-DM-", CITY, "-refcase.rds"))){
    ## Settings for reference
    refcase.ls <- intervention.model.combination (input.parameters = all.params,
                                                  current.int      = c("No interventions"))

    ## ACCUMULATE COSTS & QALYs
    refcase.cea.outcomes <- accum.outcomes.combination (input.parameters = all.params,
                                                        pop.in           = refcase.ls[, 1:19, ],
                                                        current.int      = c("No interventions"),
                                                        sums.only        = TRUE)

    # Get number of infections over the evaluation period
    refcase.infs <- time.period.infections(simul.in     = refcase.ls,
                                           start.period = start.proj,
                                           end.period   = end.proj,
                                           sust.period  = int.sus)

    ### SAVE REFERENCE OUTCOMES
    saveRDS(refcase.ls,           paste0("Inputs/Combination-DM-", CITY, "-refcase.rds"))
    saveRDS(refcase.cea.outcomes, paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds"))
    saveRDS(refcase.infs,         paste0("Inputs/Combination-DM-", CITY, "-refcase-infections.rds"))
  }

} else if (case == "SA"){
  if(file.exists(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))){
    refcase.cea.outcomes.temp <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))

    if(file.exists(paste0("Inputs/Combination-SA-", CITY, "-refcase-infections.rds"))){
      refcase.infs.temp <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-infections.rds"))
      refcase.infs      <- refcase.infs.temp[1 : param.sets]
      rm(refcase.infs.temp)
    } else if(!file.exists(paste0("Inputs/Combination-SA-", CITY, "-refcase-infections.rds"))){
      warning("Reference case infections data are missing")
    }

    refcase.cea.outcomes <- refcase.cea.outcomes.temp[1:param.sets]
    rm(refcase.cea.outcomes.temp)

  } else if (!file.exists(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))){
    if (file.exists(paste0("Inputs/", CITY, "-2040-refcase.rds"))){      #list restoring the population lists of reference case from 2000 PSA: >4GB
      refcase.ls.temp <- readRDS(paste0("Inputs/", CITY, "-2040-refcase.rds"))
    } else if (!file.exists(paste0("Inputs/", CITY, "-2040-refcase.rds"))){
      refcase.ls.temp <- foreach(kk=1:param.sets, .export = export.int.model.names
      ) %dopar% {
        out.ls.int <- intervention.model.combination(
          input.parameters = all.params.list[[kk]],
          current.int = c("No interventions"))
      }
      saveRDS(refcase.ls.temp, paste0("Inputs/", CITY, "-2040-refcase.rds"))
    }
    refcase.infs         <- vector("list", param.sets)
    refcase.cea.outcomes <- vector("list", param.sets)
    for(p in 1:param.sets){
      ## Set parameters
      refcase.pop       <- refcase.ls.temp[[p]]

      refcase.infs[[p]] <- time.period.infections (simul.in     = refcase.pop,
                                                   start.period = start.proj,
                                                   end.period   = end.proj,
                                                   sust.period  = int.sus)
      ## Set parameters
      all.params         <- all.params.list[[p]]
      StateQALYs         <- State.QALYs.ls.list[[p]]

      #These were unused in PSA for reference case but required by the function
      All.Costs.ls       <- All.Costs.ls.list[[p]]
      Int.Baseline.ls    <- Int.Baseline.ls.list[[p]]
      Int.Eff.ls         <- Int.Eff.ls.list[[p]]

      ## ACCUMULATE COSTS & QALYs
      refcase.cea.outcomes[[p]] <- accum.outcomes.combination(input.parameters = all.params,
                                                              pop.in           = refcase.pop[, 1:19, ],
                                                              current.int      = c("No interventions"),
                                                              sums.only        = TRUE)

    }
    saveRDS(refcase.infs,         paste0("Inputs/Combination-SA-", CITY, "-refcase-infections.rds"))
    saveRDS(refcase.cea.outcomes, paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))
  }
} #END "SA"/"DM" Loop

# Clean workspace by using lists for scale & costs (rm at end of file)
rm(list = ls(pattern = "(^IntCosts.)|(^StateCosts.)|(^IntScale.)|(^IntEff.)"))

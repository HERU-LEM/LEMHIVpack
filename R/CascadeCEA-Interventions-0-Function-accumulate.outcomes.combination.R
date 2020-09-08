#' Interventions
#'
#' \code{accum.outcomes.combination} Costs & QALYs calculations for each model period
#'
#' @param input.parameters model inputs
#' @param pop.in populations in each compartment over all evaluation time
#' @param current.int selected intervention
#' @param sums.only logical to only output the total of results for full time period (default = TRUE)
#'
#' @return
#' List with total and period-stratified outcomes (disaggregated costs, QALYs)
#' @export

accum.outcomes.combination<- function(input.parameters,
                                      pop.in,
                                      current.int,
                                      sums.only = TRUE){
  if (case == "DM"){
    # Set cost inputs
    list2env(c(All.Costs.ls, Int.Eff.ls, Int.Scale.ls), environment())
  } else if (case == "SA"){
    list2env(c(All.Costs.ls, Int.Baseline.ls, Int.Eff.ls, Int.Scale.ls), environment())
  }
  
  # Set TRUE/FALSE intervention parameters
  tf.temp   <- true.false.interventions.combination(current.int)
  
  # Set sustainment parameters
  int.start <- end_yr_ind[(which(int.first.year == yr, arr.ind = TRUE) - 1)]
  int.end   <- (int.start + (int.sus * 12))  # 5 = sustainment period for scenarios
  
  if (!all(current.int == c("No interventions"))){
    # Set intervention parameters for scale*effect
    scale.m <- scale.interventions.combination(current.int, tf.temp)
  }
  
  # Holding arrays
  QALYs.period            <- vector("list", dim(pop.in)[1])
  hru.costs.period        <- vector("list", dim(pop.in)[1])
  art.costs.period        <- vector("list", dim(pop.in)[1])
  art.ini.costs.period    <- vector("list", dim(pop.in)[1])
  oat.costs.period        <- vector("list", dim(pop.in)[1])
  prep.costs.period       <- vector("list", dim(pop.in)[1])
  prep.tests.costs.period <- vector("list", dim(pop.in)[1])
  tests.costs.period      <- vector("list", dim(pop.in)[1])
  # Intervention costs
  int.costs.period        <- vector("list", dim(pop.in)[1])
  int.impl.costs.period   <- vector("list", dim(pop.in)[1])
  int.sust.costs.period   <- vector("list", dim(pop.in)[1])
  
  
  for(i in 1:(dim(pop.in)[1])){
    
    CurrentPeriod <- pop.in[i, , ]
    rownames(CurrentPeriod) <- state.name[-c(20:24)]
    
    # Only consider evaluation period for accumulating outcomes
    if(i > int.start & i <= (int.start + (20 * 12))){
    # QALYs
    QALYs.period[[i]]            <- ((CurrentPeriod * StateQALYs)     / ((1 + Discounting / 12) ^ (i - int.start)))
    # STATE COSTS
    hru.costs.period[[i]]        <- ((CurrentPeriod * StateCostsHRU)  / ((1 + Discounting / 12) ^ (i - int.start)))
    art.costs.period[[i]]        <- ((CurrentPeriod * StateCostsART)  / ((1 + Discounting / 12) ^ (i - int.start)))
    oat.costs.period[[i]]        <- ((CurrentPeriod * StateCostsOAT)  / ((1 + Discounting / 12) ^ (i - int.start)))
    prep.costs.period[[i]]       <- ((CurrentPeriod * StateCostsPrEP) / ((1 + Discounting / 12) ^ (i - int.start)))
    prep.tests.costs.period[[i]] <- ((CurrentPeriod * input.parameters$psi.p * StateCostsTests.psi.p) /
                                       ((1 + Discounting / 12) ^ (i - int.start)))
    
    ####Initialize intervention cost, int, impl and sust cost = 0 outside impl&sust period
    int.costs.period[[i]]      <- (CurrentPeriod * 0)
    int.impl.costs.period[[i]] <- (CurrentPeriod * 0)
    int.sust.costs.period[[i]] <- (CurrentPeriod * 0)
    
    ############################
    # TESTING COST: time-varying testing rates
    
    ## TESTING
    if(i<12){
      psi = input.parameters$psi.m[1, ]
    } else if (i<24) {
      psi = input.parameters$psi.m[2, ]
    } else if (i<36) {
      psi = input.parameters$psi.m[3, ]
    } else if (i>=36) {
      psi = input.parameters$psi.m[4, ]
    }
    
    psi.matrix <- matrix(0, nrow = 42, ncol = 19)
    colnames(psi.matrix) <- state.name[-c(20:24)]
    
    psi.matrix[ , c("S1", "Ia", "I1")] <- psi
    psi.matrix[ , "I2"] <- psi + input.parameters$v2
    psi.matrix[ , "I3"] <- psi + input.parameters$v3
    psi.matrix <- t(psi.matrix)
    
    
    ## INTERVENTION
    
    # Testing costs
    tests.costs.period[[i]] <- ((CurrentPeriod * psi.matrix * StateCostsTests.psi) /
                                  ((1 + Discounting / 12) ^ (i - int.start)))
    
    if(tf.temp$intervention.testing.modif==TRUE){
      if(i > int.start & i <= int.end){
        if(is.element(c("Opt-out testing (ER)"), current.int)){
          IntCosts.Tests <- IntCosts.test.optout.ER
          IntCosts.impl.Tests <- IntCosts.impl.test.optout.ER
          IntCosts.sust.Tests <- IntCosts.sust.test.optout.ER
          IntCosts.impl.lump.Tests <- IntCosts.impl.lump.test.optout.ER
          IntCosts.sust.lump.Tests <- IntCosts.sust.lump.test.optout.ER
          
          temp.int <- IntEff.test.optout.ER
          scale.v <- IntScale.test.optout.ER["S1", ]
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                          rep(temp.int, (int.end - int.start - scale.up.period)))
          int.test.eff <- scale.m.temp * (scale.up.v - 1) + 1
          
          psi.matrix.temp <- matrix(0, nrow = 42, ncol = 19)
          colnames(psi.matrix.temp) <- state.name[-c(20:24)]
          psi.matrix.temp[ , c("S1", "Ia", "I1", "I2", "I3")] <- psi
          psi.matrix.temp <- t(psi.matrix.temp)
          psi.matrix.temp <- psi.matrix.temp * int.test.eff[(i - int.start), ]
          psi.matrix.temp["I2", ] <- psi.matrix.temp["I2", ] + input.parameters$v2
          psi.matrix.temp["I3", ] <- psi.matrix.temp["I3", ] + input.parameters$v3
          
          psi.matrix.full <- matrix(0, nrow = 42, ncol = 19)
          colnames(psi.matrix.full) <- state.name[-c(20:24)]
          psi.matrix.full[ , c("S1", "Ia", "I1", "I2", "I3")] <- psi
          psi.matrix.full <- t(psi.matrix.full)
          psi.matrix.full <- psi.matrix.full * int.test.eff[scale.up.period, ]
          psi.matrix.full["I2", ] <- psi.matrix.full["I2", ] + input.parameters$v2
          psi.matrix.full["I3", ] <- psi.matrix.full["I3", ] + input.parameters$v3
          
          # Incremental testing costs
          testing.costs.mod <- ((CurrentPeriod * (psi.matrix.temp - psi.matrix) * StateCostsTests.psi)
                                / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Intervention costs
          int.costs.mod <- ((CurrentPeriod * psi.matrix.temp * IntCosts.Tests)
                            / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if(i <= (scale.up.period + int.start)){
            int.impl.costs.temp <- (((CurrentPeriod * psi.matrix.full * IntCosts.impl.Tests) + IntCosts.impl.lump.Tests) /
                                      ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- (((CurrentPeriod * psi.matrix.temp * IntCosts.sust.Tests) + IntCosts.sust.lump.Tests)/
                                      ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- (int.costs.mod + testing.costs.mod) + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
          # END OF SUSTAINMENT PERIOD
        }
        if(is.element(c("Opt-out testing (PC)"), current.int)){
          IntCosts.Tests <- IntCosts.test.optout.PC
          IntCosts.impl.Tests <- IntCosts.impl.test.optout.PC
          IntCosts.sust.Tests <- IntCosts.sust.test.optout.PC
          IntCosts.impl.lump.Tests <- IntCosts.impl.lump.test.optout.PC
          IntCosts.sust.lump.Tests <- IntCosts.sust.lump.test.optout.PC
          
          temp.int <- IntEff.test.optout.PC
          scale.v <- IntScale.test.optout.PC["S1", ]
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                          rep(temp.int, (int.end - int.start - scale.up.period)))
          int.test.eff <- scale.m.temp * (scale.up.v - 1) + 1
          
          psi.matrix.temp <- matrix(0, nrow = 42, ncol = 19)
          colnames(psi.matrix.temp) <- state.name[-c(20:24)]
          psi.matrix.temp[ , c("S1", "Ia", "I1", "I2", "I3")] <- psi
          psi.matrix.temp <- t(psi.matrix.temp)
          psi.matrix.temp <- psi.matrix.temp * int.test.eff[(i - int.start), ]
          psi.matrix.temp["I2", ] <- psi.matrix.temp["I2", ] + input.parameters$v2
          psi.matrix.temp["I3", ] <- psi.matrix.temp["I3", ] + input.parameters$v3
          
          psi.matrix.full <- matrix(0, nrow = 42, ncol = 19)
          colnames(psi.matrix.full) <- state.name[-c(20:24)]
          psi.matrix.full[ , c("S1", "Ia", "I1", "I2", "I3")] <- psi
          psi.matrix.full <- t(psi.matrix.full)
          psi.matrix.full <- psi.matrix.full * int.test.eff[scale.up.period, ]
          psi.matrix.full["I2", ] <- psi.matrix.full["I2", ] + input.parameters$v2
          psi.matrix.full["I3", ] <- psi.matrix.full["I3", ] + input.parameters$v3
          
          # Incremental testing costs
          testing.costs.mod <- ((CurrentPeriod * (psi.matrix.temp - psi.matrix) * StateCostsTests.psi)
                                / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Intervention costs
          int.costs.mod <- ((CurrentPeriod * psi.matrix.temp * IntCosts.Tests)
                            / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if(i <= (scale.up.period + int.start)){
            int.impl.costs.temp <- (((CurrentPeriod * psi.matrix.full * IntCosts.impl.Tests) + IntCosts.impl.lump.Tests)/
                                      ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- (((CurrentPeriod * psi.matrix.temp * IntCosts.sust.Tests) + IntCosts.sust.lump.Tests)/
                                      ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- (int.costs.mod + testing.costs.mod) + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
          # END OF SUSTAINMENT PERIOD
          
        }
        if(is.element(c("EMR testing reminder"), current.int)){
          IntCosts.Tests <- IntCosts.test.EMR
          IntCosts.impl.Tests <- IntCosts.impl.test.EMR
          IntCosts.sust.Tests <- IntCosts.sust.test.EMR
          IntCosts.impl.lump.Tests <- IntCosts.impl.lump.test.EMR
          IntCosts.sust.lump.Tests <- IntCosts.sust.lump.test.EMR
          
          temp.int <- IntEff.test.EMR
          scale.v <- IntScale.test.EMR["S1", ]
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                          rep(temp.int, (int.end - int.start - scale.up.period)))
          int.test.eff <- scale.m.temp * (scale.up.v - 1) + 1
          
          psi.matrix.temp <- matrix(0, nrow = 42, ncol = 19)
          colnames(psi.matrix.temp) <- state.name[-c(20:24)]
          psi.matrix.temp[ , c("S1", "Ia", "I1", "I2", "I3")] <- psi
          psi.matrix.temp <- t(psi.matrix.temp)
          psi.matrix.temp <- psi.matrix.temp * int.test.eff[(i - int.start), ]
          psi.matrix.temp["I2", ] <- psi.matrix.temp["I2", ] + input.parameters$v2
          psi.matrix.temp["I3", ] <- psi.matrix.temp["I3", ] + input.parameters$v3
          
          psi.matrix.full <- matrix(0, nrow = 42, ncol = 19)
          colnames(psi.matrix.full) <- state.name[-c(20:24)]
          psi.matrix.full[ , c("S1", "Ia", "I1", "I2", "I3")] <- psi
          psi.matrix.full <- t(psi.matrix.full)
          psi.matrix.full <- psi.matrix.full * int.test.eff[scale.up.period, ]
          psi.matrix.full["I2", ] <- psi.matrix.full["I2", ] + input.parameters$v2
          psi.matrix.full["I3", ] <- psi.matrix.full["I3", ] + input.parameters$v3
          
          # Incremental testing costs
          testing.costs.mod <- ((CurrentPeriod * (psi.matrix.temp - psi.matrix) * StateCostsTests.psi)
                                / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Intervention costs
          int.costs.mod <- ((CurrentPeriod * psi.matrix.temp * IntCosts.Tests)
                            / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if(i <= (scale.up.period + int.start)){
            int.impl.costs.temp <- (((CurrentPeriod * psi.matrix.full * IntCosts.impl.Tests) + IntCosts.impl.lump.Tests)/
                                      ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- (((CurrentPeriod * psi.matrix.temp * IntCosts.sust.Tests) + IntCosts.sust.lump.Tests)/
                                      ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- (int.costs.mod + testing.costs.mod) + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
          # END OF SUSTAINMENT PERIOD
        }
        if(is.element(c("Nurse-initiated testing"), current.int)){
          IntCosts.Tests <- IntCosts.test.nurse
          IntCosts.impl.Tests <- IntCosts.impl.test.nurse
          IntCosts.sust.Tests <- IntCosts.sust.test.nurse
          IntCosts.impl.lump.Tests <- IntCosts.impl.lump.test.nurse
          IntCosts.sust.lump.Tests <- IntCosts.sust.lump.test.nurse
          
          temp.int <- IntEff.test.nurse
          scale.v <- IntScale.test.nurse["S1", ]
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                          rep(temp.int, (int.end - int.start - scale.up.period)))
          int.test.eff <- scale.m.temp * (scale.up.v - 1) + 1
          
          psi.matrix.temp <- matrix(0, nrow = 42, ncol = 19)
          colnames(psi.matrix.temp) <- state.name[-c(20:24)]
          psi.matrix.temp[ , c("S1", "Ia", "I1", "I2", "I3")] <- psi
          psi.matrix.temp <- t(psi.matrix.temp)
          psi.matrix.temp <- psi.matrix.temp * int.test.eff[(i - int.start), ]
          psi.matrix.temp["I2", ] <- psi.matrix.temp["I2", ] + input.parameters$v2
          psi.matrix.temp["I3", ] <- psi.matrix.temp["I3", ] + input.parameters$v3
          
          psi.matrix.full <- matrix(0, nrow = 42, ncol = 19)
          colnames(psi.matrix.full) <- state.name[-c(20:24)]
          psi.matrix.full[ , c("S1", "Ia", "I1", "I2", "I3")] <- psi
          psi.matrix.full <- t(psi.matrix.full)
          psi.matrix.full <- psi.matrix.full * int.test.eff[scale.up.period, ]
          psi.matrix.full["I2", ] <- psi.matrix.full["I2", ] + input.parameters$v2
          psi.matrix.full["I3", ] <- psi.matrix.full["I3", ] + input.parameters$v3
          
          # Incremental testing costs
          testing.costs.mod <- ((CurrentPeriod * (psi.matrix.temp - psi.matrix) * StateCostsTests.psi)
                                / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Intervention costs
          int.costs.mod <- ((CurrentPeriod * psi.matrix.temp * IntCosts.Tests)
                            / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if(i <= (scale.up.period + int.start)){
            int.impl.costs.temp <- (((CurrentPeriod * psi.matrix.full * IntCosts.impl.Tests) + IntCosts.impl.lump.Tests)/
                                      ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- (((CurrentPeriod * psi.matrix.temp * IntCosts.sust.Tests) + IntCosts.sust.lump.Tests)/
                                      ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- (int.costs.mod + testing.costs.mod) + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
          # END OF SUSTAINMENT PERIOD
        }
        if(is.element(c("OAT integrated testing"), current.int)){
          IntCosts.Tests <- IntCosts.test.OAT
          IntCosts.impl.Tests <- IntCosts.impl.test.OAT
          IntCosts.sust.Tests <- IntCosts.sust.test.OAT
          
          temp.int <- IntEff.test.OAT
          scale.v <- IntScale.test.OAT["S1", ]
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                          rep(temp.int, (int.end - int.start - scale.up.period)))
          int.test.eff <- scale.m.temp * (scale.up.v - 1) + 1
          
          psi.matrix.temp <- psi.matrix
          psi.matrix.temp["S1", oat] <- psi[oat] * int.test.eff[(i - int.start), oat]
          psi.matrix.temp["Ia", oat] <- psi[oat] * int.test.eff[(i - int.start), oat]
          psi.matrix.temp["I1", oat] <- psi[oat] * int.test.eff[(i - int.start), oat]
          psi.matrix.temp["I2", oat] <- psi[oat] * int.test.eff[(i - int.start), oat] + input.parameters$v2
          psi.matrix.temp["I3", oat] <- psi[oat] * int.test.eff[(i - int.start), oat] + input.parameters$v3
          
          psi.matrix.full <- psi.matrix
          psi.matrix.full["S1", oat] <- psi[oat] * int.test.eff[scale.up.period, oat]
          psi.matrix.full["Ia", oat] <- psi[oat] * int.test.eff[scale.up.period, oat]
          psi.matrix.full["I1", oat] <- psi[oat] * int.test.eff[scale.up.period, oat]
          psi.matrix.full["I2", oat] <- psi[oat] * int.test.eff[scale.up.period, oat] + input.parameters$v2
          psi.matrix.full["I3", oat] <- psi[oat] * int.test.eff[scale.up.period, oat] + input.parameters$v3
          
          # Incremental testing costs
          testing.costs.mod <- ((CurrentPeriod * (psi.matrix.temp - psi.matrix) * StateCostsTests.psi)
                                / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Intervention costs
          int.costs.mod <- ((CurrentPeriod * psi.matrix.temp * IntCosts.Tests)
                            / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if(i <= (scale.up.period + int.start)){
            int.impl.costs.temp <- ((CurrentPeriod * psi.matrix.full * IntCosts.impl.Tests) /
                                      ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- ((CurrentPeriod * psi.matrix.temp * IntCosts.sust.Tests) /
                                      ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- (int.costs.mod + testing.costs.mod) + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
          # END OF SUSTAINMENT PERIOD
        }
      }
    } # END OF TESTING LOOP
    
    
    ############################
    # ART ENGAGEMENT COST
    ## INCLUDES ART-INITIATION RELATED COSTS, i.e. CD4 & vL tests, FOR EVERY NEW ART EPISODE
    ## Same as STATE COSTS above.
    ## Placed here as psi is time-varying
    art.ini.matrix <- matrix(0, nrow = 42, ncol = 19)
    colnames(art.ini.matrix) <- state.name[-c(20:24)]
    phi   = matrix(0, length(input.parameters$names.gp), 3)
    alpha = matrix(0, length(input.parameters$names.gp), 3)
    alpha.re = numeric(length(input.parameters$names.gp))
    theta.t1O = numeric(length(input.parameters$names.gp))
    theta.t2O = numeric(length(input.parameters$names.gp))
    theta.t3O = numeric(length(input.parameters$names.gp))
    for (q in 1:18){
      ind = which(rname %in% names18[q])
      #psi[ind]=psi18[i];
      phi[ind, ]    = c(input.parameters$phi1[q],   input.parameters$phi2[q],   input.parameters$phi3[q])
      alpha[ind, ]  = c(input.parameters$alpha1[q], input.parameters$alpha2[q], input.parameters$alpha3[q])
      alpha.re[ind] = input.parameters$O_T[q]
      theta.t1O[ind]= input.parameters$T1_O[q];  theta.t2O[ind]= input.parameters$T2_O[q];  theta.t3O[ind] = input.parameters$T3_O[q]
    }
    
    if (tf.temp$intervention.testing.modif==TRUE & i > int.start & i <= int.end){
      art.ini.matrix[ , "I1"] <- psi.matrix.temp["I1", ] * phi[ , 1]
      art.ini.matrix[ , "I2"] <- psi.matrix.temp["I2", ] * phi[ , 2]
      art.ini.matrix[ , "I3"] <- psi.matrix.temp["I3", ] * phi[ , 3]
    } else {
      art.ini.matrix[ , "I1"] <- psi.matrix["I1", ] * phi[ , 1]
      art.ini.matrix[ , "I2"] <- psi.matrix["I2", ] * phi[ , 2]
      art.ini.matrix[ , "I3"] <- psi.matrix["I3", ] * phi[ , 3]
    }
    art.ini.matrix[ , c("D1", "D2", "D3")]   <- alpha[ , 1:3]
    art.ini.matrix[ , c("T1", "T2", "T3")]   <- -cbind(theta.t1O, theta.t2O, theta.t3O) # MINUS?
    art.ini.matrix[ , c("O1", "O2", "O3") ]  <- alpha.re
    art.ini.matrix                           <- t(art.ini.matrix)
    art.ini.matrix[c("T1", "T2", "T3"), oat] <- art.ini.matrix[c("T1", "T2", "T3"), oat] * input.parameters$theta.o.oat
    
    # ART INITIATION COSTS
    art.ini.costs.period[[i]] <- ((CurrentPeriod * art.ini.matrix * StateCostsARTinit) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
    
    ## ART INITIATION INTERVENTION
    if (tf.temp$intervention.initiation.modif==TRUE){
      if (i > int.start & i <= int.end){
        art.ini.matrix.temp <- art.ini.matrix
        art.ini.matrix.temp["D1", ] = alpha[ , 1] * scale.m$int.art.ini.eff[(i - int.start), ]
        art.ini.matrix.temp["D2", ] = alpha[ , 2] * scale.m$int.art.ini.eff[(i - int.start), ]
        art.ini.matrix.temp["D3", ] = alpha[ , 3] * scale.m$int.art.ini.eff[(i - int.start), ]
        
        # Incremental ART initiation costs
        art.ini.costs.mod <- ((CurrentPeriod * (art.ini.matrix.temp - art.ini.matrix) * StateCostsARTinit) /
                                ((1 + Discounting / 12) ^ (i - int.start)))
        
        # Intervention costs
        int.costs.mod <- (((CurrentPeriod * input.parameters$prop.link) * IntScale.ARTinit["D1", ] * IntCosts.ARTinit) /
                            ((1 + Discounting / 12) ^ (i - int.start)))
        
        # Implementation & sustainment costs
        if (i <= (scale.up.period + int.start)){
          int.impl.costs.temp <- (((CurrentPeriod * input.parameters$prop.link) * IntScale.ARTinit["D1", ] * IntCosts.impl.ARTinit) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
          int.sust.costs.temp <- ((CurrentPeriod * input.parameters$prop.link) * 0)
          # END OF SCALE-UP PERIOD
        } else {
          int.impl.costs.temp <- ((CurrentPeriod * input.parameters$prop.link) * 0)
          int.sust.costs.temp <- (((CurrentPeriod * input.parameters$prop.link) * IntScale.ARTinit["D1", ] * IntCosts.sust.ARTinit) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
        }
        int.costs.period[[i]] <- int.costs.mod + art.ini.costs.mod + int.costs.period[[i]]
        int.impl.costs.period[[i]] <-  int.impl.costs.temp + int.impl.costs.period[[i]]
        int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
        # END OF SUSTAINMENT PERIOD
      }
    } # END OF ART INITIATION LOOP
    
    ## ART RETENTION INTERVENTION
    if (tf.temp$intervention.retention.modif==TRUE){
      if (i > int.start & i <= int.end){
        # art.ini.matrix.temp <- art.ini.matrix
        if(is.element(c("ART retention"), current.int)){
          # art.ini.matrix.temp[c("T1", "T2", "T3"), ] <- -rbind(theta.t1O, theta.t2O, theta.t3O) * (1 / int.retention.temp.v[i - int.start])
          this.scale <- IntScale.ARTreten["T1", ]
        } else if (is.element(c("ART retention, targeted"), current.int)){
          # art.ini.matrix.temp["T3", ] <- - theta.t3O * (1 / int.retention.temp.v[i - int.start])
          IntCosts.ARTreten = IntCosts.ARTreten.target
          IntCosts.impl.ARTreten = IntCosts.impl.ARTreten.target
          IntCosts.sust.ARTreten = IntCosts.sust.ARTreten.target
          this.scale <- IntScale.ARTreten.target["T3", ]
        }
        
        # Intervention costs
        int.costs.mod <- ((CurrentPeriod * this.scale * IntCosts.ARTreten ) /
                            ((1 + Discounting / 12) ^ (i - int.start)))
        
        # Implementation & sustainment costs
        if (i <= (scale.up.period + int.start)){
          int.impl.costs.temp <- ((CurrentPeriod * this.scale * IntCosts.impl.ARTreten) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
          int.sust.costs.temp <- (CurrentPeriod * 0)
          # END OF SCALE-UP PERIOD
        } else {
          int.impl.costs.temp <- (CurrentPeriod * 0)
          int.sust.costs.temp <- ((CurrentPeriod * this.scale * IntCosts.sust.ARTreten) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
        }
        int.costs.period[[i]] <- int.costs.mod + int.costs.period[[i]]
        int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
        int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
        # END OF SUSTAINMENT PERIOD
      }
    } # END OF ART RETENTION LOOP
    
    ## ART DROPOUT EMR-PROMPT INTERVENTION
    if(tf.temp$intervention.dropout.modif==TRUE){
      if (i > int.start & i <= int.end){
        # Intervention costs
        int.costs.mod <- ((CurrentPeriod * IntScale.ARTEMRprompt["T1", ] * IntCosts.ARTEMRprompt ) /
                            ((1 + Discounting / 12) ^ (i - int.start)))
        
        # Implementation & sustainment costs
        if (i <= (scale.up.period + int.start)){
          int.impl.costs.temp <- ((CurrentPeriod * IntScale.ARTEMRprompt["T1", ] * IntCosts.impl.ARTEMRprompt) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
          int.sust.costs.temp <- (CurrentPeriod * 0)
          # END OF SCALE-UP PERIOD
        } else {
          int.impl.costs.temp <- (CurrentPeriod * 0)
          int.sust.costs.temp <- ((CurrentPeriod * IntScale.ARTEMRprompt["T1", ] * IntCosts.sust.ARTEMRprompt) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
        }
        int.costs.period[[i]] <- int.costs.mod + int.costs.period[[i]]
        int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
        int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
        # END OF SUSTAINMENT PERIOD
      }
    } # END OF ART ART DROPOUT EMR-PROMPT INTERVENTION LOOP
    
    ## IMMEDIATE ART INTERVENTION
    if(tf.temp$intervention.immediateART.modif==TRUE){
      if (i > int.start & i <= int.end){
        
        art.ini.matrix.temp <- art.ini.matrix
        if(is.element(c("RAPID ART"), current.int)){
          art.ini.matrix.temp[c("I1", "I2", "I3"), ] = psi.matrix[c("I1", "I2", "I3") , ] * t(phi[ , 1:3]) * scale.m$int.art.imm.eff[(i - int.start), ]
        } else if (is.element(c("RAPID ART, targeted"), current.int)){
          art.ini.matrix.temp["I3", ] = psi.matrix["I3", ] * phi[ , 3] * scale.m$int.art.imm.eff[(i - int.start), ]
          IntCosts.immART = IntCosts.immART.target
          IntCosts.impl.immART = IntCosts.impl.immART.target
          IntCosts.sust.immART = IntCosts.sust.immART.target
        }
        
        # Incremental ART initiation costs
        art.ini.costs.mod <- ((CurrentPeriod * (art.ini.matrix.temp - art.ini.matrix) * StateCostsARTinit) /
                                ((1 + Discounting / 12) ^ (i - int.start)))
        
        # Intervention costs
        int.costs.mod <- (((CurrentPeriod * psi.matrix) * IntScale.immART["I1", ] * IntCosts.immART ) /
                            ((1 + Discounting / 12) ^ (i - int.start)))
        
        # Implementation & sustainment costs
        if (i <= (scale.up.period + int.start)){
          
          int.impl.costs.temp <- (((CurrentPeriod * psi.matrix) * IntScale.immART["I1", ] * IntCosts.impl.immART) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
          int.sust.costs.temp <- ((CurrentPeriod * psi.matrix) * 0)
          # END OF SCALE-UP PERIOD
        } else {
          int.impl.costs.temp <- ((CurrentPeriod * psi.matrix) * 0)
          int.sust.costs.temp <- (((CurrentPeriod * psi.matrix) * IntScale.immART["I1", ] * IntCosts.sust.immART) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
        }
        int.costs.period[[i]] <- int.costs.mod + art.ini.costs.mod + int.costs.period[[i]]
        int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
        int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
        # END OF SUSTAINMENT PERIOD
      }
    } # END OF IMMEDIATE ART INTERVENTION LOOP
    
    ## ART RE-INITIATION INTERVENTION
    if(tf.temp$intervention.reinitiation.modif==TRUE){
      if (i > int.start & i <= int.end){
        if(is.element(c("ART re-initiation"), current.int)){
          temp.int <- IntEff.reART
          scale.v <- IntScale.reART["O1", ]
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                          rep(temp.int, (int.end - int.start - scale.up.period)))
          int.art.reini.eff <- scale.m.temp * (scale.up.v - 1) + 1
          
          art.ini.matrix.temp <- art.ini.matrix
          art.ini.matrix.temp["O1", ] = alpha.re * int.art.reini.eff[(i - int.start), ]
          art.ini.matrix.temp["O2", ] = alpha.re * int.art.reini.eff[(i - int.start), ]
          art.ini.matrix.temp["O3", ] = alpha.re * int.art.reini.eff[(i - int.start), ]
          
          # Incremental ART initiation costs
          art.ini.costs.mod <- ((CurrentPeriod * (art.ini.matrix.temp - art.ini.matrix) * StateCostsARTinit) /
                                  ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Intervention costs
          int.costs.mod <- ((CurrentPeriod * IntScale.reART["O1", ] * IntCosts.reART ) /
                              ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if (i <= (scale.up.period + int.start)){
            int.impl.costs.temp <- ((CurrentPeriod * IntScale.reART["O1", ] * IntCosts.impl.reART) /
                                      ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- ((CurrentPeriod * IntScale.reART["O1", ] * IntCosts.sust.reART) /
                                      ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- int.costs.mod + art.ini.costs.mod + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
          # END OF RE-ART LOOP
          
        } else if(is.element(c("ART re-linkage"), current.int)){
          temp.int <- IntEff.relink
          scale.v <- IntScale.relink["O1", ]
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                          rep(temp.int, (int.end - int.start - scale.up.period)))
          int.art.reini.eff <- scale.m.temp * (scale.up.v - 1) + 1
          
          art.ini.matrix.temp <- art.ini.matrix
          art.ini.matrix.temp["O1", ] = alpha.re * int.art.reini.eff[(i - int.start), ]
          art.ini.matrix.temp["O2", ] = alpha.re * int.art.reini.eff[(i - int.start), ]
          art.ini.matrix.temp["O3", ] = alpha.re * int.art.reini.eff[(i - int.start), ]
          
          # Incremental ART initiation costs
          art.ini.costs.mod <- ((CurrentPeriod * (art.ini.matrix.temp - art.ini.matrix) * StateCostsARTinit) /
                                  ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Intervention costs
          int.costs.mod <- ((CurrentPeriod * IntScale.relink["O1", ] * IntCosts.relink ) /
                              ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if (i <= (scale.up.period + int.start)){
            int.impl.costs.temp <- ((IntCosts.impl.lump.relink) / ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- ((IntCosts.sust.relink) / ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- int.costs.mod + art.ini.costs.mod + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
          # END OF RE-LINKAGE LOOP
        }
      } # END OF SUSTAINMENT PERIOD
    } # END OF ART RE-ENGAGEMENT LOOP
    
    ############################
    # OAT INTERVENTION
    if(tf.temp$intervention.oat.modif==TRUE){
      if (i > int.start & i <= int.end){
        pop.pwid = input.parameters$pop.pwid
        nOAT.m   = input.parameters$nOAT.m
        nOAT     = round(c(nOAT.m[4, 1:3] * c(pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6]), 1-pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6])), nOAT.m[4, 4:6]))
        if(is.element(c("OAT with BUP"), current.int)){
          IntCosts.OAT = IntCosts.BUP
          IntCosts.impl.OAT = IntCosts.impl.BUP
          IntCosts.sust.OAT = IntCosts.sust.BUP
          
          scale.growth <- IntScale.BUP
          scale.v <- IntBaseline.BUP
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(cumprod(c(1, rep((1 + scale.growth), scale.up.period)))[-1],
                          rep(((1 + scale.growth)^scale.up.period), (int.end - int.start - scale.up.period)))
          int.oat.eff <- scale.m.temp * (scale.up.v - 1) + t(replicate(int.sus * 12, nOAT.m[4,]))
          
          nOAT.int = round(c(as.vector(t(int.oat.eff[(i - int.start), 1:3])) * c(pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6]), 1-pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6])), as.vector(t(int.oat.eff[(i - int.start), 4:6]))))
          
          prop.oat.int = nOAT.int/pop.pwid
          if(any(prop.oat.int > 0.95 * 0.727)) {
            while (any(prop.oat.int > 0.95 * 0.727 + 0.00001)){
              sat            <- which(prop.oat.int >= 0.95 * 0.727)
              nOAT.int[-sat] <- nOAT.int[-sat] + sum(nOAT.int[sat] - pop.pwid[sat] * 0.95 * 0.727) * (pop.pwid[-sat] / sum(pop.pwid[-sat]))
              nOAT.int[sat]  <- pop.pwid[sat] * 0.95 * 0.727
              prop.oat.int   <- nOAT.int/pop.pwid
            }
          }
          
          oat.e     = (nOAT / pop.pwid) / (1 - (nOAT / pop.pwid)) * input.parameters$oat.q
          oat.e.int = prop.oat.int / (1 - prop.oat.int) * input.parameters$oat.q
          ## to address entry rate > 1 or <0
          oat.e[which(oat.e > 0.9 | oat.e <= 0)] <- 0.9
          oat.e.int[which(oat.e.int > 0.9 | oat.e.int <= 0)] <- 0.9
          ##
          rname = gsub(paste(c("/OAT", "/low", "/high"), collapse="|"), "", names.gp)
          pwid.name = names18[grep("PWID", names18)]
          oat.e.all.m.int <- oat.e.all.m <- matrix(0, nrow = 19, ncol = 42)
          for (q in 1:length(pwid.name)){
            ind = which(rname %in% pwid.name[q])
            oat.e.all.m[ , intersect(ind, off.oat)]     = oat.e[q]
            oat.e.all.m.int[ , intersect(ind, off.oat)] = oat.e.int[q]
          }
          
          int.costs.mod <- ((CurrentPeriod  * (oat.e.all.m.int - oat.e.all.m) * IntCosts.OAT) / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if (i <= (scale.up.period + int.start)){
            nOAT.int.full <- round(c(as.vector(t(int.oat.eff[scale.up.period, 1:3])) * c(pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6]), 1-pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6])), as.vector(t(int.oat.eff[scale.up.period, 4:6]))))
            
            prop.oat.full = nOAT.int.full/pop.pwid
            if(any(prop.oat.full > 0.95 * 0.727)) {
              while (any(prop.oat.full > 0.95 * 0.727 + 0.00001)){
                sat <- which(prop.oat.full >= 0.95 * 0.727)
                nOAT.int.full[-sat] <- nOAT.int.full[-sat] + sum(nOAT.int.full[sat] - pop.pwid[sat] * 0.95 * 0.727) * (pop.pwid[-sat] / sum(pop.pwid[-sat]))
                nOAT.int.full[sat] <- pop.pwid[sat] * 0.95 * 0.727
                prop.oat.full = nOAT.int.full/pop.pwid
              }
            }
            
            oat.e.int.full = prop.oat.full / (1 - prop.oat.full) * input.parameters$oat.q
            ## to address entry rate > 1 or <0
            oat.e.int.full[which(oat.e.int.full > 0.9 | oat.e.int.full <= 0)] <- 0.9
            ##
            oat.e.all.m.int.full = oat.e.all.m
            for (q in 1:length(pwid.name)){
              ind = which(rname %in% pwid.name[q])
              oat.e.all.m.int.full[ , intersect(ind, off.oat)] = oat.e.int.full[q]
            }
            int.impl.costs.temp <- ((CurrentPeriod  * oat.e.all.m.int.full * IntCosts.impl.OAT) / ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- ((CurrentPeriod * oat.e.all.m.int * IntCosts.sust.OAT) / ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- int.costs.mod + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
          # END OF SUSTAINMENT PERIOD
        }
        if(is.element(c("OAT with methadone"), current.int)){
          IntCosts.OAT = IntCosts.MET
          IntCosts.impl.OAT = IntCosts.impl.MET
          IntCosts.sust.OAT = IntCosts.sust.MET
          
          scale.growth <- IntScale.MET
          scale.v <- IntBaseline.MET
          scale.m.temp <- t(replicate((int.sus * 12), scale.v))
          scale.up.v <- c(cumprod(c(1, rep((1 + scale.growth), scale.up.period)))[-1],
                          rep(((1 + scale.growth)^scale.up.period), (int.end - int.start - scale.up.period)))
          int.oat.eff <- scale.m.temp * (scale.up.v - 1) + t(replicate(int.sus * 12, nOAT.m[4,]))
          
          nOAT.int = round(c(as.vector(t(int.oat.eff[(i - int.start), 1:3])) * c(pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6]), 1-pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6])), as.vector(t(int.oat.eff[(i - int.start), 4:6]))))
          
          prop.oat.int = nOAT.int/pop.pwid
          if(any(prop.oat.int > 0.95 * 0.727)) {
            while (any(prop.oat.int > 0.95 * 0.727 + 0.00001)){
              sat <- which(prop.oat.int >= 0.95 * 0.727)
              nOAT.int[-sat] <- nOAT.int[-sat] + sum(nOAT.int[sat] - pop.pwid[sat] * 0.95 * 0.727) * (pop.pwid[-sat] / sum(pop.pwid[-sat]))
              nOAT.int[sat] <- pop.pwid[sat] * 0.95 * 0.727
              prop.oat.int = nOAT.int/pop.pwid
            }
          }
          
          oat.e     = (nOAT / pop.pwid) / (1 - (nOAT / pop.pwid)) * input.parameters$oat.q
          oat.e.int = prop.oat.int / (1 - prop.oat.int) * input.parameters$oat.q
          ## to address entry rate > 1 or <0
          oat.e[which(oat.e > 0.9 | oat.e <= 0)] <- 0.9
          oat.e.int[which(oat.e.int > 0.9 | oat.e.int <= 0)] <- 0.9
          ##
          rname = gsub(paste(c("/OAT", "/low", "/high"), collapse="|"), "", names.gp)
          pwid.name = names18[grep("PWID", names18)]
          oat.e.all.m.int <- oat.e.all.m <- matrix(0, nrow = 19, ncol = 42)
          for (q in 1:length(pwid.name)){
            ind = which(rname %in% pwid.name[q])
            oat.e.all.m[ , intersect(ind, off.oat)]     = oat.e[q]
            oat.e.all.m.int[ , intersect(ind, off.oat)] = oat.e.int[q]
          }
          
          int.costs.mod <- ((CurrentPeriod  * (oat.e.all.m.int - oat.e.all.m) * IntCosts.OAT) / ((1 + Discounting / 12) ^ (i - int.start)))
          
          # Implementation & sustainment costs
          if (i <= (scale.up.period + int.start)){
            nOAT.int.full <- round(c(as.vector(t(int.oat.eff[scale.up.period, 1:3])) * c(pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6]), 1-pop.pwid[1:3]/(pop.pwid[1:3] + pop.pwid[4:6])), as.vector(t(int.oat.eff[scale.up.period, 4:6]))))
            
            prop.oat.full = nOAT.int.full/pop.pwid
            if(any(prop.oat.full > 0.95 * 0.727)) {
              while (any(prop.oat.full > 0.95 * 0.727 + 0.00001)){
                sat <- which(prop.oat.full >= 0.95 * 0.727)
                nOAT.int.full[-sat] <- nOAT.int.full[-sat] + sum(nOAT.int.full[sat] - pop.pwid[sat] * 0.95 * 0.727) * (pop.pwid[-sat] / sum(pop.pwid[-sat]))
                nOAT.int.full[sat] <- pop.pwid[sat] * 0.95 * 0.727
                prop.oat.full = nOAT.int.full/pop.pwid
              }
            }
            
            oat.e.int.full = prop.oat.full / (1 - prop.oat.full) * input.parameters$oat.q
            ## to address entry rate > 1 or <0
            oat.e.int.full[which(oat.e.int.full > 0.9 | oat.e.int.full <= 0)] <- 0.9
            ##
            oat.e.all.m.int.full = oat.e.all.m
            for (q in 1:length(pwid.name)){
              ind = which(rname %in% pwid.name[q])
              oat.e.all.m.int.full[ , intersect(ind, off.oat)] = oat.e.int.full[q]
            }
            int.impl.costs.temp <- ((CurrentPeriod  * oat.e.all.m.int.full * IntCosts.impl.OAT) / ((1 + Discounting / 12) ^ (i - int.start)))
            int.sust.costs.temp <- (CurrentPeriod * 0)
            # END OF SCALE-UP PERIOD
          } else {
            int.impl.costs.temp <- (CurrentPeriod * 0)
            int.sust.costs.temp <- ((CurrentPeriod * oat.e.all.m.int * IntCosts.sust.OAT) / ((1 + Discounting / 12) ^ (i - int.start)))
          }
          int.costs.period[[i]] <- int.costs.mod + int.costs.period[[i]]
          int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
          int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
        }
      }# END OF SUSTAINMENT PERIOD
    } # END OF OAT LOOP
    
    ############################
    # SSP INTERVENTION
    if(tf.temp$intervention.ssp.modif==TRUE){
      if (i > int.start & i <= int.end){
        v.ssp.int <- matrix(0, nrow=19, ncol=42)
        v.ssp.int[ , all.idu] <- (scale.m$int.ssp.eff[i - int.start] - IntBaseline.SSP) / 12 / sum(CurrentPeriod[ , all.idu]) * CurrentPeriod[ , all.idu]
        
        int.costs.mod <- ((v.ssp.int * IntCosts.SSP) / ((1 + Discounting / 12) ^ (i - int.start)))
        
        # Implementation & sustainment costs
        if (i <= (scale.up.period + int.start)){
          v.ssp.int.full <- matrix(0, nrow=19, ncol=42)
          v.ssp.int.full[ , all.idu] <- (scale.m$int.ssp.eff[scale.up.period] - IntBaseline.SSP) / 12 / sum(CurrentPeriod[ , all.idu]) * CurrentPeriod[ , all.idu]
          int.impl.costs.temp <- ((v.ssp.int.full * IntCosts.impl.SSP) / ((1 + Discounting / 12) ^ (i - int.start)))
          int.sust.costs.temp <- (CurrentPeriod * 0)
          # END OF SCALE-UP PERIOD
        } else {
          int.impl.costs.temp <- (CurrentPeriod * 0)
          int.sust.costs.temp <- ((v.ssp.int * IntCosts.sust.SSP) / ((1 + Discounting / 12) ^ (i - int.start)))
        }
        int.costs.period[[i]] <- int.costs.mod + int.costs.period[[i]]
        int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
        int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
      } # END OF SUSTAINMENT PERIOD
    } # END OF SSP LOOP
    
    ############################
    # PrEP INTERVENTION
    if(tf.temp$intervention.prep.modif==TRUE){
      if (i > int.start & i <= int.end){
        eta.matrix      = eta.matrix.temp = matrix(0, nrow=19, ncol=42)
        prep.proportion <- input.parameters$prep.proportion
        msm.h.scep      <- input.parameters$msm.h.scep
        prop.prep <- scale.m$int.prep.eff[i - int.start] * prep.proportion / msm.h.scep
        prep.vol  <- scale.m$int.prep.eff[i - int.start] * prep.proportion
        while (any(prop.prep > 0.92001)){
          sat            <- which(prop.prep >= 0.92)
          prep.vol[-sat] <- prep.vol[-sat] + sum(prep.vol[sat] - msm.h.scep[sat] * 0.92) * (msm.h.scep[-sat] / sum(msm.h.scep[-sat]))
          prep.vol[sat]  <- msm.h.scep[sat] * 0.92
          prop.prep      <- prep.vol/msm.h.scep
        }
        eta.matrix[c(1:2) , msm.h] = rep(rep(-log(1 - IntBaseline.prep * prep.proportion / msm.h.scep)/12 ,3), each=2)
        eta.matrix.temp[c(1:2) , msm.h] = rep(rep(prop.prep/(1-prop.prep) * input.parameters$wp, 3), each=2)
        
        ## to address entry rate > 1 (causing NA)
        eta.matrix.temp[which(is.na(eta.matrix.temp))] <- 0.9
        eta.matrix.temp[eta.matrix.temp > 0.9] <- 0.9
        ##
        
        int.costs.mod <- ((CurrentPeriod * (eta.matrix.temp - eta.matrix) * IntCosts.prep) / ((1 + Discounting / 12) ^ (i - int.start)))
        
        # Implementation & sustainment costs
        if (i <= (scale.up.period + int.start)){
          eta.matrix.full <- eta.matrix
          prop.prep.full  <- scale.m$int.prep.eff[scale.up.period] * prep.proportion / msm.h.scep
          prep.vol.full   <- scale.m$int.prep.eff[scale.up.period] * prep.proportion
          while (any(prop.prep.full > 0.92001)){
            sat                 <- which(prop.prep.full >= 0.92)
            prep.vol.full[-sat] <- prep.vol.full[-sat] + sum(prep.vol.full[sat] - msm.h.scep[sat] * 0.92) * (msm.h.scep[-sat] / sum(msm.h.scep[-sat]))
            prep.vol.full[sat]  <- msm.h.scep[sat] * 0.92
            prop.prep.full      <- prep.vol.full/msm.h.scep
          }
          eta.matrix.full[c(1:2) , msm.h] = rep(rep(prop.prep.full/(1-prop.prep.full) * input.parameters$wp, 3), each=2)
          ## to address entry rate > 1 (causing NA)
          eta.matrix.full[which(is.na(eta.matrix.full))] <- 0.9
          eta.matrix.full[eta.matrix.full > 0.9] <- 0.9
          ##
          int.impl.costs.temp <- ((CurrentPeriod * eta.matrix.full * IntCosts.impl.prep) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
          int.sust.costs.temp <- (CurrentPeriod * 0)
          # END OF SCALE-UP PERIOD
        } else {
          int.impl.costs.temp <- (CurrentPeriod * 0)
          int.sust.costs.temp <- ((CurrentPeriod * eta.matrix.temp * IntCosts.sust.prep) /
                                    ((1 + Discounting / 12) ^ (i - int.start)))
        }
        int.costs.period[[i]] <- int.costs.mod + int.costs.period[[i]]
        int.impl.costs.period[[i]] <- int.impl.costs.temp + int.impl.costs.period[[i]]
        int.sust.costs.period[[i]] <- int.sust.costs.temp + int.sust.costs.period[[i]]
      } # END OF SUSTAINMENT PERIOD
    } # END OF PrEP LOOP
    
    
    ############################
  } else { # All outcomes = 0 outside of evaluation period
    # QALYs
    QALYs.period[[i]]            <- (CurrentPeriod * 0)
    # STATE COSTS
    hru.costs.period[[i]]        <- (CurrentPeriod * 0)
    art.costs.period[[i]]        <- (CurrentPeriod * 0)
    art.ini.costs.period[[i]]    <- (CurrentPeriod * 0)
    oat.costs.period[[i]]        <- (CurrentPeriod * 0)
    tests.costs.period[[i]]      <- (CurrentPeriod * 0)
    prep.costs.period[[i]]       <- (CurrentPeriod * 0)
    prep.tests.costs.period[[i]] <- (CurrentPeriod * 0)
    # INTERVENTION COSTS
    int.costs.period[[i]]        <- (CurrentPeriod * 0)
    int.impl.costs.period[[i]]   <- (CurrentPeriod * 0)
    int.sust.costs.period[[i]]   <- (CurrentPeriod * 0)
  }
}
#### END OF LOOP OVER PERIODS

if(sums.only==FALSE){
  
  QALYs            <- abind(QALYs.period, along = 0)
  # Stratified costs
  costs.hru        <- abind(hru.costs.period, along = 0)
  costs.art        <- abind(art.costs.period, along = 0)
  costs.art.ini    <- abind(art.ini.costs.period, along = 0)
  costs.oat        <- abind(oat.costs.period, along = 0)
  costs.prep       <- abind(prep.costs.period, along = 0)
  costs.prep.tests <- abind(prep.tests.costs.period, along = 0)
  costs.test       <- abind(tests.costs.period, along = 0)
  # Stratified intervention costs
  int.costs        <- abind(int.costs.period, along = 0)
  int.impl.costs   <- abind(int.impl.costs.period, along = 0)
  int.sust.costs   <- abind(int.sust.costs.period, along = 0)
  
  # Sums
  QALYs.sum            <- sum(unlist(QALYs.period))
  costs.hru.sum        <- sum(unlist(hru.costs.period))
  costs.art.sum        <- sum(unlist(art.costs.period))
  costs.art.ini.sum    <- sum(unlist(art.ini.costs.period))
  costs.oat.sum        <- sum(unlist(oat.costs.period))
  costs.prep.sum       <- sum(unlist(prep.costs.period))
  costs.prep.tests.sum <- sum(unlist(prep.tests.costs.period))
  costs.test.sum       <- sum(unlist(tests.costs.period))
  int.costs.sum        <- sum(unlist(int.costs.period))
  int.impl.costs.sum   <- sum(unlist(int.impl.costs.period))
  int.sust.costs.sum   <- sum(unlist(int.sust.costs.period))
  
  # Total costs
  costs.total.sum <- (costs.hru.sum + costs.art.sum + costs.art.ini.sum +
                        costs.oat.sum + costs.prep.sum + costs.prep.tests.sum +
                        costs.test.sum + int.costs.sum +
                        int.impl.costs.sum + int.sust.costs.sum)
  
  # In list
  out.sum <- list(QALYs                = QALYs,
                  costs.hru            = costs.hru,
                  costs.art            = costs.art,
                  costs.art.ini        = costs.art.ini,
                  costs.oat            = costs.oat,
                  costs.prep           = costs.prep,
                  costs.prep.tests     = costs.prep.tests,
                  costs.test           = costs.test,
                  int.costs            = int.costs,
                  int.impl.costs       = int.impl.costs,
                  int.sust.costs       = int.sust.costs,
                  QALYs.sum            = QALYs.sum,
                  costs.total.sum      = costs.total.sum,
                  costs.hru.sum        = costs.hru.sum,
                  costs.art.sum        = costs.art.sum,
                  costs.art.ini.sum    = costs.art.ini.sum,
                  costs.oat.sum        = costs.oat.sum,
                  costs.prep.sum       = costs.prep.sum,
                  costs.prep.tests.sum = costs.prep.tests.sum,
                  costs.test.sum       = costs.test.sum,
                  int.costs.sum        = int.costs.sum,
                  int.impl.costs.sum   = int.impl.costs.sum,
                  int.sust.costs.sum   = int.sust.costs.sum)
  
} else if (sums.only==TRUE){
  
  # Sums
  QALYs.sum            <- sum(unlist(QALYs.period))
  costs.hru.sum        <- sum(unlist(hru.costs.period))
  costs.art.sum        <- sum(unlist(art.costs.period))
  costs.art.ini.sum    <- sum(unlist(art.ini.costs.period))
  costs.oat.sum        <- sum(unlist(oat.costs.period))
  costs.prep.sum       <- sum(unlist(prep.costs.period))
  costs.prep.tests.sum <- sum(unlist(prep.tests.costs.period))
  costs.test.sum       <- sum(unlist(tests.costs.period))
  int.costs.sum        <- sum(unlist(int.costs.period))
  int.impl.costs.sum   <- sum(unlist(int.impl.costs.period))
  int.sust.costs.sum   <- sum(unlist(int.sust.costs.period))
  
  # Total costs
  costs.total.sum <- (costs.hru.sum + costs.art.sum + costs.art.ini.sum +
                        costs.oat.sum + costs.prep.sum + costs.prep.tests.sum +
                        costs.test.sum + int.costs.sum +
                        int.impl.costs.sum + int.sust.costs.sum)
  
  # In list
  out.sum <- list(QALYs.sum            = QALYs.sum,
                  costs.total.sum      = costs.total.sum,
                  costs.hru.sum        = costs.hru.sum,
                  costs.art.sum        = costs.art.sum,
                  costs.art.ini.sum    = costs.art.ini.sum,
                  costs.oat.sum        = costs.oat.sum,
                  costs.prep.sum       = costs.prep.sum,
                  costs.prep.tests.sum = costs.prep.tests.sum,
                  costs.test.sum       = costs.test.sum,
                  int.costs.sum        = int.costs.sum,
                  int.impl.costs.sum   = int.impl.costs.sum,
                  int.sust.costs.sum   = int.sust.costs.sum)
}


return(out.sum)
}

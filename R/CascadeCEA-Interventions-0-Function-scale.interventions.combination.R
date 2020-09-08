#' Interventions
#'
#' \code{scale.interventions.combination} Function for setting up intervention combinations
#'
#' @param current.int selected intervention
#' @param tf.comb logical vector for possible intervention combinations
#' @param scale.up.period time period for intervention to reach full scale (months)
#'
#' @return
#' Value of logical vectors for intervention modeling
#' @export

#############################################################################
## Cascade CEA Model - Interventions
## Latest update: Feb 14, 2020
#############################################################################
## Output: Value of logical vectors for intervention modeling
#############################################################################
# interventions <- c("Opt-out testing (ER)", "Opt-out testing (PC)",
#                    "EMR testing reminder", "Nurse-initiated testing", "OAT integrated testing",
#                    "ART initiation", "ART retention", "ART retention, targeted",
#                    "ART EMR reminder", "RAPID ART",
#                    "ART re-initiation", "ART re-linkage",
#                    "SSP", "OAT with BUP", "OAT with methadone", "PrEP",
#                    "No interventions")
#
# Interventions, suffix
# "Opt-out testing (ER)", test.optout.ER
# "Opt-out testing (PC)", test.optout.PC
# "EMR testing reminder", test.EMR
# "Nurse-initiated testing", test.nurse
# "OAT integrated testing", test.OAT
#
# "ART initiation", ARTinit
# "ART retention", ARTreten
# "ART retention, targeted", ARTreten.target
# "ART EMR reminder", ARTEMRprompt
# "RAPID ART", immART
#
# "ART re-initiation", reART
# "ART re-linkage", relink
#
# "SSP", SSP
# "OAT with BUP", BUP
# "OAT with methadone", MET
# "PrEP", prep
#
# NAMING PROTOCOL TO BE IMPLEMENTED
#
# IntEff.suffix
# IntCosts.suffix
# IntCosts.impl.suffix
# IntCosts.sust.suffix
#############################################################################
scale.interventions.combination <- function(current.int,
                                            tf.comb,
                                            scale.up.period = 18){

  # Set scale inputs
  current.scale <- Int.Scale.ls

  if (case == "SA"){
    list2env(c(current.scale, Int.Baseline.ls, Int.Eff.ls), environment())
  } else {
    list2env(c(current.scale, Int.Eff.ls), environment())
  }

  if (tf.comb$intervention.testing.modif == TRUE){
    int.test.eff   <- matrix(0, nrow=(int.end-int.start), ncol = length(names.gp))
    if (is.element(c("Opt-out testing (ER)"), current.int)){
      temp.int     <- IntEff.test.optout.ER
      scale.v      <- IntScale.test.optout.ER["S1", ]
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                        rep(temp.int, (int.end - int.start - scale.up.period)))
      int.test.eff <- scale.m.temp * (scale.up.v - 1) + int.test.eff
    }
    if (is.element(c("Opt-out testing (PC)"), current.int)){
      temp.int     <- IntEff.test.optout.PC
      scale.v      <- IntScale.test.optout.PC["S1", ]
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                        rep(temp.int, (int.end - int.start - scale.up.period)))
      int.test.eff <- scale.m.temp * (scale.up.v - 1) + int.test.eff
    }
    if (is.element(c("EMR testing reminder"), current.int)){
      temp.int     <- IntEff.test.EMR
      scale.v      <- IntScale.test.EMR["S1", ]
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                        rep(temp.int, (int.end - int.start - scale.up.period)))
      int.test.eff <- scale.m.temp * (scale.up.v - 1) + int.test.eff
    }
    if (is.element(c("Nurse-initiated testing"), current.int)){
      temp.int     <- IntEff.test.nurse
      scale.v      <- IntScale.test.nurse["S1", ]
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                        rep(temp.int, (int.end - int.start - scale.up.period)))
      int.test.eff <- scale.m.temp * (scale.up.v - 1) + int.test.eff
    }
    if (is.element(c("OAT integrated testing"), current.int)){
      temp.int     <- IntEff.test.OAT
      scale.v      <- IntScale.test.OAT["S1", ]
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                        rep(temp.int, (int.end - int.start - scale.up.period)))
      int.test.eff <- scale.m.temp * (scale.up.v - 1) + int.test.eff
    }
    int.test.eff   <- int.test.eff + 1
  } else {int.test.eff = NULL}

  if (tf.comb$intervention.initiation.modif == TRUE){
    temp.int       <- IntEff.ARTinit
    scale.v        <- IntScale.ARTinit["D1", ]
    scale.m.temp   <- t(replicate((int.sus * 12), scale.v))
    scale.up.v     <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                        rep(temp.int, (int.end - int.start - scale.up.period)))
    int.art.ini.eff <- scale.m.temp * (scale.up.v - 1) + 1
  } else {int.art.ini.eff = NULL}

  if (tf.comb$intervention.retention.modif == TRUE){
    if (is.element(c("ART retention"), current.int)) {
      temp.int    <- IntEff.ARTreten
      scale.v     <- IntScale.ARTreten["T1", ]
    } else if (is.element(c("ART retention, targeted"), current.int)) {
      temp.int    <- IntEff.ARTreten.target
      scale.v     <- IntScale.ARTreten.target["T3", ]
    }
    scale.m.temp  <- t(replicate((int.sus * 12), scale.v))
    scale.up.v    <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                       rep(temp.int, (int.end - int.start - scale.up.period)))
    int.art.ret.eff <- scale.m.temp * (scale.up.v - 1) + 1
  } else {int.art.ret.eff = NULL}

  if (tf.comb$intervention.dropout.modif == TRUE) {
    temp.int      <- IntEff.ARTEMRprompt
    scale.v       <- IntScale.ARTEMRprompt["T1", ]
    scale.m.temp  <- t(replicate((int.sus * 12), scale.v))
    scale.up.v    <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                       rep(temp.int, (int.end - int.start - scale.up.period)))
    int.art.drop.eff <- scale.m.temp * (scale.up.v - 1) + 1
  } else {int.art.drop.eff = NULL}

  if (tf.comb$intervention.immediateART.modif == TRUE) {
    temp.int      <- IntEff.immART
    scale.v       <- IntScale.immART["I1", ]
    scale.m.temp  <- t(replicate((int.sus * 12), scale.v))
    scale.up.v    <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                       rep(temp.int, (int.end - int.start - scale.up.period)))
    int.art.imm.eff <- scale.m.temp * (scale.up.v - 1) + 1
  } else {int.art.imm.eff = NULL}

  if (tf.comb$intervention.reinitiation.modif == TRUE) {
    int.art.reini.eff <- matrix(0, nrow=(int.end-int.start), ncol = length(names.gp))
    if (is.element(c("ART re-initiation"), current.int)){
      temp.int     <- IntEff.reART
      scale.v      <- IntScale.reART["O1", ]
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                        rep(temp.int, (int.end - int.start - scale.up.period)))
      int.art.reini.eff <- scale.m.temp * (scale.up.v - 1) + int.art.reini.eff
    }
    if (is.element(c("ART re-linkage"), current.int)){
      temp.int     <- IntEff.relink
      scale.v      <- IntScale.relink["O1", ]
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(seq(1, temp.int, by = ((temp.int - 1) / scale.up.period))[-1],
                        rep(temp.int, (int.end - int.start - scale.up.period)))
      int.art.reini.eff <- scale.m.temp * (scale.up.v - 1) + int.art.reini.eff
    }
    int.art.reini.eff <- int.art.reini.eff + 1
  } else {int.art.reini.eff = NULL}

  if (tf.comb$intervention.ssp.modif == TRUE) {
    scale.growth   <- IntScale.SSP
    scale.v        <- IntBaseline.SSP
    scale.m.temp   <- t(replicate((int.sus * 12), scale.v))
    scale.up.v     <- c(cumprod(c(1, rep((1 + scale.growth), scale.up.period)))[-1],
                        rep(((1 + scale.growth)^scale.up.period), (int.end - int.start - scale.up.period)))
    int.ssp.eff    <- scale.m.temp * scale.up.v
  } else {int.ssp.eff = NULL}

  if (tf.comb$intervention.oat.modif == TRUE){
    int.oat.eff    <- matrix(0, nrow=(int.end-int.start), ncol = length(IntBaseline.BUP))
    if(is.element(c("OAT with BUP"), current.int)) {
      scale.growth <- IntScale.BUP
      scale.v      <- IntBaseline.BUP
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(cumprod(c(1, rep((1 + scale.growth), scale.up.period)))[-1],
                        rep(((1 + scale.growth)^scale.up.period), (int.end - int.start - scale.up.period)))
      int.oat.eff  <- scale.m.temp * (scale.up.v - 1) + int.oat.eff
    }
    if(is.element(c("OAT with methadone"), current.int)) {
      scale.growth <- IntScale.MET
      scale.v      <- IntBaseline.MET
      scale.m.temp <- t(replicate((int.sus * 12), scale.v))
      scale.up.v   <- c(cumprod(c(1, rep((1 + scale.growth), scale.up.period)))[-1],
                        rep(((1 + scale.growth)^scale.up.period), (int.end - int.start - scale.up.period)))
      int.oat.eff  <- scale.m.temp * (scale.up.v - 1) + int.oat.eff
    }
    int.oat.eff    <- int.oat.eff + t(replicate(nrow(int.oat.eff), IntBaseline.BUP))  + t(replicate(nrow(int.oat.eff), IntBaseline.MET))
  } else {int.oat.eff = NULL}

  if (tf.comb$intervention.prep.modif == TRUE){
    scale.growth   <- IntScale.prep
    scale.v        <- IntBaseline.prep
    scale.m.temp   <- t(replicate((int.sus * 12), scale.v))
    scale.up.v     <- c(cumprod(c(1, rep((1 + scale.growth), scale.up.period)))[-1],
                        rep(((1 + scale.growth)^scale.up.period), (int.end - int.start - scale.up.period)))
    int.prep.eff   <- scale.m.temp * scale.up.v
  } else {int.prep.eff = NULL}

  if(is.element(c("No interventions"), current.int)){
    warning("There are no interventions being modeled")
  }

  int.pop.eff <- list(int.test.eff = int.test.eff,         int.art.ini.eff = int.art.ini.eff, int.art.ret.eff = int.art.ret.eff,
                      int.art.drop.eff = int.art.drop.eff, int.art.imm.eff = int.art.imm.eff, int.art.reini.eff = int.art.reini.eff,
                      int.ssp.eff = int.ssp.eff,           int.oat.eff = int.oat.eff,         int.prep.eff = int.prep.eff)

  if(any(lengths(int.pop.eff)==0)){
    int.pop.eff <- int.pop.eff[-which(lengths(int.pop.eff)==0)]
  }
  return(int.pop.eff)
}

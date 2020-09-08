#' Interventions
#'
#' \code{true.false.interventions.combination} Set up indicators of intervention modifications.
#'
#' @param current.int selected intervention
#'
#' @return
#' Value of logical vectors for intervention modeling
#' @export

# interventions <- c("Opt-out testing (ER)", "Opt-out testing (PC)",
#                    "EMR testing reminder", "Nurse-initiated testing", "OAT integrated testing",
#                    "ART initiation", "ART retention", "ART retention, targeted",
#                    "ART EMR reminder", "RAPID ART", "RAPID ART, targeted",
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
# "RAPID ART, targeted", immART.target
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

true.false.interventions.combination <- function(current.int){
  if(any(current.int %in% c("Opt-out testing (ER)", "Opt-out testing (PC)",
                        "EMR testing reminder", "Nurse-initiated testing",
                        "OAT integrated testing"))){
          intervention.testing.modif <- TRUE
  } else {intervention.testing.modif <- FALSE}

  if(any(current.int==c("ART initiation"))) {
          intervention.initiation.modif <- TRUE
  } else {intervention.initiation.modif <- FALSE}

  if(any(current.int %in% c("ART retention", "ART retention, targeted"))) {
          intervention.retention.modif <- TRUE
  } else {intervention.retention.modif <- FALSE}

  if(any(current.int==c("ART EMR reminder"))) {
          intervention.dropout.modif <- TRUE
  } else {intervention.dropout.modif <- FALSE}

  if(any(current.int %in% c("RAPID ART","RAPID ART, targeted"))) {
          intervention.immediateART.modif <- TRUE
  } else {intervention.immediateART.modif <- FALSE}

  if(any(current.int %in% c("ART re-initiation", "ART re-linkage"))) {
          intervention.reinitiation.modif <- TRUE
  } else {intervention.reinitiation.modif <- FALSE}

  if(any(current.int==c("SSP"))) {
          intervention.ssp.modif <- TRUE
  } else {intervention.ssp.modif <- FALSE}

  if(any(current.int %in% c("OAT with BUP", "OAT with methadone"))) {
          intervention.oat.modif <- TRUE
  } else {intervention.oat.modif <- FALSE}

  if(any(current.int==c("PrEP"))) {
          intervention.prep.modif <- TRUE
  } else {intervention.prep.modif <- FALSE}

  if(any(current.int==c("No interventions"))) {
    # warning("There are no interventions being modeled")
  }

  tf.ls <- list(
    intervention.testing.modif      = intervention.testing.modif,
    intervention.initiation.modif   = intervention.initiation.modif,
    intervention.retention.modif    = intervention.retention.modif,
    intervention.dropout.modif      = intervention.dropout.modif,
    intervention.immediateART.modif = intervention.immediateART.modif,
    intervention.reinitiation.modif = intervention.reinitiation.modif,
    intervention.ssp.modif          = intervention.ssp.modif,
    intervention.oat.modif          = intervention.oat.modif,
    intervention.prep.modif         = intervention.prep.modif
  )
  return(tf.ls)
}

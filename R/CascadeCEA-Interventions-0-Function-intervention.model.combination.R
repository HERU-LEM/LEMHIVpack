#' Interventions
#'
#' \code{intervention.model.combination} Function for running the simulation model with or without interventions
#'
#' @param input.parameters model inputs
#' @param current.int selected intervention
#'
#' @return
#' List of population (42*19) at each time step
#' @export


intervention.model.combination <- function(input.parameters,
                                           current.int) {

  # Set TRUE/FALSE intervention parameters
  tf.temp <- true.false.interventions.combination(current.int)

  if (!all(current.int == c("No interventions"))){
    # Set intervention parameters for scale*effect
    scale.m <- list(int.pop.eff = scale.interventions.combination(current.int, tf.comb = tf.temp))
  }

  # Run model
  if (all(current.int %in% c("Opt-out testing (ER)", "Opt-out testing (PC)",
                             "EMR testing reminder", "Nurse-initiated testing",
                             "OAT integrated testing",
                             "Large-scale testing during COVID",
                             "ART initiation",
                             "ART retention", "ART retention, targeted",
                             "ART EMR reminder", "RAPID ART",
                             "ART re-initiation", "ART re-linkage",

                             "SSP", "OAT with BUP", "OAT with methadone",
                             "PrEP"))) {

    new.inputs <- c(input.parameters, tf.temp, current.int = list(current.int),
                    int.start = int.start, int.end = int.end,
                    scale.m)

  } else if (all(current.int == c("No interventions"))) {
    new.inputs <- c(input.parameters, tf.temp, current.int = list(current.int),
                    int.start = 0, int.end = 0)
    warning("There are no interventions being modeled")
  }

  # Call model function
  out_euler <- euler(x, vt, ode_model, new.inputs)[ ,-1]
  outa = array(out_euler[-1, ], dim = c(n, length(state.name), length(names.gp))) # initial value deleted
  dimnames(outa)[[1]] = 1:n
  dimnames(outa)[[2]] = state.name
  dimnames(outa)[[3]] = names.gp
  return(outa)

}

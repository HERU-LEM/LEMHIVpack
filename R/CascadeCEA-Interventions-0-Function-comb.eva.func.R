#' Interventions
#'
#' \code{comb.eva.func} combination evaluation function: generate population list, incidence results, CEA results
#'
#' @param input.parameters model inputs
#' @param current.int selected intervention
#'
#' @return
#' Vector of incidence and cost-effectiveness outcomes
#' @export

#############################################################################
## Cascade CEA Model - Interventions
## combination evaluation function: generate population list, incidence results, CEA results
## Latest update: Feb 19, 2020
#############################################################################
## Output: incidence and cost-effectiveness outcmes in a vector
#############################################################################
comb.eva.func <- function(input.parameters,
                          current.int){

  out.ls.int <- intervention.model.combination(input.parameters = input.parameters,
                                               current.int = current.int)

  t.ni <- time.period.infections(simul.in     = out.ls.int,
                                 start.period = start.proj,
                                 end.period   = end.proj,
                                 sust.period  = int.sus)

  out.ls.accum.int <- accum.outcomes.combination(input.parameters = input.parameters,
                                                 pop.in           = out.ls.int[, 1:19, ],
                                                 current.int      = current.int,
                                                 sums.only        = TRUE)

  return(c(unlist(t.ni), unlist(out.ls.accum.int)))
}

#' Interventions
#'
#' \code{time.period.infections} Calculate HIV infections for different time periods
#'
#' @param simul.in DESCRIPTION
#' @param start.period starting time point (months from initialization)
#' @param end.period ending time point (months from initialization)
#'
#' @return
#' List with HIV infections outputs
#' @export

time.period.infections <- function(simul.in,
                                   start.period,
                                   end.period,
                                   sust.period){

  # Get number of infections:
  # (1) over 20-year evaluation period: 2020-2040
  t.int.infs.eval <- sum(simul.in[end.period, c("inc_bo", "inc_bs", "inc_g"), ]) - sum(simul.in[start.period, c("inc_bo", "inc_bs", "inc_g"), ])

  out.inf.eval    <- as.matrix(t.int.infs.eval)
  colnames(out.inf.eval) <- paste0("Infections.total-20Y")

  # (2) over 10-year implementation period: 2020-2030
  t.int.infs.impl <- sum(simul.in[(start.period + (sust.period * 12)), c("inc_bo", "inc_bs", "inc_g"), ]) -
    sum(simul.in[start.period, c("inc_bo", "inc_bs", "inc_g"), ])

  out.inf.impl    <- as.matrix(t.int.infs.impl)
  colnames(out.inf.impl) <- paste0("Infections.total-10Y")

  # (3) over 5-year period: 2020-2025
  t.int.infs.5y   <- sum(simul.in[(start.period + (5 * 12)), c("inc_bo", "inc_bs", "inc_g"), ]) -
                              sum(simul.in[start.period, c("inc_bo", "inc_bs", "inc_g"), ])

  out.inf.5y      <- as.matrix(t.int.infs.5y)
  colnames(out.inf.5y)   <- paste0("Infections.total-5Y")

  # Get person-years among susceptibles over same time periods
  # (1) over 20-year evaluation period
  denom.sum.eval  <- as.matrix(sum(simul.in[start.period:end.period, 1:3, names.gp]) / (12 * 100000))
  colnames(denom.sum.eval) <- paste0("SuscPY-over20Y")

  # (2) over 10-year implementation period
  denom.sum.impl  <- as.matrix(sum(simul.in[start.period:(start.period + sust.period * 12), 1:3, names.gp])/ (12 * 100000))
  colnames(denom.sum.impl) <- paste0("SuscPY-over10Y")

  # (3) over 5-year period
  denom.sum.5y  <- as.matrix(sum(simul.in[start.period:(start.period + 5 * 12), 1:3, names.gp])/ (12 * 100000))
  colnames(denom.sum.5y)   <- paste0("SuscPY-over5Y")

  # Get yearly number of infections
  yr_end               <- seq(36, end.period + 12, by=12)
  t.int.infs.eval      <- apply((simul.in[yr_end, c("inc_bo", "inc_bs", "inc_g"), ]), c(1), sum)
  out.inf.yr           <- t(as.matrix(diff(t.int.infs.eval)))
  colnames(out.inf.yr) <- paste0(c(2015:lyr))

  out.inf    <- cbind(out.inf.eval, denom.sum.eval, out.inf.impl, denom.sum.impl, out.inf.5y, denom.sum.5y)
  out.inf.ls <- list (out.inf = out.inf, out.inf.yr = out.inf.yr)

  return(out.inf.ls)
}

#' Incidence Derivation
#'
#' \code{incidence.derivation} Derive incidence data for ggplot
#'
#' @param CITY City name
#' @param case Implementation scenario
#' @param ocis
#'
#' @return
#' List with disaggregated incidence outcomes
#' @export

incidence.derivation <- function(CITY, case, ocis){
  outcome    <- list()
  if (case == "Status Quo"){
    #deterministic
    outcome$pe     <- as.vector(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-infections.rds"))$out.inf.yr)
    #PSA: median, upper and lower
    outcome.list   <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-infections.rds"))
    outcome.matrix <- matrix(0, nrow = n.sample, ncol <- 26)
    for (i in 1:n.sample){
      outcome.matrix[i, ] <- outcome.list[[i]]$out.inf.yr
    }
  } else if (case == "Documented"){
    #deterministic
    outcome$pe     <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))[ocis, 7:32]
    #PSA: median, upper and lower
    outcome.matrix <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-",  ocis, "(OCIS).rds"))[ , 7:32]
  } else if (case == "Ideal"){
    outcome$pe <- readRDS(paste0("Combination/Ideal/Outcome-refvsOCIS-", CITY, "-ideal.rds"))[2, 7:32]  #1st row for ref case, 2nd row for ideal
    outcome.matrix <- readRDS(paste0("Combination/Ideal/Outcome-OCIS-", CITY, "-PSA-", ocis, "-ideal.rds"))[ , 7:32]
  }
  outcome$median <- apply(outcome.matrix, 2, median)
  outcome$lower  <- apply(outcome.matrix, 2, quantile, probs = 0.025)
  outcome$upper  <- apply(outcome.matrix, 2, quantile, probs = 0.975)

  outcome$reduction <- c(sum((outcome.matrix[ , 2020-2015]  - outcome.matrix[ , 2025-2015]) / outcome.matrix[ , 2020-2015] >= 0.75) / 2000,
                         mean((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2025-2015]) / outcome.matrix[ , 2020-2015]),
                         median((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2025-2015]) / outcome.matrix[ , 2020-2015]),
                         quantile(((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2025-2015]) / outcome.matrix[ , 2020-2015]), 0.025),
                         quantile(((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2025-2015]) / outcome.matrix[ , 2020-2015]), 0.975),
                         sum((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2030-2015]) / outcome.matrix[ , 2020-2015] >= 0.9) / 2000,
                         mean((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2030-2015]) / outcome.matrix[ , 2020-2015]),
                         median((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2030-2015]) / outcome.matrix[ , 2020-2015]),
                         quantile(((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2030-2015]) / outcome.matrix[ , 2020-2015]), 0.025),
                         quantile(((outcome.matrix[ , 2020-2015] - outcome.matrix[ , 2030-2015]) / outcome.matrix[ , 2020-2015]), 0.975))
  return(outcome)
}

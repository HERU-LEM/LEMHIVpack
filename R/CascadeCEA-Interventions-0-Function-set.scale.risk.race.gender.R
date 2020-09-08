#' Interventions
#'
#' \code{set.scale.risk.race.gender} Function for applying race/gender/risk-specific intervention scale estimates
#'
#' @param this.int selected intervention
#' @param this.scale stratified scale
#' @param these.states affected model states
#'
#' @return
#' Value of logical vectors for intervention modeling
#' @export

set.scale.risk.race.gender <- function(this.int,
                                       this.scale,
                                       these.states){

  reach.m <- matrix(0, nrow=19, ncol=42)
  colnames(reach.m) = names.gp
  rownames(reach.m) = state.name[1:19]
  adp.m <- reach.m

  if (this.scale=="pe"){
    reach.m[these.states, intersect(idu.m, white)] <- with(this.int, pe[gender == "m" & ethnicity == "w" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, white)]   <- with(this.int, pe[gender == "m" & ethnicity == "w" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu, white)]  <- with(this.int, pe[gender == "m" & ethnicity == "w" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, white)] <- with(this.int, pe[gender == "m" & ethnicity == "w" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.m, black)] <- with(this.int, pe[gender == "m" & ethnicity == "b" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, black)]   <- with(this.int, pe[gender == "m" & ethnicity == "b" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu,black)]   <- with(this.int, pe[gender == "m" & ethnicity == "b" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, black)] <- with(this.int, pe[gender == "m" & ethnicity == "b" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.m, hisp)]  <- with(this.int, pe[gender == "m" & ethnicity == "h" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, hisp)]    <- with(this.int, pe[gender == "m" & ethnicity == "h" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu, hisp)]   <- with(this.int, pe[gender == "m" & ethnicity == "h" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, hisp)]  <- with(this.int, pe[gender == "m" & ethnicity == "h" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, white)] <- with(this.int, pe[gender == "f" & ethnicity == "w" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, white)] <- with(this.int, pe[gender == "f" & ethnicity == "w" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, black)] <- with(this.int, pe[gender == "f" & ethnicity == "b" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, black)] <- with(this.int, pe[gender == "f" & ethnicity == "b" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, hisp)]  <- with(this.int, pe[gender == "f" & ethnicity == "h" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, hisp)]  <- with(this.int, pe[gender == "f" & ethnicity == "h" & risk == "het" & component == "reach"])

    adp.m[these.states, ] <- with(this.int, pe[component == "adoption"])[1]
    scale.m <- reach.m * adp.m

  } else if (this.scale=="lower"){
    reach.m[these.states, intersect(idu.m, white)] <- with(this.int, lower[gender == "m" & ethnicity == "w" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, white)]   <- with(this.int, lower[gender == "m" & ethnicity == "w" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu, white)]  <- with(this.int, lower[gender == "m" & ethnicity == "w" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, white)] <- with(this.int, lower[gender == "m" & ethnicity == "w" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.m, black)] <- with(this.int, lower[gender == "m" & ethnicity == "b" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, black)]   <- with(this.int, lower[gender == "m" & ethnicity == "b" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu,black)]   <- with(this.int, lower[gender == "m" & ethnicity == "b" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, black)] <- with(this.int, lower[gender == "m" & ethnicity == "b" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.m, hisp)]  <- with(this.int, lower[gender == "m" & ethnicity == "h" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, hisp)]    <- with(this.int, lower[gender == "m" & ethnicity == "h" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu, hisp)]   <- with(this.int, lower[gender == "m" & ethnicity == "h" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, hisp)]  <- with(this.int, lower[gender == "m" & ethnicity == "h" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, white)] <- with(this.int, lower[gender == "f" & ethnicity == "w" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, white)] <- with(this.int, lower[gender == "f" & ethnicity == "w" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, black)] <- with(this.int, lower[gender == "f" & ethnicity == "b" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, black)] <- with(this.int, lower[gender == "f" & ethnicity == "b" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, hisp)]  <- with(this.int, lower[gender == "f" & ethnicity == "h" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, hisp)]  <- with(this.int, lower[gender == "f" & ethnicity == "h" & risk == "het" & component == "reach"])

    adp.m[these.states, ] <- with(this.int, lower[component == "adoption"])[1]
    scale.m <- reach.m * adp.m

  } else if (this.scale=="upper"){
    reach.m[these.states, intersect(idu.m, white)] <- with(this.int, upper[gender == "m" & ethnicity == "w" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, white)]   <- with(this.int, upper[gender == "m" & ethnicity == "w" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu, white)]  <- with(this.int, upper[gender == "m" & ethnicity == "w" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, white)] <- with(this.int, upper[gender == "m" & ethnicity == "w" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.m, black)] <- with(this.int, upper[gender == "m" & ethnicity == "b" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, black)]   <- with(this.int, upper[gender == "m" & ethnicity == "b" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu,black)]   <- with(this.int, upper[gender == "m" & ethnicity == "b" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, black)] <- with(this.int, upper[gender == "m" & ethnicity == "b" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.m, hisp)]  <- with(this.int, upper[gender == "m" & ethnicity == "h" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(msm, hisp)]    <- with(this.int, upper[gender == "m" & ethnicity == "h" & risk == "msm" & component == "reach"])
    reach.m[these.states, intersect(midu, hisp)]   <- with(this.int, upper[gender == "m" & ethnicity == "h" & risk == "mwid" & component == "reach"])
    reach.m[these.states, intersect(het.m, hisp)]  <- with(this.int, upper[gender == "m" & ethnicity == "h" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, white)] <- with(this.int, upper[gender == "f" & ethnicity == "w" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, white)] <- with(this.int, upper[gender == "f" & ethnicity == "w" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, black)] <- with(this.int, upper[gender == "f" & ethnicity == "b" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, black)] <- with(this.int, upper[gender == "f" & ethnicity == "b" & risk == "het" & component == "reach"])
    reach.m[these.states, intersect(idu.f, hisp)]  <- with(this.int, upper[gender == "f" & ethnicity == "h" & risk == "pwid" & component == "reach"])
    reach.m[these.states, intersect(het.f, hisp)]  <- with(this.int, upper[gender == "f" & ethnicity == "h" & risk == "het" & component == "reach"])

    adp.m[these.states, ] <- with(this.int, upper[component == "adoption"])[1]
    scale.m <- reach.m * adp.m

  }

  return(scale.m)
}

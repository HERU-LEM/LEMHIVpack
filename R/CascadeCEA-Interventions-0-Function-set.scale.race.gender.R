#' Interventions
#'
#' \code{set.scale.race.gender} Function for applying race/gender-specific intervention scale estimates
#'
#' @param this.int selected intervention
#' @param this.scale stratified scale
#' @param these.states affected model states
#' @param otp logical (default = FALSE)
#'
#' @return
#' Value of logical vectors for intervention modeling
#' @export

set.scale.race.gender <- function(this.int,
                                  this.scale,
                                  these.states,
                                  otp=FALSE){

  reach.m <- matrix(0, nrow=19, ncol=42)
  colnames(reach.m) = names.gp
  rownames(reach.m) = state.name[1:19]
  adp.m <- reach.m

  # otp T/F only for OTP-integrated testing special case
  if (otp==FALSE){
    if (this.scale=="pe"){
      reach.m[these.states, intersect(m, white)] <- with(this.int, pe[gender == "m" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(m, black)] <- with(this.int, pe[gender == "m" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(m, hisp)]  <- with(this.int, pe[gender == "m" & ethnicity == "h" & component == "reach"])
      reach.m[these.states, intersect(f, white)] <- with(this.int, pe[gender == "f" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(f, black)] <- with(this.int, pe[gender == "f" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(f, hisp)]  <- with(this.int, pe[gender == "f" & ethnicity == "h" & component == "reach"])

      adp.m[these.states, ] <- with(this.int, pe[component == "adoption"])[1]
      scale.m <- reach.m * adp.m

    } else if (this.scale=="lower"){
      reach.m[these.states, intersect(m, white)] <- with(this.int, lower[gender == "m" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(m, black)] <- with(this.int, lower[gender == "m" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(m, hisp)]  <- with(this.int, lower[gender == "m" & ethnicity == "h" & component == "reach"])
      reach.m[these.states, intersect(f, white)] <- with(this.int, lower[gender == "f" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(f, black)] <- with(this.int, lower[gender == "f" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(f, hisp)]  <- with(this.int, lower[gender == "f" & ethnicity == "h" & component == "reach"])

      adp.m[these.states, ] <- with(this.int, lower[component == "adoption"])[1]
      scale.m <- reach.m * adp.m

    } else if (this.scale=="upper"){
      reach.m[these.states, intersect(m, white)] <- with(this.int, upper[gender == "m" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(m, black)] <- with(this.int, upper[gender == "m" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(m, hisp)]  <- with(this.int, upper[gender == "m" & ethnicity == "h" & component == "reach"])
      reach.m[these.states, intersect(f, white)] <- with(this.int, upper[gender == "f" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(f, black)] <- with(this.int, upper[gender == "f" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(f, hisp)]  <- with(this.int, upper[gender == "f" & ethnicity == "h" & component == "reach"])

      adp.m[these.states, ] <- with(this.int, upper[component == "adoption"])[1]
      scale.m <- reach.m * adp.m

    }

  } else if (otp==TRUE){
    if (this.scale=="pe"){
      reach.m[these.states, intersect(intersect(m, white), oat)] <- with(this.int, pe[gender == "m" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(intersect(m, black), oat)] <- with(this.int, pe[gender == "m" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(intersect(m, hisp), oat)]  <- with(this.int, pe[gender == "m" & ethnicity == "h" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, white), oat)] <- with(this.int, pe[gender == "f" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, black), oat)] <- with(this.int, pe[gender == "f" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, hisp), oat)]  <- with(this.int, pe[gender == "f" & ethnicity == "h" & component == "reach"])

      adp.m[these.states, oat] <- with(this.int, pe[component == "adoption"])[1]
      scale.m <- reach.m * adp.m

    } else if (this.scale=="lower"){
      reach.m[these.states, intersect(intersect(m, white), oat)] <- with(this.int, lower[gender == "m" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(intersect(m, black), oat)] <- with(this.int, lower[gender == "m" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(intersect(m, hisp), oat)]  <- with(this.int, lower[gender == "m" & ethnicity == "h" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, white), oat)] <- with(this.int, lower[gender == "f" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, black), oat)] <- with(this.int, lower[gender == "f" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, hisp), oat)]  <- with(this.int, lower[gender == "f" & ethnicity == "h" & component == "reach"])

      adp.m[these.states, oat] <- with(this.int, lower[component == "adoption"])[1]
      scale.m <- reach.m * adp.m

    } else if (this.scale=="upper"){
      reach.m[these.states, intersect(intersect(m, white), oat)] <- with(this.int, upper[gender == "m" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(intersect(m, black), oat)] <- with(this.int, upper[gender == "m" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(intersect(m, hisp), oat)]  <- with(this.int, upper[gender == "m" & ethnicity == "h" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, white), oat)] <- with(this.int, upper[gender == "f" & ethnicity == "w" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, black), oat)] <- with(this.int, upper[gender == "f" & ethnicity == "b" & component == "reach"])
      reach.m[these.states, intersect(intersect(f, hisp), oat)]  <- with(this.int, upper[gender == "f" & ethnicity == "h" & component == "reach"])

      adp.m[these.states, oat] <- with(this.int, upper[component == "adoption"])[1]
      scale.m <- reach.m * adp.m

    }
  }


  return(scale.m)
}

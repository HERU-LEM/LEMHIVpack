## Derive free parameter information and values ##
names.gp   = as.vector(vparameters$names.gp)
names18    = as.vector(vparameters$names18)
n.gp       = length(vparameters$names.gp)
state.name = vparameters$state.name
leng       = calpar.info$plength
names      = calpar.info$names

## Assigning parameter values for selected free parameters ##
par2   = as.list(leng)
for(l in 1:length(leng)){
  if (l==1) {
    par2[[l]]   = calpar$pe[1:leng[l]]
  }
  else {
    par2[[l]]   = calpar$pe[(sum(leng[1:(l-1)])+1):sum(leng[1:l])]
  }
}
names(par2)   = names

# update vparameters list with calibrated free parameters
for (l in 1:length(names)){
  if (length(unlist(vparameters[names[l]])) == 1 & par_info[par_info$parameter == names[l], ]$stratification == 0) { #single parameters
    vparameters[names[l]][[1]] = par2[names[l]][[1]]
  }
  if (par_info[par_info$parameter == names[l], ]$stratification == 0) { #single parameters
    vlist[[names[l]]]$pe = par2[names[l]][[1]]
  }
  if (par_info[par_info$parameter == names[l], ]$stratification == 2) { #parameters with 2 dimensions: gender, ethnicity
    for (j in 1:leng[l]) {
      vlist[[names[l]]][vlist[[names[l]]]$gender == calpar[calpar$par == names[l], ][j, ]$gender & vlist[[names[l]]]$ethnicity == calpar[calpar$par == names[l], ][j,]$ethnicity, ]$pe = par2[names[l]][[1]][j]
    }}
  if (par_info[par_info$parameter == names[l], ]$stratification == 2.2) { #parameters with 2 dimensions: gender, risk
    for (j in 1:leng[l]) {
      vlist[[names[l]]][vlist[[names[l]]]$gender == calpar[calpar$par == names[l], ][j, ]$gender & vlist[[names[l]]]$risk == calpar[calpar$par == names[l], ][j,]$risk, ]$pe = par2[names[l]][[1]][j]
    }}
  if (par_info[par_info$parameter == names[l], ]$stratification == 3) { #parameters with 3 dimensions: gender, ethnicity, risk
    for (j in 1:leng[l]) {
      vlist[[names[l]]][vlist[[names[l]]]$gender == calpar[calpar$par == names[l], ][j, ]$gender & vlist[[names[l]]]$ethnicity == calpar[calpar$par == names[l], ][j,]$ethnicity & vlist[[names[l]]]$risk == calpar[calpar$par == names[l], ][j,]$risk, ]$pe = par2[names[l]][[1]][j]
    }}
  if (par_info[par_info$parameter == names[l], ]$stratification == 3.5) { #parameters with 3 dimensions: gender, ethnicity, component
    for (j in 1:leng[l]) {
      if (calpar[calpar$par == names[l], ][j,]$component != "reach"){
        vlist[[names[l]]][vlist[[names[l]]]$component == calpar[calpar$par == names[l], ][j,]$component, ]$pe = par2[names[l]][[1]][j]
      } else {
        vlist[[names[l]]][vlist[[names[l]]]$gender == calpar[calpar$par == names[l], ][j, ]$gender & vlist[[names[l]]]$ethnicity == calpar[calpar$par == names[l], ][j,]$ethnicity & vlist[[names[l]]]$component == calpar[calpar$par == names[l], ][j,]$component, ]$pe = par2[names[l]][[1]][j]
      }
    }}
  if (par_info[par_info$parameter == names[l], ]$stratification == 4) { #parameters with 4 dimensions: gender, ethnicity, risk, sexual intensity
    for (j in 1:leng[l]) {
      vlist[[names[l]]][vlist[[names[l]]]$gender == calpar[calpar$par == names[l], ][j, ]$gender & vlist[[names[l]]]$ethnicity == calpar[calpar$par == names[l], ][j,]$ethnicity & vlist[[names[l]]]$risk == calpar[calpar$par == names[l], ][j,]$risk & vlist[[names[l]]]$sexual.intensity == calpar[calpar$par == names[l], ][j,]$sexual.intensity, ]$pe = par2[names[l]][[1]][j]
    }}
  if (par_info[par_info$parameter == names[l], ]$stratification == 1) { #parameters with 1 dimension: CD4
    for (j in 1:leng[l]) {
      vlist[[names[l]]][vlist[[names[l]]]$cd4 == calpar[calpar$par == names[l], ][j, ]$cd4, ]$pe = par2[names[l]][[1]][j]
    }}
}

source("01_Setup/CascadeCEA-Model-0-Parameterization.R")

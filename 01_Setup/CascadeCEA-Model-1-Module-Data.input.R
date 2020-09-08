################################################################################
## DATA INPUT                                                                 ##
################################################################################
## Read in data from excel file ##
if (exists("int.scale")){
  myFile.name <- c("Data Files/Ideal/Evidence-Inputs-Master-Ideal")
} else {
  myFile.name <- c("Data Files/Evidence-Inputs-Master")
}
WB <- loadWorkbook(paste0(myFile.name,".xlsx"))

updated.prep2020 <- read.csv("Data Files/LEMmodel-UpdatedPrEPdata2020.csv")[, c(1:6)]
colnames(updated.prep2020) <- c("city","year","pe","low","hi","coverage")
updated.prep2020 <- na.omit(updated.prep2020)

par_info      <- read.xlsx(WB, sheet="parameter_info")  #first tab "parameter_info" for parameter info, e.g. name, dimension
vlist         <- list()                                 #initialize intermediate vlist (as a list) to save inputs
length(vlist) <- nrow(par_info)                         #number of input parameters
names (vlist) <- par_info$parameter                     #name of input parameters
vparameters   <- vlist                                  #initialize vparameters - parameter list used in model

city_sp     <- grep('Y', par_info$city_specific)        #City-specific parameters
non_city_sp <- grep('N', par_info$city_specific)        #Non-city-specific parameters

uncert      <- grep('Y', par_info$uncertain)            #Uncertain parameters
cert        <- grep('N', par_info$uncertain)            #Parameters fixed in next steps

## Parametrization for single-dimension parameters, i.e. dimension = 1
#city-scpecific parameters
for (i in city_sp){
  vlist[[i]] = subset(read.xlsx(WB, sheet=par_info$tab[i]), city == CITY)[ ,-1]
  if (par_info[i, ]$dimension == 1){
    vparameters[[i]] = vlist[[i]]$pe
  }
}
#non-city-scpecific parameters
for (i in non_city_sp){
  vlist[[i]] = read.xlsx(WB, sheet=par_info$tab[i])
  if (par_info[i, ]$dimension == 1){
    vparameters[[i]] = vlist[[i]]$pe
  }
}

## String variables for variable/group/target names
if (CITY != "MIA"){
  namelist   = read.xlsx(WB, sheet="common")
} else {
  namelist   = read.xlsx(WB, sheet="common_MIA")
}

names.gp   = namelist$names.gp                                 #group names, 42 groups: fullly stratified 42 groups
names18    = namelist$names18[!is.na(namelist$names18)]        #group names, 18 groups: gender*risk group*ethnicity (aggregate)
names.pwid = namelist$names.pwid[!is.na(namelist$names.pwid)]  #group names, 9 PWID groups: gender*risk group*ethnicity (aggregate)
names.msm  = namelist$names.msm[!is.na(namelist$names.msm)]    #group names, 18 MSM and MWID groups: gender*risk group*ethnicity * risk level * OAT (fully stratified)
names.e    = namelist$names.e[!is.na(namelist$names.e)]        #group names, 6 groups: high/low*ethnicity (highly aggregate)
state.name = c("S1", "S2", "Sp", "Ia", "I1", "I2", "I3", "Iap", "Ip",
               "Da", "D1", "D2", "D3", "T1", "T2", "T3", "O1",  "O2", "O3",
               "inc_bo", "inc_bs", "inc_g", "diag", "death")   #names for the 19 health states

vparameters = c(vparameters, as.data.frame(names.gp), as.data.frame(names18), as.data.frame(names.pwid), as.data.frame(names.msm), as.data.frame(names.e), as.data.frame(state.name))
# Add string variables in vparameters

source("01_Setup/CascadeCEA-Model-0-Group.number.R")

## Parametrization for parameters with multiple dimensions
source("01_Setup/CascadeCEA-Model-0-Parameterization.R")

## Weight vectors distinct for MIA (fewer targets) and SEA (adjustment for black), only used in calibration
if (CITY != "SEA" & CITY != "MIA"){
  vparameters$w = with(vlist$w, pe[city == "other"])
} else if (CITY == "SEA"){
  vparameters$w = with(vlist$w, pe[city == "SEA"])
} else {
  vparameters$w = with(vlist$w, pe[city == "MIA"])
}

vparameters = vparameters[-which(sapply(vparameters, is.null))]

## Read in target (calibration and validation) data ##
diag18.obs  = data.matrix(subset(data.frame(read.xlsx("Data Files/Target.xlsx", sheet="diag18.obs",  colNames =T)), city == CITY)[ , -1]) #total diagnosed cases, 18 groups
ndiag18.obs = data.matrix(subset(data.frame(read.xlsx("Data Files/Target.xlsx", sheet="ndiag18.obs", colNames =T)), city == CITY)[ , -1]) #new diagnoses, 18 groups
death18.obs = data.matrix(subset(data.frame(read.xlsx("Data Files/Target.xlsx", sheet="death18.obs", colNames =T)), city == CITY)[ , -1]) #all-casue deaths, 18 groups
obs.inc.all = subset(data.frame(read.xlsx("Data Files/Target.xlsx", sheet="inc.all.obs", colNames =T)), city == CITY)[ , -1]              #new incidence, total and range
obs.inc.msm = subset(data.frame(read.xlsx("Data Files/Target.xlsx", sheet="inc.msm.obs", colNames =T)), city == CITY)[ , -1]              #new incidence, MSM and range

## Derive model initials ##
#source("01_Setup/CascadeCEA-Model-0-Function-model.initial.R")
#### initial proportion of 42 groups;
init  = model.initial(par = vparameters, diag18 = diag18.obs[1, ])  #42*19 initials
inits = cbind(init, inc_bo=0, inc_bs=0, inc_g=0, diag=0, death=0)   #adding states to track # infections (by 3 routes of transmission), new diagnoses and deaths among PLHIV
x     = as.vector(t(inits))                                         #ode function requires init as vector (x)
vparameters$init.group.prop = as.vector(rowSums(init)/sum(init))

## initial population for 18 groups (collapsing onOAt/offOAT, low/high)
init.collap   = numeric(18)
rname = gsub(paste(c("/OAT","/low","/high"), collapse="|"), "", names.gp)
for (i in 1:18){
  ind = which(rname %in% names18[i])
  init.collap[i] = sum(init[ind, ])
}

vparameters$prop.adj     = TRUE     # force the risk group proportions to remain constant
vparameters$inf.prop.adj = TRUE     # force the risk level proportions to remain constant among infected
vparameters$bal          = TRUE     # balance # of sexual parnership between males & females


## These two variables are required for estimating OAT and PrEP entry rates in ODE module (only use initial values) ##
vparameters$pop.pwid   = init.collap[grep("PWID", names18)]                       # number of PWID (include MWID) within the 9 collapsed groups
vparameters$msm.h.scep = rowSums(matrix(rowSums(inits[msm.h , 1:3]), nrow=3))  # number of high-risk MSM (include MWID) within the 3 collapsed groups (race only)
#prep.coverage <- matrix(unlist(lapply(prep.by.year, function(x) x*prep.proportion/vparameters$msm.h.scep)), nrow=length(prep.by.year), byrow=TRUE)
#rownames(prep.coverage) <- names(coverage.by.year)
#colnames(prep.coverage) <- c("w","b","h")
#vparameters$prep.coverage <- prep.coverage

## READ in free parameters ##
calpar  = read.xlsx("Data Files/cali_par_all.xlsx", sheet=CITY)  # Load free parameters
calpar.info = as.list(2)                              # To save information for free.par
calpar.info$names   = unique(calpar$par)              # names of free.par
calpar.info$plength = as.numeric(table(factor(calpar$par, levels=calpar.info$names))) # length of each free.par

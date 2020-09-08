#' Set initial population
#'
#' \code{model.initial} sets initial model state occupancy
#'
#' @param par *FILL IN DETAILS ABOUT PAR*
#' @param diag18 Number of HIV-diagnosed individuals in 18 groups
#'
#' @return
#' Matrix of initial state populations
#' @export

model.initial = function(par, diag18){
  with(as.list(c(par, diag18 = diag18)), {
    # diag18: # of hiv diagnosed in 18 groups, created by "targetNYC.R"
    #       from NYC surveillance reports

    # population in 18 groups, stratified by gender, ethnicity, and risk ("pop")
    pop.total  = pop.male + pop.female
    pop.msm    = pop.male  *prop.msm.among.male
    pop.pwid.f = pop.total *prop.pwid.among.total *(1-prop.male.among.pwid)
    pop.pwid.m = pop.total *prop.pwid.among.total *prop.male.among.pwid

    pop.miduP  = pop.pwid.m * prop.mpwid.among.pwid
    pop.miduM  = pop.msm    * prop.mpwid.among.msm
    pop.midu   = (pop.miduP + pop.miduM) / 2

    pop.pwid.m = pop.pwid.m - pop.midu
    pop.msm    = pop.msm    - pop.midu

    pop.het.m  = pop.male   - pop.msm - pop.pwid.m - pop.midu
    pop.het.f  = pop.female - pop.pwid.f
    pop        = round(c(pop.msm, pop.midu, pop.pwid.m, pop.pwid.f, pop.het.m, pop.het.f))

    # infected but not diag
    # D/(I+D)=prop.HIV.aware => I=(D-prop*D)/prop
    I18 = (diag18 - prop.HIV.aware *diag18) /prop.HIV.aware
    # susceptible
    S18 = pop - diag18 - I18
    # ever on ART
    T.ever18 = diag18 *prop.ever.art
    # on ART among ever on ART
    T18 = T.ever18 *prop.curr.art
    # drop off ART
    O18 = T.ever18 - T18
    # diagnosed but not on ART
    D18 = diag18 - T18 - O18

    init18            = cbind(S18, I18, D18, T18, O18)
    row.names(init18) = names18

    nOAT = round(c(nOAT.m[1, 1:3] * c(pop.midu/(pop.midu + pop.pwid.m), 1-pop.midu/(pop.midu + pop.pwid.m)), nOAT.m[1, 4:6]))

    # stratify OAT into 5 HIV states, according to proportion of HIV states among PWID
    pwid.ind18 = grep("PWID", names18)
    init.OAT   = matrix(0, length(pwid.ind18), 5)
    for (i in 1:length(pwid.ind18)){
      prop.pwid     = init18[pwid.ind18[i], ]/ sum(init18[pwid.ind18[i], ])
      init.OAT[i, ] = nOAT[i] *prop.pwid
    }

    n.gp  = length(names.gp)
    # change n.gp group names without OAT, low, high
    rname = gsub(paste(c("/OAT", "/low", "/high"), collapse="|"), "", names.gp)
    #pwid group names without OAT, low, high
    #pwid.name=names18[grep("PWID",names18)]

    #init has n.gp in row, populated with same n if same rname
    init  = matrix(0, n.gp, 5) #
    row.names(init) = names.gp
    colnames(init)  = c("S", "I", "D", "T", "O")
    prop.high.sus.all =numeric(n.gp)
    prop.high.inf.all =numeric(n.gp)
    for (i in 1:18){
      ind = which(rname %in% names18[i])
      prop.high.sus.all[ind] = prop.high.sus[i]
      prop.high.inf.all[ind] = prop.high.inf[i]
      for (j in 1:length(ind)){
        init[ind[j], ] = init18[i, ]
      }
    }

    # populate OAT in init
    init[oat, ]     = rbind(init.OAT[1:3, ], init.OAT) #MSM/PWID has low/high

    # populate off OAT
    init[off.oat, ] = init[off.oat,] - init[oat, ]

    # low/high for MSM, MSM/PWID and HET
    init[high,"S"]  = init[high, "S"] *prop.high.sus.all[high]
    init[low,"S"]   = init[low, "S"]  *(1 - prop.high.sus.all[high])
    init[high, c("I","D","T","O")] = init[high, c("I","D","T","O")] *prop.high.inf.all[high]
    init[low , c("I","D","T","O")] = init[low,  c("I","D","T","O")] *(1-prop.high.inf.all[high])

    # 19 states
    S2 = prop.S2 *init[ ,"S"]
    S1 = init[ ,"S"] - S2
    I  = init[ ,"I"] *cbind(prop.Ia, prop.I1, prop.I2, prop.I3)
    D  = init[ ,"D"] *cbind(prop.Da, prop.D1, prop.D2, prop.D3)
    T  = init[ ,"T"] *cbind(prop.T1, prop.T2, prop.T3)
    O  = init[ ,"O"] *cbind(prop.O1, prop.O2, prop.O3)
    init2 = cbind(S1, S2, 0, I, 0, 0, D, T, O) #0 for PrEP

    state.name = c("S1", "S2", "Sp", "Ia", "I1", "I2", "I3", "Iap", "Ip", "Da",
                   "D1", "D2", "D3", "T1", "T2", "T3", "O1", "O2", "O3")
    colnames(init2) = state.name

    return(init2)
  })
}


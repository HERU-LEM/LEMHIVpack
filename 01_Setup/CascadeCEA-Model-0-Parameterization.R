################################################################################
## Parameterization for parameters that require manipulations                 ##
################################################################################

#### theta.ad = theta.ai ####
vparameters$delta_H = vparameters$delta_M = vlist$delta_sex$pe

#### theta.ad = theta.ai ####
vparameters$theta.ad = vparameters$theta.ai

#### Parameter in pwid (1st 18 in gp) domain ####
pwid.gn = pwid.gn.fun (names.pwid)

nBUP.m = matrix(0, nrow = 4, ncol = 6)
nBUP.m[ , 1] = round(vlist$OATDATACapacity$pe[-1] * (1 - vlist$PropGenderOATBup$pe) * with(vlist$PropEthnicityOATBup, pe[gender == "m" & ethnicity == "w" ]) * vlist$PropDATAPhysCity$pe * vlist$PropPWIDPatients$pe)
nBUP.m[ , 2] = round(vlist$OATDATACapacity$pe[-1] * (1 - vlist$PropGenderOATBup$pe) * with(vlist$PropEthnicityOATBup, pe[gender == "m" & ethnicity == "b" ]) * vlist$PropDATAPhysCity$pe * vlist$PropPWIDPatients$pe)
nBUP.m[ , 3] = round(vlist$OATDATACapacity$pe[-1] * (1 - vlist$PropGenderOATBup$pe) * with(vlist$PropEthnicityOATBup, pe[gender == "m" & ethnicity == "h" ]) * vlist$PropDATAPhysCity$pe * vlist$PropPWIDPatients$pe)
nBUP.m[ , 4] = round(vlist$OATDATACapacity$pe[-1] * vlist$PropGenderOATBup$pe * with(vlist$PropEthnicityOATBup, pe[gender == "f" & ethnicity == "w" ]) * vlist$PropDATAPhysCity$pe * vlist$PropPWIDPatients$pe)
nBUP.m[ , 5] = round(vlist$OATDATACapacity$pe[-1] * vlist$PropGenderOATBup$pe * with(vlist$PropEthnicityOATBup, pe[gender == "f" & ethnicity == "b" ]) * vlist$PropDATAPhysCity$pe * vlist$PropPWIDPatients$pe)
nBUP.m[ , 6] = round(vlist$OATDATACapacity$pe[-1] * vlist$PropGenderOATBup$pe * with(vlist$PropEthnicityOATBup, pe[gender == "f" & ethnicity == "h" ]) * vlist$PropDATAPhysCity$pe * vlist$PropPWIDPatients$pe)
nMET.m = matrix(0, nrow = 4, ncol = 6)
nMET.m[ , 1] = round(with(vlist$nOAT_OTP, pe[gender == "m" & ethnicity == "w"]))
nMET.m[ , 2] = round(with(vlist$nOAT_OTP, pe[gender == "m" & ethnicity == "b"]))
nMET.m[ , 3] = round(with(vlist$nOAT_OTP, pe[gender == "m" & ethnicity == "h"]))
nMET.m[ , 4] = round(with(vlist$nOAT_OTP, pe[gender == "f" & ethnicity == "w"]))
nMET.m[ , 5] = round(with(vlist$nOAT_OTP, pe[gender == "f" & ethnicity == "b"]))
nMET.m[ , 6] = round(with(vlist$nOAT_OTP, pe[gender == "f" & ethnicity == "h"]))

nOAT.m <- nBUP.m + nMET.m
vparameters$nOAT.m = nOAT.m
vparameters$nBUP.m = nBUP.m
vparameters$nMET.m = nMET.m

##s: 6 to 9, mwid = pwid
s = numeric(length(names.pwid))
s = rep(vlist$s$pe, length(names.pwid))
vparameters$s = s

#### Parameter in MSM domain ####
msm.gn = msm.gn.fun (names.msm)

##nsG: 6 to 18, mwid = msm, non-OAT = OAT
nsG = numeric(length(names.msm))
nsG[msm.gn$low]  = with(vlist$nsG, pe[sexual.intensity == "low"  & risk == "msm"])  #low-risk
nsG[msm.gn$high] = with(vlist$nsG, pe[sexual.intensity == "high" & risk == "msm"]) #high-risk
vparameters$nsG = nsG

##uis: 6 to 18, mwid = msm, non-OAT = OAT
uis = numeric(length(names.msm))
uis[msm.gn$low]  = with(vlist$uis, pe[sexual.intensity == "low"  & risk == "msm"])  #low-risk
uis[msm.gn$high] = with(vlist$uis, pe[sexual.intensity == "high" & risk == "msm"]) #high-risk
vparameters$uis = uis


#### Parameter in pop18 (race*gender*risk) domain ####
gp18.gn = gp18.gn.fun (names18)

##T1_O: 12 to 18, m=f for pwid, het
T1_O = numeric(length(names18))
T1_O[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T1_O, pe[risk == "pwid" & gender == "m"])
T1_O[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T1_O, pe[risk == "pwid" & gender == "f"])
T1_O[gp18.gn$msm]      = with(vlist$T1_O, pe[risk == "msm"])
T1_O[gp18.gn$mwid]     = with(vlist$T1_O, pe[risk == "mwid"])
T1_O[gp18.gn$het.f]    = with(vlist$T1_O, pe[risk == "het" & gender == "f"])
T1_O[gp18.gn$het.m]    = with(vlist$T1_O, pe[risk == "het" & gender == "m"])
vparameters$T1_O = T1_O

##T2_O: 12 to 18, m=f for pwid, het
T2_O = numeric(length(names18))
T2_O[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T2_O, pe[risk == "pwid" & gender == "m"])
T2_O[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T2_O, pe[risk == "pwid" & gender == "f"])
T2_O[gp18.gn$msm]      = with(vlist$T2_O, pe[risk == "msm"])
T2_O[gp18.gn$mwid]     = with(vlist$T2_O, pe[risk == "mwid"])
T2_O[gp18.gn$het.f]    = with(vlist$T2_O, pe[risk == "het" & gender == "f"])
T2_O[gp18.gn$het.m]    = with(vlist$T2_O, pe[risk == "het" & gender == "m"])
vparameters$T2_O = T2_O


##T3_O: 12 to 18, m=f for pwid, het
T3_O = numeric(length(names18))
T3_O[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T3_O, pe[risk == "pwid" & gender == "m"])
T3_O[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T3_O, pe[risk == "pwid" & gender == "f"])
T3_O[gp18.gn$msm]      = with(vlist$T3_O, pe[risk == "msm"])
T3_O[gp18.gn$mwid]     = with(vlist$T3_O, pe[risk == "mwid"])
T3_O[gp18.gn$het.f]    = with(vlist$T3_O, pe[risk == "het" & gender == "f"])
T3_O[gp18.gn$het.m]    = with(vlist$T3_O, pe[risk == "het" & gender == "m"])
vparameters$T3_O = T3_O

##T1_T2: 18 to 18
T1_T2 = numeric(length(names18))
T1_T2[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T1_T2, pe[risk == "pwid" & gender == "m"])
T1_T2[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T1_T2, pe[risk == "pwid" & gender == "f"])
T1_T2[gp18.gn$msm]      = with(vlist$T1_T2, pe[risk == "msm"])
T1_T2[gp18.gn$mwid]     = with(vlist$T1_T2, pe[risk == "mwid"])
T1_T2[gp18.gn$het.f]    = with(vlist$T1_T2, pe[risk == "het" & gender == "f"])
T1_T2[gp18.gn$het.m]    = with(vlist$T1_T2, pe[risk == "het" & gender == "m"])
vparameters$T1_T2 = T1_T2

##T1_T3: 18 to 18
T1_T3 = numeric(length(names18))
T1_T3[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T1_T3, pe[risk == "pwid" & gender == "m"])
T1_T3[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T1_T3, pe[risk == "pwid" & gender == "f"])
T1_T3[gp18.gn$msm]      = with(vlist$T1_T3, pe[risk == "msm"])
T1_T3[gp18.gn$mwid]     = with(vlist$T1_T3, pe[risk == "mwid"])
T1_T3[gp18.gn$het.f]    = with(vlist$T1_T3, pe[risk == "het" & gender == "f"])
T1_T3[gp18.gn$het.m]    = with(vlist$T1_T3, pe[risk == "het" & gender == "m"])
vparameters$T1_T3 = T1_T3

##T2_T3: 18 to 18
T2_T3 = numeric(length(names18))
T2_T3[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T2_T3, pe[risk == "pwid" & gender == "m"])
T2_T3[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T2_T3, pe[risk == "pwid" & gender == "f"])
T2_T3[gp18.gn$msm]      = with(vlist$T2_T3, pe[risk == "msm"])
T2_T3[gp18.gn$mwid]     = with(vlist$T2_T3, pe[risk == "mwid"])
T2_T3[gp18.gn$het.f]    = with(vlist$T2_T3, pe[risk == "het" & gender == "f"])
T2_T3[gp18.gn$het.m]    = with(vlist$T2_T3, pe[risk == "het" & gender == "m"])
vparameters$T2_T3 = T2_T3

##T2_T1: 18 to 18
T2_T1 = numeric(length(names18))
T2_T1[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T2_T1, pe[risk == "pwid" & gender == "m"])
T2_T1[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T2_T1, pe[risk == "pwid" & gender == "f"])
T2_T1[gp18.gn$msm]      = with(vlist$T2_T1, pe[risk == "msm"])
T2_T1[gp18.gn$mwid]     = with(vlist$T2_T1, pe[risk == "mwid"])
T2_T1[gp18.gn$het.f]    = with(vlist$T2_T1, pe[risk == "het" & gender == "f"])
T2_T1[gp18.gn$het.m]    = with(vlist$T2_T1, pe[risk == "het" & gender == "m"])
vparameters$T2_T1 = T2_T1

##T3_T1: 18 to 18
T3_T1 = numeric(length(names18))
T3_T1[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T3_T1, pe[risk == "pwid" & gender == "m"])
T3_T1[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T3_T1, pe[risk == "pwid" & gender == "f"])
T3_T1[gp18.gn$msm]      = with(vlist$T3_T1, pe[risk == "msm"])
T3_T1[gp18.gn$mwid]     = with(vlist$T3_T1, pe[risk == "mwid"])
T3_T1[gp18.gn$het.f]    = with(vlist$T3_T1, pe[risk == "het" & gender == "f"])
T3_T1[gp18.gn$het.m]    = with(vlist$T3_T1, pe[risk == "het" & gender == "m"])
vparameters$T3_T1 = T3_T1

##T3_T2: 18 to 18
T3_T2 = numeric(length(names18))
T3_T2[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$T3_T2, pe[risk == "pwid" & gender == "m"])
T3_T2[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$T3_T2, pe[risk == "pwid" & gender == "f"])
T3_T2[gp18.gn$msm]      = with(vlist$T3_T2, pe[risk == "msm"])
T3_T2[gp18.gn$mwid]     = with(vlist$T3_T2, pe[risk == "mwid"])
T3_T2[gp18.gn$het.f]    = with(vlist$T3_T2, pe[risk == "het" & gender == "f"])
T3_T2[gp18.gn$het.m]    = with(vlist$T3_T2, pe[risk == "het" & gender == "m"])
vparameters$T3_T2 = T3_T2

##O_T: 12 to 18, m=f for pwid, het
O_T = numeric(length(names18))
O_T[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$O_T, pe[risk == "pwid" & gender == "m"])
O_T[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$O_T, pe[risk == "pwid" & gender == "f"])
O_T[gp18.gn$msm]      = with(vlist$O_T, pe[risk == "msm"])
O_T[gp18.gn$mwid]     = with(vlist$O_T, pe[risk == "mwid"])
O_T[gp18.gn$het.f]    = with(vlist$O_T, pe[risk == "het" & gender == "f"])
O_T[gp18.gn$het.m]    = with(vlist$O_T, pe[risk == "het" & gender == "m"])
vparameters$O_T = O_T

##rho.m
rho.m = numeric(length(names18))
rho.m[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$rho.m, pe[gender == "m" & risk == "pwid"])
rho.m[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$rho.m, pe[gender == "f" & risk == "pwid"])
rho.m[gp18.gn$msm]  = with(vlist$rho.m, pe[risk == "msm"])
rho.m[gp18.gn$mwid] = with(vlist$rho.m, pe[risk == "mwid"])
rho.m[gp18.gn$het.m] = with(vlist$rho.m, pe[gender == "m" & risk == "het"])
rho.m[gp18.gn$het.f] = with(vlist$rho.m, pe[gender == "f" & risk == "het"])
vparameters$rho.m = rho.m

##rho: 6 to 18, only by ethnicity and gender
rho = numeric(length(names18))
rho[gp18.gn$male]   <- with(vlist$growth, pe[gender == "m"]) + with(vlist$mor_S, pe[risk == "het" & gender == "m"])
rho[gp18.gn$female] <- with(vlist$growth, pe[gender == "f"]) + with(vlist$mor_S, pe[risk == "het" & gender == "f"])
vparameters$rho = rho

##mat: 6 to 18, only by ethnicity and gender
mat = numeric(length(names18))
mat[gp18.gn$male]   = with(vlist$mat, pe[gender == "m"])
mat[gp18.gn$female] = with(vlist$mat, pe[gender == "f"])
vparameters$mat = mat

##phi1, phi2, phi3, alpha1, alpha2, alpha3: derived from phi1_, phi2_, phi3_, p1, alpha1_, alpha2_, alpha3_
##phi1_, phi2_, phi3_: 7 to 18, msm*ethnicity, pwid, mwid, het*gender
##alpha1_, alpha2_, alpha3_: 8 to 18, msm*ethnicity, pwid, het.f, mwid. het.m.w=het.m.h, het.m.b
##p1 = prop.ever.art
##phi1 =phi1_*p1; phi2 =phi2_*p1; phi3 =phi3_*p1; alpha1 =alpha1_*(p1*(1-phi1_))/(1-p1*phi1_); alpha2 =alpha2_*(p1*(1-phi1_))/(1-p1*phi2_); alpha3 =alpha3_*(p1*(1-phi3_))/(1-p1*phi3_)
phi1_ = numeric(length(names18)); phi2_ = numeric(length(names18)); phi3_ = numeric(length(names18))
p1 = numeric(length(names18))
alpha1_ = numeric(length(names18)); alpha2_ = numeric(length(names18)); alpha3_ = numeric(length(names18))

if (CITY != "SEA"){
  phi1_[gp18.gn$msm]     = with(vlist$phi1_, pe[risk == "msm"])[1]
  phi1_[gp18.gn$het.m]   = with(vlist$phi1_, pe[risk == "het" & gender == "m"])[1]
  phi1_[gp18.gn$het.f]   = with(vlist$phi1_, pe[risk == "het" & gender == "f"])[1]
} else {
  phi1_[gp18.gn$msm]     = with(vlist$phi1_, pe[risk == "msm"])
  phi1_[gp18.gn$het.m]   = with(vlist$phi1_, pe[risk == "het" & gender == "m"])
  phi1_[gp18.gn$het.f]   = with(vlist$phi1_, pe[risk == "het" & gender == "f"])
}
phi1_[gp18.gn$pwid]    = with(vlist$phi1_, pe[risk == "pwid"])[1]
phi1_[gp18.gn$mwid]    = with(vlist$phi1_, pe[risk == "mwid"])[1]


if (CITY != "SEA"){
  phi2_[gp18.gn$msm]     = with(vlist$phi2_, pe[risk == "msm"])[1]
  phi2_[gp18.gn$het.m]   = with(vlist$phi2_, pe[risk == "het" & gender == "m"])[1]
  phi2_[gp18.gn$het.f]   = with(vlist$phi2_, pe[risk == "het" & gender == "f"])[1]
} else {
  phi2_[gp18.gn$msm]     = with(vlist$phi2_, pe[risk == "msm"])
  phi2_[gp18.gn$het.m]   = with(vlist$phi2_, pe[risk == "het" & gender == "m"])
  phi2_[gp18.gn$het.f]   = with(vlist$phi2_, pe[risk == "het" & gender == "f"])
}
phi2_[gp18.gn$pwid]    = with(vlist$phi2_, pe[risk == "pwid"])[1]
phi2_[gp18.gn$mwid]    = with(vlist$phi2_, pe[risk == "mwid"])[1]


if (CITY != "SEA"){
  phi3_[gp18.gn$msm]     = with(vlist$phi3_, pe[risk == "msm"])[1]
  phi3_[gp18.gn$het.m]   = with(vlist$phi3_, pe[risk == "het" & gender == "m"])[1]
  phi3_[gp18.gn$het.f]   = with(vlist$phi3_, pe[risk == "het" & gender == "f"])[1]
} else {
  phi3_[gp18.gn$msm]     = with(vlist$phi3_, pe[risk == "msm"])
  phi3_[gp18.gn$het.m]   = with(vlist$phi3_, pe[risk == "het" & gender == "m"])
  phi3_[gp18.gn$het.f]   = with(vlist$phi3_, pe[risk == "het" & gender == "f"])
}
phi3_[gp18.gn$pwid]    = with(vlist$phi3_, pe[risk == "pwid"])[1]
phi3_[gp18.gn$mwid]    = with(vlist$phi3_, pe[risk == "mwid"])[1]


p1[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$p1, pe[risk == "pwid" & gender =="m"])
p1[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$p1, pe[risk == "pwid" & gender =="f"])
p1[gp18.gn$het.m] = with(vlist$p1, pe[risk == "het" & gender == "m"])
p1[gp18.gn$het.f] = with(vlist$p1, pe[risk == "het" & gender == "f"])
p1[gp18.gn$msm]   = with(vlist$p1, pe[risk == "msm"])
p1[gp18.gn$mwid]  = with(vlist$p1, pe[risk == "mwid"])


if (CITY != "SEA"){
  alpha1_[gp18.gn$msm]   = with(vlist$alpha1_, pe[risk == "msm"])[1]
  alpha1_[gp18.gn$het.f] = with(vlist$alpha1_, pe[risk == "het" & gender == "f"])[1]
  alpha1_[gp18.gn$het.m] = with(vlist$alpha1_, pe[risk == "het" & gender == "m"])[1]
} else {
  alpha1_[gp18.gn$msm]   = with(vlist$alpha1_, pe[risk == "msm"])
  alpha1_[gp18.gn$het.f] = with(vlist$alpha1_, pe[risk == "het" & gender == "f"])
  alpha1_[gp18.gn$het.m] = with(vlist$alpha1_, pe[risk == "het" & gender == "m"])
}
alpha1_[gp18.gn$mwid]  = with(vlist$alpha1_, pe[risk == "mwid"])[1]
alpha1_[gp18.gn$pwid]  = with(vlist$alpha1_, pe[risk == "pwid"])[1]


if (CITY != "SEA"){
  alpha2_[gp18.gn$msm]   = with(vlist$alpha2_, pe[risk == "msm"])[1]
  alpha2_[gp18.gn$het.f] = with(vlist$alpha2_, pe[risk == "het" & gender == "f"])[1]
  alpha2_[gp18.gn$het.m] = with(vlist$alpha2_, pe[risk == "het" & gender == "m"])[1]
} else {
  alpha2_[gp18.gn$msm]   = with(vlist$alpha2_, pe[risk == "msm"])
  alpha2_[gp18.gn$het.f] = with(vlist$alpha2_, pe[risk == "het" & gender == "f"])
  alpha2_[gp18.gn$het.m] = with(vlist$alpha2_, pe[risk == "het" & gender == "m"])
}
alpha2_[gp18.gn$mwid]  = with(vlist$alpha2_, pe[risk == "mwid"])[1]
alpha2_[gp18.gn$pwid]  = with(vlist$alpha2_, pe[risk == "pwid"])[1]


if (CITY != "SEA"){
  alpha3_[gp18.gn$msm]   = with(vlist$alpha3_, pe[risk == "msm"])[1]
  alpha3_[gp18.gn$het.f] = with(vlist$alpha3_, pe[risk == "het" & gender == "f"])[1]
  alpha3_[gp18.gn$het.m] = with(vlist$alpha3_, pe[risk == "het" & gender == "m"])[1]
} else {
  alpha3_[gp18.gn$msm]   = with(vlist$alpha3_, pe[risk == "msm"])
  alpha3_[gp18.gn$het.f] = with(vlist$alpha3_, pe[risk == "het" & gender == "f"])
  alpha3_[gp18.gn$het.m] = with(vlist$alpha3_, pe[risk == "het" & gender == "m"])
}
alpha3_[gp18.gn$mwid]  = with(vlist$alpha3_, pe[risk == "mwid"])[1]
alpha3_[gp18.gn$pwid]  = with(vlist$alpha3_, pe[risk == "pwid"])[1]


vparameters$phi1 = phi1_ * p1
vparameters$phi2 = phi2_ * p1
vparameters$phi3 = phi3_ * p1
vparameters$alpha1 = alpha1_ * (p1 * (1 - phi1_)) / (1 - p1 * phi1_)
vparameters$alpha2 = alpha2_ * (p1 * (1 - phi2_)) / (1 - p1 * phi2_)
vparameters$alpha3 = alpha3_ * (p1 * (1 - phi3_)) / (1 - p1 * phi3_)

##prop.link: % of diag linked to care
prop.link = numeric(length(names.gp))
rname = gsub(paste(c("/OAT", "/low", "/high"), collapse="|"), "", names.gp)
for (i in 1:18){
  ind = which(rname %in% names18[i])
  prop.link[ind]    = p1[i]
}
vparameters$prop.link = prop.link

##prop.HIV.aware: 6 to 18
prop.HIV.aware = numeric(length(names18))
prop.HIV.aware[intersect(gp18.gn$pwid, gp18.gn$male)]   = with(vlist$prop.HIV.aware, pe[risk == "pwid" & gender == "m"])
prop.HIV.aware[intersect(gp18.gn$pwid, gp18.gn$female)] = with(vlist$prop.HIV.aware, pe[risk == "pwid" & gender == "f"])
prop.HIV.aware[gp18.gn$msm]   = with(vlist$prop.HIV.aware, pe[risk == "msm"  & gender == "m"])
prop.HIV.aware[gp18.gn$mwid]  = with(vlist$prop.HIV.aware, pe[risk == "mwid" & gender == "m"])
prop.HIV.aware[gp18.gn$het.m] = with(vlist$prop.HIV.aware, pe[risk == "het"  & gender == "m"])
prop.HIV.aware[gp18.gn$het.f] = with(vlist$prop.HIV.aware, pe[risk == "het"  & gender == "f"])
vparameters$prop.HIV.aware = prop.HIV.aware


##prop.ever.art: 9 to 18, 1-D data to 3-D data, use male white het as the reference level..= p1
vparameters$prop.ever.art = p1

##prop.curr.art: 3 to  18, only by ethnicity
prop.curr.art = numeric(length(names18))
prop.curr.art[gp18.gn$white] = with(vlist$prop.curr.art, pe[ethnicity == "w"])
prop.curr.art[gp18.gn$black] = with(vlist$prop.curr.art, pe[ethnicity == "b"])
prop.curr.art[gp18.gn$hisp]  = with(vlist$prop.curr.art, pe[ethnicity == "h"])
vparameters$prop.curr.art = prop.curr.art

##prop.high.sus: 7 to  18, 0.25 certain for msm, mpwid, NA for pwid
prop.high.sus = rep(NA, length(names18))
prop.high.sus[gp18.gn$all.msm] = with(vlist$prop.high.sus, pe[risk == "msm"])[1]
prop.high.sus[gp18.gn$het.m]   = with(vlist$prop.high.sus, pe[risk == "het" & gender == "m"])
prop.high.sus[gp18.gn$het.f]   = with(vlist$prop.high.sus, pe[risk == "het" & gender == "f"])
vparameters$prop.high.sus = prop.high.sus

##prop.high.inf: 8 to  18, 0.27 certain for msm, 0.3 certain for mpwid, NA for pwid
prop.high.inf = rep(NA, length(names18))
prop.high.inf[gp18.gn$msm] = with(vlist$prop.high.inf, pe[risk == "msm"])[1]
prop.high.inf[gp18.gn$mwid] = with(vlist$prop.high.inf, pe[risk == "mwid"])[1]
prop.high.inf[gp18.gn$het.m]   = with(vlist$prop.high.inf, pe[risk == "het" & gender == "m"])
prop.high.inf[gp18.gn$het.f]   = with(vlist$prop.high.inf, pe[risk == "het" & gender == "f"])
vparameters$prop.high.inf = prop.high.inf


#### Parameter in 42 full gp domain ####
gp.gn = gp.gn.fun (names.gp)

##noG: 18 to 42, msm=mwid, pwid=high.het, non-OAT=OAT
noG = numeric(length(names.gp))
noG[gp.gn$L.allmsm] = with(vlist$noG, pe[risk == "msm" & sexual.intensity == "low"])
noG[gp.gn$H.allmsm] = with(vlist$noG, pe[risk == "msm" & sexual.intensity == "high"])
noG[gp.gn$L.het.m]  = with(vlist$noG, pe[risk == "het" & sexual.intensity == "low" & gender =="m"])
noG[gp.gn$L.het.f]  = with(vlist$noG, pe[risk == "het" & sexual.intensity == "low" & gender =="f"])
noG[gp.gn$H.het.m]  = with(vlist$noG, pe[risk == "het" & sexual.intensity == "high" & gender =="m"])
noG[gp.gn$H.het.f]  = with(vlist$noG, pe[risk == "het" & sexual.intensity == "high" & gender =="f"])
noG[gp.gn$pwid.m]   = noG[gp.gn$L.het.m] * vparameters$noG.pwid
noG[gp.gn$pwid.f]   = noG[gp.gn$L.het.f] * vparameters$noG.pwid 
vparameters$noG = noG

##uio: 12 to 42, no risk diff in low,  pwid=high het, msm=mwid, H.het.f=L.het.f
uio = numeric(length(names.gp))
uio[intersect(gp.gn$L.m, gp.gn$white)] = with(vlist$uio, pe[ethnicity == "w" & gender == "m" & sexual.intensity == "low"])[1]
uio[intersect(gp.gn$L.m, gp.gn$black)] = with(vlist$uio, pe[ethnicity == "b" & gender == "m" & sexual.intensity == "low"])[1]
uio[intersect(gp.gn$L.m, gp.gn$hisp)]  = with(vlist$uio, pe[ethnicity == "h" & gender == "m" & sexual.intensity == "low"])[1]

uio[intersect(gp.gn$female, gp.gn$white)] = with(vlist$uio, pe[ethnicity == "w" & gender == "f"])[1]
uio[intersect(gp.gn$female, gp.gn$black)] = with(vlist$uio, pe[ethnicity == "b" & gender == "f"])[1]
uio[intersect(gp.gn$female, gp.gn$hisp)]  = with(vlist$uio, pe[ethnicity == "h" & gender == "f"])[1]

uio[gp.gn$H.allmsm] = with(vlist$uio, pe[risk == "msm" & sexual.intensity == "high"])[1:3]
uio[gp.gn$H.het.m]  = with(vlist$uio, pe[risk == "het" & gender =="m" & sexual.intensity == "high"])[1:3]
uio[intersect(gp.gn$pwid, gp.gn$male)]   = uio[gp.gn$H.het.m]

vparameters$uio = uio

##psi: directly pasted from the model calibration, no need to manipulate
vparameters$psi = vlist$psi$pe

##mor_S: 13 to 42, all pwid equal, low=high
mor_S = numeric(length(names.gp))
mor_S[gp.gn$pwid] = with(vlist$mor_S, pe[risk == "pwid"])[1]
mor_S[gp.gn$msm]  = with(vlist$mor_S, pe[risk == "msm"])
mor_S[gp.gn$mwid] = with(vlist$mor_S, pe[risk == "mwid"])
mor_S[gp.gn$het.m]= with(vlist$mor_S, pe[risk == "het" & gender == "m"])
mor_S[gp.gn$het.f]= with(vlist$mor_S, pe[risk == "het" & gender == "f"])
mor_S[gp.gn$OAT] = mor_S[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_S = mor_S

##mor_Ia: = mor_S
vparameters$mor_Ia = vparameters$mor_S

##mor_I1: 13 to 42, all pwid=, 
vparameters$mor_I1 = vparameters$mor_S

##mor_I2: 1 to 42, only diff by risk
mor_I2 = numeric(length(names.gp))
mor_I2 = rep(vlist$mor_I2$pe, length(names.gp))
mor_I2[gp.gn$all.pwid] = mor_I2[gp.gn$all.pwid] * vparameters$pwid_mor2
#mor_I2[gp.gn$mwid] = (mor_I2[gp.gn$mwid] + mor_I2[gp.gn$msm])/2
mor_I2[gp.gn$OAT]  = mor_I2[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_I2 = mor_I2

##mor_I3: 1 to 42, only diff by risk
mor_I3 = numeric(length(names.gp))
mor_I3 = rep(vlist$mor_I3$pe, length(names.gp))
mor_I3[gp.gn$all.pwid] = mor_I3[gp.gn$all.pwid] * vparameters$pwid_mor3
#mor_I3[gp.gn$mwid] = (mor_I3[gp.gn$mwid] + mor_I3[gp.gn$msm])/2
mor_I3[gp.gn$OAT]  = mor_I3[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_I3 = mor_I3

##mor_T1: 18 to 42
mor_T1 = numeric(length(names.gp))
mor_T1[gp.gn$pwid.m] = with(vlist$mor_T1, pe[risk == "pwid" & gender == "m"])
mor_T1[gp.gn$pwid.f] = with(vlist$mor_T1, pe[risk == "pwid" & gender == "f"])
mor_T1[gp.gn$msm]    = with(vlist$mor_T1, pe[risk == "msm"])
mor_T1[gp.gn$mwid]   = with(vlist$mor_T1, pe[risk == "pwid"])
mor_T1[gp.gn$het.m]  = with(vlist$mor_T1, pe[risk == "het" & gender == "m"])
mor_T1[gp.gn$het.f]  = with(vlist$mor_T1, pe[risk == "het" & gender == "f"])
mor_T1[gp.gn$OAT]  = mor_T1[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_T1 = mor_T1

##mor_T2: 18 to 42
mor_T2 = numeric(length(names.gp))
mor_T2[gp.gn$pwid.m] = with(vlist$mor_T2, pe[risk == "pwid" & gender == "m"])
mor_T2[gp.gn$pwid.f] = with(vlist$mor_T2, pe[risk == "pwid" & gender == "f"])
mor_T2[gp.gn$msm]    = with(vlist$mor_T2, pe[risk == "msm"])
mor_T2[gp.gn$mwid]   = with(vlist$mor_T2, pe[risk == "pwid"])
mor_T2[gp.gn$het.m]  = with(vlist$mor_T2, pe[risk == "het" & gender == "m"])
mor_T2[gp.gn$het.f]  = with(vlist$mor_T2, pe[risk == "het" & gender == "f"])
mor_T2[gp.gn$OAT]  = mor_T2[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_T2 = mor_T2

##mor_T3: 18 to 42
mor_T3 = numeric(length(names.gp))
mor_T3[gp.gn$pwid.m] = with(vlist$mor_T3, pe[risk == "pwid" & gender == "m"])
mor_T3[gp.gn$pwid.f] = with(vlist$mor_T3, pe[risk == "pwid" & gender == "f"])
mor_T3[gp.gn$msm]    = with(vlist$mor_T3, pe[risk == "msm"])
mor_T3[gp.gn$mwid]   = with(vlist$mor_T3, pe[risk == "pwid"])
mor_T3[gp.gn$het.m]  = with(vlist$mor_T3, pe[risk == "het" & gender == "m"])
mor_T3[gp.gn$het.f]  = with(vlist$mor_T3, pe[risk == "het" & gender == "f"])
mor_T3[gp.gn$OAT]  = mor_T3[gp.gn$OAT] * vparameters$oat_mor
vparameters$mor_T3 = mor_T3

##prop.S2: 21 to 42, LOW: mwid=msm=het, HIGH: msm=mwid
prop.S2 =  numeric(length(names.gp))
prop.S2[gp.gn$pwid.m]  = with(vlist$prop.S2, pe[risk == "pwid"  & gender == "m"])
prop.S2[gp.gn$pwid.f]  = with(vlist$prop.S2, pe[risk == "pwid"  & gender == "f"])
prop.S2[gp.gn$L.het.m] = prop.S2[intersect(gp.gn$msm, gp.gn$low)] = prop.S2[intersect(gp.gn$mwid, gp.gn$low)] = with(vlist$prop.S2, pe[risk == "het" & gender == "m" & sexual.intensity == "low"])
prop.S2[gp.gn$L.het.f] = with(vlist$prop.S2, pe[risk == "het" & gender == "f" & sexual.intensity == "low"])
prop.S2[intersect(gp.gn$msm, gp.gn$high)]  = with(vlist$prop.S2, pe[risk == "msm" & sexual.intensity == "high"])
prop.S2[intersect(gp.gn$mwid, gp.gn$high)] = prop.S2[intersect(gp.gn$msm, gp.gn$high)]
prop.S2[gp.gn$H.het.m] = with(vlist$prop.S2, pe[risk == "het" & gender == "m" & sexual.intensity == "high"])
prop.S2[gp.gn$H.het.f] = with(vlist$prop.S2, pe[risk == "het" & gender == "f" & sexual.intensity == "high"])
vparameters$prop.S2 = prop.S2

##prop.a: 1 to 42
prop.a  = rep(vlist$prop.a$pe, 42)
vparameters$prop.Ia = vparameters$prop.Da = prop.a

##prop.I: 1*3 to 42*3
prop.I = matrix(0, nrow=length(names.gp), ncol=3)
prop.I[ ,1] = as.numeric(with(vlist$prop.I, pe[cd4 ==1]))
prop.I[ ,2] = as.numeric(with(vlist$prop.I, pe[cd4 ==2]))
prop.I[ ,3] = as.numeric(with(vlist$prop.I, pe[cd4 ==3]))
vparameters$prop.I1 = prop.I[ ,1]
vparameters$prop.I2 = prop.I[ ,2]
vparameters$prop.I3 = prop.I[ ,3]

##prop.D: 13*3 to 42*3
prop.D = matrix(0, nrow=length(names.gp), ncol=3)
prop.D[ ,1][gp.gn$pwid]  = with(vlist$prop.D, pe[cd4 ==1 & risk == "pwid"])[1:3]
prop.D[ ,2][gp.gn$pwid]  = with(vlist$prop.D, pe[cd4 ==2 & risk == "pwid"])[1:3]
prop.D[ ,3][gp.gn$pwid]  = with(vlist$prop.D, pe[cd4 ==3 & risk == "pwid"])[1:3]
prop.D[ ,1][gp.gn$msm]   = with(vlist$prop.D, pe[cd4 ==1 & risk == "msm"])
prop.D[ ,2][gp.gn$msm]   = with(vlist$prop.D, pe[cd4 ==2 & risk == "msm"])
prop.D[ ,3][gp.gn$msm]   = with(vlist$prop.D, pe[cd4 ==3 & risk == "msm"])
prop.D[ ,1][gp.gn$mwid]  = with(vlist$prop.D, pe[cd4 ==1 & risk == "mwid"])
prop.D[ ,2][gp.gn$mwid]  = with(vlist$prop.D, pe[cd4 ==2 & risk == "mwid"])
prop.D[ ,3][gp.gn$mwid]  = with(vlist$prop.D, pe[cd4 ==3 & risk == "mwid"])
prop.D[ ,1][gp.gn$het.m] = with(vlist$prop.D, pe[cd4 ==1 & risk == "het" & gender == "m"])
prop.D[ ,2][gp.gn$het.m] = with(vlist$prop.D, pe[cd4 ==2 & risk == "het" & gender == "m"])
prop.D[ ,3][gp.gn$het.m] = with(vlist$prop.D, pe[cd4 ==3 & risk == "het" & gender == "m"])
prop.D[ ,1][gp.gn$het.f] = with(vlist$prop.D, pe[cd4 ==1 & risk == "het" & gender == "f"])
prop.D[ ,2][gp.gn$het.f] = with(vlist$prop.D, pe[cd4 ==2 & risk == "het" & gender == "f"])
prop.D[ ,3][gp.gn$het.f] = with(vlist$prop.D, pe[cd4 ==3 & risk == "het" & gender == "f"])
vparameters$prop.D1 = prop.D[ ,1]
vparameters$prop.D2 = prop.D[ ,2]
vparameters$prop.D3 = prop.D[ ,3]

##prop.T: 12*3 to 42*3
prop.T = matrix(0, nrow=length(names.gp), ncol=3)
prop.T[ ,1][gp.gn$pwid]  = with(vlist$prop.T, pe[cd4 ==1 & risk == "pwid"])[1:3]
prop.T[ ,2][gp.gn$pwid]  = with(vlist$prop.T, pe[cd4 ==2 & risk == "pwid"])[1:3]
prop.T[ ,3][gp.gn$pwid]  = with(vlist$prop.T, pe[cd4 ==3 & risk == "pwid"])[1:3]
prop.T[ ,1][gp.gn$msm]   = with(vlist$prop.T, pe[cd4 ==1 & risk == "msm"])
prop.T[ ,2][gp.gn$msm]   = with(vlist$prop.T, pe[cd4 ==2 & risk == "msm"])
prop.T[ ,3][gp.gn$msm]   = with(vlist$prop.T, pe[cd4 ==3 & risk == "msm"])
prop.T[ ,1][gp.gn$mwid]  = with(vlist$prop.T, pe[cd4 ==1 & risk == "pwid"])[1:3]
prop.T[ ,2][gp.gn$mwid]  = with(vlist$prop.T, pe[cd4 ==2 & risk == "pwid"])[1:3]
prop.T[ ,3][gp.gn$mwid]  = with(vlist$prop.T, pe[cd4 ==3 & risk == "pwid"])[1:3]
prop.T[ ,1][gp.gn$het.m] = with(vlist$prop.T, pe[cd4 ==1 & risk == "het" & gender == "m"])
prop.T[ ,2][gp.gn$het.m] = with(vlist$prop.T, pe[cd4 ==2 & risk == "het" & gender == "m"])
prop.T[ ,3][gp.gn$het.m] = with(vlist$prop.T, pe[cd4 ==3 & risk == "het" & gender == "m"])
prop.T[ ,1][gp.gn$het.f] = with(vlist$prop.T, pe[cd4 ==1 & risk == "het" & gender == "f"])
prop.T[ ,2][gp.gn$het.f] = with(vlist$prop.T, pe[cd4 ==2 & risk == "het" & gender == "f"])
prop.T[ ,3][gp.gn$het.f] = with(vlist$prop.T, pe[cd4 ==3 & risk == "het" & gender == "f"])
vparameters$prop.T1 = prop.T[ ,1]
vparameters$prop.T2 = prop.T[ ,2]
vparameters$prop.T3 = prop.T[ ,3]

##prop.O: 12*3 to 42*3
prop.O = matrix(0, nrow=length(names.gp), ncol=3)
prop.O[ ,1][gp.gn$pwid]  = with(vlist$prop.O, pe[cd4 ==1 & risk == "pwid"])[1:3]
prop.O[ ,2][gp.gn$pwid]  = with(vlist$prop.O, pe[cd4 ==2 & risk == "pwid"])[1:3]
prop.O[ ,3][gp.gn$pwid]  = with(vlist$prop.O, pe[cd4 ==3 & risk == "pwid"])[1:3]
prop.O[ ,1][gp.gn$msm]   = with(vlist$prop.O, pe[cd4 ==1 & risk == "msm"])
prop.O[ ,2][gp.gn$msm]   = with(vlist$prop.O, pe[cd4 ==2 & risk == "msm"])
prop.O[ ,3][gp.gn$msm]   = with(vlist$prop.O, pe[cd4 ==3 & risk == "msm"])
prop.O[ ,1][gp.gn$mwid]  = with(vlist$prop.O, pe[cd4 ==1 & risk == "pwid"])[1:3]
prop.O[ ,2][gp.gn$mwid]  = with(vlist$prop.O, pe[cd4 ==2 & risk == "pwid"])[1:3]
prop.O[ ,3][gp.gn$mwid]  = with(vlist$prop.O, pe[cd4 ==3 & risk == "pwid"])[1:3]
prop.O[ ,1][gp.gn$het.m] = with(vlist$prop.O, pe[cd4 ==1 & risk == "het" & gender == "m"])
prop.O[ ,2][gp.gn$het.m] = with(vlist$prop.O, pe[cd4 ==2 & risk == "het" & gender == "m"])
prop.O[ ,3][gp.gn$het.m] = with(vlist$prop.O, pe[cd4 ==3 & risk == "het" & gender == "m"])
prop.O[ ,1][gp.gn$het.f] = with(vlist$prop.O, pe[cd4 ==1 & risk == "het" & gender == "f"])
prop.O[ ,2][gp.gn$het.f] = with(vlist$prop.O, pe[cd4 ==2 & risk == "het" & gender == "f"])
prop.O[ ,3][gp.gn$het.f] = with(vlist$prop.O, pe[cd4 ==3 & risk == "het" & gender == "f"])
vparameters$prop.O1 = prop.O[ ,1]
vparameters$prop.O2 = prop.O[ ,2]
vparameters$prop.O3 = prop.O[ ,3]


#### Parameter in transmissibility domain: add acute stage ####
sigmaO.FM = numeric(4)
sigmaO.FM[2:4] = vlist$sigmaO.FM$pe
sigmaO.FM[1]   = vparameters$trans.acute * sigmaO.FM[2]
vparameters$sigmaO.FM = sigmaO.FM

sigmaO.MF = numeric(4)
sigmaO.MF[2:4] = vlist$sigmaO.MF$pe
sigmaO.MF[1]   = vparameters$trans.acute * sigmaO.MF[2]
vparameters$sigmaO.MF = sigmaO.MF

sigmaS = numeric(4)
sigmaS[2:4] = vlist$sigmaS$pe
sigmaS[1]   = vparameters$trans.acute * sigmaS[2]
vparameters$sigmaS = sigmaS

tau = numeric(4)
tau[2:4] = vlist$tau$pe
tau[1]   = vparameters$trans.acute * tau[2]
vparameters$tau = tau


#### Parameter in assortativeness domain ####
ass.eO = numeric(length(names.e))
ass.eO[grep("low", names.e)]  = with(vlist$ass.eO, pe[sexual.intensity == "low"])
ass.eO[grep("high", names.e)] = with(vlist$ass.eO, pe[sexual.intensity == "high"])
vparameters$ass.eO = ass.eO

ass.eS = numeric(length(names.e))
ass.eS[grep("low", names.e)]  = with(vlist$ass.eS, pe[sexual.intensity == "low"])
ass.eS[grep("high", names.e)] = with(vlist$ass.eS, pe[sexual.intensity == "high"])
vparameters$ass.eS = ass.eS


#### Parameter in population demongraphics domain ####

##pop.male
pop.male = numeric(3)
pop.male[1] = with(vlist$pop.total, pe[ethnicity == "w" & gender == "m"])
pop.male[2] = with(vlist$pop.total, pe[ethnicity == "b" & gender == "m"])
pop.male[3] = with(vlist$pop.total, pe[ethnicity == "h" & gender == "m"])
vparameters$pop.male = pop.male

##pop.female
pop.female = numeric(3)
pop.female[1] = with(vlist$pop.total, pe[ethnicity == "w" & gender == "f"])
pop.female[2] = with(vlist$pop.total, pe[ethnicity == "b" & gender == "f"])
pop.female[3] = with(vlist$pop.total, pe[ethnicity == "h" & gender == "f"])
vparameters$pop.female = pop.female

##prop.msm.among.male: 1 to 3
prop.msm.among.male = numeric(3)
if (CITY == "NYC"){
  region.prop_m.msm <- numeric(5)
  region.prop_m.msm[1:4] = vlist$prop.msm.among.male$pe[1:4]
  region.prop_m.msm[5]   = with(vlist$prop.msm.among.male, pe[region == "brooklyn"])
  prop.msm.among.male    = sum(region.prop_m.msm * as.numeric(vlist$prop.msm.among.male$male.pop)) / sum(as.numeric(vlist$prop.msm.among.male$male.pop))
  vparameters$prop.msm.among.male = rep(prop.msm.among.male, 3)
} else {
  vparameters$prop.msm.among.male = rep(vlist$prop.msm.among.male$pe, 3)
}

##prop.male.among.pwid: 1 to 3
prop.male.among.pwid <- rep(vlist$prop.male.among.pwid$pe, 3)
vparameters$prop.male.among.pwid = prop.male.among.pwid

##prop.pwid.among.total
prop.pwid.among.total = numeric(3)
prop.pwid.among.total[1] = with(vlist$prop.pwid.among.total, pe[ethnicity == "w"])
prop.pwid.among.total[2] = with(vlist$prop.pwid.among.total, pe[ethnicity == "b"])
prop.pwid.among.total[3] = with(vlist$prop.pwid.among.total, pe[ethnicity == "h"])
vparameters$prop.pwid.among.total = prop.pwid.among.total

##prop.mpwid.among.pwid
prop.mpwid.among.pwid = numeric(3)
prop.mpwid.among.pwid[1] = with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "w"])
prop.mpwid.among.pwid[2] = with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "b"])
prop.mpwid.among.pwid[3] = with(vlist$prop.mpwid.among.pwid, pe[ethnicity == "h"])
vparameters$prop.mpwid.among.pwid = prop.mpwid.among.pwid

##prop.mpwid.among.msm
prop.mpwid.among.msm = numeric(3)
prop.mpwid.among.msm[1] = with(vlist$prop.mpwid.among.msm, pe[ethnicity == "w"])
prop.mpwid.among.msm[2] = with(vlist$prop.mpwid.among.msm, pe[ethnicity == "b"])
prop.mpwid.among.msm[3] = with(vlist$prop.mpwid.among.msm, pe[ethnicity == "h"])
vparameters$prop.mpwid.among.msm = prop.mpwid.among.msm

#### PrEP parameter ####
vparameters$prep.total = vlist$prep.total$pe * vparameters$prep.scn

prep.proportion = numeric(3)
prep.proportion[1] = with(vlist$prep.proportion, pe[ethnicity == "w"])
prep.proportion[2] = with(vlist$prep.proportion, pe[ethnicity == "b"])
prep.proportion[3] = with(vlist$prep.proportion, pe[ethnicity == "h"])
vparameters$prep.proportion = prep.proportion

coverage.by.year <- updated.prep2020[which(updated.prep2020$city==CITY),"coverage"]
names(coverage.by.year) <- updated.prep2020[which(updated.prep2020$city==CITY),"year"]
prep.by.year <- updated.prep2020[which(updated.prep2020$city==CITY),"pe"]

#### Screening rate parameter ####: now include increased screening rate
vparameters$psi.m = rbind(vparameters$psi, vparameters$psi*(1+vparameters$psi.slope), vparameters$psi*(1+vparameters$psi.slope)^2, vparameters$psi*(1+vparameters$psi.slope)^3)

#### INITIALIZING CEA parameters  ####
StateQALYs = matrix(0, nrow=19, ncol=42)
colnames(StateQALYs) = names.gp
rownames(StateQALYs) = state.name[1:19]
StateCostsHRU = StateCostsART = StateCostsARTinit = StateCostsOAT = StateCostsPrEP = StateCostsTests.psi.p = StateCostsTests.psi = StateQALYs
StateQALYs[,] = 1/12

IntCosts.test.optout.ER  = IntCosts.impl.test.optout.ER  = IntCosts.sust.test.optout.ER  = StateCostsHRU
IntCosts.impl.lump.test.optout.ER = IntCosts.sust.lump.test.optout.ER                    = StateCostsHRU
IntCosts.test.optout.PC  = IntCosts.impl.test.optout.PC  = IntCosts.sust.test.optout.PC  = StateCostsHRU
IntCosts.impl.lump.test.optout.PC = IntCosts.sust.lump.test.optout.PC                    = StateCostsHRU
IntCosts.test.EMR        = IntCosts.impl.test.EMR        = IntCosts.sust.test.EMR        = StateCostsHRU
IntCosts.impl.lump.test.EMR = IntCosts.sust.lump.test.EMR                                = StateCostsHRU
IntCosts.test.nurse      = IntCosts.impl.test.nurse      = IntCosts.sust.test.nurse      = StateCostsHRU
IntCosts.impl.lump.test.nurse = IntCosts.sust.lump.test.nurse                            = StateCostsHRU
IntCosts.test.OAT        = IntCosts.impl.test.OAT        = IntCosts.sust.test.OAT        = StateCostsHRU
IntCosts.ARTinit         = IntCosts.impl.ARTinit         = IntCosts.sust.ARTinit         = StateCostsHRU
IntCosts.ARTreten        = IntCosts.impl.ARTreten        = IntCosts.sust.ARTreten        = StateCostsHRU
IntCosts.ARTreten.target = IntCosts.impl.ARTreten.target = IntCosts.sust.ARTreten.target = StateCostsHRU
IntCosts.ARTEMRprompt    = IntCosts.impl.ARTEMRprompt    = IntCosts.sust.ARTEMRprompt    = StateCostsHRU
IntCosts.immART          = IntCosts.impl.immART          = IntCosts.sust.immART          = StateCostsHRU
IntCosts.reART           = IntCosts.impl.reART           = IntCosts.sust.reART           = StateCostsHRU
IntCosts.relink          = IntCosts.impl.relink          = IntCosts.sust.relink          = StateCostsHRU
IntCosts.impl.lump.relink                                                                = StateCostsHRU
IntCosts.SSP             = IntCosts.impl.SSP             = IntCosts.sust.SSP             = StateCostsHRU
IntCosts.BUP             = IntCosts.impl.BUP             = IntCosts.sust.BUP             = StateCostsHRU
IntCosts.MET             = IntCosts.impl.MET             = IntCosts.sust.MET             = StateCostsHRU
IntCosts.prep            = IntCosts.impl.prep            = IntCosts.sust.prep            = StateCostsHRU

#### QALY parameetrs ####
StateQALYs[c("I1", "Ip"), ] = StateQALYs[c("I1", "Ip"), ] * with(vlist$qaly_I, pe[cd4 == 1])
StateQALYs["I2", ] = StateQALYs["I2", ] * with(vlist$qaly_I, pe[cd4 == 2])
StateQALYs["I3", ] = StateQALYs["I3", ] * with(vlist$qaly_I, pe[cd4 == 3])
StateQALYs[c("D1", "Da"), ] = StateQALYs[c("D1", "Da"), ] * with(vlist$qaly_D, pe[cd4 == 1])
StateQALYs["D2", ] = StateQALYs["D2", ] * with(vlist$qaly_D, pe[cd4 == 2])
StateQALYs["D3", ] = StateQALYs["D3", ] * with(vlist$qaly_D, pe[cd4 == 3])
StateQALYs["T1", ] = StateQALYs["T1", ] * with(vlist$qaly_T, pe[cd4 == 1])
StateQALYs["T2", ] = StateQALYs["T2", ] * with(vlist$qaly_T, pe[cd4 == 2])
StateQALYs["T3", ] = StateQALYs["T3", ] * with(vlist$qaly_T, pe[cd4 == 3])
StateQALYs[c("O1", "O2", "O3"), ] = StateQALYs[c("D1", "D2", "D3"), ]
StateQALYs[ , all.idu] = StateQALYs[ , all.idu] * vparameters$qaly_pwid
StateQALYs[ , oat] = StateQALYs[ , oat] * vparameters$qaly_oat

#### State costs ####
StateCostsHRU[c("S1", "S2", "Sp", "Ia", "Iap", "Da"), ] <- vparameters$cost_sus
StateCostsHRU[c("S1", "S2", "Sp", "Ia", "Iap", "Da"), grep("PWID", names.gp)] <- vparameters$cost_pwid_mul * StateCostsHRU[c("S1", "S2", "Sp", "Ia", "Iap", "Da"), grep("PWID", names.gp)]
StateCostsHRU[c("I1", "D1", "O1", "Ip"), idu.m] <- with(vlist$cost_IDO1, pe[gender == "m" & risk == "pwid"])
StateCostsHRU[c("I2", "D2", "O2"), idu.m] <- with(vlist$cost_IDO2, pe[gender == "m" & risk == "pwid"])
StateCostsHRU[c("I3", "D3", "O3"), idu.m] <- with(vlist$cost_IDO3, pe[gender == "m" & risk == "pwid"])
StateCostsHRU[c("I1", "D1", "O1", "Ip"), midu] <- with(vlist$cost_IDO1, pe[gender == "m" & risk == "mwid"])
StateCostsHRU[c("I2", "D2", "O2"), midu] <- with(vlist$cost_IDO2, pe[gender == "m" & risk == "mwid"])
StateCostsHRU[c("I3", "D3", "O3"), midu] <- with(vlist$cost_IDO3, pe[gender == "m" & risk == "mwid"])
StateCostsHRU[c("I1", "D1", "O1", "Ip"), idu.f] <- with(vlist$cost_IDO1, pe[gender == "f" & risk == "pwid"])
StateCostsHRU[c("I2", "D2", "O2"), idu.f] <- with(vlist$cost_IDO2, pe[gender == "f" & risk == "pwid"])
StateCostsHRU[c("I3", "D3", "O3"), idu.f] <- with(vlist$cost_IDO3, pe[gender == "f" & risk == "pwid"])
StateCostsHRU[c("I1", "D1", "O1", "Ip"), het.m] <- with(vlist$cost_IDO1, pe[gender == "m" & risk == "het"])
StateCostsHRU[c("I2", "D2", "O2"), het.m] <- with(vlist$cost_IDO2, pe[gender == "m" & risk == "het"])
StateCostsHRU[c("I3", "D3", "O3"), het.m] <- with(vlist$cost_IDO3, pe[gender == "m" & risk == "het"])
StateCostsHRU[c("I1", "D1", "O1", "Ip"), het.f] <- with(vlist$cost_IDO1, pe[gender == "f" & risk == "het"])
StateCostsHRU[c("I2", "D2", "O2"), het.f] <- with(vlist$cost_IDO2, pe[gender == "f" & risk == "het"])
StateCostsHRU[c("I3", "D3", "O3"), het.f] <- with(vlist$cost_IDO3, pe[gender == "f" & risk == "het"])
StateCostsHRU[c("I1", "D1", "O1", "Ip"), msm] <- with(vlist$cost_IDO1, pe[gender == "m" & risk == "msm"])
StateCostsHRU[c("I2", "D2", "O2"), msm] <- with(vlist$cost_IDO2, pe[gender == "m" & risk == "msm"])
StateCostsHRU[c("I3", "D3", "O3"), msm] <- with(vlist$cost_IDO3, pe[gender == "m" & risk == "msm"])

StateCostsHRU["T1", idu.m] <- with(vlist$cost_T1, pe[gender == "m" & risk == "pwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T2", idu.m] <- with(vlist$cost_T2, pe[gender == "m" & risk == "pwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T3", idu.m] <- with(vlist$cost_T3, pe[gender == "m" & risk == "pwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T1", midu]  <- with(vlist$cost_T1, pe[gender == "m" & risk == "mwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T2", midu]  <- with(vlist$cost_T2, pe[gender == "m" & risk == "mwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T3", midu]  <- with(vlist$cost_T3, pe[gender == "m" & risk == "mwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T1", idu.f] <- with(vlist$cost_T1, pe[gender == "f" & risk == "pwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T2", idu.f] <- with(vlist$cost_T2, pe[gender == "f" & risk == "pwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T3", idu.f] <- with(vlist$cost_T3, pe[gender == "f" & risk == "pwid"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T1", het.m] <- with(vlist$cost_T1, pe[gender == "m" & risk == "het"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T2", het.m] <- with(vlist$cost_T2, pe[gender == "m" & risk == "het"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T3", het.m] <- with(vlist$cost_T3, pe[gender == "m" & risk == "het"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T1", het.f] <- with(vlist$cost_T1, pe[gender == "f" & risk == "het"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T2", het.f] <- with(vlist$cost_T2, pe[gender == "f" & risk == "het"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T3", het.f] <- with(vlist$cost_T3, pe[gender == "f" & risk == "het"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T1", msm]   <- with(vlist$cost_T1, pe[gender == "m" & risk == "msm"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T2", msm]   <- with(vlist$cost_T2, pe[gender == "m" & risk == "msm"]) * (1 - vparameters$cost_art_prop)
StateCostsHRU["T3", msm]   <- with(vlist$cost_T3, pe[gender == "m" & risk == "msm"]) * (1 - vparameters$cost_art_prop)
#ART state cost
StateCostsART[c("T1", "T2", "T3"), ] <- StateCostsHRU[c("T1", "T2", "T3"), ] / (1 - vparameters$cost_art_prop) * vparameters$cost_art_prop
StateCostsOAT[ , oat] <- vparameters$cost_oat
StateCostsPrEP[c("Sp", "Iap", "Ip"), msm.h] <- vparameters$cost_prep

#regular test cost
StateCostsTests.psi["S1", ] <- vparameters$cost_test_sus
StateCostsTests.psi[c("Ia", "I1", "I2", "I3"), ] <- vparameters$cost_test_inf
#prep test cost
StateCostsTests.psi.p["Sp", msm.h] <- vparameters$cost_test_prep + vparameters$cost_test_sus
StateCostsTests.psi.p[c("Ip", "Iap"), msm.h] <- vparameters$cost_test_prep + vparameters$cost_test_inf
#ART initiation cost
StateCostsARTinit[c("I1", "I2", "I3", "D1", "D2", "D3", "O1", "O2", "O3"), ] <- vparameters$cost_art_init


#### Intervention costs ####
## TESTING ##
#ER Testing intervention#
IntCosts.test.optout.ER[c("S1", "Ia", "I1", "I2", "I3"), ]      <- with(vlist$int_cost_test_ER, pe[component == "unit-all"])
IntCosts.impl.test.optout.ER[c("S1", "Ia", "I1", "I2", "I3"), ] <- with(vlist$int_cost_test_ER, pe[component == "implementation"])
IntCosts.sust.test.optout.ER[c("S1", "Ia", "I1", "I2", "I3"), ] <- with(vlist$int_cost_test_ER, pe[component == "sustainment"])
IntCosts.impl.lump.test.optout.ER[c("S1"), 1] <- with(vlist$int_cost_test_ER, pe[component == "impl.lump"])
IntCosts.sust.lump.test.optout.ER[c("S1"), 1] <- with(vlist$int_cost_test_ER, pe[component == "sust.lump"])

#PC Testing intervention#
IntCosts.test.optout.PC[c("S1", "Ia", "I1", "I2", "I3"), ]      <- with(vlist$int_cost_test_PC, pe[component == "unit-all"])
IntCosts.impl.test.optout.PC[c("S1", "Ia", "I1", "I2", "I3"), ] <- with(vlist$int_cost_test_PC, pe[component == "implementation"])
IntCosts.sust.test.optout.PC[c("S1", "Ia", "I1", "I2", "I3"), ] <- with(vlist$int_cost_test_PC, pe[component == "sustainment"])
IntCosts.impl.lump.test.optout.PC[c("S1"), 1] <- with(vlist$int_cost_test_PC, pe[component == "impl.lump"])
IntCosts.sust.lump.test.optout.PC[c("S1"), 1] <- with(vlist$int_cost_test_PC, pe[component == "sust.lump"])

#EMR Testing intervention#
IntCosts.test.EMR[c("S1", "Ia", "I1", "I2", "I3"), ]      <- with(vlist$int_cost_test_EMR, pe[component == "unit-all"])
IntCosts.impl.test.EMR[c("S1", "Ia", "I1", "I2", "I3"), ] <- with(vlist$int_cost_test_EMR, pe[component == "implementation"])
IntCosts.sust.test.EMR[c("S1", "Ia", "I1", "I2", "I3"), ] <- with(vlist$int_cost_test_EMR, pe[component == "sustainment"])
IntCosts.impl.lump.test.EMR[c("S1"), 1] <- with(vlist$int_cost_test_EMR, pe[component == "impl.lump"])
IntCosts.sust.lump.test.EMR[c("S1"), 1] <- with(vlist$int_cost_test_EMR, pe[component == "sust.lump"])

#nurse Testing intervention#
IntCosts.test.nurse[c("S1", "Ia", "I1", "I2", "I3"), ]     <- with(vlist$int_cost_test_nurse, pe[component == "unit-all"])
IntCosts.impl.test.nurse[c("S1", "Ia", "I1", "I2", "I3"), ] <- with(vlist$int_cost_test_nurse, pe[component == "implementation"])
IntCosts.sust.test.nurse[c("S1", "Ia", "I1", "I2", "I3"), ] <- with(vlist$int_cost_test_nurse, pe[component == "sustainment"])
IntCosts.impl.lump.test.nurse[c("S1"), 1] <- with(vlist$int_cost_test_nurse, pe[component == "impl.lump"])
IntCosts.sust.lump.test.nurse[c("S1"), 1] <- with(vlist$int_cost_test_nurse, pe[component == "sust.lump"])

#OTP Testing intervention#
IntCosts.test.OAT[c("S1", "Ia", "I1", "I2", "I3"), oat]      <- with(vlist$int_cost_test_OAT, pe[component == "unit-all"])
IntCosts.impl.test.OAT[c("S1", "Ia", "I1", "I2", "I3"), oat] <- with(vlist$int_cost_test_OAT, pe[component == "implementation"])
IntCosts.sust.test.OAT[c("S1", "Ia", "I1", "I2", "I3"), oat] <- with(vlist$int_cost_test_OAT, pe[component == "sustainment"])

## ART ##
#Case management on initiation#
IntCosts.ARTinit[c("D1", "D2", "D3"), ] <- with(vlist$int_cost_art_ini, pe[component == "unit-all"])
IntCosts.impl.ARTinit[c("D1", "D2", "D3"), ] <- with(vlist$int_cost_art_ini, pe[component == "implementation"])
IntCosts.sust.ARTinit[c("D1", "D2", "D3"), ] <- with(vlist$int_cost_art_ini, pe[component == "sustainment"])

#Case management on dropout#
IntCosts.ARTreten[c("T1", "T2", "T3"), ] <- with(vlist$int_cost_art_ret, pe[component == "unit-all"])
IntCosts.impl.ARTreten[c("T1", "T2", "T3"), ] <- with(vlist$int_cost_art_ret, pe[component == "implementation"])
IntCosts.sust.ARTreten[c("T1", "T2", "T3"), ] <- with(vlist$int_cost_art_ret, pe[component == "sustainment"])

#Case management on dropout - targeted#
IntCosts.ARTreten.target["T3", ] <- with(vlist$int_cost_art_ret, pe[component == "unit-all"])
IntCosts.impl.ARTreten.target["T3", ] <- with(vlist$int_cost_art_ret, pe[component == "implementation"])
IntCosts.sust.ARTreten.target["T3", ] <- with(vlist$int_cost_art_ret, pe[component == "sustainment"])

#EMR prompt ART engagement#
IntCosts.ARTEMRprompt[c("T1", "T2", "T3"), ] <- with(vlist$int_cost_art_EMRprompt, pe[component == "unit-all"])
IntCosts.impl.ARTEMRprompt[c("T1", "T2", "T3"), ] <- with(vlist$int_cost_art_EMRprompt, pe[component == "implementation"])
IntCosts.sust.ARTEMRprompt[c("T1", "T2", "T3"), ] <- with(vlist$int_cost_art_EMRprompt, pe[component == "sustainment"])

#RAPID ART initiation#
IntCosts.immART[c("I1", "I2", "I3"), ] <- with(vlist$int_cost_art_immART, pe[component == "unit-all"])
IntCosts.impl.immART[c("I1", "I2", "I3"), ] <- with(vlist$int_cost_art_immART, pe[component == "implementation"])
IntCosts.sust.immART[c("I1", "I2", "I3"), ] <- with(vlist$int_cost_art_immART, pe[component == "sustainment"])

#ART re-initiation, personal contact#
IntCosts.reART[c("O1", "O2", "O3"), ] <- with(vlist$int_cost_reART, pe[component == "unit-all"])
IntCosts.impl.reART[c("O1", "O2", "O3"), ] <- with(vlist$int_cost_reART, pe[component == "implementation"])
IntCosts.sust.reART[c("O1", "O2", "O3"), ] <- with(vlist$int_cost_reART, pe[component == "sustainment"])

#ART re-linkage program#
IntCosts.relink[c("O1", "O2", "O3"), ] <- with(vlist$int_cost_relink, pe[component == "unit-all"])
IntCosts.impl.relink[c("O1", "O2", "O3"), ] <- with(vlist$int_cost_relink, pe[component == "implementation"])
IntCosts.impl.lump.relink[c("O1"), 1] <- with(vlist$int_cost_relink, pe[component == "impl.lump"])
IntCosts.sust.relink[c("O1", "O2", "O3"), ] <- with(vlist$int_cost_relink, pe[component == "sustainment"])

#SSP program#
IntCosts.SSP[ , all.idu] <- with(vlist$int_cost_ssp, pe[component == "unit-all"])
IntCosts.impl.SSP[ , all.idu] <- with(vlist$int_cost_ssp, pe[component == "implementation"])
IntCosts.sust.SSP[ , all.idu] <- with(vlist$int_cost_ssp, pe[component == "sustainment"])

#OAT - BUP#
IntCosts.BUP[ , off.oat] <- with(vlist$int_cost_BUP, pe[component == "unit-all"])
IntCosts.impl.BUP[ , off.oat] <- with(vlist$int_cost_BUP, pe[component == "implementation"])
IntCosts.sust.BUP[ , off.oat] <- with(vlist$int_cost_BUP, pe[component == "sustainment"])

#OAT - MET#
IntCosts.MET[ , off.oat] <- with(vlist$int_cost_MET, pe[component == "unit-all"])
IntCosts.impl.MET[ , off.oat] <- with(vlist$int_cost_MET, pe[component == "implementation"])
IntCosts.sust.MET[ , off.oat] <- with(vlist$int_cost_MET, pe[component == "sustainment"])

#PrEP program#
IntCosts.prep[c("S1", "S2"), msm.h] <- with(vlist$int_cost_prep, pe[component == "unit-all"])   #PREP enrty rates calculated over all susceptible, though transition only from S2
IntCosts.impl.prep[c("S1", "S2"), msm.h] <- with(vlist$int_cost_prep, pe[component == "implementation"])
IntCosts.sust.prep[c("S1", "S2"), msm.h] <- with(vlist$int_cost_prep, pe[component == "sustainment"])


#### Intervention effect ####
#ER Testing intervention#
IntEff.test.optout.ER <- with(vlist$int_test_ER, pe[component == "effectiveness"])[1]
IntScale.test.optout.ER <- set.scale.race.gender(vlist$int_test_ER, c("pe"), c("S1", "Ia", "I1", "I2", "I3"))

#PC Testing intervention#
IntEff.test.optout.PC <- with(vlist$int_test_PC, pe[component == "effectiveness"])[1]
IntScale.test.optout.PC <- set.scale.race.gender(vlist$int_test_PC, c("pe"), c("S1", "Ia", "I1", "I2", "I3"))

#EMR Testing intervention#
IntEff.test.EMR <- with(vlist$int_test_EMR, pe[component == "effectiveness"])[1]
IntScale.test.EMR <- set.scale.race.gender(vlist$int_test_EMR, c("pe"), c("S1", "Ia", "I1", "I2", "I3"))

#nurse Testing intervention#
IntEff.test.nurse <- with(vlist$int_test_nurse, pe[component == "effectiveness"])[1]
IntScale.test.nurse <- set.scale.race.gender(vlist$int_test_nurse, c("pe"), c("S1", "Ia", "I1", "I2", "I3"))

#OTP Testing intervention#
IntEff.test.OAT <- with(vlist$int_test_OAT, pe[component == "effectiveness"])[1]
IntScale.test.OAT <- set.scale.race.gender(vlist$int_test_OAT, c("pe"), c("S1", "Ia", "I1", "I2", "I3"), otp = TRUE)

#Case management on initiation#
IntEff.ARTinit <- with(vlist$int_art_ini, pe[component == "effectiveness"])[1]
IntScale.ARTinit <- set.scale.race.gender(vlist$int_art_ini, c("pe"), c("D1", "D2", "D3"))

#Case management on dropout#
IntEff.ARTreten <- with(vlist$int_art_ret, pe[component == "effectiveness"])[1]
IntScale.ARTreten <- set.scale.race.gender(vlist$int_art_ret, c("pe"), c("T1", "T2", "T3"))

#Case management on dropout - Targeted#
IntEff.ARTreten.target <- with(vlist$int_art_ret_tg, pe[component == "effectiveness"])[1]
IntScale.ARTreten.target <- set.scale.risk.race.gender(vlist$int_art_ret_tg, c("pe"), c("T3"))

#EMR prompt ART engagement#
IntEff.ARTEMRprompt <- with(vlist$int_art_EMRprompt, pe[component == "effectiveness"])[1]
IntScale.ARTEMRprompt <- set.scale.race.gender(vlist$int_art_EMRprompt, c("pe"), c("T1", "T2", "T3"))

#RAPID ART initiation#
IntEff.immART <- with(vlist$int_art_immART, pe[component == "effectiveness"])[1]
IntScale.immART <- set.scale.risk.race.gender(vlist$int_art_immART, c("pe"), c("I1", "I2", "I3"))

#ART re-initiation, personal contact#
IntEff.reART <- with(vlist$int_reART, pe[component == "effectiveness"])[1]
IntScale.reART <- set.scale.race.gender(vlist$int_reART, c("pe"), c("O1", "O2", "O3"))

#ART re-linkage#
IntEff.relink <- with(vlist$int_relink, pe[component == "effectiveness"])[1]
IntScale.relink <- set.scale.race.gender(vlist$int_relink, c("pe"), c("O1", "O2", "O3"))

#SSP program#
IntBaseline.SSP <- with(vlist$v.ssp, pe)
IntEff.SSP <- with(vlist$int_ssp, pe[component == "maximum"]) * with(vlist$int_ssp, pe[component == "pwid-2020"])
IntScale.SSP <- (1 + with(vlist$int_ssp, pe[component == "growth"]))^(1/12) - 1

#OAT - BUP#
IntBaseline.BUP <- vparameters$nBUP.m[4, ]
IntEff.BUP <- with(vlist$int_BUP, pe[component == "maximum"]) * with(vlist$int_BUP, pe[component == "pwid-2020"])[-1] * with(vlist$int_BUP, pe[component == "pwid-opioid"])
if(any(IntEff.BUP < nOAT.m[4,])){
  oat.pop <- c("Male White", "Male Black", "Male Hispanic", "Female White", "Female Black", "Female Hispanic")
  oat_warn<-which(IntEff.BUP < nOAT.m[4,])
  #print(paste0("Warning: baseline OAT scaleup in 2015 greater than maximum intervention level for: ", oat.pop[oat_warn], " PWID, thus replacing maximum level with baseline scaleup"))
  IntEff.BUP[oat_warn] <- nOAT.m[4, oat_warn]
}
IntScale.BUP <- (1 + with(vlist$int_BUP, pe[component == "growth"]))^(1/12) - 1

#OAT - MET#
IntBaseline.MET <- vparameters$nMET.m[4, ]
IntEff.MET <- with(vlist$int_MET, pe[component == "maximum"]) * with(vlist$int_MET, pe[component == "pwid-2020"])[-1] * with(vlist$int_MET, pe[component == "pwid-opioid"])
if(any(IntEff.MET < nOAT.m[4,])){
  oat.pop  <- c("Male White", "Male Black", "Male Hispanic", "Female White", "Female Black", "Female Hispanic")
  oat_warn <- which(IntEff.BUP < nOAT.m[4,])
  #print(paste0("Warning: baseline OAT scaleup in 2015 greater than maximum intervention level for: ", oat.pop[oat_warn], " PWID, thus replacing maximum level with baseline scaleup"))
  IntEff.MET[oat_warn] <- nOAT.m[4, oat_warn]
}
IntScale.MET <- (1 + with(vlist$int_MET, pe[component == "growth"]))^(1/12) - 1

#PrEP program#
IntBaseline.prep <- vparameters$prep.total[length(vparameters$prep.total)]
IntEff.prep <- with(vlist$int_prep, pe[component == "coverage"]) * with(vlist$int_prep, pe[component == "msm-2020"]) * with(vlist$int_prep, pe[component == "indicated"]) 
IntScale.prep <- (1 + with(vlist$int_prep, pe[component == "growth"]))^(1/12) - 1
#################################################################
####Generate grouping indicators for the 42 population groups####
#################################################################

m        = c(grep("MSM", names.gp), grep("/male", names.gp))  # males: 30 groups
f        = grep("female", names.gp)                           # female: 12 groups
white    = grep("white", names.gp)                            # white: 14 groups
black    = grep("black", names.gp)                            # black: 14 groups
hisp     = grep("hisp", names.gp)                             # hispanic: 14 groups
all.msm  = grep("MSM", names.gp)                              # All MSM, including MWID: 18 groups
all.idu  = grep("PWID", names.gp)                             # All PWID, including MWID: 24 groups
midu     = intersect(all.idu, all.msm)                        # MWID: 12 groups
oat      = grep("OAT", names.gp)                              # OAT: 12 groups
het      = grep("HET", names.gp)                              # HET: 12 groups
low      = grep("low", names.gp)                              # Low-risk: 15 groups, no PWID
high     = grep("high", names.gp)                             # High-risk: 15 groups, no PWID

msm.l    = intersect(all.msm, low)                            # low-risk MSM, including midu: 9 groups
msm.h    = setdiff(all.msm, msm.l)                            # high-risk MSM, including midu: 9 groups
het.l    = intersect(het, low)                                # low-risk HET: 6 groups
het.m.l  = intersect(het.l, m)                                # low-risk male HET: 6 groups
het.f.l  = intersect(het.l, f)                                # low-risk female HET: 6 groups

msm      = setdiff(all.msm, all.idu)                          # MSM only, excluding midu: 6 groups          
idu      = setdiff(all.idu, all.msm)                          # PWID only, excluding midu: 12 groups 
het.m    = intersect(het, m)                                  # HET male: 6 groups
het.f    = intersect(het, f)                                  # HET female: 6 groups
off.oat  = setdiff(all.idu, oat)                              # All PWID (including MWID) not on OAT: 12 groups

idu.m    = intersect(idu, m)                                  # male PWID, excluding MWID: 6 groups
idu.f    = intersect(idu, f)                                  # female PWID: 6 groups

msmL     = intersect(msm, low)                                # low-risk MSM (excluding midu): 3 groups
msmH     = intersect(msm, high)                               # high-risk MSM (excluding midu): 3 groups
miduL    = intersect(midu, low)                               # low-risk MWID: 6 groups
miduH    = intersect(midu, high)                              # high-risk MWID: 6 groups

het.mL   = intersect(het.m, low)                              # male HET, low-risk: 3 groups
het.mH   = intersect(het.m, high)                             # male HET, high-risk: 3 groups
het.fL   = intersect(het.f, low)                              # female HET, low-risk: 3 groups
het.fH   = intersect(het.f, high)                             # female HET, high-risk: 3 groups

## used in mixing matrix and balance, definition of low/high group (diff from sexual intensity level)
m.high   = setdiff(m, het.m.l)                                #high-risk male: Including PWID, high-risk MSM, high-risk MWID, high-risk HET
f.high   = setdiff(f, het.f.l)                                #high-risk female: Including PWID, high-risk HET



################################################################################
#### Functions to derive group number within different domains of parameter ####
################################################################################
pwid.gn.fun = function (names.pwid){
  female = grep("female",  names.pwid)
  male   = grep("female",  names.pwid, invert =T)
  white  = grep("white",   names.pwid)
  black  = grep("black",   names.pwid)
  hisp   = grep("hispanic",names.pwid)
  F.w    = intersect(female, white)
  F.b    = intersect(female, black)
  F.h    = intersect(female, hisp)
  M.w    = intersect(male, white)
  M.b    = intersect(male, black)
  M.h    = intersect(male, hisp)
  return(list(female=female, male=male, white=white, black=black, hisp=hisp, F.w=F.w, F.b=F.b, F.h=F.h, M.w=M.w, M.b=M.b, M.h=M.h))
}

msm.gn.fun = function (names.msm){
  white  = grep("white",   names.msm)
  black  = grep("black",   names.msm)
  hisp   = grep("hispanic",names.msm)
  high   = grep("high",    names.msm)
  low    = grep("low",     names.msm)
  H.w    = intersect(high, white)
  H.b    = intersect(high, black)
  H.h    = intersect(high, hisp)
  L.w    = intersect(low, white)
  L.b    = intersect(low, black)
  L.h    = intersect(low, hisp)
  return(list(white=white, black=black, hisp=hisp, high=high, low=low, H.w=H.w, H.b=H.b, H.h=H.h, L.w=L.w, L.b=L.b, L.h=L.h))
}

gp18.gn.fun = function (names.18){
  white    = grep("white",   names.18)
  black    = grep("black",   names.18)
  hisp     = grep("hispanic",names.18)
  female   = grep("female",  names.18)
  male     = grep("female",  names.18, invert =T)
  all.msm  = grep("MSM",     names.18)
  all.pwid = grep("PWID",    names.18)
  mwid     = grep("MSM/PWID",names.18)
  het      = grep("HET",     names.18)
  het.m    = intersect(het, male)
  het.f    = intersect(het, female)
  msm      = setdiff(all.msm, all.pwid)
  pwid     = setdiff(all.pwid, all.msm)
  return(list(white=white, black=black, hisp=hisp, female=female, male=male, all.msm=all.msm, all.pwid=all.pwid, msm=msm, pwid=pwid, mwid=mwid, het=het, het.m=het.m, het.f=het.f))
}

gp.gn.fun = function (names.gp){
  white    = grep("white",   names.gp)
  black    = grep("black",   names.gp)
  hisp     = grep("hispanic",names.gp)
  female   = grep("female",  names.gp)
  male     = grep("female",  names.gp, invert =T)
  low      = grep("low",     names.gp)
  high     = grep("high",    names.gp)
  all.msm  = grep("MSM",     names.gp)
  all.pwid = grep("PWID",    names.gp)
  msm      = setdiff(all.msm, all.pwid)
  pwid     = setdiff(all.pwid, all.msm)
  mwid     = grep("MSM/PWID",names.gp)
  het      = grep("HET",     names.gp)
  OAT      = grep("OAT",     names.gp)
  H.allmsm = intersect(high, all.msm)
  L.allmsm = intersect(low,  all.msm)
  het.m    = intersect(het, male)
  het.f    = intersect(het, female)
  pwid.m   = intersect(pwid, male)
  pwid.f   = intersect(pwid, female)
  H.het.m  = intersect(high, het.m)
  H.het.f  = intersect(high, het.f)
  L.het.m  = intersect(low, het.m)
  L.het.f  = intersect(low, het.f)
  H.m      = intersect(high, male)
  H.f      = intersect(high, female)
  L.m      = intersect(low, male)
  L.f      = intersect(low, female)
  return(list(white=white, black=black, hisp=hisp, female=female, male=male, 
              low=low, high=high, all.msm=all.msm, all.pwid=all.pwid, 
              msm=msm, pwid=pwid, mwid=mwid, het=het, het.m=het.m, het.f=het.f,
              pwid.m=pwid.m, pwid.f=pwid.f, H.allmsm=H.allmsm, L.allmsm=L.allmsm, 
              H.het.m=H.het.m, H.het.f=H.het.f, L.het.m=L.het.m, L.het.f=L.het.f,
              H.m=H.m, H.f=H.f, L.m=L.m,  L.f=L.f, OAT=OAT))
}
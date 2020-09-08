#' Force of infection equations
#'
#' \code{FoI} combines parameters related to HIV transmission via needle sharing, heterosexual sex and homosexual sex
#'
#' @param y number of individuals in each compartment (n.gp*19 matrix)
#' @param no number of opposite sexual partners for each compartment (n.gp*19 matrix)
#' @param uoC condom use adjustment term for opposite sex (n.gp*19 matrix)
#' @param ns number of same sexual partners for each compartment (18*19 matrix)
#' @param usC condom use adjustment term for same sex (18*19 matrix)
#' @param eO assortative coefficient for heterosexual mixing between ethnic groups (vector of 3 for white, black, hispanic)
#' @param eS assortative coefficient for homosexual mixing between ethnic groups   (vector of 3 for white, black, hispanic)
#' @param sigmaFM probability of transmission by F->M for 16 HIV+ states
#' @param sigmaMF probability of transmission by M->F for 16 HIV+ states
#' @param simgaM probability of transmission by same sex for 16 HIV+ states
#' @param tau probability of transmission by injection for 16 HIV+ states
#' @param eff.prep % reduction in risk of infection for PrEP
#' @param d number of injection
#' @param s proportion of injections that are shared (9 groups)
#' @param eff.oat % reduction in # of shared injections reduced due to OAT
#' @param cov.ssp coverage of syringe services program
#' @param outputs matrix of dim c(n.gp,2) with row of each group. second column for PrEP
#'
#' @return
#' Sufficient contact rate (sum of transmission via needle sharing, opposite and sex same contacts)
#' @export

# sufficient contact rate
FoI = function(y, no, uoC, ns, usC, eO,eS, sigmaFM, sigmaMF, sigmaM, tau, eff.prep, d, s,
               eff.oat, cov.ssp, eff.ssp=eff.ssp, s.multi, bal=F){

  #### indicator for gender, ethnicity, risk groups ####
  names.gp = row.names(y)
  n.gp     = length(names.gp)

  # source("ModelCoreModules/CascadeCEA-Model-0-Group.number.R")

  # set the max e
  eO[eO>1]=1; eS[eS>1]=1;

  # total number of sexual partners
  xnSum=function(e, ind, n, uC){
    sum((1-e) * y[ind, ] * n[ind, ] * uC[ind, ])
  }

#'Mixing matrix
#'
#' \code{mixMatrix} calculates assortative sexual mixing matrix
#'
#' @param ind index for groups of people to be mixed 
#' @param ind.low index for groups of people at low risk of HIV transmission
#' @param ass.e Assortativity coefficient
#' @param n Number of contacts
#' @param uC condom use matrix
#' @param indM indicator of male partner
#' @param indF indicator of female partner
#'
#' @return
#' Complete mixing matrix
#' @export

  mixMatrix = function(ind, ind.low, ass.e, n, uC, indM = F, indF = F){
    if (indM == T){
      ass.e_pt = c(rep(ass.e[4:6], 8), ass.e)   #assortativeness for male partner
    } else if (indF == T){
      ass.e_pt = c(ass.e[4:6], ass.e[4:6], ass.e)                #assortativeness for female partner
    } else {
      ass.e_pt = ass.e
    }
    xn.total  = xnSum(e = ass.e_pt, ind, n, uC)
    # numer: number of sexual parnership for specific group
    xn.undiag = rowSums(y[ind, 1:9]) * n[ind, 1] * uC[ind, 1] #sus & undiagosed
    xn.diag   = rowSums(y[ind, 10:19])*n[ind, 10]* uC[ind, 10] #diagnosed
    xn.j = matrix((1-ass.e_pt) * c(xn.undiag, xn.diag), 6, length(ind)*2, byrow=T)

    l.loc = match(ind.low, ind) #indicates location of het.low among ind
    h.loc = setdiff(1:length(ind), l.loc)

    high.prop = xn.j[1:3, c(-l.loc, -(length(ind)+l.loc))] / rowSums(xn.j[1:3, c(-l.loc, -(length(ind)+l.loc))])
    low.prop  = xn.j[4:6, c(-h.loc, -(length(ind)+h.loc))] / rowSums(xn.j[4:6, c(-h.loc, -(length(ind)+h.loc))])

    xn.j[1:3, c(-l.loc, -(length(ind)+l.loc))] = 0
    xn.j[4:6, c(-h.loc, -(length(ind)+h.loc))] = 0

    # random mixing
    r.mix = xn.j/rowSums(xn.j)
    if (xn.total==0) r.mix=0
    # xn.j without ass.e
    xn.j.wo.e = matrix(c(xn.undiag, xn.diag), 6, length(ind)*2, byrow=T)
    # several diagnonal matrices, horizontally aligned
    # low risk
    k.delta.l = matrix(0, 3, length(ind))

    k.delta.l[ ,l.loc] = diag(1, 3, 3)
    k.delta.l = cbind(k.delta.l, k.delta.l)
    # high risk
    k.delta.h = matrix(0, 3, length(ind))

    k.delta.h[ ,h.loc] = diag(1, 3, 3)
    k.delta.h = cbind(k.delta.h, k.delta.h)
    # combine low/high
    k.delta   = rbind(k.delta.l, k.delta.h) # 6 x length(ind)*2
    # among the same ethnicity, mixing proportion depends on the number of partners
    k.delta.prop = (xn.j.wo.e * k.delta) / rowSums(xn.j.wo.e * k.delta)

    mmr = ass.e * k.delta.prop + (1-ass.e) * r.mix
    mmr[1:3, c(-l.loc, -(length(ind)+l.loc))] = 0.01*high.prop
    mmr[4:6, c(-h.loc, -(length(ind)+h.loc))] = 0.01*low.prop
    mmr = mmr / rowSums(mmr)
    return(mmr)

  }

  # mij for female (3 rows; length(m)*2 columns)
  mf = mixMatrix(m,       het.m.l, eO, no, uoC, indM = T)
  #ind = m; ind.low = het.m.l; ass.e = eO; n = no; uC = uoC; indM = T
  #assuming low-risk MSM & mwid as a subgroup of low-risk het, others as high

  # mij for male opposite sex (3 rows; length(f)*2 columns)
  mO = mixMatrix(f,       het.f.l, eO, no, uoC, indF = T)

  # mij for male same sex (3 rows; length(all.msm)*2 columns)
  mS = mixMatrix(all.msm, msm.l,   eS, ns, usC)
  #ind = all.msm; ind.low = msm.l; ass.e = eS; n = ns; uC = usC; indM = F; indF = F
  ########### balance the sexual partnership ##########
  if (bal==T) {
    # number of total sexual parnters for female (X*n*mij),
    # stratified by ethnicity and risk behaviour (HET low vs. others)
    xnmf=matrix(0, 6, 6)
    xnmf.tot=numeric(6)
    # total partners for female het low
    xnmf.tot[1] = xnSum(0, intersect(white, het.f.l), no, uoC) # partners for white females
    xnmf.tot[2] = xnSum(0, intersect(black, het.f.l), no, uoC) # partners for black females
    xnmf.tot[3] = xnSum(0, intersect(hisp,  het.f.l), no, uoC) # partners for hisp females
    # total partners for female others
    xnmf.tot[4] = xnSum(0, intersect(white, f.high),  no, uoC) # partners for white females
    xnmf.tot[5] = xnSum(0, intersect(black, f.high),  no, uoC) # partners for black females
    xnmf.tot[6] = xnSum(0, intersect(hisp,  f.high),  no, uoC) # partners for hisp females

    # number of partners by ethnicity and risk
    l.loc.m=(rep(m,2) %in% het.m.l) #true if het male low, including low-risk msm
    np.l=sum(l.loc.m)/3
    other.loc.m=(rep(m,2) %in% m.high) #true if male others
    np.h=sum(other.loc.m)/3
    for (i in 1:6){
      xnmf[i,1] = sum(mf[i, l.loc.m][(1:np.l)*3-2]     * xnmf.tot[i]) # white het male low partners
      xnmf[i,2] = sum(mf[i, l.loc.m][(1:np.l)*3-1]     * xnmf.tot[i]) # black het male low partners
      xnmf[i,3] = sum(mf[i, l.loc.m][(1:np.l)*3]       * xnmf.tot[i]) # hisp het male low partners
      xnmf[i,4] = sum(mf[i, other.loc.m][(1:np.h)*3-2] * xnmf.tot[i]) # white male other partners
      xnmf[i,5] = sum(mf[i, other.loc.m][(1:np.h)*3-1] * xnmf.tot[i]) # black male otherpartners
      xnmf[i,6] = sum(mf[i, other.loc.m][(1:np.h)*3]   * xnmf.tot[i]) # hisp male other partners
    }

    # number of total sexual parnters for male,
    # stratified by ethnic groups (X*n*mij)
    xnmm=matrix(0, 6, 6)
    xnmm.tot=numeric(6)
    # total partners for male het low
    xnmm.tot[1] = xnSum(0, intersect(white, het.m.l), no, uoC) # partners for white males
    xnmm.tot[2] = xnSum(0, intersect(black, het.m.l), no, uoC) # partners for black males
    xnmm.tot[3] = xnSum(0, intersect(hisp,  het.m.l), no, uoC) # partners for hisp males
    # total partners for male others
    xnmm.tot[4] = xnSum(0, intersect(white, m.high),  no, uoC) # partners for white males
    xnmm.tot[5] = xnSum(0, intersect(black, m.high),  no, uoC) # partners for black males
    xnmm.tot[6] = xnSum(0, intersect(hisp,  m.high),  no, uoC) # partners for hisp males
    # number of partners by ethnicity and risk
    l.loc.f = (rep(f,2) %in% het.f.l) #true if het female low
    np.l = sum(l.loc.f)/3
    other.loc.f = (rep(f,2) %in% f.high) #true if female others
    np.h = sum(other.loc.f)/3

    for (i in 1:6){
      xnmm[i,1] = sum(mO[i, l.loc.f][(1:np.l)*3-2]     * xnmm.tot[i]) # white female partners
      xnmm[i,2] = sum(mO[i, l.loc.f][(1:np.l)*3-1]     * xnmm.tot[i]) # black female partners
      xnmm[i,3] = sum(mO[i, l.loc.f][(1:np.l)*3]       * xnmm.tot[i]) # hisp female partners
      xnmm[i,4] = sum(mO[i, other.loc.f][(1:np.h)*3-2] * xnmm.tot[i]) # white female partners
      xnmm[i,5] = sum(mO[i, other.loc.f][(1:np.h)*3-1] * xnmm.tot[i]) # black female partners
      xnmm[i,6] = sum(mO[i, other.loc.f][(1:np.h)*3]   * xnmm.tot[i]) # hisp female partners
    }

    # imbalance
    v=xnmm/t(xnmf)
    v[is.na(v)]=0 #for fully assortative mixing
    # degree of compromise between the two sexes.
    theta=0.5  #set to 0.5 to be equal between the two groups

    # adjust the imbalance
    imb.adj.m = matrix(0, 6, length(m)*2)
    imb.adj.m[,l.loc.m]     = v[ ,1:3]^theta
    imb.adj.m[,other.loc.m] = v[ ,4:6]^theta
    mf = mf * imb.adj.m

    imb.adj.f = matrix(0, 6, length(f)*2)
    imb.adj.f[,l.loc.f]     = v[ ,1:3]^(theta-1)
    imb.adj.f[,other.loc.f] = v[ ,4:6]^(theta-1)
    imb.adj.f[mapply(is.infinite, imb.adj.f)] <- 0
    mO = mO * imb.adj.f
  }


  # sum of sufficient contact rates among susceptible i with HIV-infected j
  betaI = function(i, ind, mij, sigma, homo=F){
    # denominator of selecting a partner for undiag/diag => length=length(ind)*2
    den_beta = c(rowSums(y[ind, 1:9]), rowSums(y[ind, 10:19])) #all population
    c.prob   = mij/den_beta
    # make the c.prob into matrix of dim=c(length(ind),16)
    c.prob2  = matrix(c(rep(c.prob[1:length(ind)], 6), #undiagnosed
                      rep(c.prob[(length(ind)+1):(length(ind)*2)], 10)), #diagnosed
                      length(ind), 16)
    # probability selecting j compartment
    prob = y[ind, 4:19]*c.prob2
    # not on PreP
    beta = -log(1- prob * rep(sigma, each =length(ind)))
    # if on PreP, the transmission prob. will be reduced by eff.prep
    betaP= -log(1- prob * rep(sigma*(1-eff.prep), each =length(ind)))
    if (homo == T) {
      nuC =ns[i,1]*usC[i,1]
    }  else {
      nuC = no[i,1]*uoC[i,1]
    }
    return(c (nuC * sum(beta),     #no PrEP
              nuC * sum(betaP)))   #   PrEP
  }

  ####### sufficient contact rate for heterosex #######
  beta_o = matrix(0,n.gp,2)

  for (i in m) {#heterosexual for male susceptible
    if (i %in% het.l){
      if(i%in%white) beta_o[i, ] = betaI(i, f, mO[1, ], sigmaFM)
      if(i%in%black) beta_o[i, ] = betaI(i, f, mO[2, ], sigmaFM)
      if(i%in%hisp)  beta_o[i, ] = betaI(i, f, mO[3, ], sigmaFM)
    }
    else {
      if(i%in%white) beta_o[i, ] = betaI(i, f, mO[4, ], sigmaFM)
      if(i%in%black) beta_o[i, ] = betaI(i, f, mO[5, ], sigmaFM)
      if(i%in%hisp)  beta_o[i, ] = betaI(i, f, mO[6, ], sigmaFM)
    }
  }

  for (i in f) {#heterosexual for female susceptible
    if (i %in% het.l){
      if(i%in%white) beta_o[i, ] = betaI(i, m, mf[1, ], sigmaMF)
      if(i%in%black) beta_o[i, ] = betaI(i, m, mf[2, ], sigmaMF)
      if(i%in%hisp)  beta_o[i, ] = betaI(i, m, mf[3, ], sigmaMF)
    }
    else {
      if(i%in%white) beta_o[i, ] = betaI(i, m, mf[4, ], sigmaMF)
      if(i%in%black) beta_o[i, ] = betaI(i, m, mf[5, ], sigmaMF)
      if(i%in%hisp)  beta_o[i, ] = betaI(i, m, mf[6, ], sigmaMF)
    }
  }


  ###### sufficient contact rate for homosex #######
  beta_s = matrix(0, n.gp, 2)

  for (i in all.msm) {
    if (i %in% msm.l){
      if(i%in%white) beta_s[i, ] = betaI(i, all.msm, mS[1, ], sigmaM, homo=T)
      if(i%in%black) beta_s[i, ] = betaI(i, all.msm, mS[2, ], sigmaM, homo=T)
      if(i%in%hisp)  beta_s[i, ] = betaI(i, all.msm, mS[3, ], sigmaM, homo=T)
    }
    else {
      if(i%in%white) beta_s[i, ] = betaI(i, all.msm, mS[4, ], sigmaM, homo=T)
      if(i%in%black) beta_s[i, ] = betaI(i, all.msm, mS[5, ], sigmaM, homo=T)
      if(i%in%hisp)  beta_s[i, ] = betaI(i, all.msm, mS[6, ], sigmaM, homo=T)
    }

  }

  ###### sufficient contact rate for shared needle injection #######
  gamma  = matrix(0, n.gp, 2)
  ds.all = numeric(n.gp); # 0 if not PWID

  # PWID not on OAT
  ind = setdiff(all.idu, oat)
  ds  = d*s
  ds.all[ind] = ds[ind]* (1-cov.ssp[ind]*(1-eff.ssp))
  # PWID on OAT
  ds.all[oat] = ds[oat]* (1-eff.oat)* (1-cov.ssp[oat]*(1-eff.ssp))

  # if diagnosed, sharing prob gets decreased by s.multi
  ds.all2 = matrix(ds.all, n.gp, 19)
  ds.all2[ ,10:19] = ds.all*(1- s.multi)
  den_gamma = sum(y*ds.all2) #denominator of selecting a partner
  # not on PreP
  gamma.sum   = sum(-log (1- (y[ ,-(1:3)]*ds.all2[ ,-(1:3)]/den_gamma) * rep(tau, each=n.gp)))
  # on PreP
  gamma.sum.p = sum(-log (1- (y[ ,-(1:3)]*ds.all2[ ,-(1:3)]/den_gamma) * rep(tau, each=n.gp)*(1-eff.prep)))
  if (den_gamma == 0) {
    gamma.sum   = 0
    gamma.sum.p = 0
  }
  gamma = cbind(ds.all * gamma.sum,     #no PrEP
                ds.all * gamma.sum.p)   #   PrEP

  ##### overall sufficient contact rate #####
  lambda = gamma + beta_o + beta_s
  #lambda[-all.msm,2]=0
  return(list(lambda = lambda, gamma = gamma, beta_o = beta_o, beta_s = beta_s))
}

# updated on Oct 31, 2017
# assortative mixing on low/high and ethnicity
#
# updated on Oct 3, 2017
# - HIV transmission was reduced due to CCR5 mutation in susceptiable population
#
# updated on Sep 18, 2017
# - mixing matrix was updated to differenciate the number of sexual partners
#   within an ethnic group. Pevious version has a 3x3 matrix for the mixing matrix mij.
# - The current version has (3 rows for 3 ethnic groups):
#   - a 3x18 matrix for male hetero [9 female groups *2 (sus+diag/not diag)]
#   - a 3x54 matrix for female [27 male groups *2 (sus+diag/not diag)]
#   - a 3x36 matrix for msm homo [(18 msm/msm-pwid groups * 2 (sus+diag/not diag)]
# - updated to balance the number of partnerships between males and females

# updated on May 30, 2017
# - effect of SSP (syringe services program) added
# - transmission prob differ by different genders for heterosex

# updated on May 4, 2017
# - preferred mixing given an assortatitive coefficient (eO or eS)
#   for the same ethnic group
# - the other propotion (1-eO or 1-eS) will have a proportional mixing.
# - number of sexual partnership was balanced by ethnic groups.
# - beta= n*mij*sigma*X/sumX

#' Specify ODE equations for heterosexuals and MSM
#'
#' \code{ode.list} Calculate the change of population in cells of heterosexual and male homosexual population by the ODE model. 
#'
#' @param x a vector containing the number of individuals in each HIV state x = c(S1,S2,Sp,Ia,I1,I2,I3,Iap,Ip,Da,D1,D2,D3,T1,T2,T3,O1,O2,O3)
#' @param lambda the sufficient contact rate
#' @param lambda.p the sufficient contact rate for those on PrEP.
#' @param rho the entry rate (assume the same for all groups)
#' @param 1/ws average duration uninfected individuals in compartment S remain identified after screening
#' @param 1/wp average duration uninfected individuals in compartment Sp remain on PrEP
#' @param mu_mat maturation rates (age>65)
#' @param mo mortality rates for 19 states
#' @param psi screening rates for susceptible or infected
#' @param psi.p screening rates for Iap (acute HIV on PrEP)
#' @param theta.ai transition rate from acute states (Ia) to chronic state (CD4>=500; I1)
#' @param theta.ad transition rate from acute states (Da) to chronic state (CD4>=500; D1)
#' @param phi % infected of people receiving ART once diagnosed
#' @param v2,v3 symptom-based case finding rate for I2 and I3, respectively
#' @param alpha ART initiation rate for D1,D2, and D3
#' @param alpha.re ART re-initiation rate for o1,O2, and O3
#' @param theta.1 HIV disease progression rate for those not on ART, from I1/D1 to I2/D2
#' @param theta.2 HIV disease progression rate for those not on ART, from I2/D2 to I3/D3
#' @param theta.t transition probabilities for those on ART
#' @param theta.o ART dropout probability from states T1,T2,T3
#' @param eta PrEP entry rate
#'
#' @return
#' HIV diagnoses, incidence and deaths
#' @export

ode.list = function (x, beta_o, beta_s, gamma, rho, ws, wp, mu_mat, mo, eta,
                     psi, psi.p, theta.ai, theta.ad, phi, v2, v3,
                     alpha, alpha.re, theta.1, theta.2, theta.t12, theta.t13,
                     theta.t21, theta.t23, theta.t31, theta.t32,
                     theta.t1O, theta.t2O, theta.t3O, rho.m)  {

S1=x[1];  S2=x[2];  Sp=x[3];
Ia=x[4];  I1=x[5];  I2=x[6];  I3=x[7];  Iap=x[8]; Ip=x[9];
Da=x[10]; D1=x[11]; D2=x[12]; D3=x[13];
T1=x[14]; T2=x[15]; T3=x[16]; O1=x[17]; O2=x[18]; O3=x[19]

alpha1=alpha[1]; alpha2=alpha[2]; alpha3=alpha[3]
alpha.re1 = alpha.re2 = alpha.re3 = alpha.re

lambda   = sum(beta_o[1], beta_s[1], gamma[1])
lambda.p = sum(beta_o[2], beta_s[2], gamma[2])

### Maturation is only for PLHIV with the exception of Ia & Iap ###
dS1 = rho*sum(x) - psi*S1 + ws*S2 - lambda*S1                - mo[1]*S1  - rho.m*sum(x[4:19]) - eta*S1
dS2 = psi*S1  + wp*Sp     - ws*S2 - lambda*S2                - mo[2]*S2  - eta*(S2)
dSp =         - wp*Sp             - lambda.p*Sp              - mo[3]*Sp  + eta*(S2+S1)

dIa = lambda*(S1+S2) - psi*Ia      - theta.ai*Ia             - mo[4]*Ia  + rho.m*Ia
dI1 = theta.ai*Ia    - psi*I1      - theta.1*I1  - mu_mat*I1 - mo[5]*I1  + rho.m*I1
dI2 = theta.1*I1     - (psi+v2)*I2 - theta.2*I2  - mu_mat*I2 - mo[6]*I2  + rho.m*I2
dI3 = theta.2*I2     - (psi+v3)*I3               - mu_mat*I3 - mo[7]*I3  + rho.m*I3
dIap= lambda.p*Sp    - psi.p*Iap   - theta.ai*Iap            - mo[8]*Iap + rho.m*Iap
dIp = theta.ai*Iap   - psi.p*Ip                  - mu_mat*Ip - mo[9]*Ip  + rho.m*Ip

dDa = psi*Ia      + psi.p*Iap                    - theta.ad*Da                        - mo[10]*Da + rho.m*Da
dD1 = theta.ad*Da + psi*(1-phi[1])*I1 + psi.p*Ip - theta.1*D1 - alpha1*D1 - mu_mat*D1 - mo[11]*D1 + rho.m*D1
dD2 = theta.1*D1  + (psi+v2)*(1-phi[2])*I2       - theta.2*D2 - alpha2*D2 - mu_mat*D2 - mo[12]*D2 + rho.m*D2
dD3 = theta.2*D2  + (psi+v3)*(1-phi[3])*I3                    - alpha3*D3 - mu_mat*D3 - mo[13]*D3 + rho.m*D3

dT1 = theta.t21*T2 + theta.t31*T3 + alpha1*D1 + alpha.re1*O1 + psi*phi[1]*I1 -
      (theta.t12 + theta.t13 + theta.t1O)*T1 - mu_mat*T1 - mo[14]*T1 + rho.m*T1
dT2 = theta.t12*T1 + theta.t32*T3 + alpha2*D2 + alpha.re2*O2 + (psi+v2)*phi[2]*I2-
      (theta.t21 + theta.t23 + theta.t2O)*T2 - mu_mat*T2 - mo[15]*T2 + rho.m*T2
dT3 = theta.t13*T1 + theta.t23*T2 + alpha3*D3 + alpha.re3*O3 + (psi+v3)*phi[3]*I3-
      (theta.t31 + theta.t32 +theta.t3O)*T3  - mu_mat*T3 - mo[16]*T3 + rho.m*T3

dO1 = theta.t1O*T1  - theta.1*O1 - alpha.re1*O1            -mu_mat*O1 - mo[17]*O1 + rho.m*O1
dO2 = theta.t2O*T2  + theta.1*O1 - (alpha.re2 +theta.2)*O2 -mu_mat*O2 - mo[18]*O2 + rho.m*O2
dO3 = theta.t3O*T3  + theta.2*O2 - alpha.re3*O3            -mu_mat*O3 - mo[19]*O3 + rho.m*O3

#incidence
inc_bo = beta_o[1]*(S1+S2) + beta_o[2]*Sp
inc_bs = beta_s[1]*(S1+S2) + beta_s[2]*Sp
inc_g  = gamma[1]*(S1+S2)  + gamma[2]*Sp

#new diagnosis
diagm  = psi*(Ia+I1+I2+I3) + psi.p*(Iap+Ip) + v2*I2 + v3*I3

# mortality
deathm = sum(mo[10:19]*x[10:19])

out = c(dS1, dS2, dSp, dIa, dI1, dI2, dI3, dIap, dIp, dDa, dD1, dD2, dD3, dT1, dT2, dT3, dO1, dO2, dO3,
        inc_bo, inc_bs, inc_g, diagm, deathm)
# the output should be the same order as the initial values

#### adjustment for negative cell
out[1:19][which((out[1:19]+x[1:19]) < 0)] = -x[1:19][which((out[1:19]+x[1:19]) < 0)]

out
}

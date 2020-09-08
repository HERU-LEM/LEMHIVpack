#############################################################################################
## Cascade CEA Model - Combination Interventions
## Plot production functions in a 6-panel graph
## Last updated: March 11, 2020
############################################################################################
rm(list=ls())
library(rstudioapi)
library(Hmisc)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

CITY.list <- c("ATL", "BAL", "LA", "MIA", "NYC", "SEA")
CITY.name <- c("(A) Atlanta", " (B) Baltimore", "(C) Los Angeles", "(D) Miami", "(E) New York City", "(F) Seattle")

par(mfrow = c(3, 2))
par(mar=c(3.8,3.8,1,1), oma = c(1, 1, 1, 1))

for (cc in 1:6){
  CITY <- CITY.list[cc]

  source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

  combination.list <- readRDS("Combination/Combination.list.rds")

  frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
  ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis

  outcome.comb     <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
  outcome.ref      <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))

  frontier.matrix  <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[frontier , c("QALYs.sum", "costs.total.sum")])
  Strategy.ind     <- c(0, frontier)
  frontier.matrix  <- cbind(Strategy.ind, frontier.matrix)

  costsCol <- 3; qalyCol <- 2

  #frontier matrix, first row is reference case
  frontier.matrix[ , qalyCol]  <- frontier.matrix[ , qalyCol] - frontier.matrix[1, qalyCol]
  frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] - frontier.matrix[1, costsCol]

  frontier.matrix  <- frontier.matrix[-1, ]  #remove the reference case

  frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] / 1000000

  #matrix for other dominated strategies
  other_comb          <- outcome.comb[-frontier , c("QALYs.sum", "costs.total.sum")]
  other_comb.mx       <- other_comb
  other_comb.mx[ , 1] <- other_comb.mx[ , 1] - outcome.ref["QALYs.sum"]
  other_comb.mx[ , 2] <- other_comb.mx[ , 2] - outcome.ref["costs.total.sum"]
  other_comb.mx[ , 2] <- other_comb.mx[ , 2] / 1000000

  xlim <- c(min(frontier.matrix[ , costsCol]) *1.2, max(other_comb.mx[ , 2]* 1.02))
  ylim <- c(0 , max(frontier.matrix[ , qalyCol])*1.25)
  plot(frontier.matrix[ , costsCol],  frontier.matrix[ , qalyCol], col = adjustcolor("dodgerblue", alpha = 0.8) , pch = 16, xlab="Incremental cost, versus statuo quo (US$, million)", ylab="Incremental QALYs, versus status quo", xlim=xlim, ylim=ylim, cex=1.2, cex.axis=0.95)
  points(other_comb.mx[,2], other_comb.mx[,1], col = adjustcolor("lightgrey", alpha = 0.5), pch=16, cex = 0.5)
  points(frontier.matrix[ , costsCol],  frontier.matrix[ , qalyCol], col = adjustcolor("dodgerblue", alpha = 0.8) , pch = 16)
  lines(frontier.matrix[ , costsCol], frontier.matrix[ , qalyCol], col = adjustcolor("dodgerblue", alpha = 0.8), lwd=2)
  points(frontier.matrix[which(frontier == ocis) , costsCol],  frontier.matrix[which(frontier == ocis) , qalyCol], col = adjustcolor("firebrick2", alpha = 1) , cex = 1.25, pch = 16)
  abline(v = 0, lty=3, col = "grey")
  title(CITY.name[cc], adj = 0.02, line = - 1)
  minor.tick(nx=2, ny=2)
  legend(x="bottomright",
         legend=c(paste0("OCIS (", CITY, ")"), "Strategies on PF","Other strategies"),
         col=c("dodgerblue","firebrick2", "lightgrey"),
         lwd=c(1.2, 1, 0.5), lty = c(1, NA, NA), pch=c(16, 16,16), pt.cex = c(1,1.1,1), cex = 0.8, bty = "n")
}

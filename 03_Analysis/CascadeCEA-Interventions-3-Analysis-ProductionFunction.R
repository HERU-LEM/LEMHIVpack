####################################################################
####### PART 1: constructing the health production function ########
####################################################################
# Name: getFrontier.R
# Goal: Find the CEA frontier, up to a given WTP level, by
#       identifying strategies with the highest NMB
# Reference: An Efficient, Noniterative Method of Identifying the Cost-Effectiveness Frontier, Sze-chuan Suen, Jeremy D. Goldhaber-Fiebert
# Modifier: Xiao Zang
# Latest update: Feb 20, 2020
# Notes:
#    ~User can specify the maximum willingness-to-pay level to
#      consider (maxWTP).  Can be Inf for infinity.
#
#    ~QALY-reducing strategies will be on the frontier if they save
#      enough money (script assumes maximum willingness to save money
#      per QALY lost is equivalent to maximum willingness to pay per QALY
#      gained). If the user does not wish to consider such policies as
#      being on the frontier, do not include strategies with negative
#      QALYs in the input csv file.
#
#    ~Script does not use the one-line code cited in the text
#      as the max function is slow. This implementation is
#      faster and methodologically does the same thing.
#
#    ~May take a few minutes if thousands of strategies and
#       processing resources are low.  Please be patient.


rm(list=ls())
library(rstudioapi)
library(tictoc)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
all.cities <- c("ATL", "BAL", "LA", "MIA", "NYC", "SEA")
maxWTP <- 200000        # any positive value or Inf

# SELECT city ##
CITY <- select.list(all.cities, multiple = FALSE,
                    title = 'Select city', graphics = FALSE)
# ww <- 1; CITY <- all.cities[[ww]] # Otherwise you can set city by its index

####################################################################
####################################################################

# load the costs and QALY values for all strategies
outcome.comb <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref  <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))

CEmat                          <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[ , c("QALYs.sum", "costs.total.sum")])
rownames(CEmat)[1]             <- "Ref"
rownames(CEmat)[2:nrow(CEmat)] <- c(1:(nrow(CEmat)-1))
Strategy.ind                   <- 1:dim(CEmat)[1]
CEmat                          <- cbind(Strategy.ind, CEmat)

# check for duplicated strategies
dups <- CEmat[c(duplicated(CEmat[,2:3]) | duplicated(CEmat[,2:3], fromLast = TRUE)),1]

# initialize some variables
costsCol <- 3; qalyCol <- 2

# 1st screening: remove stratgies whose QALYs are lower than the least costly strategy
lowestcost  <- as.numeric(which.min(CEmat[ , costsCol]))
CEmat       <- CEmat[(CEmat[ ,qalyCol] >= CEmat[lowestcost,qalyCol]), ]

# 2nd screening: remove stratgies whose costs are higher than the strategy with highest QALYs
highestqaly <- as.numeric(which.max(CEmat[ , qalyCol]))
CEmat       <- CEmat[(CEmat[ , costsCol] <= CEmat[highestqaly, costsCol]), ]

# Number of strategies remaining after screening
numStrat <- dim(CEmat)[1]

# find WTP levels to test so that all strategies on frontier will be captured
# this means testing on either side of all NMB intersections, which are just all the pairwise ICERs
ICERmat <- matrix(1, numStrat, numStrat)
for (i in 1:numStrat) {
  indexStrat            <- matrix(1, numStrat, 3)
  indexStrat[,costsCol] <- indexStrat[,costsCol]*CEmat[i,costsCol]
  indexStrat[,qalyCol]  <- indexStrat[,qalyCol]*CEmat[i,qalyCol]
  delCostQalys          <- CEmat - indexStrat
  ICERmat[,i]           <- delCostQalys[,costsCol] / delCostQalys[,qalyCol]
}

ICERvec <- numeric(numStrat*numStrat/2 - numStrat)
for (ij in 1:(numStrat-1)){
  ICERvec[(((numStrat-1) + (numStrat-ij+1))*(ij-1)/2 + 1):(((numStrat-1) + (numStrat-ij))*ij/2)] <- ICERmat[ij, (ij+1):numStrat]
}
rm(ICERmat)
intersections <- sort(unique(ICERvec))
rm(ICERvec)
intersections <- intersections[is.finite(intersections)]
WTPtestPoints <- c(0, intersections[intersections >= 0], maxWTP)

#Release the memory
rm(intersections)

# Find the strategy with the max NMB at each of the WTP test points
indiciesOfMax <- numeric(length(WTPtestPoints))

tic("CEA estimation")

for (i in 1:length(WTPtestPoints)){
  romd <- which.max((WTPtestPoints[i]*CEmat[,qalyCol]) - CEmat[,costsCol])
  indiciesOfMax[i] <- CEmat[romd, 1]
}
toc()

frontier <- unique(indiciesOfMax)  #find strategy that maximizes NMB at each WTP

frontier <- frontier - 1 #subtract 1 because reference case has been considered as strategy 1

######################################################################
####### PART 2: Generate the cost-effectiveness results/table ########
######################################################################
outcome.comb <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref  <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))
combination.list <- readRDS("Combination/Combination.list.rds")

CEthreshold <- 100000  #cost-effectiveness threshold

frontier.matrix  <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[frontier , c("QALYs.sum", "costs.total.sum")])

Strategy.ind     <- c(0, frontier)
frontier.matrix  <- cbind(Strategy.ind, frontier.matrix)

costsCol <- 3; qalyCol <- 2

frontier.matrix[ , qalyCol]  <- frontier.matrix[ , qalyCol]  - frontier.matrix[1, qalyCol]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] - frontier.matrix[1, costsCol]

frontier.matrix <- frontier.matrix[-1, ]
frontier.matrix <- as.data.frame(frontier.matrix)

if (any(frontier.matrix$costs.total.sum < 0)){
  frontier.matrix$ICER[frontier.matrix$costs.total.sum < 0] <- "CS"
  NCS  <- sum(frontier.matrix$costs.total.sum < 0)  #number of cost savings
  if (NCS == nrow(frontier.matrix)){
    ocis       <- frontier.matrix$Strategy.ind[nrow(frontier.matrix)]
    comparator <- frontier.matrix$Strategy.ind[nrow(frontier.matrix) - 1]
  } else{
    frontier.matrix$ICER[(NCS+1):nrow(frontier.matrix)] <-
      diff(frontier.matrix$costs.total.sum[NCS:nrow(frontier.matrix)]) / diff(frontier.matrix$QALYs.sum[NCS:nrow(frontier.matrix)])
    icer <- as.numeric(frontier.matrix$ICER[(NCS+1):nrow(frontier.matrix)])  #ICER for non cost-saving strategies
    if (all(icer >= CEthreshold)){
      ocis       <- frontier.matrix$Strategy.ind[NCS]
      comparator <- frontier.matrix$Strategy.ind[NCS - 1]
    } else{
      ocis.ind   <- which.max(icer[icer < CEthreshold]) + NCS
      ocis       <- frontier.matrix$Strategy.ind[ocis.ind]
      comparator <- frontier.matrix$Strategy.ind[ocis.ind - 1]
    }
  }

} else{
  frontier.matrix$ICER[1] <- frontier.matrix$costs.total.sum[1] / frontier.matrix$QALYs.sum[1]
  frontier.matrix$ICER[2:nrow(frontier.matrix)] <-
    diff(frontier.matrix$costs.total.sum) / diff(frontier.matrix$QALYs.sum)
  ocis.ind   <- which.max(frontier.matrix$ICER[frontier.matrix$ICER < CEthreshold])
  ocis       <- frontier.matrix$Strategy.ind[ocis.ind]
  comparator <- frontier.matrix$Strategy.ind[ocis.ind - 1]
}

frontier.list <- list()
frontier.list$frontier   <- frontier
frontier.list$ocis       <- ocis
frontier.list$comparator <- comparator

saveRDS(frontier.list, paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))

names(frontier.matrix) <- c("Strategy", "Incremental QALYs", "Incremental cost", "ICER")
write.xlsx(frontier.matrix, file = paste0("Outputs/Production Function Table/ProductionFunction.CEAResults-", CITY, ".xlsx"))
# Then combine all results in "Combination implementation strategies-Production Function.xlsx" in the same folder

#####################################################################################
####### PART 3: Print the included interventions in the selected combination ########
#####################################################################################
source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")
print(interventions[combination.list[[ocis]]])
print(interventions[combination.list[[comparator]]])
for (i in 1:length(frontier)){
  print(paste0("####COMBINATION-", i, ":"))
  print(interventions[combination.list[[frontier[i]]]])
}

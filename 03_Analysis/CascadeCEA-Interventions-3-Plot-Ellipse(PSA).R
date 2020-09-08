#############################################################################################
## Cascade CEA Model - Combination Interventions
## Plot PSA ellipse for OCIS in a 6-panel graph
## Last updated: March 11, 2020
############################################################################################
rm(list=ls())
library(rstudioapi)
library(Hmisc)
library(maptools)
library(ggplot2)
library(SIBER)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

CITY.name <- c("(A) Atlanta", " (B) Baltimore", "(C) Los Angeles", "(D) Miami", "(E) New York City", "(F) Seattle")
combination.list <- readRDS("Combination/Combination.list.rds")

## level for ellipse ##
pell <- 0.95

#############################
#### Ellipse for ATLANTA ####
#############################
ww <- 1; CITY <- all.cities[[ww]]
frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
ocis.ind1        <- which(frontier == ocis)

outcome.comb   <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref    <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))
outcome.SA.ref <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))

ref.matrix      <- matrix(0, nrow = 2000, ncol =2)
ref.matrix[ ,1] <- unlist(lapply(outcome.SA.ref, "[[", 1))
ref.matrix[ ,2] <- unlist(lapply(outcome.SA.ref, "[[", 2))

frontier.matrix <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[frontier , c("QALYs.sum", "costs.total.sum")])

Strategy.ind    <- c(0, frontier)
frontier.matrix <- cbind(Strategy.ind, frontier.matrix)

costsCol <- 3; qalyCol <- 2

#matrix for stratgies on PF, deterministic values
frontier.matrix[ , qalyCol]  <- frontier.matrix[ , qalyCol] - frontier.matrix[1, qalyCol]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] - frontier.matrix[1, costsCol]
frontier.matrix              <- frontier.matrix[-1, ]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] / 1000000

#matrix for OCIS, PSA values
SA_comb          <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", ocis, "(OCIS).rds"))
SA_comb.mx       <- SA_comb[ , c("QALYs.sum", "costs.total.sum")]
SA_comb.mx[ , 1] <- SA_comb.mx[ , 1] - ref.matrix[ ,1]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] - ref.matrix[ ,2]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] / 1000000

# GGPLOT ##
xlim <- c(min(frontier.matrix[ , costsCol]) *1.2, max(max(SA_comb.mx[ , 2]), max(frontier.matrix[ ,costsCol])))
ylim <- c(0 , max(SA_comb.mx[ , 1]))
plot_ellipse1 <- ggplot(data = data.frame(frontier.matrix), aes(x=costs.total.sum, y = QALYs.sum) ) +
  ggtitle(paste0((CITY.name[ww]), "")) +
  geom_point(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =3) +
  geom_line(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =1) +
  geom_point(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), colour = "firebrick2", size =1, alpha = 0.13, shape = 16) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue1", size = 1, alpha=1, level = pell) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue4", size = 1, alpha=1, level = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.7) +
  geom_point(colour = "dodgerblue1", size =3) +
  geom_line(colour = "dodgerblue1", size =1) +
  geom_point(aes(x=costs.total.sum[ocis.ind1], y = QALYs.sum[ocis.ind1]), colour = "firebrick", size =4, shape=18) +
  ylim(ylim) +
  xlim(xlim) +
  labs(y="Incremental QALYs, versus status quo", x="Incremental cost, versus statuo quo (US$, million)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key = element_rect(fill = "transparent", colour = "transparent"),
        plot.title=element_text(hjust=0.02, vjust=-7), legend.justification=c(1,0), legend.position=c(1,0))


###############################
#### Ellipse for BALTIMORE ####
###############################
ww <- 2; CITY <- all.cities[[ww]]
frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
ocis.ind2        <- which(frontier == ocis)

outcome.comb   <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref    <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))
outcome.SA.ref <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))

ref.matrix      <- matrix(0, nrow = 2000, ncol =2)
ref.matrix[ ,1] <- unlist(lapply(outcome.SA.ref, "[[", 1))
ref.matrix[ ,2] <- unlist(lapply(outcome.SA.ref, "[[", 2))

frontier.matrix <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[frontier , c("QALYs.sum", "costs.total.sum")])

Strategy.ind    <- c(0, frontier)
frontier.matrix <- cbind(Strategy.ind, frontier.matrix)

costsCol <- 3; qalyCol <- 2

#matrix for stratgies on PF, deterministic values
frontier.matrix[ , qalyCol]  <- frontier.matrix[ , qalyCol] - frontier.matrix[1, qalyCol]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] - frontier.matrix[1, costsCol]
frontier.matrix              <- frontier.matrix[-1, ]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] / 1000000

#matrix for OCIS, PSA values
SA_comb          <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", ocis, "(OCIS).rds"))
SA_comb.mx       <- SA_comb[ , c("QALYs.sum", "costs.total.sum")]
SA_comb.mx[ , 1] <- SA_comb.mx[ , 1] - ref.matrix[ ,1]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] - ref.matrix[ ,2]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] / 1000000

# GGPLOT ##
xlim <- c(min(frontier.matrix[ , costsCol]) *1.2, max(max(SA_comb.mx[ , 2]), max(frontier.matrix[ ,costsCol])))
ylim <- c(0 , max(SA_comb.mx[ , 1]))
plot_ellipse2 <- ggplot(data = data.frame(frontier.matrix), aes(x=costs.total.sum, y = QALYs.sum) ) +
  ggtitle(paste0((CITY.name[ww]), "")) +
  geom_point(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =3) +
  geom_line(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =1) +
  geom_point(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), colour = "firebrick2", size =1, alpha = 0.13, shape = 16) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue1", size = 1, alpha=1, level = pell) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue4", size = 1, alpha=1, level = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.7) +
  geom_point(colour = "dodgerblue1", size =3) +
  geom_line(colour = "dodgerblue1", size =1) +
  geom_point(aes(x=costs.total.sum[ocis.ind2], y = QALYs.sum[ocis.ind2]), colour = "firebrick", size =4, shape=18) +
  ylim(ylim) +
  xlim(xlim) +
  labs(y="Incremental QALYs, versus status quo", x="Incremental cost, versus statuo quo (US$, million)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key = element_rect(fill = "transparent", colour = "transparent"),
        plot.title=element_text(hjust=0.02, vjust=-7), legend.justification=c(1,0), legend.position=c(1,0))

#################################
#### Ellipse for LOS ANGELES ####
#################################
ww <- 3; CITY <- all.cities[[ww]]
frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
ocis.ind3        <- which(frontier == ocis)

outcome.comb   <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref    <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))
outcome.SA.ref <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))

ref.matrix      <- matrix(0, nrow = 2000, ncol =2)
ref.matrix[ ,1] <- unlist(lapply(outcome.SA.ref, "[[", 1))
ref.matrix[ ,2] <- unlist(lapply(outcome.SA.ref, "[[", 2))

frontier.matrix <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[frontier , c("QALYs.sum", "costs.total.sum")])

Strategy.ind    <- c(0, frontier)
frontier.matrix <- cbind(Strategy.ind, frontier.matrix)

costsCol <- 3; qalyCol <- 2

#matrix for stratgies on PF, deterministic values
frontier.matrix[ , qalyCol]  <- frontier.matrix[ , qalyCol] - frontier.matrix[1, qalyCol]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] - frontier.matrix[1, costsCol]
frontier.matrix              <- frontier.matrix[-1, ]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] / 1000000

#matrix for OCIS, PSA values
SA_comb          <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", ocis, "(OCIS).rds"))
SA_comb.mx       <- SA_comb[ , c("QALYs.sum", "costs.total.sum")]
SA_comb.mx[ , 1] <- SA_comb.mx[ , 1] - ref.matrix[ ,1]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] - ref.matrix[ ,2]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] / 1000000

# GGPLOT ##
xlim <- c(min(frontier.matrix[ , costsCol]) *1.2, max(max(SA_comb.mx[ , 2]), max(frontier.matrix[ ,costsCol])))
ylim <- c(0 , max(SA_comb.mx[ , 1]))
plot_ellipse3 <- ggplot(data = data.frame(frontier.matrix), aes(x=costs.total.sum, y = QALYs.sum) ) +
  ggtitle(paste0((CITY.name[ww]), "")) +
  geom_point(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =3) +
  geom_line(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =1) +
  geom_point(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), colour = "firebrick2", size =1, alpha = 0.13, shape = 16) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue1", size = 1, alpha=1, level = pell) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue4", size = 1, alpha=1, level = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.7) +
  geom_point(colour = "dodgerblue1", size =3) +
  geom_line(colour = "dodgerblue1", size =1) +
  geom_point(aes(x=costs.total.sum[ocis.ind3], y = QALYs.sum[ocis.ind3]), colour = "firebrick", size =4, shape=18) +
  ylim(ylim) +
  xlim(xlim) +
  labs(y="Incremental QALYs, versus status quo", x="Incremental cost, versus statuo quo (US$, million)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key = element_rect(fill = "transparent", colour = "transparent"),
        plot.title=element_text(hjust=0.02, vjust=-7), legend.justification=c(1,0), legend.position=c(1,0))


###########################
#### Ellipse for MIAMI ####
###########################
ww <- 4; CITY <- all.cities[[ww]]
frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
ocis.ind4        <- which(frontier == ocis)

outcome.comb   <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref    <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))
outcome.SA.ref <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))

ref.matrix      <- matrix(0, nrow = 2000, ncol =2)
ref.matrix[ ,1] <- unlist(lapply(outcome.SA.ref, "[[", 1))
ref.matrix[ ,2] <- unlist(lapply(outcome.SA.ref, "[[", 2))

frontier.matrix <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[frontier , c("QALYs.sum", "costs.total.sum")])

Strategy.ind    <- c(0, frontier)
frontier.matrix <- cbind(Strategy.ind, frontier.matrix)

costsCol <- 3; qalyCol <- 2

#matrix for stratgies on PF, deterministic values
frontier.matrix[ , qalyCol]  <- frontier.matrix[ , qalyCol] - frontier.matrix[1, qalyCol]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] - frontier.matrix[1, costsCol]
frontier.matrix              <- frontier.matrix[-1, ]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] / 1000000

#matrix for OCIS, PSA values
SA_comb          <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", ocis, "(OCIS).rds"))
SA_comb.mx       <- SA_comb[ , c("QALYs.sum", "costs.total.sum")]
SA_comb.mx[ , 1] <- SA_comb.mx[ , 1] - ref.matrix[ ,1]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] - ref.matrix[ ,2]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] / 1000000

# GGPLOT ##
xlim <- c(min(frontier.matrix[ , costsCol]) *1.2, max(max(SA_comb.mx[ , 2]), max(frontier.matrix[ ,costsCol])))
ylim <- c(0 , max(SA_comb.mx[ , 1]))
plot_ellipse4 <- ggplot(data = data.frame(frontier.matrix), aes(x=costs.total.sum, y = QALYs.sum) ) +
  ggtitle(paste0((CITY.name[ww]), "")) +
  geom_point(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =3) +
  geom_line(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =1) +
  geom_point(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), colour = "firebrick2", size =1, alpha = 0.13, shape = 16) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue1", size = 1, alpha=1, level = pell) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue4", size = 1, alpha=1, level = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.7) +
  geom_point(colour = "dodgerblue1", size =3) +
  geom_line(colour = "dodgerblue1", size =1) +
  geom_point(aes(x=costs.total.sum[ocis.ind4], y = QALYs.sum[ocis.ind4]), colour = "firebrick", size =4, shape=18) +
  ylim(ylim) +
  xlim(xlim) +
  labs(y="Incremental QALYs, versus status quo", x="Incremental cost, versus statuo quo (US$, million)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key = element_rect(fill = "transparent", colour = "transparent"),
        plot.title=element_text(hjust=0.02, vjust=-7), legend.justification=c(1,0), legend.position=c(1,0))


###################################
#### Ellipse for NEW YORK CITY ####
###################################
ww <- 5; CITY <- all.cities[[ww]]
frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
ocis.ind5        <- which(frontier == ocis)

outcome.comb   <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref    <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))
outcome.SA.ref <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))

ref.matrix      <- matrix(0, nrow = 2000, ncol =2)
ref.matrix[ ,1] <- unlist(lapply(outcome.SA.ref, "[[", 1))
ref.matrix[ ,2] <- unlist(lapply(outcome.SA.ref, "[[", 2))

frontier.matrix <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[frontier , c("QALYs.sum", "costs.total.sum")])

Strategy.ind    <- c(0, frontier)
frontier.matrix <- cbind(Strategy.ind, frontier.matrix)

costsCol <- 3; qalyCol <- 2

#matrix for stratgies on PF, deterministic values
frontier.matrix[ , qalyCol]  <- frontier.matrix[ , qalyCol] - frontier.matrix[1, qalyCol]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] - frontier.matrix[1, costsCol]
frontier.matrix              <- frontier.matrix[-1, ]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] / 1000000

#matrix for OCIS, PSA values
SA_comb          <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", ocis, "(OCIS).rds"))
SA_comb.mx       <- SA_comb[ , c("QALYs.sum", "costs.total.sum")]
SA_comb.mx[ , 1] <- SA_comb.mx[ , 1] - ref.matrix[ ,1]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] - ref.matrix[ ,2]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] / 1000000

# GGPLOT ##
xlim <- c(min(frontier.matrix[ , costsCol]) *1.2, max(max(SA_comb.mx[ , 2]), max(frontier.matrix[ ,costsCol])))
ylim <- c(0 , max(SA_comb.mx[ , 1]))
plot_ellipse5 <- ggplot(data = data.frame(frontier.matrix), aes(x=costs.total.sum, y = QALYs.sum) ) +
  ggtitle(paste0((CITY.name[ww]), "")) +
  geom_point(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =3) +
  geom_line(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =1) +
  geom_point(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), colour = "firebrick2", size =1, alpha = 0.13, shape = 16) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue1", size = 1, alpha=1, level = pell) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue4", size = 1, alpha=1, level = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.7) +
  geom_point(colour = "dodgerblue1", size =3) +
  geom_line(colour = "dodgerblue1", size =1) +
  geom_point(aes(x=costs.total.sum[ocis.ind5], y = QALYs.sum[ocis.ind5]), colour = "firebrick", size =4, shape=18) +
  ylim(ylim) +
  xlim(xlim) +
  labs(y="Incremental QALYs, versus status quo", x="Incremental cost, versus statuo quo (US$, million)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key = element_rect(fill = "transparent", colour = "transparent"),
        plot.title=element_text(hjust=0.02, vjust=-7), legend.justification=c(1,0), legend.position=c(1,0))


#############################
#### Ellipse for SEATTLE ####
#############################
ww <- 6; CITY <- all.cities[[ww]]
frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis
ocis.ind6        <- which(frontier == ocis)

outcome.comb   <- readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
outcome.ref    <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))
outcome.SA.ref <- readRDS(paste0("Inputs/Combination-SA-", CITY, "-refcase-outcomes.rds"))

ref.matrix      <- matrix(0, nrow = 2000, ncol =2)
ref.matrix[ ,1] <- unlist(lapply(outcome.SA.ref, "[[", 1))
ref.matrix[ ,2] <- unlist(lapply(outcome.SA.ref, "[[", 2))

frontier.matrix <- rbind(outcome.ref[c("QALYs.sum", "costs.total.sum")], outcome.comb[frontier , c("QALYs.sum", "costs.total.sum")])

Strategy.ind    <- c(0, frontier)
frontier.matrix <- cbind(Strategy.ind, frontier.matrix)

costsCol <- 3; qalyCol <- 2

#matrix for stratgies on PF, deterministic values
frontier.matrix[ , qalyCol]  <- frontier.matrix[ , qalyCol] - frontier.matrix[1, qalyCol]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] - frontier.matrix[1, costsCol]
frontier.matrix              <- frontier.matrix[-1, ]
frontier.matrix[ , costsCol] <- frontier.matrix[ , costsCol] / 1000000

#matrix for OCIS, PSA values
SA_comb          <- readRDS(paste0("Combination/Outcome-Combination-", CITY, "-PSA-", ocis, "(OCIS).rds"))
SA_comb.mx       <- SA_comb[ , c("QALYs.sum", "costs.total.sum")]
SA_comb.mx[ , 1] <- SA_comb.mx[ , 1] - ref.matrix[ ,1]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] - ref.matrix[ ,2]
SA_comb.mx[ , 2] <- SA_comb.mx[ , 2] / 1000000

# GGPLOT ##
xlim <- c(min(frontier.matrix[ , costsCol]) *1.2, max(max(SA_comb.mx[ , 2]), max(frontier.matrix[ ,costsCol])))
ylim <- c(min(-50, min(SA_comb.mx[ , 1])) , max(SA_comb.mx[ , 1]))
plot_ellipse6 <- ggplot(data = data.frame(frontier.matrix), aes(x=costs.total.sum, y = QALYs.sum) ) +
  ggtitle(paste0((CITY.name[ww]), "")) +
  geom_point(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =3) +
  geom_line(aes(x=costs.total.sum, y = QALYs.sum), colour = "dodgerblue1", size =1) +
  geom_point(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), colour = "firebrick2", size =1, alpha = 0.13, shape = 16) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue1", size = 1, alpha=1, level = pell) +
  stat_ellipse(data = data.frame(SA_comb.mx), aes(x=costs.total.sum, y = QALYs.sum), linetype = 2 ,color = "skyblue4", size = 1, alpha=1, level = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.7) +
  geom_point(colour = "dodgerblue1", size =3) +
  geom_line(colour = "dodgerblue1", size =1) +
  geom_point(aes(x=costs.total.sum[ocis.ind6], y = QALYs.sum[ocis.ind6]), colour = "firebrick", size =4, shape=18) +
  ylim(ylim) +
  xlim(xlim) +
  labs(y="Incremental QALYs, versus status quo", x="Incremental cost, versus statuo quo (US$, million)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key = element_rect(fill = "transparent", colour = "transparent"),
        plot.title=element_text(hjust=0.02, vjust=-7), legend.justification=c(1,0), legend.position=c(1,0))


####Combine all 6 plots####
library(cowplot)
p <- plot_grid(plot_ellipse1, plot_ellipse2, plot_ellipse3, plot_ellipse4, plot_ellipse5, plot_ellipse6, ncol = 2,
               align = 'v', axis = 'l') # aligning vertically along the left axis

p

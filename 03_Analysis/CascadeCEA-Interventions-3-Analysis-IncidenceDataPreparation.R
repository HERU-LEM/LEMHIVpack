#############################################################################################
## Cascade CEA Model - Combination Interventions (Core)
## Derive the new incidence data to facilitate the ggplot of annual absolute number of new infections
## Last updated: March 13, 2020
############################################################################################

#INITIALIZE the data frame and matrix
city.column      <- c(rep("Atlanta", 26*3), rep("Baltimore", 26*3), rep("Los Angeles", 26*3), rep("Miami", 26*3), rep("New York City", 26*3), rep("Seattle", 26*3))
scenario.column  <- rep(c(rep("Status Quo", 26), rep("Documented", 26), rep("Ideal",26)), 6)
year.column      <- rep(c(2015:2040), 18)
ggplotdata       <- data.frame(City = city.column, Scenario = scenario.column, Year = year.column)
ggplotdata$PE    <- rep(0, 26*6*3)

reduction.matrix.documented           <- matrix(0, ncol = 10, nrow = 6)
colnames(reduction.matrix.documented) <- c("2025_target", "2025_mean", "2025_median", "2025_lower", '2025_upper',
                                           "2030_target", "2030_mean", "2030_median", "2030_lower", '2030_upper')
rownames(reduction.matrix.documented) <- c("Atlanta", "Baltimore", "Los Angeles", "Miami", "New York City", "Seattle")
reduction.matrix.ideal                <- reduction.matrix.documented

#LOAD the combination strategies
combination.list <- readRDS("Combination/Combination.list.rds")
#LOAD city and functions
all.cities       <- c("ATL", "BAL", "LA", "MIA", "NYC", "SEA")
city.name.list   <- c("Atlanta", "Baltimore", "Los Angeles", "Miami", "New York City", "Seattle")
n.sample         <- 2000
#source("Scripts/CascadeCEA-Interventions-0-Function-incidence.derivation.R")

for (ww in 1:6){
  CITY      <- all.cities[ww]
  CITY.name <- city.name.list[ww]
  ocis      <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis

  case      <- "Status Quo"
  incidence <- incidence.derivation(CITY = CITY, case = case, ocis = ocis)
  ggplotdata$PE[ggplotdata$City     == CITY.name & ggplotdata$Scenario == case] <- incidence$pe
  ggplotdata$Median[ggplotdata$City == CITY.name & ggplotdata$Scenario == case] <- incidence$median
  ggplotdata$Lower[ggplotdata$City  == CITY.name & ggplotdata$Scenario == case] <- incidence$lower
  ggplotdata$Upper[ggplotdata$City  == CITY.name & ggplotdata$Scenario == case] <- incidence$upper

  case      <- "Documented"
  incidence <- incidence.derivation(CITY = CITY, case = case, ocis = ocis)
  ggplotdata$PE[ggplotdata$City     == CITY.name & ggplotdata$Scenario == case] <- incidence$pe
  ggplotdata$Median[ggplotdata$City == CITY.name & ggplotdata$Scenario == case] <- incidence$median
  ggplotdata$Lower[ggplotdata$City  == CITY.name & ggplotdata$Scenario == case] <- incidence$lower
  ggplotdata$Upper[ggplotdata$City  == CITY.name & ggplotdata$Scenario == case] <- incidence$upper
  reduction.matrix.documented[CITY.name, ] <- incidence$reduction

  case      <- "Ideal"
  incidence <- incidence.derivation(CITY = CITY, case = case, ocis = ocis)
  ggplotdata$PE[ggplotdata$City     == CITY.name & ggplotdata$Scenario == case] <- incidence$pe
  ggplotdata$Median[ggplotdata$City == CITY.name & ggplotdata$Scenario == case] <- incidence$median
  ggplotdata$Lower[ggplotdata$City  == CITY.name & ggplotdata$Scenario == case] <- incidence$lower
  ggplotdata$Upper[ggplotdata$City  == CITY.name & ggplotdata$Scenario == case] <- incidence$upper
  reduction.matrix.ideal[CITY.name, ] <- incidence$reduction
}

write.csv(ggplotdata,'Outputs/Incidence/AnnualIncidenceRate.csv')
saveRDS(ggplotdata, paste0("Outputs/Incidence/incidence_ggplot_range.rds"))

write.csv(reduction.matrix.documented,'Outputs/Incidence/IncidenceReduction(documented).csv')
write.csv(reduction.matrix.ideal,'Outputs/Incidence/IncidenceReduction(ideal).csv')

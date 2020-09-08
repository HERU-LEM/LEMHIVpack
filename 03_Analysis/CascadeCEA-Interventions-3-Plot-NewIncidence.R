#############################################################################################
## Cascade CEA Model - Combination Interventions
## Plot the estimated number of new infections during the study time horizon
## Last updated: March 11, 2020
############################################################################################
rm(list=ls())
library(rstudioapi)
library(ggplot2)
library(reshape2)
library(LEMHIVpack)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

# SELECT city ##
# CITY <- select.list(all.cities, multiple = FALSE,
#                     title = 'Select city', graphics = FALSE)
ww <- 1; CITY <- all.cities[[ww]] # Otherwise you can set city by its index
CITY.name  <- c("(A) Atlanta", "(B) Baltimore", "(C) Los Angeles", "(D) Miami", "(E) New York City", "(F) Seattle")

combination.list <- readRDS("Combination/Combination.list.rds")

# Load frontier and ocis #
frontier         <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier
ocis             <- readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$ocis

plot.data        <- matrix(0, nrow = length(2016:2040), ncol = (length(frontier)+1))
ref.case.inf     <- t(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-infections.rds"))$out.inf.yr)
ref.case.inf     <- ref.case.inf[-dim(ref.case.inf)[1], ]
plot.data[ , 1]  <- ref.case.inf

colnames(plot.data)    <- rep("Frontiner", ncol(plot.data))
colnames(plot.data)[1] <- "SQ"
colnames(plot.data)[which(frontier == ocis) + 1] <- "OCIS"

comb.outcome <-  readRDS(paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
plot.data[ , 2:ncol(plot.data)] <- t(comb.outcome[frontier , 7:31])

Year       <- 2016:2040
plot.data  <- cbind(Year, plot.data)

ggplotdata <- melt(data.frame(plot.data), id.vars = c("Year"))
ggplotdata <- rbind(ggplotdata[ggplotdata$variable=="SQ",], subset(ggplotdata[ggplotdata$variable!="SQ",], !(Year %in% c(2015:2018))))
levels(ggplotdata$variable)[1]     <- "Status Quo"
ggplotdata[1:length(2016:2040), 2] <- "Status Quo"

level = unique(ggplotdata$variable)[order(unique(ggplotdata$variable))]

color.panel <- c("firebrick1", "#8dd3c7", "#1f78b4","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#ccebc5","#ffed6f")
color.panel[length(frontier)+1] = "#bc80bd"

plot_out <- ggplot(data = ggplotdata, aes(x=Year, y = value, color = factor(variable, levels = level), alpha = factor(variable, levels = level), size = variable)) +
  ggtitle(CITY.name[ww]) +
  geom_line() +
  scale_color_manual(values= color.panel) +
  scale_alpha_manual(values = c(1, rep(0.9, (length(unique(ggplotdata$variable))-1)))) +
  scale_size_manual(values = c(rep(1, which(frontier == ocis)), 2, rep(1, (length(frontier) - which(frontier == ocis))))) +
  scale_x_continuous(breaks = c(2016, 2020, 2025, 2030, 2035, 2040)) +
  ylim(0, max(ggplotdata$value)*1.2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey", size = 0.7) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = "grey", size = 0.7) +
  geom_vline(xintercept = 2030, linetype = "dashed", color = "grey", size = 0.7) +
  labs(y="Number of new HIV infections", x="Year", col = "Strategy", alpha = "Strategy") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title=element_text(size=11), axis.text = element_text(size=10), title=element_text(size=11), legend.text = element_text(size=9), legend.key = element_rect(fill = "transparent", colour = "transparent"))

plot_out

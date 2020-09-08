library(gtools)

interventions <- c("Opt-out testing (ER)", "Opt-out testing (PC)",
                   "EMR testing reminder", "Nurse-initiated testing",
                   "OAT integrated testing",
                   "ART initiation", "ART retention", "ART retention, targeted",
                   "ART EMR reminder", "RAPID ART",
                   "ART re-initiation", "ART re-linkage",
                   "SSP", "OAT with BUP", "OAT with methadone", "PrEP",
                   "No interventions")

name.intervention   <- interventions[-length(interventions)]

no.intervention     <- length(name.intervention)

index.intervention  <- 1: no.intervention

matrix.intervention <- data.frame(ind = index.intervention, name = name.intervention)

## list interventions that are mutually exclusive that should be excluded from forming combinations
list.excl <- list()
list.excl[[1]]  <- c(7, 8)    # ART retention & ART retention, targeted
list.excl[[2]]  <- c(2, 4)    # Opt-out (PC) & Nurse-initiated testing
list.excl[[3]]  <- c(7, 9)    # ART retention & ART EMR reminder
list.excl[[4]]  <- c(1, 3)    # Opt-out (ER) & EMR testing


"%contain%" <- function(values,x) {
  tx <- table(x)
  tv <- table(values)
  z <- tv[names(tx)] - tx
  all(z >= 0 & !is.na(z))
}


# Create an empty list
comb.list <- list()

# Find all possible combinations, save it to a list
for (i in 1:no.intervention) {
  comb.list[[i]]<- combinations(no.intervention, i, index.intervention)
  rem.row <- NULL
    for (j in 1:nrow(comb.list[[i]])){
      if (comb.list[[i]][j,] %contain% list.excl[[1]]){
        rem.row <- c(rem.row, j)
      } else if (comb.list[[i]][j,] %contain% list.excl[[2]]){
        rem.row <- c(rem.row, j)
      } else if (comb.list[[i]][j,] %contain% list.excl[[3]]){
        rem.row <- c(rem.row, j)
      } else if (comb.list[[i]][j,] %contain% list.excl[[4]]){
        rem.row <- c(rem.row, j)
      }
    }
  if(!is.null(rem.row)){
      comb.list[[i]] <- comb.list[[i]][-rem.row, ]
  }
}

comb.list <- comb.list[-which(lengths(comb.list)==0)]

print(sum(sapply(comb.list, NROW)))

n_comb <- sapply(comb.list, NROW)
combination_list <- vector("list", sum(n_comb))
for (nnc in 1:length(comb.list)){
  for (nns in 1:nrow(comb.list[[nnc]])){
    combination_list[[sum(n_comb[1:nnc-1])+nns]] <- comb.list[[nnc]][nns, ]
  }
}
saveRDS(combination_list, file = paste0("Combination/Combination.list.rds"))

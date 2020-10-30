#########################
### PARAMETERS TO SET ###
#########################
rm(list = ls())
library(dgpsim)
library(qwraps2)
library(xtable)
library(parallel)
source("Code/new-ver/helper_funs.R")

set.seed(861226)
design_mat_100_A <- dgp(100, c(1, 1), true_mean = TRUE)
design_mat_500_A <- dgp(500, c(1, 1), true_mean = TRUE)
design_mat_1000_A <- dgp(1000, c(1, 1), true_mean = TRUE)

initialize_all_data()

# Generate all the data

for (i in 1:3) {

    if(i == 1) {
        data_set <- design_mat_100_A
    } else if(i == 2) {
        data_set <- design_mat_500_A
    } else if(i == 3) {
        data_set <- design_mat_1000_A
    }

    for (df in c(2.5, 5, 30)) {

        n_per_core <- 20
        n_cores <- 5

        new_rows_list <- mclapply(rep(n_per_core, n_cores),
                                  generate_new_rows,
                                  mc.cores = n_cores,
                                  deg_f = df,
                                  n_bss = 1e3,
                                  sigma_2 = 0,
                                  design_mat = data_set)

        new_rows <- do.call(rbind, new_rows_list)
        
        rm(all_data)
        load("all_data.RData")
        all_data <- rbind(all_data, new_rows)
        save(all_data, file = "all_data.RData")
        print("Current number of rows: ")
        print(nrow(all_data))
    }
}


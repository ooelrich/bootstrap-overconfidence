#########################
### PARAMETERS TO SET ###
#########################
rm(list = ls())
library(dgpsim)
library(qwraps2)
library(xtable)
source("Code/new-ver/helper_funs.R")


# Create input data and control that it's still the same
# since we want to use the same design matrix for all
# repetitions
set.seed(861226)
design_mat_50_A <- dgp(50, c(1, 1), true_mean = TRUE)
design_mat_100_A <- dgp(100, c(1, 1), true_mean = TRUE)
design_mat_1000_A <- dgp(1000, c(1, 1), true_mean = TRUE)

# Verify that the column sums have not changed somehow
check_data_sets()

# Only run this if you do not have a data_all, as it will wipe
# out the old one!!!
initialize_all_data()

# This function generates a new number of observations equal to
# runs. One observation is one parent sample from the given DGP
# and n_bss bootstrap replicates based on that parent. Each row
# contains the sim setup, the LBF of the parent, and the bootstrap
# estimate of var(LBF)

for (i in 1:3) {

    if(i == 1) {
        data_set <- design_mat_50_A
    } else if(i == 2) {
        data_set <- design_mat_100_A
    } else if(i == 3) {
        data_set <- design_mat_1000_A
    }

    for (df in c(2.5, 5, 30)) {

        new_rows <- generate_new_rows(deg_f = df, n_bss = 1e3, sigma_2 = 0,
                              runs = 4e3, design_mat = data_set)

        rm(all_data)
        load("all_data.RData")
        all_data <- rbind(all_data, new_rows)
        save(all_data, file = "all_data.RData")
        print("Current number of rows: ")
        print(nrow(all_data))
    }
}






# Add some stuff to the data frame for summarizing
all_data$radical <- as.numeric(abs(all_data$LBF) > 5)
all_data$setup <- paste0("n: ", all_data$n_obs, " df: ", all_data$deg_f)

summary_appendix <- 
    list("Simulation results" =
        list("truth"  = ~mean(.data$radical),
             "boot_approx"  = ~qwraps2::mean_sd(.data$p_radical) )
        )

t(summary_table(dplyr::group_by(all_data, setup), summary_appendix))


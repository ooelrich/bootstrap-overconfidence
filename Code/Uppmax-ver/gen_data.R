initialize_all_data <- function() {
  all_data <- data.frame(boot_reps = integer(),
                        n_obs = integer(),
                        deg_f = double(),
                        lbf = double(),
                        var_lbf = double(),
                        p_radical = double())
  save(all_data, file = "/proj/dennis/nobackup/all_data.RData")
}

initialize_all_data()

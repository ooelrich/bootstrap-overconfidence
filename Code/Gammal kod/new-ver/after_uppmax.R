# Things to do with the data once I get it from Uppmax


all_data$radical <- as.numeric(abs(all_data$LBF) > 5)
all_data$setup <- paste0("n: ", all_data$n_obs, " df: ", all_data$deg_f)

all_data$setup <- factor(all_data$setup, levels = c("n: 50 df: 2.5",  "n: 50 df: 5",
                        "n: 50 df: 30", "n: 100 df: 2.5", "n: 100 df: 5", "n: 100 df: 30", 
                        "n: 1000 df: 2.5", "n: 1000 df: 5", "n: 1000 df: 30"))
levels(all_data$setup)





# Add some stuff to the data frame for summarizing

summary_appendix <- 
    list("Simulation results" =
        list("truth"  = ~mean(.data$radical),
             "boot_approx"  = ~qwraps2::mean_sd(.data$p_radical, digits = 3) )
        )

a <- t(summary_table(dplyr::group_by(all_data, setup), summary_appendix))



check_data_sets <- function(){
    
    check_vec <- rep(NA_real_, 9)

    #check_vec[1] <- (colMeans(design_mat_50_A)[1] == -0.09955195527523756637844)
    #check_vec[2] <- (colMeans(design_mat_50_A)[2] == -0.09251772635616362094879)
    #check_vec[3] <- (colMeans(design_mat_50_A)[3] == -0.00703422891907393935812)

    # same for 100 here

    check_vec[1] <- (colMeans(design_mat_100_A)[1] == -0.02692726207431405879844)
    check_vec[2] <- (colMeans(design_mat_100_A)[2] == -0.07525495003443211095018)
    check_vec[3] <- (colMeans(design_mat_100_A)[3] == 0.04832768796011805562118)

    # n = 500

    check_vec[4] <- (colMeans(design_mat_100_A)[1] == )
    check_vec[5] <- (colMeans(design_mat_100_A)[2] == )
    check_vec[6] <- (colMeans(design_mat_100_A)[3] == )

    # same for 1000 here

    check_vec[7] <- (colMeans(design_mat_1000_A)[1] == -0.01641358056059107062286)
    check_vec[8] <- (colMeans(design_mat_1000_A)[2] == -0.02671633330173790385476)
    check_vec[9] <- (colMeans(design_mat_1000_A)[3] == 0.0103027527411468332319)

    if (sum (as.numeric(check_vec)) == 9) {
        data_status <- "pass"
    } else {
        data_status <- "fail"
    }

    return(data_status)
}
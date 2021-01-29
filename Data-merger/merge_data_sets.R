data_list <- list()
for (i in seq_len(600)) {
    data_list[[i]] <- readRDS(sprintf("Data-merger/n500-data-2/data%s.Rds", i))
}

all_data <- do.call(rbind, data_list)
all_data[1:100,]
saveRDS(all_data, file = "n500_2.Rds")


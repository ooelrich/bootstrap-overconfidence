library(data.table)
library(ggplot2)
library(reshape2)

n100 <- data.frame(readRDS("Data sets/n100.Rds"))
n500 <- data.frame(readRDS("Data sets/n500.Rds"))
n1000 <- data.frame(readRDS("Data sets/n1000.Rds"))

varnames <- c("boot_reps", "n", "df", "lbf", "var_lbf", "p_radical")

colnames(n500) <- varnames
colnames(n1000) <- varnames
colnames(n100) <- varnames
bdf <- rbind(n100, n500, n1000)

head(bdf)


bdf <- data.table(bdf)
ans <- bdf[, .(true_var = var(lbf),
               mean_var_lbf = mean(var_lbf),
               var_var_lfb = var(var_lbf),
               true = tabulate(as.numeric(abs(lbf) > 5)) / .N,
               mean_p_radical =  mean(p_radical),
               var_p_radical = var(p_radical),
               median = median(p_radical),
               q_25 = quantile(p_radical, 0.25),
               q_75 = quantile(p_radical, 0.75),
               q_025 = quantile(p_radical, 0.025),
               q_975 = quantile(p_radical, 0.975)),
            by = .(df, n)]
ans

# Pick out the columns you need for the graph, turn into data.frame
df <- data.frame(ans[, .(df, n, true, q_025, q_25, median, q_75, q_975)])
df$df <- as.factor(df$df)
df$n <- as.factor(df$n)
df$x <- 1
df$y <- 1

# Actual plot for paper

g <- ggplot(df, aes(y)) +
        geom_boxplot(aes(ymin = q_025, lower = q_25, middle = median, upper = q_75,
            ymax = q_975), width = 0.03, stat = "identity") + 
        geom_point(aes(x = 1, y = true, col = "red")) + coord_flip() +
        ylim(c(0,1)) + xlim(c(0.9, 1.1)) +
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.ticks.y = element_blank()) +
        theme(axis.text.y = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
        theme(axis.text=element_text(size = 8, face = "bold")) +
        facet_grid(df ~ n, labeller = label_both)


# Change the font on the facet strips
g + theme(strip.text.x = element_text(size = 10, face = "bold"),
          strip.text.y = element_text(size = 10, face = "bold"))

ggsave("bootplot.png", width = 8, height = 2.5, dpi = 300)



###########################################################
## SECOND DATA SET ########################################
###########################################################

n100 <- data.frame(readRDS("Data sets/n100_2.Rds"))
n500 <- data.frame(readRDS("Data sets/n500_2.Rds"))
n1000 <- data.frame(readRDS("Data sets/n1000_2.Rds"))

varnames <- c("boot_reps", "n", "df", "lbf", "var_lbf", "p_radical")

colnames(n500) <- varnames
colnames(n1000) <- varnames
colnames(n100) <- varnames
bdf <- rbind(n100, n500, n1000)

head(bdf)


bdf <- data.table(bdf)
ans <- bdf[, .(true_var = var(lbf),
               mean_var_lbf = mean(var_lbf),
               var_var_lfb = var(var_lbf),
               true = tabulate(as.numeric(abs(lbf) > 5)) / .N,
               mean_p_radical =  mean(p_radical),
               var_p_radical = var(p_radical),
               median = median(p_radical),
               q_25 = quantile(p_radical, 0.25),
               q_75 = quantile(p_radical, 0.75),
               q_025 = quantile(p_radical, 0.025),
               q_975 = quantile(p_radical, 0.975)),
            by = .(df, n)]
ans

# Pick out the columns you need for the graph, turn into data.frame
df <- data.frame(ans[, .(df, n, true, q_025, q_25, median, q_75, q_975)])
df$df <- as.factor(df$df)
df$n <- as.factor(df$n)
df$x <- 1
df$y <- 1

# Actual plot for paper

g <- ggplot(df, aes(y)) +
        geom_boxplot(aes(ymin = q_025, lower = q_25, middle = median, upper = q_75,
            ymax = q_975), width = 0.03, stat = "identity") + 
        geom_point(aes(x = 1, y = true, col = "red")) + coord_flip() +
        ylim(c(0,1)) + xlim(c(0.9, 1.1)) +
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.ticks.y = element_blank()) +
        theme(axis.text.y = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
        theme(axis.text=element_text(size = 8, face = "bold")) +
        facet_grid(df ~ n, labeller = label_both)


# Change the font on the facet strips
g + theme(strip.text.x = element_text(size = 10, face = "bold"),
          strip.text.y = element_text(size = 10, face = "bold"))

ggsave("bootplot.png", width = 8, height = 2.5, dpi = 300)


###########################################################
## THIRD DATA SET ########################################
###########################################################

n100 <- data.frame(readRDS("Data sets/n100_3.Rds"))
n500 <- data.frame(readRDS("Data sets/n500_3.Rds"))
n1000 <- data.frame(readRDS("Data sets/n1000_3.Rds"))

varnames <- c("boot_reps", "n", "df", "lbf", "var_lbf", "p_radical")

colnames(n500) <- varnames
colnames(n1000) <- varnames
colnames(n100) <- varnames
bdf <- rbind(n100, n500, n1000)

head(bdf)


bdf <- data.table(bdf)
ans <- bdf[, .(true_var = var(lbf),
               mean_var_lbf = mean(var_lbf),
               var_var_lfb = var(var_lbf),
               true = tabulate(as.numeric(abs(lbf) > 5)) / .N,
               mean_p_radical =  mean(p_radical),
               var_p_radical = var(p_radical),
               median = median(p_radical),
               q_25 = quantile(p_radical, 0.25),
               q_75 = quantile(p_radical, 0.75),
               q_025 = quantile(p_radical, 0.025),
               q_975 = quantile(p_radical, 0.975)),
            by = .(df, n)]
ans <- cbind(ans, ans[, .(bias = mean_p_radical - true)])



# Pick out the columns you need for the graph, turn into data.frame
df <- data.frame(ans[, .(df, n, true, q_025, q_25, median, q_75, q_975)])
df$df <- as.factor(df$df)
df$n <- as.factor(df$n)
df$x <- 1
df$y <- 1

# Actual plot for paper

g <- ggplot(df, aes(y)) +
        geom_boxplot(aes(ymin = q_025, lower = q_25, middle = median, upper = q_75,
            ymax = q_975), width = 0.03, stat = "identity") + 
        geom_point(aes(x = 1, y = true, col = "red")) + coord_flip() +
        ylim(c(0,1)) + xlim(c(0.9, 1.1)) +
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.ticks.y = element_blank()) +
        theme(axis.text.y = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
        theme(axis.text=element_text(size = 8, face = "bold")) +
        facet_grid(df ~ n, labeller = label_both)


# Change the font on the facet strips
g + theme(strip.text.x = element_text(size = 10, face = "bold"),
          strip.text.y = element_text(size = 10, face = "bold"))

ggsave("bootplot.png", width = 8, height = 2.5, dpi = 300)

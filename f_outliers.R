library(fda)
library(ggplot2)

dat <- readRDS(file = "/Users/apple/Desktop/ISU 2019 fall/STAT547/final/clean_data_7_state.rds")
X <- as.matrix( dat[dat$state=="IL", -(1:6)] )
n <- nrow(X)
m <- ncol(X)
time <- seq(1996+4/12, 2019+9/12, length.out = m)


## functional depth
depth <- fbplot(fit = t(X), plot = FALSE)$depth
qt <- quantile(depth, 0.05)
label <- as.numeric(depth < qt) 


DD <- data.frame(x = rep(time, n), y = c(t(X)), 
                 metro = rep(dat[dat$state=="IL",]$metro, each = m),
                 id = rep(1:n, each = m),
                 outlier = rep(as.factor(label), each = m))




ggplot() + 
  geom_line(data = DD[DD$outlier==0,], 
            aes(x = x, y = y, group = id), alpha = 0.4) + 
  geom_line(data = DD[DD$outlier==1,], 
            aes(x = x, y = y, group = id, colour = metro), size = 1) +
  xlab("time") + 
  ylab("ZHVI Cumulative Return for IL") +
  guides(colour=guide_legend(title="Belong to Metro"))
  






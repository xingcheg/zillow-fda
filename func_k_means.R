library(ggplot2)
library(fda.usc)
library(ggmap)

dat <- readRDS(file = "/Users/apple/Desktop/ISU 2019 fall/STAT547/final/clean_data_7_state1.rds")

dat1 <- dat[dat$state=="IA", ]

time <- seq(2008+0/12, 2012 + 11/12, length.out = 60)

fclust_id <- kmeans.fd(dat1[,-(1:6)], ncl = 2, draw = FALSE)$clust


n <- nrow(dat1)
m <- length(time)

DD <- data.frame(x = rep(time, n), y = c(t(dat1[,-(1:6)])), 
                 clust = as.factor(rep(fclust_id, each = m)),
                 group = rep(1:n, each = m))



ggplot(data = DD) +
  geom_line(aes(x = x, y = y, group = group, colour = clust), alpha = 0.75) + 
  xlab("time") + 
  geom_smooth(aes(x = x, y = y), se = FALSE, colour = "black", size = 1) + 
  facet_wrap(~clust) + 
  ylab("ZHVI Rate of Change") + 
  scale_color_manual(values = c("red", "blue"))






plot_clust_geo <- function(dat, fclust_id){
  lon_range <- range(dat$lon)
  lat_range <- range(dat$lat)
  borders <- c(lon_range[1] - 0.5,
               lat_range[1] - 0.5, 
               lon_range[2] + 0.5,
               lat_range[2] + 0.5)
  geo_map <- get_stamenmap(bbox = borders, maptype = "toner-lite", zoom=6)
  p0 <- ggmap(geo_map)
  dd <- data.frame(lon = dat$lon, lat = dat$lat, label = fclust_id)
  p1 <- p0 + 
    geom_point(data = dd, aes(x = lon, y = lat, colour = as.factor(label)),
               size = 1.5) + 
    scale_color_manual(values = c("red", "blue"))
  p1
}


plot_clust_geo(dat1, fclust_id)


## IA: 2
## FL: 2
## IL: 2



library(ggplot2)
library(fdapace)
library(gridExtra)
library(ggmap)

dat <- readRDS(file = "/Users/apple/Desktop/ISU 2019 fall/STAT547/final/clean_data_7_state.rds")
nGrid <- ncol(dat) - 6
time <- 1:nGrid
nPlace <- nrow(dat)


#### FPCA (ignore spatial correlation) ####
simple_FPCA_state <- function(dat, state_name){
  input <- MakeFPCAInputs(tVec = time, 
                          yVec = as.matrix(dat[dat$state==state_name, -(1:6)]))
  fit <- FPCA(Ly = input$Ly, Lt = input$Lt,
                 optns = list(methodSelectK = "AIC",
                              methodXi = "IN"))
  return(fit)
}


fit_CA <- simple_FPCA_state(dat, "CA")
fit_FL <- simple_FPCA_state(dat, "FL")
fit_IA <- simple_FPCA_state(dat, "IA")
fit_IL <- simple_FPCA_state(dat, "IL")
fit_NY <- simple_FPCA_state(dat, "NY")
fit_TX <- simple_FPCA_state(dat, "TX")
fit_WA <- simple_FPCA_state(dat, "WA")


## plot mean funciton for the 7 states
mu_state <- data.frame(time = rep(time, 7),
                       state = rep(c("CA", "FL", "IA", "IL",
                                     "NY", "TX", "WA"), each = nGrid),
                       mu = c(fit_CA$mu, fit_FL$mu, fit_IA$mu, 
                              fit_IL$mu, fit_NY$mu, fit_TX$mu,
                              fit_WA$mu))


r_dat <- data.frame( state = rep(dat$state, each = nGrid),
                         id = rep(1:nPlace, each = nGrid),
                         time = rep(time, nPlace),
                         value = c( t(dat[,-(1:6)]) ) )


ggplot() + 
  geom_line(data = r_dat, aes(x = time, y = value, group = id), 
            size = 0.3) + 
  geom_line(data = mu_state, aes(x = time, y = mu), 
            colour = "red", size = 0.8) + 
  facet_wrap(~state, ncol = 3) + 
  ylab("ZHVI Cumulative Return")



## plot FVE for the 7 states
FVE_state <- data.frame(K = rep(1:10, 7),
                        state = rep(c("CA", "FL", "IA", "IL",
                                      "NY", "TX", "WA"), each = 10),
                        cumFVE = c(fit_CA$cumFVE[1:10], fit_FL$cumFVE[1:10], 
                                   fit_IA$cumFVE[1:10], fit_IL$cumFVE[1:10], 
                                   fit_NY$cumFVE[1:10], fit_TX$cumFVE[1:10],
                                   fit_WA$cumFVE[1:10]))

ggplot(data = FVE_state) + 
  geom_line(aes(x = K, y = cumFVE/100), linetype = 3) + 
  geom_point(aes(x = K, y = cumFVE/100), colour = "blue") + 
  geom_hline(yintercept = 0.95, colour = "red", linetype = 2) + 
  geom_hline(yintercept = 0.99, colour = "red", linetype = 2) + 
  facet_wrap(~state, ncol = 3) 



## plot eigen-functions for the 7 states
phi_state <- data.frame(time = rep(time, 7*3),
                       state = rep(c("CA", "FL", "IA", "IL",
                                     "NY", "TX", "WA"), each = nGrid*3),
                       phi_id = as.factor(rep(rep(1:3, each = nGrid), 7)),
                       phi = c( c(fit_CA$phi[,1:3]), c(fit_FL$phi[,1:3]),
                                c(fit_IA$phi[,1:3]), c(fit_IL$phi[,1:3]),
                                c(fit_NY$phi[,1:3]), c(fit_TX$phi[,1:3]),
                                c(fit_WA$phi[,1:3]) ))

ggplot(data = phi_state) + 
  geom_smooth(aes(x = time, y = phi, colour = phi_id), se = FALSE, size = 0.8,
              span = 0.25) + 
  facet_wrap(~state, ncol = 3) 



## spatial plots
plot_xi_geo <- function(dat, fit, state_name){
  dat1 <- dat[dat$state==state_name,]
  lon_range <- range(dat1$lon)
  lat_range <- range(dat1$lat)
  borders <- c(lon_range[1] - 0.5,
               lat_range[1] - 0.5, 
               lon_range[2] + 0.5,
               lat_range[2] + 0.5)
  geo_map <- get_stamenmap(bbox = borders, maptype = "toner-lite", zoom=6)
  p0 <- ggmap(geo_map)
  Xi_dat <- data.frame(lon = dat1$lon, lat = dat1$lat,
                       xi_1 = fit$xiEst[,1], xi_2 = fit$xiEst[,2],
                       xi_3 = fit$xiEst[,3])
  p1 <- p0 + 
    geom_point(data = Xi_dat, aes(x = lon, y = lat, colour = xi_1), 
               size = 1, alpha = 0.7) + 
    scale_color_continuous(low = "yellow", high = "red")
  p2 <- p0 + 
    geom_point(data = Xi_dat, aes(x = lon, y = lat, colour = xi_2), 
               size = 1, alpha = 0.7) + 
    scale_color_continuous(low = "yellow", high = "red")
  p3 <- p0 + 
    geom_point(data = Xi_dat, aes(x = lon, y = lat, colour = xi_3), 
               size = 1, alpha = 0.7) + 
    scale_color_continuous(low = "yellow", high = "red")
  grid.arrange(p1, p2, p3, ncol = 2)
}


plot_xi_geo(dat, fit_CA, 'CA')
plot_xi_geo(dat, fit_FL, 'FL')
plot_xi_geo(dat, fit_IA, 'IA')
plot_xi_geo(dat, fit_IL, 'IL')
plot_xi_geo(dat, fit_NY, 'NY')
plot_xi_geo(dat, fit_TX, 'TX')
plot_xi_geo(dat, fit_WA, 'WA')




## spatial analysis on FPCs
library(geoR)


## CA
geo_Xi_CA <- data.frame(lon = dat[dat$state=='CA',]$lon, 
                        lat = dat[dat$state=='CA',]$lat,
                        Xi1 = fit_CA$xiEst[,1])
geo_Xi_CA[,1:2] <- jitterDupCoords(geo_Xi_CA[,1:2], max = 0.01)
geo_Xi_CA <- as.geodata(geo_Xi_CA)

plot(variog4(geo_Xi_CA, trend="cte", estimator.type="classical",max.dist=5),
     lwd = 3)


## FL
geo_Xi_FL <- data.frame(lon = dat[dat$state=='FL',]$lon, 
                        lat = dat[dat$state=='FL',]$lat,
                        Xi1 = fit_FL$xiEst[,1])
geo_Xi_FL[,1:2] <- jitterDupCoords(geo_Xi_FL[,1:2], max = 0.01)
geo_Xi_FL <- as.geodata(geo_Xi_FL)

plot(variog4(geo_Xi_FL, trend="cte", estimator.type="classical",max.dist=4),
     lwd = 3)





## NY
geo_Xi_NY <- data.frame(lon = dat[dat$state=='NY',]$lon, 
                        lat = dat[dat$state=='NY',]$lat,
                        Xi2 = fit_NY$xiEst[,2])
geo_Xi_NY[,1:2] <- jitterDupCoords(geo_Xi_NY[,1:2], max = 0.01)
geo_Xi_NY <- as.geodata(geo_Xi_NY)

plot(variog4(geo_Xi_NY, trend="cte", estimator.type="classical",max.dist=4),
     lwd = 3)


## IL
geo_Xi_IL <- data.frame(lon = dat[dat$state=='IL',]$lon, 
                        lat = dat[dat$state=='IL',]$lat,
                        Xi2 = fit_IL$xiEst[,2])
geo_Xi_IL[,1:2] <- jitterDupCoords(geo_Xi_IL[,1:2], max = 0.01)
geo_Xi_IL <- as.geodata(geo_Xi_IL)

plot(variog4(geo_Xi_IL, trend="cte", estimator.type="classical",max.dist=3),
     lwd = 3)









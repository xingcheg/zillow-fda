setwd("/Users/apple/Desktop/ISU 2019 fall/STAT547/final")

library(ggplot2)
library(reshape2)
library(ggmap)

#### Read Data ####
ZHVI0 <- read.csv(file = "City_Zhvi_AllHomes.csv")
X0 <- ZHVI0[,-(1:6)]

# delete NA
ind_na <- which(apply(X0, 1, function(x){sum(is.na(x))})!=0)
ZHVI <- ZHVI0[-ind_na,]
X <- ZHVI[,-(1:6)]

RegionName <- ZHVI$RegionName
State <- ZHVI$State
Metro <- ZHVI$Metro
CountyName <- ZHVI$CountyName

X1 <- sweep(X, 1, X[,1], "-")
X2 <- sweep(X1, 1, X[,1], "/")
dat <- data.frame(state = State, region = RegionName, 
                  metro = Metro, county = CountyName, X2)

state_need <- c("CA", "FL", "IA", "IL", "NY", "TX", "WA")
dat1 <- dat[dat$state %in% state_need, ]

dat1$state <- as.character(dat1$state)
dat1$region <- as.character(dat1$region)
dat1$metro <- as.character(dat1$metro)
dat1$county <- as.character(dat1$county)


## CA
latlon_CA <- geocode(paste( dat1[dat1$state=="CA",]$region, ", CA", sep = ""), 
                         output = 'latlon')
# save(latlon_CA, file = "latlon/latlon_CA.RData")

## FL
latlon_FL <- geocode(paste( dat1[dat1$state=="FL",]$region, ", FL", sep = ""), 
                     output = 'latlon')
# save(latlon_FL, file = "latlon/latlon_FL.RData")

## IA
latlon_IA <- geocode(paste( dat1[dat1$state=="IA",]$region, ", IA", sep = ""), 
                     output = 'latlon')
# save(latlon_IA, file = "latlon/latlon_IA.RData")

## IL
latlon_IL <- geocode(paste( dat1[dat1$state=="IL",]$region, ", IL", sep = ""), 
                     output = 'latlon')
# save(latlon_IL, file = "latlon/latlon_IL.RData")

## NY
latlon_NY <- geocode(paste( dat1[dat1$state=="NY",]$region, ", NY", sep = ""), 
                     output = 'latlon')
# save(latlon_NY, file = "latlon/latlon_NY.RData")

## TX
latlon_TX <- geocode(paste( dat1[dat1$state=="TX",]$region, ", TX", sep = ""), 
                     output = 'latlon')
# save(latlon_TX, file = "latlon/latlon_TX.RData")

## WA
latlon_WA <- geocode(paste( dat1[dat1$state=="WA",]$region, ", WA", sep = ""), 
                     output = 'latlon')
# save(latlon_WA, file = "latlon/latlon_WA.RData")




dat2 <- data.frame(dat1[,1:4], lon = rep(NA, nrow(dat1)), lat = rep(NA, nrow(dat1)),
                   dat1[-(1:4)])

dat2[dat2$state=="CA",5:6] <- latlon_CA
dat2[dat2$state=="FL",5:6] <- latlon_FL
dat2[dat2$state=="IA",5:6] <- latlon_IA
dat2[dat2$state=="IL",5:6] <- latlon_IL
dat2[dat2$state=="NY",5:6] <- latlon_NY
dat2[dat2$state=="TX",5:6] <- latlon_TX
dat2[dat2$state=="WA",5:6] <- latlon_WA


saveRDS(dat2, file = "clean_data_7_state.rds")




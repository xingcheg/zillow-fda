setwd("/Users/apple/Desktop/ISU 2019 fall/STAT547/final")



#### Read Data ####
ZHVI0 <- read.csv(file = "City_Zhvi_AllHomes.csv")
X0 <- ZHVI0[,-(1:6)]

# delete NA
ind_na <- which(apply(X0, 1, function(x){sum(is.na(x))})!=0)
ZHVI <- ZHVI0[-ind_na,]
X <- ZHVI[,147:207]

RegionName <- ZHVI$RegionName
State <- ZHVI$State
Metro <- ZHVI$Metro
CountyName <- ZHVI$CountyName

X1 <- X[,-1] - X[-ncol(X)]
X2 <- X1/X[-ncol(X)]
dat <- data.frame(state = State, region = RegionName, 
                  metro = Metro, county = CountyName, X2)

state_need <- c("CA", "FL", "IA", "IL", "NY", "TX", "WA")
dat1 <- dat[dat$state %in% state_need, ]

dat1$state <- as.character(dat1$state)
dat1$region <- as.character(dat1$region)
dat1$metro <- as.character(dat1$metro)
dat1$county <- as.character(dat1$county)


load("latlon/latlon_CA.RData")
load("latlon/latlon_FL.RData")
load("latlon/latlon_IA.RData")
load("latlon/latlon_IL.RData")
load("latlon/latlon_NY.RData")
load("latlon/latlon_TX.RData")
load("latlon/latlon_WA.RData")


dat2 <- data.frame(dat1[,1:4], lon = rep(NA, nrow(dat1)), lat = rep(NA, nrow(dat1)),
                   dat1[-(1:4)])

dat2[dat2$state=="CA",5:6] <- latlon_CA
dat2[dat2$state=="FL",5:6] <- latlon_FL
dat2[dat2$state=="IA",5:6] <- latlon_IA
dat2[dat2$state=="IL",5:6] <- latlon_IL
dat2[dat2$state=="NY",5:6] <- latlon_NY
dat2[dat2$state=="TX",5:6] <- latlon_TX
dat2[dat2$state=="WA",5:6] <- latlon_WA


saveRDS(dat2, file = "clean_data_7_state1.rds")


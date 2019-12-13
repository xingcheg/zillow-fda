# install.packages("fdANOVA")
library(fdANOVA)
dat <- readRDS(file = "/Users/apple/Desktop/ISU 2019 fall/STAT547/final/clean_data_7_state.rds")

dat1 <- dat[dat$state %in% c("CA", "FL"), ]
dat2 <- dat[dat$metro %in% c("San Francisco-Oakland-Hayward", 
                             "San Jose-Sunnyvale-Santa Clara"), ]
dat3 <- dat[dat$metro %in% c("Des Moines-West Des Moines", 
                             "Cedar Rapids"), ]
dat4 <- dat[dat$metro %in% c("Ames", 
                             "Iowa City"), ]



fanova.tests(x = t(dat1[,-(1:6)]), group.label = dat1$state,
             test = "FB")

fanova.tests(x = t(dat2[,-(1:6)]), group.label = dat2$metro,
             test = "FB")

fanova.tests(x = t(dat3[,-(1:6)]), group.label = dat3$metro,
             test = "FB")

fanova.tests(x = t(dat4[,-(1:6)]), group.label = dat4$metro,
             test = "FB")

















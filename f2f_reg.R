library(fdapace)
library(ggplot2)

setwd("/Users/apple/Desktop/ISU 2019 fall/STAT547/final")
load("ZHVI2.RData")
load("House_income2.RData")

X <- log( as.matrix( House_income2[,-1] ) )
Y <- log( as.matrix( ZHVI2[,-1] ) )
time <- seq(1996+1/4, 2018+3/4, length.out = 91)

matplot(time, t(X), type = "l", ylab = "log(House Income)")
matplot(1:91, t(Y), type = "l", ylab = "log(ZHVI)")

xx <- apply(X, 1, mean)
yy <- apply(Y, 1, mean)
plot(xx,yy)


X_list <- MakeFPCAInputs(tVec = 1:91, yVec = X) 
Y_list <- MakeFPCAInputs(tVec = 1:91, yVec = Y) 

X_list1 <- list(Lt = X_list$Lt, Ly = X_list$Ly)
Y_list1 <- list(Lt = Y_list$Lt, Ly = Y_list$Ly)

FPC_res <- FPCReg(vars = list(X1 = X_list1, Y = Y_list1))
tGrid_fine <- seq(1996+1/4, 2018+3/4, length.out = 51)


beta <- FPC_res$estiBeta
image(tGrid_fine, tGrid_fine, beta$betaX1Y, main = "beta1")


r_beta <- data.frame(t = rep(tGrid_fine, 51), 
                     s = rep(tGrid_fine, each = 51),
                     z = c(beta$betaX1Y))

ggplot(data = r_beta) + 
  geom_raster(aes(x = t, y = s, fill = z)) + 
  scale_fill_gradient2(low = "blue", high = "red")







library(fdapace)

######################## Simulation Based Method for fSCB #######################
fSCB <- function(X1, X2, alpha = 0.05){
  ## dimensions
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  r <- n1/n2
  m <- ncol(X1)
  ## mean and covariance functions
  mu1 <- apply(X1, 2, mean)
  mu2 <- apply(X2, 2, mean)
  G1 <- cov(X1)
  G2 <- cov(X2)
  diag_G12 <- diag(G1) + r * diag(G2) + 1e-4
  ## eigenvalues and eigenfunctions
  eig1 <- eigen(G1)
  eig2 <- eigen(G2)
  lam1 <- eig1$values / m 
  lam2 <- eig2$values / m 
  phi1 <- eig1$vectors * sqrt(m)
  phi2 <- eig2$vectors * sqrt(m)
  ## generate Gaussian processes
  N_simu <- 1000
  K1 <- which( cumsum(lam1)/sum(lam1) > 0.999 )[1]
  K2 <- which( cumsum(lam2)/sum(lam2) > 0.999 )[1]
  generate_GP <- function(lam, phi, K){
    fpc <- rnorm(K, 0, 1)
    gp <- apply(fpc * sqrt(lam[1:K]) * t(phi[,1:K]), 2, sum)
    return(gp)
  }
  GP1 <- replicate(N_simu, generate_GP(lam1, phi1, K1))
  GP2 <- replicate(N_simu, generate_GP(lam2, phi2, K2))
  GP_use <- ( GP1 + sqrt(r) * GP2 ) / sqrt( diag_G12 )
  GP_max <- apply(abs(GP_use), 2, max)
  ## compute SCB
  Q12 <- quantile(GP_max, probs = 1 - alpha)
  cent <- mu1 - mu2
  upper <- cent + Q12 * diag_G12 / sqrt(n1)
  lower <- cent -  Q12 * diag_G12 / sqrt(n1)
  return(data.frame(cent, upper, lower))
}





library(ggplot2)

dat <- readRDS(file = "/Users/apple/Desktop/ISU 2019 fall/STAT547/final/clean_data_7_state.rds")

time <- seq(1996+4/12, 2019+9/12, length.out = 282)



## Ames vs Iowa City
X1 <- as.matrix( dat[dat$metro=="Ames", -(1:6)] )
X2 <- as.matrix( dat[dat$metro=="Iowa City", -(1:6)] )

out1 <- fSCB(X1, X2, 0.05)



## Ames vs Des Moines-West Des Moines
X1 <- as.matrix( dat[dat$metro=="Ames", -(1:6)] )
X2 <- as.matrix( dat[dat$metro=="Des Moines-West Des Moines", -(1:6)] )

out2 <- fSCB(X1, X2, 0.05)



## San Francisco-Oakland-Hayward vs San Jose-Sunnyvale-Santa Clara
X1 <- as.matrix( dat[dat$metro=="San Francisco-Oakland-Hayward", -(1:6)] )
X2 <- as.matrix( dat[dat$metro=="San Jose-Sunnyvale-Santa Clara", -(1:6)] )

out3 <- fSCB(X1, X2, 0.05)



## San Francisco-Oakland-Hayward vs Los Angeles-Long Beach-Anaheim
X1 <- as.matrix( dat[dat$metro=="San Francisco-Oakland-Hayward", -(1:6)] )
X2 <- as.matrix( dat[dat$metro=="Los Angeles-Long Beach-Anaheim", -(1:6)] )

out4 <- fSCB(X1, X2, 0.05)



## CA vs FL
X1 <- as.matrix( dat[dat$state=="CA", -(1:6)] )
X2 <- as.matrix( dat[dat$state=="FL", -(1:6)] )

out5 <- fSCB(X1, X2, 0.05)



## CA vs WA
X1 <- as.matrix( dat[dat$state=="CA", -(1:6)] )
X2 <- as.matrix( dat[dat$state=="WA", -(1:6)] )

out6 <- fSCB(X1, X2, 0.05)




DD <- data.frame(time = rep(time, 6),
                 cent = c(out1$cent, out2$cent, out3$cent, 
                          out4$cent, out5$cent, out6$cent),
                 upper = c(out1$upper, out2$upper, out3$upper, 
                           out4$upper, out5$upper, out6$upper),
                 lower = c(out1$lower, out2$lower, out3$lower, 
                           out4$lower, out5$lower, out6$lower),
                 label = rep(c("Ames vs Iowa City", 
                               "Ames vs Des Moines",
                               "San Francisco vs San Jose",
                               "San Francisco vs Los Angeles",
                               "CA vs FL",
                               "CA vs WA"), 
                             each = length(time)) )


ggplot(data = DD) + 
  geom_line(aes(x = time, y = cent), size = 1) + 
  geom_line(aes(x = time, y = lower), size = 0.5, colour = "blue") + 
  geom_line(aes(x = time, y = upper), size = 0.5, colour = "blue") + 
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), fill = "blue",
              alpha = 0.3) + 
  geom_hline(yintercept = 0, colour = "red", linetype = 2, size = 1) + 
  facet_wrap(~label, scales = "free_y", ncol = 2) + 
  ylab("mu1 - mu2") + 
  theme_bw()



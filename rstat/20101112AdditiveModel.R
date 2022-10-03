#data loading
 source("/Users/ushima/Documents/Work/R/packCER.R")
 d.meta <- read.csv("/Users/ushima/Documents/Work/Result/2009- meta analysis/200907 Tannin meta analysis/Statistics/20091121TMACompile.csv")
 d <- read.csv("/Users/ushima/Documents/Work/Result/2009- meta analysis/200907 Tannin meta analysis/Statistics/20091121TMAdata.csv")
  

#plot overall pattern
 plot(d$CT~d$LAT, pch=21, bg="orange", cex=1.5)
 plot(d$CT~d$MAT, pch=21, bg="orange", cex=1.5)

#load library
 library(mapdata)
 library(mapproj)
 library(maps)

#read lat&long data
 site <- read.csv("/Users/ushima/Documents/Work/Result/2009- meta analysis/200907 Tannin meta analysis/Statistics/SiteData.csv")
 LAT <- site$LATcent
 LOT1 <- site$LOTcent
 LOT2 <- site$LOTcent2

#draw maps
 map('world', interior=F, lty=1, proj='aitoff'); map.grid(col="black")
 map('world', interior=F, lty=1)
 map.axes()
 points(LOT1, LAT, cex=1, pch=21, col="black", bg="red")




 #Load packages
 library(mgcv); library(nlme)
 
 #GAMM modeling
 M1 <- gam(CT ~ s(MAT), data=d)
 E <- resid(M1)
 F <- fitted(M1)
 summary(M1)
 
 op <- par(mfrow = c(2,1), mar = c(5,4,1,1))
 plot(M1)
 plot(F, E, xlab = "Fitted values", ylab = "Residuals")
 par(op)
 
 lmc <- lmeControl(niterEM = 25, msMaxIter=3000) #condition setting
 f1 <- formula(CT ~ s(MAT))
  
 M1.1 <- gamm(f1, random=list(Site =~1),
              method = "REML", control = lmc, data = d)
 M1.2 <- gamm(f1, random=list(Site =~1),
              method = "REML", control = lmc, data = d,
              weights = varIdent(form =~1 | Site))
 M1.3 <- gamm(f1, random=list(Site =~1),
              method = "REML", control = lmc, data = d,
              weights = varPower(form = ~ MAT))

 AIC(M1.1$lme, M1.2$lme, M1.3$lme)
 anova(M1.1$lme, M1.2$lme, M1.3$lme) 

 

#M1.2 is the best model!! AIC=8535.373
 summary(M1.2); plot(M1.2$gam); summary(M1.2$gam)
 P <- predict(M1.2$gam, se=TRUE)
 plot(M1.2$lme)

 plot(d$CT~d$MAT, ylim=range(0, 700), pch=21, cex=1, bg="orange", xlab="MAT", ylab="CT")
 I1 <- order(d$MAT);
 lines(d$MAT[I1], P$fit[I1], lty = 1, lwd=4)
 lines(d$MAT[I1], P$fit[I1] + 2 * P$se[I1], lty = 2, lwd=2)
 lines(d$MAT[I1], P$fit[I1] - 2 * P$se[I1], lty = 2, lwd=2)
 
 
 M2 <- lm(CT ~ MAT, data=d)
 M2$fitted  
 M2$resid
 str(M2)
  

#load data
 d <- read.csv("/Users/ushima/Documents/Work/Seminar/2010年度/20101029 LinearModel/20101029LinearModel.csv")


#matrix
 d$col1 <- 0
 fix(d); d[,-9] #how to remove a column from a matrix

#F-distribution
 curve(df(x,5,15), xlim=range(0,7), type="l")

#residuals plot
 x <- (40:90)/10; y <- sin(x)
 plot(x,y, cex=5, pch=21, col="red", bg=4)
 x.lm <- lm(y~x); abline(x.lm, lwd=4); summary(x.lm)
 plot(x.lm$fitted, x.lm$residuals, pch=21, cex=0.5)
 abline(h=0); hist(x.lm$residuals)

#symbol and color list
 op1 <- par(mfrow=c(2,1)); op2 <- par(mar=c(2,2,2,2))
 plot(c(rep(1:10,2),1:5), c(rep(1,10),rep(2,10),rep(3,5)), pch=c(1:25),
      cex=2, ylim=range(0.8,3.2), tck=0.05)
 plot(1:8, rep(1,8), pch=21, bg=c(1:8), cex=3, tck=0.05)
 par(op1); par(op2) 


 
#one-way ANOVA
 plot(d)
 library(AED)
 pairs(d,lower.panel=panel.smooth2, upper.panel=panel.cor,diag.panel=panel.hist)
 boxplot(d$apa~d$tree,main="Acid phosphatase",xlab="tree",ylab="activity")
 
 apa.lm.1 <- lm(apa~tree,data=d); summary(apa.lm.1); anova(apa.lm.1)
 apa.lm.2 <- aov(apa~tree,data=d); summary(apa.lm.2); anova(apa.lm.2)
 AIC(apa.lm.1, apa.lm.2)
 apa.lm.3 <- aov(apa~tree+category, data=d); summary(apa.lm.3)
 apa.lm.4 <- aov(apa~category+tree, data=d); summary(apa.lm.4)

#Linear regression 
 plot(d$n~d$c, main="N-C", xlab="C (%)", ylab="N (%)")
 nc.lm <- lm(n~c,data=d)
 summary(nc.lm)
 abline(nc.lm)
 
 plot(d$n~d$c, main="N-C", xlab="C (%)", ylab="N (%)", pch=21, bg=3)
 abline(nc.lm, lty=3)
 text(30,2.0, "P < 0.001")

 plot(nc.lm$fitted, nc.lm$residuals, pch=16); abline(h=0)
 hist(nc.lm$residuals)


#two-way ANOVA
#Ishikawa data
 di <- read.csv("/Users/ushima/Documents/Work/Seminar/2010年度/20101029 LinearModel/data_ishikawa.csv")
 pairs(di,lower.panel=panel.smooth2, upper.panel=panel.cor,diag.panel=panel.hist)
 di.aov.1 <- aov(d13C~light+flow+light:flow, data=di)
 summary(di.aov.1); plot(di.aov.1$fitted, di.aov.1$residuals)

 di.aov.2 <- aov(d13C~light+flow, data=di)
 summary(di.aov.2); plot(di.aov.2$fitted, di.aov.2$residuals)
 AIC(di.aov.1, di.aov.2)

 boxplot(d13C~light:flow, data=di, col="green", main="Ishikawa data")


#ANCOVA
 di.aov.2 <- aov(d15N~light*d13C, data=di)
 summary(di.aov.2)
 plot(d15N~d13C, data=di, bg=c(3,2)[unclass(di$light)], pch=21, main="Ishikawa data")
 abline(lm(d15N~d13C, subset(di,light=="open")), col=3, lty=3, lwd=5)
 abline(lm(d15N~d13C, subset(di,light=="shaded")), col=2, lty=3)

 subset(di,light!="open")

#post-hoc
 l <- read.csv("/Users/ushima/Documents/Work/Seminar/2010年度/20101029 LinearModel/LipidData.csv")
 boxplot(l$conc~l$lipid) 
 
 TukeyHSD(apa.lm.1) #cannot run
 TukeyHSD(apa.lm.2) #significant difference between Dacrydium and Lithocarpus
 TukeyHSD(di.aov.1)

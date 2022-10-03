#package download
 options(CRAN="http://cran.md.tsukuba.ac.jp")
 Sys.setenv("http_proxy"="http://proxy.kuins.net:8080")
 
#loading data
 d <- read.csv("/Users/ushima/Documents/Work/Seminar/2010年度/20101022 BasicStat/20101022SESdemo.csv")
 
#handle matrix
 d[,1]; d[,2]; d[,3]; d[,4]; d[,5]
 summary(d); tail(d,5); head(d,10); fix(d)
 d$id; d$ind; d$tree; d$depth; d$ct

 plot(d)
 boxplot(d$ct~d$tree)
 boxplot(ct~tree,data=d)
 boxplot(d[,5]~d[,3])


#analysis
 hist(d$ct)
 shapiro.test(d$ct) # <- from R-source

 d$ct.log <- log(d$ct,10)
 d$ct.log
 hist(d$ct.log)
 shapiro.test(d$ct.log) # <- from R-source

#linear mixed model (two fixed factor, one random factor)
 library(nlme); help(package="nlme"); ?lme
 d.lme <- lme(ct.log~tree*depth, random=~1|ind, data=d)
 anova(d.lme)
 plot(d.lme)

#lm
 d.lm <- lm(ct~tree*depth,data=d)
 anova(d.lm)
 summary(d.lm)
 plot(d.lm$fitted,d.lm$residuals)
 
 
#citation
 citation()
 citation(package="nlme")
 
#multiple figures
 par(mfrow=c(2,2))
 curve(dnorm,-4,4,type="l") #normal distribution
 plot(0:10,dbinom(0:10,10,0.5),type="h",lwd=5) #binominal
 plot(0:10,dnbinom(0:10,10,0.5),type="h",lwd=5) #negative binominal
 plot(0:10,dpois(0:10,10,0.5),type="h",lwd=5) #poisson
 dev.off()
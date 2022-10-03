

setwd("D:/Users/TFHaraguchi/Documents/bayes/101203seminar")
dat<-read.table("boatrace.txt",header=T)
#データの整形
dat<-dat[4:length(dat$year),]
y<-as.vector(dat$iscambridge)
N<-length(y)
in1<-in2<-in3<-list(tau=1)

data<-list("N","y")
parameters<-c("tau","p","Isigma")
inits<-list(in1,in2,in3)


library(R2WinBUGS)

#setwd("D:/Users/TFHaraguchi/Documents/bayes/101203seminar")
boat.res<-bugs(
data,inits,parameters,model.file="model_race.txt"
,debug=T,n.chains=3,n.iter=30000,n.burnin=1000,n.thin=100,
bugs.directory="D:/Users/TFHaraguchi/Documents/bayes/WinBUGS14",
working.directory=getwd())

names(boat.res)
print(boat.res,digit=3)

#収束判定*chainの平均のサンプリング値
plot(as.mcmc(boat.res$sims.matrix))
#収束判定*各chainごとのサンプリング値
plot(
mcmc.list(
lapply(1:boat.res$n.chains, #1からres$n.chainsまで（要は指定したchain数
function(x) as.mcmc(boat.res$sims.array[, x, ]))))
#値の分布をmedianと80パーセンタイルで表すグラフ
#3つのchainは収束？・Rハットは1.05以下？
plot(boat.res)
plot(dat$year,dat$iscambridge)
names(boat.res)
summary<-as.data.frame(boat.res$summary)
plot(dat$year[4:174],boat.res$last.values[[1]]$p,type="l",lwd=2,ylim=c(0,1),xlim=c(1830,2010),ann=F,axes=F)
par(new=T)
plot(dat$year[1:length(dat$year)],summary$"97.5%"[2:172],type="l",col="pink",lwd=2,ylim=c(0,1),xlim=c(1830,2010),ann=F,axes=F)
par(new=T)
plot(dat$year[1:length(dat$year)],summary$"2.5%"[2:172],type="l",col="pink",lwd=2,ylim=c(0,1),xlim=c(1830,2010),ann=F,axes=F)
par(new=T)
plot(dat$year,dat$iscambridge,ylim=c(0,1),xlim=c(1830,2010))


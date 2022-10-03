
library(R2WinBUGS)
setwd("D:/Users/TFHaraguchi/Documents/bayes/101203seminar/script_naoe")
## 以下３行をいじる
sp <- "ao"
yr <- "07"
sps <- c("ao", "uwa", "mizuki")
## sps <- c("ao", "uwa", "kasumi", "mizuki", "kosi", "tuta")

## コードや結果の出力先
modeltxt <- paste(sp,yr,"_05bugs.txt",sep="")
output <- paste(sp,yr,"_05result.csv",sep="")
script <- paste(sp,yr,"_05script.txt",sep="")

## データの読み込み
file <- paste("all",yr,"sum.csv",sep="")
d <- read.csv(file)
n.plot <- nrow(d)

## 樹種数、目的変数名
n.sp <- length(sps)
target <- paste(sp,"bd",sep="")


## car.normal用
spneigh<-function(x,y,xrange=10,yrange=10){
       num<-length(x)
       cell<-data.frame(x=x,y=y,a=1:num)
       nearest<-nnear<-c(NULL)

       for(i in 1:num){
               subcell<-subset(cell,(cell$x<=cell[i,]$x+xrange)&
                       (cell$x>=cell[i,]$x-xrange)&
                       (cell$y<=cell[i,]$y+yrange)&
                       (cell$y>=cell[i,]$y-yrange)&
                       (cell$a!=cell[i,]$a))
               nearest<-append(nearest,subcell$a)
               nnear<-append(nnear,nrow(subcell))
               if(i%%100==0){print(paste(round(i/num*100),"% finished...",sep=""))}
       }
       print("100% finished!")
       return(list(Adj=nearest,Num=nnear))
}
x <- d$x
y <- d$y
result<-spneigh(x,y)
adj <- result[[1]]
num <- result[[2]]
weight <- rep(1, sum(num))


## bugsコード
model1 <- function() {
 for (i in 1:n.plot) {
## 各樹種の自然落下のcar.normal
   exp.re1[i] <- exp(re1[i])
   exp.re2[i] <- exp(re2[i])
   exp.re3[i] <- exp(re3[i])
##     exp.re4[i] <- exp(re4[i])
##     exp.re5[i] <- exp(re5[i])
##     exp.re6[i] <- exp(re6[i])
## 各樹種の自然落下の期待値
   m.nf1[i] <- inc1 * exp.re1[i]
   m.nf2[i] <- inc2 * exp.re2[i]
   m.nf3[i] <- inc3 * exp.re3[i]
##     m.nf4[i] <- inc4 * exp.re4[i]
##     m.nf5[i] <- inc5 * exp.re5[i]
##     m.nf6[i] <- inc6 * exp.re6[i]
## 各樹種の自然落下の観測値はポアソン乱数からの抽出
   nf1[i] ~ dpois(m.nf1[i])
   nf2[i] ~ dpois(m.nf2[i])
   nf3[i] ~ dpois(m.nf3[i])
##     nf4[i] ~ dpois(m.nf4[i])
##     nf5[i] ~ dpois(m.nf5[i])
##     nf6[i] ~ dpois(m.nf6[i])
## 目的樹種の鳥散布のcar.normal
   exp.re[i] <- exp(re[i])
## 各樹種の自然落下による上乗せ
   p1[i] <- a1 * m.nf1[i]
   p2[i] <- a2 * m.nf2[i]
   p3[i] <- a3 * m.nf3[i]
##     p4[i] <- a4 * nf4[i]
##     p5[i] <- a5 * nf5[i]
##     p6[i] <- a6 * nf6[i]
## 目的樹種の鳥散布の期待値
   m.y[i] <- inc * exp.re[i] + p1[i] + p2[i] + p3[i]
## 目的樹種の鳥散布の観測値はポアソン乱数からの抽出
   y[i] ~ dpois(m.y[i])
 }
 ## car.normal
 re[1:n.plot]  ~ car.normal(adj[], weight[], num[], tau)
 re1[1:n.plot] ~ car.normal(adj[], weight[], num[], tau1)
 re2[1:n.plot] ~ car.normal(adj[], weight[], num[], tau2)
 re3[1:n.plot] ~ car.normal(adj[], weight[], num[], tau3)
##   re4[1:n.plot] ~ car.normal(adj[], weight[], num[], tau4)
##   re5[1:n.plot] ~ car.normal(adj[], weight[], num[], tau5)
##   re6[1:n.plot] ~ car.normal(adj[], weight[], num[], tau6)
 ## 無情報事前分布
 tau ~ dgamma(0.001, 0.001)
 tau1 ~ dgamma(0.001, 0.001)
 tau2 ~ dgamma(0.001, 0.001)
 tau3 ~ dgamma(0.001, 0.001)
##   tau4 ~ dgamma(0.001, 0.001)
##   tau5 ~ dgamma(0.001, 0.001)
##   tau6 ~ dgamma(0.001, 0.001)          
 a1 ~ dlnorm(0, 0.01)
 a2 ~ dlnorm(0, 0.01)
 a3 ~ dlnorm(0, 0.01)
##   a4 ~ dlnorm(0, 0.01)
##   a5 ~ dlnorm(0, 0.01)
##   a6 ~ dlnorm(0, 0.01)
 inc  ~ dlnorm(myu,  sigma)
 inc1 ~ dlnorm(myu1, sigma1)
 inc2 ~ dlnorm(myu2, sigma2)
 inc3 ~ dlnorm(myu3, sigma3)
##   inc4 ~ dlnorm(myu4, sigma4)
##   inc5 ~ dlnorm(myu5, sigma5)
##   inc6 ~ dlnorm(myu6, sigma6)
}
## bugsコード保存
write.model(model1, modeltxt)


## 目的変数
y <- d[,paste(sp,"bd",sep="")]

## 説明変数
explanatory <- c()
for (i in 1:n.sp) {
 explanatory <- c(explanatory, paste(sps[i],"nf",sep=""))
}
nf1 <- d[,explanatory[1]]
nf2 <- d[,explanatory[2]]
nf3 <- d[,explanatory[3]]
## nf4 <- d[,explanatory[4]]
## nf5 <- d[,explanatory[5]]
## nf6 <- d[,explanatory[6]]


## dlnorm(myu, sigma)
## mean(y)が平均値となる対数正規分布のパラメータmyu
myu <- log(mean(y)*mean(y) / sqrt(var(y)+mean(y)*mean(y)))
myu1<- log(mean(nf1)*mean(nf1) / sqrt(var(nf1)+mean(nf1)*mean(nf1)))
myu2<- log(mean(nf2)*mean(nf2) / sqrt(var(nf2)+mean(nf2)*mean(nf2)))
myu3<- log(mean(nf3)*mean(nf3) / sqrt(var(nf3)+mean(nf3)*mean(nf3)))
## myu4<- log(mean(nf4)*mean(nf4) / sqrt(var(nf4)+mean(nf4)*mean(nf4)))
## myu5<- log(mean(nf5)*mean(nf5) / sqrt(var(nf5)+mean(nf5)*mean(nf5)))
## myu6<- log(mean(nf6)*mean(nf6) / sqrt(var(nf6)+mean(nf6)*mean(nf5)))

## sd(y)が標準偏差となる対数正規分布のパラメータsigma。
## ただし、値が暴れないように10をかけて分散を小さくする。
## パソコンの性能がよく、早く収束するようなら、「*10」は不要かも。
sigma <- sqrt(log(var(y)/(mean(y)*mean(y)) + 1))*10
sigma1 <- sqrt(log(var(nf1)/(mean(nf1)*mean(nf1)) + 1))*10
sigma2 <- sqrt(log(var(nf2)/(mean(nf2)*mean(nf2)) + 1))*10
sigma3 <- sqrt(log(var(nf3)/(mean(nf3)*mean(nf3)) + 1))*10
## sigma4 <- sqrt(log(var(nf4)/(mean(nf4)*mean(nf4)) + 1))*10
## sigma5 <- sqrt(log(var(nf5)/(mean(nf5)*mean(nf5)) + 1))*10
## sigma6 <- sqrt(log(var(nf6)/(mean(nf6)*mean(nf6)) + 1))*10


## 観測データのリスト
data <- list(  "n.plot","y", "adj", "num", "weight"
            ,"nf1"
            ,"nf2"
            ,"nf3"
##              ,"nf4"
##              ,"nf5"
##              ,"nf6"
            , "myu" , "sigma"
            , "myu1", "sigma1"
            , "myu2", "sigma2"
            , "myu3", "sigma3"
##              , "myu4", "sigma4"
##              , "myu5", "sigma5"
##              , "myu6", "sigma6"
            )


## 初期値を用意
in1 <- list(  inc =rpois(1, 10), re =rnorm(n.plot, 0, 0.1), tau =1
           , inc1=rpois(1, 10), re1=rnorm(n.plot, 0, 0.1), tau1=1, a1=1
           , inc2=rpois(1, 10), re2=rnorm(n.plot, 0, 0.1), tau2=1, a2=1
           , inc3=rpois(1, 10), re3=rnorm(n.plot, 0, 0.1), tau3=1, a3=1
##             , inc4=rpois(1, 10), re4=rnorm(n.plot, 0, 0.1), tau4=1, a4=1
##             , inc5=rpois(1, 10), re5=rnorm(n.plot, 0, 0.1), tau5=1, a5=1
##             , inc6=rpois(1, 10), re6=rnorm(n.plot, 0, 0.1), tau6=1, a6=1
           )
in2 <- list(  inc =rpois(1, 10), re =rnorm(n.plot, 0, 0.1), tau =1
           , inc1=rpois(1, 10), re1=rnorm(n.plot, 0, 0.1), tau1=1, a1=1
           , inc2=rpois(1, 10), re2=rnorm(n.plot, 0, 0.1), tau2=1, a2=1
           , inc3=rpois(1, 10), re3=rnorm(n.plot, 0, 0.1), tau3=1, a3=1
##             , inc4=rpois(1, 10), re4=rnorm(n.plot, 0, 0.1), tau4=1, a4=1
##             , inc5=rpois(1, 10), re5=rnorm(n.plot, 0, 0.1), tau5=1, a5=1
##             , inc6=rpois(1, 10), re6=rnorm(n.plot, 0, 0.1), tau6=1, a6=1
           )
in3 <- list(  inc =rpois(1, 10), re =rnorm(n.plot, 0, 0.1), tau =1
           , inc1=rpois(1, 10), re1=rnorm(n.plot, 0, 0.1), tau1=1, a1=1
           , inc2=rpois(1, 10), re2=rnorm(n.plot, 0, 0.1), tau2=1, a2=1
           , inc3=rpois(1, 10), re3=rnorm(n.plot, 0, 0.1), tau3=1, a3=1
##             , inc4=rpois(1, 10), re4=rnorm(n.plot, 0, 0.1), tau4=1, a4=1
##             , inc5=rpois(1, 10), re5=rnorm(n.plot, 0, 0.1), tau5=1, a5=1
##             , inc6=rpois(1, 10), re6=rnorm(n.plot, 0, 0.1), tau6=1, a6=1
           )
init <- list(in1, in2, in3)

## 推定値を保存したいパラメータのリスト
para <- list(  "a1"
            , "a2"
            , "a3"
##              , "a4"
##              , "a5"
##              , "a6"
##              , "exp.re"
            , "m.y"
            , "inc"
            )
                                                      
## MCMCの繰り返し回数等
## iter <-   30000
## burnin <- 20000
## thin <-       1
## iter <-   1100
## burnin <-  100
## thin <-      1
## iter <-   10
## burnin <-  5
## thin <-    1
 iter <-   100
  burnin <- 10
   thin <-       1


## WinBUGSを呼び出し、結果をresという名前のオブジェクトに格納。
## 「bugs.directory」は環境にあわせて変更。
res <- bugs(data, init, para,
           model.file=modeltxt,
           n.chains=3, n.iter=iter, n.thin=thin, n.burnin=burnin,
           bugs.directory="D:/Users/TFHaraguchi/Documents/bayes/WinBUGS14",
           working.directory=getwd(),
           debug=TRUE
           )
## 推定値の一覧をresultという名前のデータフレームに格納
result <- res$summary
write.csv(result, output)
plot(res)
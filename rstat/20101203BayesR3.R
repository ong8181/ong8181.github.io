###############################################################
#            Chapter 8: Normal linear regression              #
###############################################################

##データは16年間かけて採取した鳥のデータ
##各センサス場所に鳥がいるかどうかをチェックした→目的変数yは鳥がセンサス場所にいる確率（%）
##データは実際に取ったものではなく、これから作成する
##解析には線形モデルを用いる

library(R2WinBUGS)

### 8.2. Data generation
n <- 16					# Number of years
a = 40					# Intercept
b = -1.5				# Slope

x <- 1:16 				# Values of covariate year
eps <- rnorm(n, mean = 0, sd = 5)    ##epsは残差で,平均0,sd5の正規分布に従う

y <- a + b*x + eps			# Assemble data set
plot(x, y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)



### 8.3. Analysis using R
print(summary(lm(y ~ x) )   )
abline(lm(y~ x), col = "blue", lwd = 2)



### 8.4. Analysis using WinBUGS


### 8.4.1. Fitting the model
# Write model
setwd("D:/Users/TFHaraguchi/Documents/bayes/101203seminar")
sink("linreg.txt")
cat("
model {

# Priors  事前分布
 alpha ~ dnorm(0,0.001) ##無情報にしたい＝分散を大きくしたい＝sdを大きくする→平均0,sd1000の正規分布
 beta ~ dnorm(0,0.001)
 sigma ~ dunif(0, 100)  ##下限0,上限100の一様分布

# Likelihood      尤度
 for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta*x[i]
 }

# Derived quantities
 tau <- 1/ (sigma * sigma)   ##yのsdであるtauを小さくしたい＝yの分散を大きくしたい

}
",fill=TRUE)
sink()

# Bundle data     用いるデータ
win.data <- list("x","y", "n")

# Inits function   初期値
inits <- function(){ list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1))} ##sigmaは0になるとマズイ

# Parameters to estimate  求めたいパラメータ
params <- c("alpha","beta",  "sigma")

# MCMC settings    MCMCのチェーン数、回転数、捨て去る初期の回転数、間引き
nc = 3  ;  ni=11000  ;  nb=1000  ;  nt=1

# Start Gibbs sampler　上記の条件でWinBUGS上でMCMCさせる
out <- bugs(data = win.data, inits = inits, parameters = params, model = "linreg.txt",
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE,
working.directory=getwd(),bugs.directory="D:/Users/TFHaraguchi/Documents/bayes/WinBUGS14")

print(out, dig = 3)  ##結果を小数点第3位まで表示させる
plot(out)

### 8.4.3. Forming predictions
plot(x, y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)
abline(lm(y~ x), col = "blue", lwd = 2)
pred.y <- out$mean$alpha + out$mean$beta * x
points(1:16, pred.y, type = "l", col = "red", lwd = 2)
text(5, 20, labels = "blue - 最尤推定; red - ベイズ推定", cex = 1)

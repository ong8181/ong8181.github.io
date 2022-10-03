#GLM using poisson regression model
d<-read.delim("clipboard")
plot(d)
library(AED)
pairs(d,lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)
res.p1<-glm(rss~feed+mainstem+light+topo,data=d,family=poisson(link="log"))
summary(res.p1)
library(MASS); stepAIC(res.p1)
res.p2<-glm(rss~feed+mainstem,data=d,family=poisson)
summary(res.p2)


#GLMM using poisson regression model with glmmML package
library(glmmML)
res.pmm1<-glmmML(rss~feed+mainstem+light+topo,data=d,family=poisson,cluster=site)
summary(res.pmm1)
library(MASS); stepAIC(res.pmm1)
res.pmm2<-glmmML(rss~feed+mainstem,data=d,family=poisson,cluster=site)
summary(res.pmm2)

#GLMM using poisson regression model with lme4 package
library(lme4)
res.plm1<-lmer(rss~feed+mainstem+light+topo+(1|site),data=d,family=poisson)
summary(res.plm1)
res.plm2<-lmer(rss~feed+mainstem+topo+(1|site),data=d,family=poisson)
res.plm3<-lmer(rss~feed+mainstem+light+(1|site),data=d,family=poisson)
res.plm4<-lmer(rss~feed+mainstem+(1|site),data=d,family=poisson)
AIC(res.plm1,res.plm2,res.plm3,res.plm4)
res.plm5<-lmer(rss~feed+(1|site),data=d,family=poisson)
res.plm6<-lmer(rss~mainstem+(1|site),data=d,family=poisson)
res.plm7<-lmer(rss~(1|site),data=d,family=poisson)
AIC(res.plm4,res.plm5,res.plm6,res.plm7)
summary(res.plm4)


#GLM using binomial regression model
d<-read.delim("clipboard")
res.b<-glm(bite~height*site,data=d,family=binomial(link="logit"))
library(MASS); stepAIC(res.b)
summary(res.b)


#GLMM and offest value
d<-read.delim("clipboard")
library(glmmML)
res.off.g<-glmmML(rss~FF,family=poisson,data=d,cluster=year,offset=log(total))
summary(res.off.g)

library(lme4)
res.off.l<-lmer(rss~FF+(1|year),family=poisson,data=d,offset=log(total))
summary(res.off.l)


#load data
 lipid <- read.csv("/Users/ushima/Documents/Work/Seminar/2010年度/20101119 MultiVariate/lipid.csv")

#principle component analysis
 lipid.pca <- prcomp(lipid[,16:33], scale=TRUE)
 lipid.pri <- princomp(lipid[,16:33])
 summary(lipid.pca)
 str(lipid.pca)
 sco <- lipid.pca$x
 rot <- lipid.pca$rotation

 plot(sco[,1:2], main="PCA scores", xlab="PC1 (46.4%)", ylab="PC1 (13.7%)")
 
 plot(rot[,1:2], main="PCA rotation", xlab="PC1 (46.4%)", ylab="PC2 (13.7%)")
 arrows(0,0,rot[,1],rot[,2],col=2,length=0.05)
 text(rot[,1],rot[,2],rownames(rot),cex=0.6)

 biplot(lipid.pca, choices=1:2, scale=0)  #scale means values*lambda^scale



#RDA/CCA
 library(vegan)

 lipid.rda <- rda(lipid[,8:11], lipid[,c(5,12,14:33)], scale=TRUE)
 summary(lipid.rda); plot(lipid.rda)

 rda.scores <- read.csv("/Users/ushima/Documents/Work/Seminar/2010年度/20101119 MultiVariate/RDAscores.csv")
 rda.scores
 op <- par(mfrow=c(1,2),mar=c(4,4,2,2))
 plot(rda.scores[,2:3], main="linear combination", xlim=range(-2,1), ylim=range(-3,1.5))
 plot(rda.scores[,4:5], main="site scores", xlim=range(-2,1), ylim=range(-3,1.5))
 par(op)

 op <- par(mfrow=c(1,2),mar=c(4,4,2,2))
 plot(lipid.rda, choices=1:2, scaling=2, main="scaling=2")
 plot(lipid.rda, choices=1:2, scaling=0, main="scaling=0")
 par(op)
 

 lipid.cca <- cca(lipid[,8:11], lipid[,c(5,12,14:33)], scale=TRUE)  #CCA
 summary(lipid.cca)
 op <- par(mfrow=c(1,2),mar=c(4,4,2,2))  #compare RDA and CCA
 plot(lipid.rda, choices=1:2, scaling=0, main="RDA")
 plot(lipid.cca, choices=1:2, scaling=0, main="CCA")
 par(op)


 plot(lipid.rda, xlab="Axis1 (54.8%)", ylab="Axis2 (9.58%)")
 scores(lipid.rda)      #check site score
 RsquareAdj(lipid.rda)  #adjusted R square（from Peres-Neto et al. 2006 Ecology, important!）
 ratio <- RsquareAdj(lipid.rda)$adj.r.squared/RsquareAdj(lipid.rda)$r.squared
 54.75*ratio
 9.581*ratio
 plot(lipid.rda, xlab="Axis1 (47.15%)", ylab="Axis2 (8.25%)")
 
 
 
#Permutation test
 rda.permu <- rda(lipid[,8:11]~lipid[,5]+lipid[,12]+lipid[,14]+lipid[,15]+lipid[,16]+lipid[,17]+lipid[,18]+
                               lipid[,19]+lipid[,20]+lipid[,21]+lipid[,22]+lipid[,23]+lipid[,24]+lipid[,25]+
                               lipid[,26]+lipid[,27]+lipid[,28]+lipid[,29]+lipid[,30]+lipid[,31]+lipid[,32]+
                               lipid[,33], scale=TRUE)
 anova(rda.permu, by="term", permu=999)
 summary(rda.permu)

 op <- par(mfrow=c(1,2),mar=c(4,4,2,2))  #compare RDA and CCA
 plot(lipid.rda, choices=1:2, scaling=0, main="RDA")
 plot(rda.permu, choices=1:2, scaling=0, main="Permutation")
 par(op)


#Partial RDA
 prda.1 <- rda(lipid[,8:11], lipid[,16:33], lipid[,c(5,12,14:15)], scale=TRUE)
 summary(prda.1)  #lipid effects after excluding effecs of physicochemical parameters

 prda.2 <- rda(lipid[,8:11], lipid[,c(5,12,14:15)], lipid[,16:33], scale=TRUE)
 summary(prda.2)  #physicochemical parameters after excluding effects of lipid effects

 op <- par(mfrow=c(1,2),mar=c(4,4,2,2))
 plot(prda.1)
 plot(prda.2)
 par(op)

#Variation Partitioning (Peres-Neto et al. 2006 Ecology)
 lipid.vp <- varpart(lipid[,8:11], lipid[,16:33], lipid[,c(5,12,14:15)],scale=TRUE)
 lipid.vp
 
 
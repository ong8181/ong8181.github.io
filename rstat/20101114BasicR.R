#R�g�����׋���͂��߂܂�
#R�̃_�E�����[�h�E�C���X�g�[��
	#�p�b�P�[�W�̃C���X�g�[��
options(repos="ftp://ftp.u-aizu.ac.jp/pub/lang/R/CRAN")
install.packages("vegan",lib="C:/Program Files/R/R-2.11.1-x64/library")

	#Task View�̓p�b�P�[�W���Z�b�g�ɂ����֗��c�[���i���߂Ă̂ЂƂ͑�̂���ő���邩���j
library(ctv)
#install.views("Environmetrics")
install.views()
		#Task View�̗�Ɋւ��Ă�http://www.okada.jp.org/RWiki/?CRAN%20Task%20View�Q��
		#��܂Ƃ߂āA���ł���
install.packages(c("randomForest","vegan"),repos="ftp://ftp.u-aizu.ac.jp/pub/lang/R/CRAN",lib="C:/Program Files/R/R-2.11.1-x64/library") 
	#�S�Ẵp�b�P�[�W���A�b�v�f�[�g
#update.packages(ask=F,destdir=".")

	#�p�b�P�[�W�̒ǉ��ŁA����܂łł��Ȃ��������Ƃ������\�ɂȂ�
scatterplot3d()
library(scatterplot3d)
z<-seq(-10,10,0.01)
x<-cos(z);y<-sin(z)
scatterplot3d(x,y,z,highlight.3d=T)


	#�����ė~�����Ƃ���
?plot()
help(plot)

	#�ȑO������������Ăяo�����L�[

#�֐��d��Ƃ��Ă�R
	#�悭�g�����Z�q
		#<-�͑�����Ӗ�����B=�Ɠ��`�ł���
d<-1
d
d=3
d
		# + - * / ^�̓G�N�Z���Ɠ��l
d<-3+4*2/4
d

		#���[�g��w���֐��A�O�p�֐��Ȃǂ��G�N�Z���ƂقƂ�ǈꏏ
sqrt(3);3^(1/2)
sin(3.141593);cos(3.141593)
sin(pi);cos(pi)
log(2.718);log2(4);log10(100)
log(9,3)
round(4.48)
abs(-5)
sign(-10);sign(10)
asin(1/sqrt(2))

		#������O�����A���������R�}���h�ɂ͗l�X�ȃo���G�[�V����������Br�ł͂��܂邢�낢��ȃR�}���h����B
runif(10)
runif(3)
			#rnorm(n, mean = 0, sd = 1)
rnorm(10,3,0.01)
rbinom(20,3,0.5)
rnbinom(10,3,0.5)
rpois(10,8)

dnorm(10,2,0.01)
		#���������n���̃R�}���h�Ƃ���
		#dnorm(x, mean = 0, sd = 1, log = FALSE)�m�����x�֐�
		#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)���K���z�̂��ƂŁA����l�����z�̂ǂ���ւ�ɂ���̂�(�ݐϊm�����z)���ׂ�
		#qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)quantile�𒲂ׂ�
x<-seq(0,0.5,0.025)
dnorm(x,0,1)
pnorm(0.6745)
qnorm(0.25);qnorm(0.75);qnorm(0.975)

#�I�u�W�F�N�g�i�x�N�g���E�s��E�f�[�^�t���[���E���X�g�j
	#���łɉ��񂩎g���Ă��邪�A�I�u�W�F�N�g�Ƃ����̂̓f�[�^��O���t�ȂǁA�����Ȃ��̂����܂��Ă����锠�̂��ƁB
d<-100;d
d<-'���v';d<-d;d
	#�I�u�W�F�N�g�ɂ͊�{�I�Ɏ����̍D���Ȗ��O���t�����邪�A�֑�������
1a<-100;1-a<-100;@<-100;stock\<-100
	#�I�u�W�F�N�g�ɓ��������̂͒P�ƂłȂ��Ă��悢
	#c(,,,)�̓x�N�g�������֐�
vect<-c(1,2,3,4);vect
	#�x�N�g�����A�������������Ƃ͌���Ȃ�
fruit<-c("�����S","�X�C�J","������");fruit
	#R�́A�X�̗v�f���ǂ̂悤�ȑ����������A�����I�ɔ��f���Ă���
is.numeric(vect)
is.numeric(vect[1:3])
is.numeric(fruit)
	#�x�N�g��������ȏ�A���R�u�s��v������
matr<-matrix(nrow=3,c(1,2,3,4,5,6,7,8,9,10,11,12));dat2
	#R��Ő���������ł́A�G�N�Z���̂悤�ȁA�Z�����h���b�O���Ĕ͈͎w��A�̂悤�Ȃ��Ƃ����͔̂��ɖʓ|�B����Ɂc
rep(10,4)
seq(2,10,2)
length(vect)
length(matr)
dat<-rbind(matr,vect)
cbind(t(matr),vect)#�G�N�Z���Ō����΁A�f�[�^�������₵������ɑ�������
fix(dat)
	#�f�[�^����͂��邤���ōł��������ȒP�ȂЂƂ����܂�̃f�[�^�Z�b�g���f�[�^�t���[���ƌĂ�
	#data.frame�Ƃ́A������̃��[���ɏ]���č��ꂽ���X�g�ł���B�t�Ɍ����΁A���X�g�Ƃ����̂́A������I�u�W�F�N�g����荞�߂�
	#���ł����āAdata.frame�͂���ɑ΂��Ĉ��̋K����^�������̂ł���B
matr2<-matrix(nrow=5,seq(1,15,1))

list<-list(matr2,matr)
list[1];list[2]

	#�f�[�^�t���[���̊e�s�E��͕K�����x���������A���x���ɂ�鑀�삪�\�ł���B�l�����Ƃ��ẮA�f�[�^�t���[���̗�v�f���ꑱ����
	#�x�N�g���Ƃ��ĔF������Ă���A����̒P�ʂƂȂ�Ǝv���Ă���Ƃ킩��₷���B
v1<-rep(1,4);v2<-rep(2,4);v3<-rep(3,4);v4<-rep(4,4)
dat<-cbind(v1,v2,v3,v4)
	#��Ldat���f�[�^�t���[���Ƃ��ĕϊ����Ă��ƁA��v�f���x�N�g���Ƃ��ČʂɎ��o���Ă��邱�Ƃ��ȒP�ɂȂ�
is.data.frame(dat)
dat$v1
dat<-data.frame(dat)
dat$v1
	#�f�[�^�t���[���͍s��Ɠ��l�̍\���������Ă��Ȃ���΂Ȃ�Ȃ��B�i�����̈Ⴄ�x�N�g���͌����ł��Ȃ��j
v5<-seq(1,4,1);v6<-seq(1,5,1)
data.frame(cbind(v5,v6))
df<-data.frame(matr2)
colnames(df)<-c("Xysticus","Heptathela","Cornus")
df;df$Cornus
	#�e��͎����I�ɂǂ̂悤�ȑ������������l���H���F������Ă���
is.numeric(df$Cornus)
fix(df)

v7<-c(1,2,NA,3,4)
df2<-data.frame(v7)
fix(df2)

#�f�[�^�̃C���|�[�g�E��͂ɓK�����f�[�^�̗^����
	#R��ł��ׂẴf�[�^�ҏW���s���̂͌����I�ł͂Ȃ��B���G�N�Z���ŕҏW����R�Ɏ�荞�ނ̂���ʓI�H
	#�f�[�^�̓f�[�^�t���[���Ƃ��Ď�荞��
	#�̂ŁA�f�[�^�t���[�����L���Ă���\���ɏ]���ăG�N�Z���f�[�^�V�[�g���쐬����̂��]�܂����B
	#�܂�A��������Ƃ�������͂��s��
	#�܂��́A�����Ƃ���y�Ƀf�[�^���ڂ����@
d14C<-read.delim("clipboard",header=T)
	#�ǂ�ȃf�[�^�Z�b�g���g�����̂��L�^�Ɏc������������A��x�ɂ��������
	#�e�L�X�g�t�@�C�����J���������ɕ֗��ȕ��@
setwd("D:/Users/TFHaraguchi/Desktop/R�׋���")
d14C<-read.delim("testfile.txt",header=T)
	#�ގ��̊֐��Ƃ��āA
#read.table("testfile.txt",header=T,sep='\t')
#read.csv()
	#���ۂɒ��g������ƁA����������ϐ��E������ϐ����񂲂ƂɊi�[����Ă���
	#�l�q��������
head(d14C)
fix(d14C)
		#�G�N�Z����̋󗓂�NA�ɕϊ������^�܂��A�f�[�^�ǂݍ��݂�
		#�ۂ�na.strings="������"�ƒǉ����邱�ƂŁA�����l���w�肷��
		#������������Œ�`���邱�Ƃ��ł���
#�f�[�^�n���h�����O
	#��͂ɂ������ẮA������͗ޕʕϐ��^�����͘A���ϐ��Ƃ��Ď����I�ɔF�������
	#�K�v�ɉ����ĘA���ϐ���ޕʕϐ��ɕϊ����邱�Ƃ��\
as.factor(d14C$site)
ana1<-aov(d14C$d14Cvalue~d14C$site)
ana2<-aov(d14C$d14Cvalue~as.factor(d14C$site))
summary(ana1)
summary(ana2)
	#R��ŉ�͂����邤���ŕ֗��Ȃ̂��A�K�X�f�[�^���\�[�g���Ȃ����͂�i�߂��邱��
		#d14C�f�[�^�͂��낢��ȃN���̓��ʑ̔�𑪂����f�[�^�B���̒�����Ԃ𒣂�N��
		#��������͑Ώۂɂ������B
		#�G�N�Z���I���@���f�[�^�V�[�g����������Ȃ��f�[�^���폜�Ƃ��H
d14Cweb<-subset(d14C,feeding=="web")
plot(d14Cweb[4:10])

	#�f�[�^�������ōi�荞�ނɂ͘_�����Z�q��֌W���Z�q�ɂ��Ēm���Ă������Ƃ��K�v
		#���Z�q�Ƃ́A�������u�̊֌W��\���L���ł���
		#R�ɂ����Ă�=�͑����\���L���ł����āA���Z�q�ł͂Ȃ�
		#subset()���Ŏg�����ƂŁA�G�N�Z���֐��ł�������if�O���[�v�̊֐���A�t�B���^�ɑ�������@�\��S��

#�������@==
#not�@!=
#and�c&
#or�c|
#�召��r�c<, >, <=,>=

	#�ގ��̎g�����̂ł���֐��Ƃ���
split<-split(d14C,d14C$feeding)
split$web
split
		#�X�v���b�g���ꂽ�f�[�^�̓��X�g�Ƃ��Ĉ�ɂ܂Ƃ߂��Ă���
		#�X�v���b�g�֐��̓f�[�^�t���[���̓���s�ɑ΂��Ď��s�����Ƃ�
		#����Ȃ��̂ŁAsplit�̃J�e�S����̎w���XX$XX�̌`�ōs���K�v������
split(d14C,feeding)
	#���בւ���order�֐������p���邱�ƂŎ����ł���
order(d14C$d14Cvalue)
d14C<-d14C[order(d14C$d14Cvalue),]
fix(d14C)
	#apply�n�̊֐��̓f�[�^�̐������s����Ŏg����i���Ƃ�����j
	#�G�N�Z���Ō����Ƃ����pibot-table�ɑ������邱�Ƃ̈ꕔ���ł���
tapply(d14C$d14Cvalue,d14C$feeding,mean);tapply(d14C$d14Cvalue,d14C$feeding,sd);tapply(d14C$d14Cvalue,d14C$feeding,length)

	#�G�N�Z���ɂ�����lookup�n�ɑ�������֐��Ƃ��Ă�merge���p�ӂ���Ă���

spp<-c("A","B","C","D")
mode<-c("web","web","web","wander")
category<-data.frame(cbind(spp,mode))
category

type<-c("A","A","A","B","C","C","D","D")
measured<-cbind(type,data.frame(matrix(ncol=3,rep(c(1,3,5),8))))
colnames(measured)<-c("sp","m1","m2","m3")
measured

merge(x=category,y=measured,by.x="spp",by.y="sp")

#�e��֐��ɂ�鐧��
	#R�ł́A���łɗ^����ꂽ�֐��ȊO�ɂ��֐������삷��I�v�V����������
summary2<-function(data){
rbind(
tapply(data$d14Cvalue,data$feeding,mean),
tapply(data$d14Cvalue,data$feeding,sd),
tapply(data$d14Cvalue,data$feeding,length)
)}
summary2(data=d14C)

#source()�́A����t�@�C���ɏ����ꂽ���e�����ׂĎ��s����X�N���v�g
source("MixSIR�\�[�X�~�b�N�X�O���t.R")

#sink()�́A���v��͂̌��ʂȂǁA�o�́i�����ŏo�͂���镔���j���e�L�X�g�t�@�C���ɋL�^���ĕۑ����Ă����X�N���v�g
setwd("D:/Users/TFHaraguchi/Desktop/R�׋���")
sink(file="results.txt")
as.matrix(summary(ana1))
as.matrix(summary(ana2,append=T))
sink()

	#������g�ݍ��킹��ƁA
		#1 �A��₱�����֐��Ƃ��O���t���ߕ����쐬
		#�Q�Asink��R�t�@�C���Ƃ��ĕۑ�
		#�R�Asource�œK�X�ǂݍ���Ŋ��p
		#�Ƃ�������ŉ�͂ł���悤�ɂȂ�B

#�O���t�쐬
	#�܂��͒���b�A�v���b�g�֐��̐���
d14C2<-
plot(d14C[,c(4,7,8,9,15,16)])
	#����̋����ŁAAED���g���Ă݂�http://www.highstat.com/book2.htm���_�E�����[�h
library(AED)
pairs(d14C[,c(4,7,8,9,15,16)],
lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)
	#�X�Ƀf�[�^���w�肵�ē񎟌��̃v���b�g�������ꍇ�ɂ͈ȉ��̂悤�Ɏw�肷��
f1<-formula(d14Cvalue~d13C�␳+d15N�␳)
plot(f1,data=d14C)

plot(x=d14C$d13C�␳,y=d14C$d14Cvalue)

attach(d14C)
plot(d13C�␳,d14Cvalue,ann=F,axes=F)
detach(d14C)
sink()

cbind(lapply(pdfFonts(),function(x){x$family}))

#�t�H���g�ꗗ�̎擾
cbind(lapply(pdfFonts(),function(x){x$family}))

pdf(file="sampleplot.pdf")
par(mfrow=c(2,2))
par(family="Japan1")
plot(d14C$d13C�␳,d14C$d14Cvalue)
par(family="Japan1GothicBBB")
plot(d14C$d13C�␳,d14C$d14Cvalue)
par(family="Japan1Ryumin")
plot(d14C$d13C�␳,d14C$d14Cvalue)
par(family="Japan1HeiMin")
plot(d14C$d13C�␳,d14C$d14Cvalue)

dev.off()

source("http://r.nakama.ne.jp/AI/AI_UCS2.R")
pdf(file="sampleplot2.pdf",family="Japan1")
plot(d14C$d13C�␳,d14C$d14Cvalue)
dev.off()

	#�������f�[�^�̌X���������������Ȃ炱�ꂾ���ł��\���ȃO���t�������邪�A
	#���ꂢ�ȃO���t�����������Ȃ�e�N�j�b�N���������K�v�ł���B
		#�܂��A�`��̈�𑀍삷����@�i�c���E�����̃O���t����肽���I�j
		#par()�́A�O���t�B�b�N�X�p�����[�^�̕ҏW�ɂ�����閽�߂��o��
par()$din
par(mfrow=c(2,2),mar=c(0.5,0.5,0.5,0.5),oma=c(0.3,0.3,0.3,0.3),family="sans"
)
#par(fin=c(1.5,3))
plot(d14C$d13C�␳,d14C$d14Cvalue)
plot(d14C$d15N�␳,d14C$d14Cvalue)
plot(d14C$d13C�␳,d14C$d14Cvalue)
plot(d14C$d15N�␳,d14C$d14Cvalue)

		#��荂���̕�����ʂ̕`����@�Ƃ��āA
		#layout�֐����Љ��
		#mfrow�֐��ł́A�����̃O���t�͒P���figure region�ɕ`�悳��邪�A
		#layout�֐��ł́A�e�O���t�͊efigure region�ɓƗ��ɕ`�悳��邱�ƂɂȂ�
		#figure region�𒲐����邱�ƂŁA��莩�R�x�̍��������O���t�̕`�悪�\�ł���B
m<-matrix(seq(1,6),byrow=T,ncol=2)
m
layout(m)
layout.show(6)

layout()
layout(m,respect=T,heights=c(1,1,3),width=c(3,1))
layout.show(6)

layout(m,respect=T,heights=c(1,1,2),width=c(2,2))
par(family="sans")
plot(d14C$d13C�␳,d14C$d14Cvalue)
plot(d14C$d15N�␳,d14C$d14Cvalue)
par(family="serif")
plot(d14C$d13C�␳,d14C$d14Cvalue)
plot(d14C$d15N�␳,d14C$d14Cvalue)
par(family="mono")
plot(d14C$d13C�␳,d14C$d14Cvalue)
plot(d14C$d15N�␳,d14C$d14Cvalue)

		#���R�A�O���t�̃t�H���g�⎲���x���A���̑�����^�C�v�E�J���[�Ȃǂɂ��Ă��ύX���������Ƃ����邾�낤
		#���ׂẴP�[�X�ɂ��ċ�����̂͌����I�ł͂Ȃ��̂ŁA��\�I�Ȋ֐��ɂ��Ă�����������
layout(matrix(c(1,2),ncol=2),heights=c(1,1),width=c(1.41,1.41),respect=T)
par(mar=c(2,2,2,2),oma=c(1,1,1,1))
plot(d14Cvalue~d13C�␳,data=d14C,pch=c(1,2)[unclass(d14C$feeding)],col=c(colwand,colweb)[unclass(d14C$feeding)])
abline(lm(d14Cvalue~d13C�␳,data=d14C))

colweb<-"red"
colwand<-"blue"

d14Cweb<-subset(d14C,feeding=="web")
d14Cwand<-subset(d14C,feeding=="wander")
	#"type"�p�����[�^�͂��͂�T�|�[�g����Ȃ�!
abline(lm(d14Cvalue~d13C�␳,data=d14Cweb),lty="dotted",col=colweb,lwd=4)
abline(lm(d14Cvalue~d13C�␳,data=d14Cwand),lty="dashed",col=colwand,lwd=.5)

plot(d14Cvalue~d15N�␳,data=d14C,pch=c(1,2)[unclass(d14C$feeding)],col=c(colwand,colweb)[unclass(d14C$feeding)])
abline(lm(d14Cvalue~d15N�␳,data=d14Cweb),lty="dotted",col=colweb,lwd=4)
abline(lm(d14Cvalue~d15N�␳,data=d14Cwand),lty="dashed",col=colwand,lwd=.5)
	#���Ȃǂ܂Ŕz�����ċL�q�������ꍇ�͈ȉ��Ɂc
plot(d14Cvalue~d15N�␳,data=d14C,pch=c(1,2)[unclass(d14C$feeding)],col=c(colwand,colweb)[unclass(d14C$feeding)],
ann=F,axes=F,xlim=c(0,7),ylim=c(30,100))
axis(side=1,at=seq(0,7),pos=30,cex.axis=2)
axis(side=2,at=seq(30,100,10),pos=0)
rect(0,30,7,100,lwd=2)
mtext(side=1,"��15N(��)",line=2,cex=1.5)
mtext(side=2,"��14C(��)",line=2,cex=1.5,at=40)
mtext(side=3,"spider ��14C~��15N",cex=2)

text(4,50,"wandering",col=colwand)
text(6,80,"web weaving",col=colweb)
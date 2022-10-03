#R使い方勉強会はじめます
#Rのダウンロード・インストール
	#パッケージのインストール
options(repos="ftp://ftp.u-aizu.ac.jp/pub/lang/R/CRAN")
install.packages("vegan",lib="C:/Program Files/R/R-2.11.1-x64/library")

	#Task Viewはパッケージをセットにした便利ツール（初めてのひとは大体これで足りるかも）
library(ctv)
#install.views("Environmetrics")
install.views()
		#Task Viewの例に関してはhttp://www.okada.jp.org/RWiki/?CRAN%20Task%20View参照
		#二つまとめて、もできる
install.packages(c("randomForest","vegan"),repos="ftp://ftp.u-aizu.ac.jp/pub/lang/R/CRAN",lib="C:/Program Files/R/R-2.11.1-x64/library") 
	#全てのパッケージをアップデート
#update.packages(ask=F,destdir=".")

	#パッケージの追加で、これまでできなかったことが実現可能になる
scatterplot3d()
library(scatterplot3d)
z<-seq(-10,10,0.01)
x<-cos(z);y<-sin(z)
scatterplot3d(x,y,z,highlight.3d=T)


	#助けて欲しいときは
?plot()
help(plot)

	#以前やった履歴を呼び出す↑キー

#関数電卓としてのR
	#よく使う演算子
		#<-は代入を意味する。=と同義である
d<-1
d
d=3
d
		# + - * / ^はエクセルと同様
d<-3+4*2/4
d

		#ルートや指数関数、三角関数などもエクセルとほとんど一緒
sqrt(3);3^(1/2)
sin(3.141593);cos(3.141593)
sin(pi);cos(pi)
log(2.718);log2(4);log10(100)
log(9,3)
round(4.48)
abs(-5)
sign(-10);sign(10)
asin(1/sqrt(2))

		#当たり前だが、乱数生成コマンドには様々なバリエーションがある。rではじまるいろいろなコマンドあり。
runif(10)
runif(3)
			#rnorm(n, mean = 0, sd = 1)
rnorm(10,3,0.01)
rbinom(20,3,0.5)
rnbinom(10,3,0.5)
rpois(10,8)

dnorm(10,2,0.01)
		#こうした系統のコマンドとして
		#dnorm(x, mean = 0, sd = 1, log = FALSE)確率密度関数
		#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)正規分布のもとで、ある値が分布のどこらへんにいるのか(累積確率分布)調べる
		#qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)quantileを調べる
x<-seq(0,0.5,0.025)
dnorm(x,0,1)
pnorm(0.6745)
qnorm(0.25);qnorm(0.75);qnorm(0.975)

#オブジェクト（ベクトル・行列・データフレーム・リスト）
	#すでに何回か使っているが、オブジェクトというのはデータやグラフなど、いろんなものをしまっておける箱のこと。
d<-100;d
d<-'統計';d<-d;d
	#オブジェクトには基本的に自分の好きな名前が付けられるが、禁則もある
1a<-100;1-a<-100;@<-100;stock\<-100
	#オブジェクトに入れられるものは単独でなくてもよい
	#c(,,,)はベクトルを作る関数
vect<-c(1,2,3,4);vect
	#ベクトルも、数だけを扱うとは限らない
fruit<-c("リンゴ","スイカ","メロン");fruit
	#Rは、個々の要素がどのような属性を持つか、自動的に判断している
is.numeric(vect)
is.numeric(vect[1:3])
is.numeric(fruit)
	#ベクトルがある以上、当然「行列」もある
matr<-matrix(nrow=3,c(1,2,3,4,5,6,7,8,9,10,11,12));dat2
	#R上で数を扱う上では、エクセルのような、セルをドラッグして範囲指定、のようなことをやるのは非常に面倒。代わりに…
rep(10,4)
seq(2,10,2)
length(vect)
length(matr)
dat<-rbind(matr,vect)
cbind(t(matr),vect)#エクセルで言えば、データ列を一つ増やした操作に相当する
fix(dat)
	#データを解析するうえで最も扱いが簡単なひとかたまりのデータセットをデータフレームと呼ぶ
	#data.frameとは、ある一定のルールに従って作られたリストである。逆に言えば、リストというのは、あらゆるオブジェクトを放り込める
	#箱であって、data.frameはそれに対して一定の規則を与えたものである。
matr2<-matrix(nrow=5,seq(1,15,1))

list<-list(matr2,matr)
list[1];list[2]

	#データフレームの各行・列は必ずラベルを持ち、ラベルによる操作が可能である。考え方としては、データフレームの列要素が一続きの
	#ベクトルとして認識されており、操作の単位となると思っているとわかりやすい。
v1<-rep(1,4);v2<-rep(2,4);v3<-rep(3,4);v4<-rep(4,4)
dat<-cbind(v1,v2,v3,v4)
	#上記datをデータフレームとして変換してやると、列要素をベクトルとして個別に取り出してくることが簡単になる
is.data.frame(dat)
dat$v1
dat<-data.frame(dat)
dat$v1
	#データフレームは行列と同様の構造を持っていなければならない。（長さの違うベクトルは結合できない）
v5<-seq(1,4,1);v6<-seq(1,5,1)
data.frame(cbind(v5,v6))
df<-data.frame(matr2)
colnames(df)<-c("Xysticus","Heptathela","Cornus")
df;df$Cornus
	#各列は自動的にどのような属性を持った値か？が認識されている
is.numeric(df$Cornus)
fix(df)

v7<-c(1,2,NA,3,4)
df2<-data.frame(v7)
fix(df2)

#データのインポート・解析に適したデータの与え方
	#R上ですべてのデータ編集を行うのは現実的ではない。→エクセルで編集してRに取り込むのが一般的？
	#データはデータフレームとして取り込む
	#ので、データフレームが有している構造に従ってエクセルデータシートを作成するのが望ましい。
	#つまり、列を次元とする情報入力を行う
	#まずは、もっとも手軽にデータを移す方法
d14C<-read.delim("clipboard",header=T)
	#どんなデータセットを使ったのか記録に残したかったり、一度にたくさんの
	#テキストファイルを開きたい時に便利な方法
setwd("D:/Users/TFHaraguchi/Desktop/R勉強会")
d14C<-read.delim("testfile.txt",header=T)
	#類似の関数として、
#read.table("testfile.txt",header=T,sep='\t')
#read.csv()
	#実際に中身を見ると、いわゆる説明変数・被説明変数が列ごとに格納されている
	#様子が分かる
head(d14C)
fix(d14C)
		#エクセル上の空欄はNAに変換される／また、データ読み込みの
		#際にna.strings="文字列"と追加することで、欠損値を指定する
		#文字列を自分で定義することもできる
#データハンドリング
	#解析にあたっては、文字列は類別変数／数字は連続変数として自動的に認識される
	#必要に応じて連続変数を類別変数に変換することも可能
as.factor(d14C$site)
ana1<-aov(d14C$d14Cvalue~d14C$site)
ana2<-aov(d14C$d14Cvalue~as.factor(d14C$site))
summary(ana1)
summary(ana2)
	#R上で解析をするうえで便利なのが、適宜データをソートしながら解析を進められること
		#d14Cデータはいろいろなクモの同位体比を測ったデータ。この中から網を張るクモ
		#だけを解析対象にしたい。
		#エクセル的方法→データシート複製＆いらないデータを削除とか？
d14Cweb<-subset(d14C,feeding=="web")
plot(d14Cweb[4:10])

	#データを条件で絞り込むには論理演算子や関係演算子について知っておくことが必要
		#演算子とは、引数同志の関係を表す記号である
		#Rにおいては=は代入を表す記号であって、演算子ではない
		#subset()内で使うことで、エクセル関数でいう所のifグループの関数や、フィルタに相当する機能を担う

#等しい　==
#not　!=
#and…&
#or…|
#大小比較…<, >, <=,>=

	#類似の使い方のできる関数として
split<-split(d14C,d14C$feeding)
split$web
split
		#スプリットされたデータはリストとして一つにまとめられている
		#スプリット関数はデータフレームの特定行に対して実行されるとは
		#限らないので、splitのカテゴリ列の指定はXX$XXの形で行う必要がある
split(d14C,feeding)
	#並べ替えはorder関数を応用することで実現できる
order(d14C$d14Cvalue)
d14C<-d14C[order(d14C$d14Cvalue),]
fix(d14C)
	#apply系の関数はデータの整理を行う上で使える（こともある）
	#エクセルで言うところのpibot-tableに相当することの一部ができる
tapply(d14C$d14Cvalue,d14C$feeding,mean);tapply(d14C$d14Cvalue,d14C$feeding,sd);tapply(d14C$d14Cvalue,d14C$feeding,length)

	#エクセルにおけるlookup系に相当する関数としてはmergeが用意されている

spp<-c("A","B","C","D")
mode<-c("web","web","web","wander")
category<-data.frame(cbind(spp,mode))
category

type<-c("A","A","A","B","C","C","D","D")
measured<-cbind(type,data.frame(matrix(ncol=3,rep(c(1,3,5),8))))
colnames(measured)<-c("sp","m1","m2","m3")
measured

merge(x=category,y=measured,by.x="spp",by.y="sp")

#各種関数による制御
	#Rでは、すでに与えられた関数以外にも関数を自作するオプションがある
summary2<-function(data){
rbind(
tapply(data$d14Cvalue,data$feeding,mean),
tapply(data$d14Cvalue,data$feeding,sd),
tapply(data$d14Cvalue,data$feeding,length)
)}
summary2(data=d14C)

#source()は、あるファイルに書かれた内容をすべて実行するスクリプト
source("MixSIRソースミックスグラフ.R")

#sink()は、統計解析の結果など、出力（青文字で出力される部分）をテキストファイルに記録して保存しておくスクリプト
setwd("D:/Users/TFHaraguchi/Desktop/R勉強会")
sink(file="results.txt")
as.matrix(summary(ana1))
as.matrix(summary(ana2,append=T))
sink()

	#これらを組み合わせると、
		#1 、ややこしい関数とかグラフ命令文を作成
		#２、sinkでRファイルとして保存
		#３、sourceで適宜読み込んで活用
		#という流れで解析できるようになる。

#グラフ作成
	#まずは超基礎、プロット関数の説明
d14C2<-
plot(d14C[,c(4,7,8,9,15,16)])
	#これの強化版、AEDを使ってみるhttp://www.highstat.com/book2.htmよりダウンロード
library(AED)
pairs(d14C[,c(4,7,8,9,15,16)],
lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)
	#個々にデータを指定して二次元のプロットを書く場合には以下のように指定する
f1<-formula(d14Cvalue~d13C補正+d15N補正)
plot(f1,data=d14C)

plot(x=d14C$d13C補正,y=d14C$d14Cvalue)

attach(d14C)
plot(d13C補正,d14Cvalue,ann=F,axes=F)
detach(d14C)
sink()

cbind(lapply(pdfFonts(),function(x){x$family}))

#フォント一覧の取得
cbind(lapply(pdfFonts(),function(x){x$family}))

pdf(file="sampleplot.pdf")
par(mfrow=c(2,2))
par(family="Japan1")
plot(d14C$d13C補正,d14C$d14Cvalue)
par(family="Japan1GothicBBB")
plot(d14C$d13C補正,d14C$d14Cvalue)
par(family="Japan1Ryumin")
plot(d14C$d13C補正,d14C$d14Cvalue)
par(family="Japan1HeiMin")
plot(d14C$d13C補正,d14C$d14Cvalue)

dev.off()

source("http://r.nakama.ne.jp/AI/AI_UCS2.R")
pdf(file="sampleplot2.pdf",family="Japan1")
plot(d14C$d13C補正,d14C$d14Cvalue)
dev.off()

	#自分がデータの傾向を見たいだけならこれだけでも十分なグラフを書けるが、
	#きれいなグラフが書きたいならテクニックがいくつか必要である。
		#まず、描画領域を操作する方法（縦長・横長のグラフを作りたい！）
		#par()は、グラフィックスパラメータの編集にかかわる命令を出す
par()$din
par(mfrow=c(2,2),mar=c(0.5,0.5,0.5,0.5),oma=c(0.3,0.3,0.3,0.3),family="sans"
)
#par(fin=c(1.5,3))
plot(d14C$d13C補正,d14C$d14Cvalue)
plot(d14C$d15N補正,d14C$d14Cvalue)
plot(d14C$d13C補正,d14C$d14Cvalue)
plot(d14C$d15N補正,d14C$d14Cvalue)

		#より高次の分割画面の描画方法として、
		#layout関数を紹介する
		#mfrow関数では、複数のグラフは単一のfigure regionに描画されるが、
		#layout関数では、各グラフは各figure regionに独立に描画されることになる
		#figure regionを調整することで、より自由度の高い複数グラフの描画が可能である。
m<-matrix(seq(1,6),byrow=T,ncol=2)
m
layout(m)
layout.show(6)

layout()
layout(m,respect=T,heights=c(1,1,3),width=c(3,1))
layout.show(6)

layout(m,respect=T,heights=c(1,1,2),width=c(2,2))
par(family="sans")
plot(d14C$d13C補正,d14C$d14Cvalue)
plot(d14C$d15N補正,d14C$d14Cvalue)
par(family="serif")
plot(d14C$d13C補正,d14C$d14Cvalue)
plot(d14C$d15N補正,d14C$d14Cvalue)
par(family="mono")
plot(d14C$d13C補正,d14C$d14Cvalue)
plot(d14C$d15N補正,d14C$d14Cvalue)

		#当然、グラフのフォントや軸ラベル、線の太さやタイプ・カラーなどについても変更したいことがあるだろう
		#すべてのケースについて挙げるのは現実的ではないので、代表的な関数についてだけ説明する
layout(matrix(c(1,2),ncol=2),heights=c(1,1),width=c(1.41,1.41),respect=T)
par(mar=c(2,2,2,2),oma=c(1,1,1,1))
plot(d14Cvalue~d13C補正,data=d14C,pch=c(1,2)[unclass(d14C$feeding)],col=c(colwand,colweb)[unclass(d14C$feeding)])
abline(lm(d14Cvalue~d13C補正,data=d14C))

colweb<-"red"
colwand<-"blue"

d14Cweb<-subset(d14C,feeding=="web")
d14Cwand<-subset(d14C,feeding=="wander")
	#"type"パラメータはもはやサポートされない!
abline(lm(d14Cvalue~d13C補正,data=d14Cweb),lty="dotted",col=colweb,lwd=4)
abline(lm(d14Cvalue~d13C補正,data=d14Cwand),lty="dashed",col=colwand,lwd=.5)

plot(d14Cvalue~d15N補正,data=d14C,pch=c(1,2)[unclass(d14C$feeding)],col=c(colwand,colweb)[unclass(d14C$feeding)])
abline(lm(d14Cvalue~d15N補正,data=d14Cweb),lty="dotted",col=colweb,lwd=4)
abline(lm(d14Cvalue~d15N補正,data=d14Cwand),lty="dashed",col=colwand,lwd=.5)
	#軸などまで配慮して記述したい場合は以下に…
plot(d14Cvalue~d15N補正,data=d14C,pch=c(1,2)[unclass(d14C$feeding)],col=c(colwand,colweb)[unclass(d14C$feeding)],
ann=F,axes=F,xlim=c(0,7),ylim=c(30,100))
axis(side=1,at=seq(0,7),pos=30,cex.axis=2)
axis(side=2,at=seq(30,100,10),pos=0)
rect(0,30,7,100,lwd=2)
mtext(side=1,"δ15N(‰)",line=2,cex=1.5)
mtext(side=2,"δ14C(‰)",line=2,cex=1.5,at=40)
mtext(side=3,"spider δ14C~δ15N",cex=2)

text(4,50,"wandering",col=colwand)
text(6,80,"web weaving",col=colweb)
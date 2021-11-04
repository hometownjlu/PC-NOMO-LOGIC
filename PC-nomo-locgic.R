library(rms)
library(foreign)
library(survival)
library(glmnet)
library(Hmisc)

setwd("D:/r-pan")
install.packages("forestplot")
#森林???
#http://www.iikx.com/news/statistics/1985.html
rs_forest<-read.csv("slt2-pc.csv")
View(rs_forest)
names(rs_forest)
library(forestplot)
pdf("bc-forest-2.pdf")
#tiff('bc-forest-1.tiff',height = 600,width = 700,res= 60)
forestplot(as.matrix(rs_forest[,1:2]), 
           rs_forest$v3, rs_forest$v4, rs_forest$v5, 
           zero = 1, clip = c(0.4,15), graph.pos = 2, 
           xticks = c(0.1,1,10,100), 
           boxsize=0.05, xlog=TRUE)
dev.off()


mydata2<-read.csv("mydata-2.csv")
mydata1<-read.csv("mydata-1.csv")
mydata3<-read.csv("mydata-3.csv")

summary(mydata1$sur)
sd(c(mydata1$sur))
summary(mydata1)
dd1 <- datadist(mydata1)
options(datadist='dd1')
mydata1

summary(mydata2$sur)
sd(c(mydata2$sur))
summary(mydata2)
dd2 <- datadist(mydata2)
options(datadist='dd2')

summary(mydata3$sur)
sd(c(mydata3$sur))
summary(mydata3)
dd3 <- datadist(mydata3)
options(datadist='dd3')

fmlap1<-as.formula(sur~race+tnm+diacon+statge+reason+regnode+pregnode+dxliver+dxlung+tsize+ext++cscur+csorg+agedia+insur)
fmlap2<-as.formula(sur~race+t+m+reason+dxlung+cscur+csorg+eth)
fmlap3<-as.formula(sur~grade+m+reason+regnode+dxliver+csorg+seqnum)

fmlap13<-as.formula(sur~grade+m+reason+regnode+dxliver+csorg+seqnum)
fmlap23<-as.formula(sur~grade+m+reason+regnode+dxliver+csorg+seqnum)

fmlap2<-as.formula(sur~.)
#fmlap32<-as.formula(sur~grade+m+reason+regnode+dxliver+ext+csorg+seqnum+racel+insur)
#fmlap21<-as.formula(sur~race+grade+statge+t+m+reason+dxlung+cscur+csorg+race.1+insur)


#fit<-lrm(fmla,data=mydata,x=T,y=T)
fit3<-lrm(fmlap3,data=mydata3,x=T,y=T)
fit2<-lrm(fmlap2,data=mydata2,x=T,y=T)
fit1<-lrm(fmlap1,data=mydata1,x=T,y=T)

fit23<-lrm(fmlap23,data=mydata2,x=T,y=T)
fit13<-lrm(fmlap13,data=mydata1,x=T,y=T)

#pit<-psm(cla~+uncs+uncsh+maad+siecs+bn,data=mydata,dist = 'lognormal')
fit1
summary(fit1)
nomomodelA <- nomogram(fit1,
                       lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Diagnostic possibility")
pdf("pan-nomo-1t.pdf")
plot(nomomodelA,xfrac = 0.15,cex.axis = 1,cex.var = 1.1)

dev.off()
nomomodelA
nomomodelA$total.points
nomomodelA
fit2
summary(fit2)
nomomodel <- nomogram(fit2,
                       lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Diagnostic possibility")
pdf("pan-nomo-2t.pdf")
plot(nomomodel,xfrac = 0.15,cex.axis = 1,cex.var = 1.1)

dev.off()
nomomodel


fit13
summary(fit13)
nomomodelA <- nomogram(fit13,
                       lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.2),
                       funlabel="Diagnostic possibility")
pdf("pan-nomo-1.pdf")
plot(nomomodelA,xfrac = 0.15,cex.axis = 1,cex.var = 1.1)
dev.off()
nomomodelA

fit23
summary(fit23)
nomomodel <- nomogram(fit23,
                      lp=F, 
                      fun=function(x)1/(1+exp(-x)),
                      fun.at=seq(0.1,1,by=0.2),
                      funlabel="Diagnostic possibility")


fit3
summary(fit3)
nomomodelc <- nomogram(fit3,
                       lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Diagnostic possibility")
pdf("pan-nomo-3t.pdf")
plot(nomomodelc,xfrac = 0.15,cex.axis = 1,cex.var = 1.1)

dev.off()
nomomodelc
summary(nomomodelc)
#dca curve
library(rmda)
modul=decision_curve(fmlap1,data=mydata1,
                     family=binomial(link='logit'),
                     thresholds=seq(0,1,by=0.01),
                     confidence.intervals=0.95)
modul2=decision_curve(fmlap2,data=mydata1,
                     family=binomial(link='logit'),
                     thresholds=seq(0,1,by=0.01),
                     confidence.intervals=0.95)

modul3=decision_curve(fmlap3,data=mydata1,
                      family=binomial(link='logit'),
                      thresholds=seq(0,1,by=0.01),
                      confidence.intervals=0.95)

List=list(modul,modul2,modul3)

pdf("pan-dca-123.pdf")
plot_decision_curve(List,
                    #curve.names = "Pan Prediction Nomogram",
                    curve.names=c('1-Year','2-Year','3-Year'),
                    xlab="Threshold Probability",ylab="Net Benifit",
                    cost.benefit.axis=FALSE,
                    #col=c('red','blue','green'),
                    confidence.intervals=FALSE,
                    standardize=FALSE)

dev.off()
modul
modul2
modul3


# com 1-100;bc 7-100;bn


#?ڲ? ??????????֤
# modul  the start is the first value of FPR less than 1,
#end is the value of last colom(snB) is less than 0
set.seed(30)
myc=validate(fit13,method = "b",B=30,pr=T,dxy=T)
c_index=(myc[1,5]+1)/2
c_index

set.seed(30)
myc=validate(fit23,method = "b",B=30,pr=T,dxy=T)
c_index=(myc[1,5]+1)/2
c_index

set.seed(30)
myc=validate(fit1,method = "b",B=30,pr=T,dxy=T)
c_index=(myc[1,5]+1)/2
c_index

set.seed(30)
myc=validate(fit2,method = "b",B=30,pr=T,dxy=T)
c_index=(myc[1,5]+1)/2
c_index

set.seed(30)
myc=validate(fit3,method = "b",B=30,pr=T,dxy=T)
c_index=(myc[1,5]+1)/2
c_index

#1.4-??????logistic?ع?-?漰nomogram????ͼ
#https://www.bilibili.com/video/BV1AV411d7rv
c=rcorrcens(sur~predict(fit1,newdata=mydata1),data=mydata1)
c[1,1]
c[1,1]-1.96*c[1,4]/2
c[1,1]+1.96*c[1,4]/2
summary(c)

c=rcorrcens(sur~predict(fit2,newdata=mydata2),data=mydata2)
c[1,1]
c[1,1]-1.96*c[1,4]/2
c[1,1]+1.96*c[1,4]/2
summary(c)

c=rcorrcens(sur~predict(fit3,newdata=mydata3),data=mydata3)
c[1,1]
c[1,1]-1.96*c[1,4]/2
c[1,1]+1.96*c[1,4]/2
summary(c)

# coxpe<-predict(fit)
# c_index=1-rcorr.cens(coxpe,mydata$Dia)
# c_index
library(ROCR)
pre_rate=predict(fit1)
roc1=prediction(pre_rate,mydata1$sur)
roc2=performance(roc1,"tpr","fpr")
auc=performance(roc1,"auc")

auc

pdf("pan-roc-1.pdf")
plot(roc2,col="blue",xlab="False Positive Rate",ylab="True Positive Rate",)
abline(0,1,lty=2,lwd=3)
dev.off()
summary(auc)


pre_rate=predict(fit2)
roc1=prediction(pre_rate,mydata2$sur)
roc2=performance(roc1,"tpr","fpr")
auc=performance(roc1,"auc")

auc

pdf("pan-roc-2.pdf")
plot(roc2,col="blue",xlab="False Positive Rate",ylab="True Positive Rate",)
abline(0,1,lty=2,lwd=3)
dev.off()
summary(auc)

pre_rate=predict(fit3)
roc1=prediction(pre_rate,mydata3$sur)
roc2=performance(roc1,"tpr","fpr")
auc=performance(roc1,"auc")

auc

pdf("pan-roc-3.pdf")
plot(roc2,col="blue",xlab="False Positive Rate",ylab="True Positive Rate",)
abline(0,1,lty=2,lwd=3)
dev.off()
summary(auc)


#缁樺埗鏅€氬垪绾垮浘


library(forestplot)

#fun.at=c(.01,.05,seq(.1,.9,by=.1),.95,.99)

#c(0.0001,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.9999)
#fun.at=seq(0.1,1,by=0.1)
#缁樺埗鏅€氬垪绾垮浘

plot(nomomodelA,xfrac=.45)
library(regplot)
# ????ģ?Ͳ?????????Ϣ
nomomodelA

#<- calibrate(fit,  method = "KM", B = 1000)
 
cal <- calibrate(fit1,  method = "boot", B = 1000)
pdf("pan-cali-1.pdf")
plot(cal, xlab = "Nomogram Predicted Survial", ylab = "Actual Survial",main = "Calibration Curve")
dev.off()

cal <- calibrate(fit2,  method = "boot", B = 1000)
pdf("pan-cali-2.pdf")
plot(cal, xlab = "Nomogram Predicted Survial", ylab = "Actual Survial",main = "Calibration Curve")
dev.off()

cal <- calibrate(fit3,  method = "boot", B = 1000)
pdf("pan-cali-3.pdf")
plot(cal, xlab = "Nomogram Predicted Survial", ylab = "Actual Survial",main = "Calibration Curve")
dev.off()


#cal <- calibrate(fit,cmethod="KM",method = 'boot',x=T,y=T)
#cal <- calibrate(fit,method ='boot'),x=TRUE,y=TRU
#plot(xlim = c(0,1.0),ylim = C(0,1.0),xlab = "prob dia",ylab = "ob dia")




#cal <- calibrate(fit,method = 'boot',x=TRUE,y=TRUE)
#cal <- calibrate(fit,method ='boot')
#plot(xlim = c(0,1.0),ylim = C(0,1.0),xlab = "prob dia",ylab = "ob dia")
#plot(cal)

#森林???
#http://www.iikx.com/news/statistics/1985.html1:2]
rs_forest<-read.csv("slt2.csv")
#View(dat)
names(rs_forest)
library(forestplot)
pdf("bc-forest.pdf")
tiff('bc-forest.tiff',height = 600,width = 700,res= 60)
forestplot(as.matrix(rs_forest[,1:2]), 
           rs_forest$v3, rs_forest$v4, rs_forest$v5, 
           zero = 1, clip = c(0.4,15), graph.pos = 2, 
           xticks = c(0.1,1,10,100), 
           boxsize=0.05, xlog=TRUE)
dev.off()

#https://www.meiwen.com.cn/subject/jjbmyftx.html
#https://www.plob.org/article/22371.html


forestplot(labeltext = as.matrix(rs_forest[,0:2]),rs_forest$v2,rs_forest$v3,rs_forest$v4,)
           
           #?????????ı?չʾ???У??˴??????????ݵ?ǰ??????Ϊ?ı?????ͼ??չʾ
           
          
           #mean = rs_forest$V2, #???þ?ֵ
           
           #lower = rs_forest$V3, #???þ?ֵ??lowlimits??
           
           #upper = rs_forest$V4, #???þ?ֵ??uplimits??
           
           #is.summary=c(T,T,T,F,F,T,F,F,T,F,F,T,F,F,F,T,F,F,T,F,F,T,F,F,T,F,F),
           
           #?ò???????һ???߼???��?????ڶ?????????ÿһ???Ƿ??ǻ???ֵ?????ǣ????ڶ?Ӧλ??????ΪTRUE??????????????ΪFALSE??????ΪTRUE???????Դ???????
           
           #zero = 1, #???ò???ֵ???˴?????չʾ????HRֵ???ʲ???ֵ??1????????0
           
          # boxsize = 0.4, #???õ??��Ƶķ??δ?С
           
           #lineheight = unit(8,'mm'),#????ͼ???е??о?
           
           #colgap = unit(2,'mm'),#????ͼ???е??м???
           
           #lwd.zero = 2,#???òο??ߵĴ?ϸ
           
           #lwd.ci = 2,#?????????��??ߵĴ?ϸ
           
           #col=fpColors(box='#458B00',summary="#8B008B",lines = 'black',zero = '#7AC5CD'),
           
           #ʹ??fpColors()????????ͼ??Ԫ?ص???ɫ?????????ҷֱ???Ӧ???��Ʒ??Σ?????ֵ???????��??ߣ??ο???
           
          # xlab="The estimates",#????x????ǩ
           
           #lwd.xaxis=2,#????X???ߵĴ?ϸ
           
          # lty.ci = "solid",
           
           #graph.pos = 1)#????ɭ??ͼ??λ?ã??˴?????Ϊ4?????????ڵ?????




dev.off()

library(regplot)

#glm model ???ã??ٲ?ʲô??˼?????????Իع?ģ?ͣ?????Ԥ??

modelC <- glm(fmla, data = mydata, family = binomial(link="logit"))
summary(modelC)
modelC

pdf("glmmodel.pdf")

regplot(modelC)
dev.off()

cbind(coef= coef(modelC),confint(modelC))
exp(cbind(OR= coef(modelC),confint(modelC)))

mydata$predmodelC<- predict(newdata=mydata,modelC,"response")

regplot(modelC,observation=mydata[1,]) 
regplot(modelC)

Lusurv<-Surv(time=Lung$time,event = Lung$status)
Lufit <- survfit(Lusurv~Lung$sex)
survdiff(Lusurv~Lung$sex)




dev = rawdata[rawdata$category==0,]
vad = rawdata[rawdata$category==1,]

#鎵撳寘鏁版嵁

ddist <- datadist(dev)
options(datadist='ddist')

#鏋勫缓涓変釜鍥炲綊妯″???

modelA2 <- lrm(MN ~lnuPCX,data=dev)
modelB2 <- lrm(MN ~ageper10 + eGFRper10 + DM,data=dev)
modelC2 <- lrm(MN ~ageper10 + eGFRper10 + DM + lnuPCX,data=dev)

#璁剧疆鍒楃嚎鍥惧弬鏁?
#绗竴琛宮odelA灏辨槸鍒氭墠logistic鍥炲綊鐨勬ā鍨嬪悕绉般€俵p閫夋嫨True鎴朏alse锛屾槸鍚︽樉绀虹嚎鎬ч娴嬪潗鏍囷紙linear predictor锛夛紝fun鏄鑷繁璁句竴涓嚱鏁帮紝瀵筶p杩涜杞崲锛屽苟寤虹珛涓€涓柊鍧愭爣杞淬€傛澶勫氨鐢╨ogit鍙樻崲鐨勫弽鍑芥暟锛屽皢lp杞崲涓烘垜浠啛鎮夌殑椋庨櫓姒傜巼-銆俧unction(x) 1/(1+exp(-x))杩欎竴涓诧紝鍗充娇鐢╢unction()鏋勫缓涓€涓嚜瀹氫箟鍑芥暟锛屾嫭鍙蜂腑鐨剎浠巐p鐨勮寖鍥翠腑鍙栧€硷紝浠ｅ叆1/(1+exp(-x))涓繍绠椼€?
#fun.at鍒欐槸缁欐柊鐨勫潗鏍囪酱璁剧疆鑼冨洿銆俧unlabel鍒欐槸缁欎笂闈㈣浆鎹㈠ソ鐨勬柊鍧愭爣杞磋捣涓悕瀛楋紝Diagnostic possibility銆傚叾瀹炴湁浜嗚繖鏉″潗鏍囪酱锛屼笂闈p閭ｉ噷涔熷彲浠ヨ涓篎锛屼笉鏄剧ず浜嗐€?

nomomodelA <- nomogram(modelA2,lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Diagnostic possibility")

nomomodelB <- nomogram(modelB2,lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Diagnostic possibility")

nomomodelC <- nomogram(modelC2,lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Diagnostic possibility")


#缁樺埗鏅€氬垪绾垮浘

plot(nomomodelA)
plot(nomomodelB)
plot(nomomodelC)

#缁樺埗浜や簰寮忓垪绾垮浘瀹夎绋嬪簭鍖卛nstall.packages("regplot")

library(regplot)

#浜や簰寮忓垪绾垮浘蹇呴』鐢╣lm鍑芥???

modelC <- glm(MN ~ageper10 + eGFRper10 + DM + lnuPCX, data = dev, family = binomial(link="logit"))
summary(modelC)
regplot(modelC) 



cbind(coef= coef(modelC),confint(modelC))
exp(cbind(OR= coef(modelC),confint(modelC)))

mydata$predmodelC<- predict(newdata=dev,modelC,"response")
regplot(modelC,observation=mydata[10,]) 




#LASSO分析
v1<-as.matrix(mydata[,c(3:35)])
v2<-mydata[,2]

#myfit<-glmnet(v1,v2,)

myfit = glmnet(v1,v2,family = "binomial")

pdf("lambda.pdf")
plot(myfit,xvar="lambda",label=TRUE)
dev.off()

myfit2=cv.glmnet(v1,v2,family="binomial")
pdf("min.pdf")
plot(myfit2)
abline(v=log(c(myfit2$lambda.min,myfit2$lambda.1se)),lty="dashed")
dev.off()
myfit2$lambda.min

coe=coef(myfit,s=myfit2$lambda.min)
act_index=which(coe!=0)
act_coe= coe[act_index]
row.names(coe)[act_index]
coe
act_coe

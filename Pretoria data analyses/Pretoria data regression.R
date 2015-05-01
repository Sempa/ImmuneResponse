rm(list=ls(all=TRUE))

library(foreign)
library(epicalc)

Pretoria<-read.csv(file.choose()) 
#read in a csv file

attach(Pretoria)
codebook(Pretoria)

sqrtCD4BL=sqrt(CD4m0)  # for normality
Pretoria=data.frame(Pretoria,sqrtCD4BL)#include it into the dataset

logVLslope12=logVLm12-logVLm0 #new variable
Pretoria=data.frame(Pretoria,logVLslope12) #include again

sapply(VL_supp_cat, data.class) #another categorical variable
VL_supp_cat.f=as.factor(VL_supp_cat)
Pretoria=data.frame(Pretoria,VL_supp_cat.f)

######################################

#Automated stepwise variable selection

#the variables incuded here should be compared with Marconi
#model1a=glm(sqrtCD4_gain~age_cont+as.factor(sex.f)+sqrtCD4BL+logVLslope12+VL_supp_cat.f +side_effects,na.action=na.exclude, family=gaussian)
#step(model1a, direction="backward")
#summary(model1a)
#confint(model1a)

#PLOT
#par(mfrow=c(1,1))
#plot(fitted(model1a),sqrtCD4_gain, ylim= c(0,30), xlim =c(0,30))
#abline(0,1)

#######################################

#Manually selected models

model1=glm(sqrtCD4_gain~age_cont+as.factor(sex.f)+sqrtCD4BL+logVLslope12+VL_supp_cat.f, na.action=na.exclude, family=gaussian)
summary(model1)

model2=glm(sqrtCD4_gain~age_cont+as.factor(sex.f)+sqrtCD4BL+totLOGVLpy0+VL_supp_cat.f,na.action=na.exclude, family=gaussian)
summary(model2)

model3=glm(meanCD4after2~age_cont+as.factor(sex.f)+sqrtCD4BL+logVLslope12+VL_supp_cat.f,na.action=na.exclude, family=gaussian)
summary(model3)

model4=glm(meanCD4after2~age_cont+as.factor(sex.f)+sqrtCD4BL+totLOGVLpy0+VL_supp_cat.f,na.action=na.exclude, family=gaussian)
summary(model4)

model5=glm(totCD4gain~age_cont+as.factor(sex.f)+sqrtCD4BL+logVLslope12+VL_supp_cat.f,na.action=na.exclude, family=gaussian)
summary(model5)

model6=glm(totCD4gain~age_cont+as.factor(sex.f)+sqrtCD4BL+totLOGVLpy0+VL_supp_cat.f,na.action=na.exclude, family=gaussian)
summary(model6)

######################################################

#Model1 diagnostics
residuals<-residuals(model1,type="deviance")
summary(residuals)
rstudent<-rstudent(model1)
summary(rstudent)
rstandard<-rstandard(model1)
summary(rstandard)
hvalues=hatvalues(model1)
summary(hvalues)
dstandard<-residuals/sqrt(1-hvalues)
summary(dstandard)
cooksd=cooks.distance(model1)
summary(cooksd)
dfbeta=dfbetas(model1)
summary(dfbeta)
vcov(model1)

fitted1<-fitted(model1)
Pretoria=data.frame(Pretoria,fitted1)#include it into the dataset

#PLOTS1
par(mfrow=c(1,1))
plot(fitted1,rstudent)
plot(cooksd,rstudent)
hist(residuals)

###############################################################

#Model2 diagnostics
residuals<-residuals(model2,type="deviance")
summary(residuals)
rstudent<-rstudent(model2)
summary(rstudent)
rstandard<-rstandard(model2)
summary(rstandard)
hvalues=hatvalues(model2)
summary(hvalues)
dstandard<-residuals/sqrt(1-hvalues)
summary(dstandard)
cooksd=cooks.distance(model2)
summary(cooksd)
dfbeta=dfbetas(model2)
summary(dfbeta)
vcov(model2)

fitted2<-fitted(model2)
Pretoria=data.frame(Pretoria,fitted2)#include it into the dataset

#PLOTS2
par(mfrow=c(1,1))
plot(fitted2,rstudent)
plot(cooksd,rstudent)
hist(residuals)

###################################################################

#Model3 diagnostics
residuals<-residuals(model3,type="deviance")
summary(residuals)
rstudent<-rstudent(model3)
summary(rstudent)
rstandard<-rstandard(model3)
summary(rstandard)
hvalues=hatvalues(model3)
summary(hvalues)
dstandard<-residuals/sqrt(1-hvalues)
summary(dstandard)
cooksd=cooks.distance(model3)
summary(cooksd)
dfbeta=dfbetas(model3)
summary(dfbeta)
vcov(model3)

fitted3<-fitted(model3)
Pretoria=data.frame(Pretoria,fitted3)#include it into the dataset

#PLOTS3
par(mfrow=c(1,1))
plot(fitted3,rstudent)
plot(cooksd,rstudent)
hist(residuals)

###############################################################

#Model4 diagnostics
residuals<-residuals(model4,type="deviance")
summary(residuals)
rstudent<-rstudent(model4)
summary(rstudent)
rstandard<-rstandard(model4)
summary(rstandard)
hvalues=hatvalues(model4)
summary(hvalues)
dstandard<-residuals/sqrt(1-hvalues)
summary(dstandard)
cooksd=cooks.distance(model4)
summary(cooksd)
dfbeta=dfbetas(model4)
summary(dfbeta)
vcov(model4)

fitted4<-fitted(model4)
Pretoria=data.frame(Pretoria,fitted4)#include it into the dataset

#PLOTS4
par(mfrow=c(1,1))
plot(fitted4,rstudent)
plot(cooksd,rstudent)
hist(residuals)

###############################################################

#Model5 diagnostics
residuals<-residuals(model5,type="deviance")
summary(residuals)
rstudent<-rstudent(model5)
summary(rstudent)
rstandard<-rstandard(model5)
summary(rstandard)
hvalues=hatvalues(model5)
summary(hvalues)
dstandard<-residuals/sqrt(1-hvalues)
summary(dstandard)
cooksd=cooks.distance(model5)
summary(cooksd)
dfbeta=dfbetas(model5)
summary(dfbeta)
vcov(model5)

fitted5<-fitted(model5)
Pretoria=data.frame(Pretoria,fitted5)#include it into the dataset

#PLOTS5
par(mfrow=c(1,1))
plot(fitted5,rstudent)
plot(cooksd,rstudent)
hist(residuals)

##################################################################

#Model6 diagnostics
residuals<-residuals(model6,type="deviance")
summary(residuals)
rstudent<-rstudent(model6)
summary(rstudent)
rstandard<-rstandard(model6)
summary(rstandard)
hvalues=hatvalues(model6)
summary(hvalues)
dstandard<-residuals/sqrt(1-hvalues)
summary(dstandard)
cooksd=cooks.distance(model6)
summary(cooksd)
dfbeta=dfbetas(model6)
summary(dfbeta)
vcov(model6)

fitted6<-fitted(model6)
Pretoria=data.frame(Pretoria,fitted6)#include it into the dataset

#PLOTS6
par(mfrow=c(1,1))
plot(fitted6,rstudent)
plot(cooksd,rstudent)
hist(residuals)

###################################################################

par(mfrow=c(1,1))

plot(fitted1,sqrtCD4_gain, ylim= c(-5,15), xlim =c(-5,15))
#abline(0,1)
abline(lsfit(fitted1,sqrtCD4_gain))

plot(fitted2,sqrtCD4_gain, ylim= c(-5,15), xlim =c(-5,15))
#abline(0,1)
abline(lsfit(fitted2,sqrtCD4_gain))

plot(fitted3,meanCD4after2, ylim= c(0,40), xlim =c(0,40))
#abline(0,1)
abline(lsfit(fitted3,meanCD4after2))

plot(fitted4,meanCD4after2, ylim= c(0,40), xlim =c(0,40))
#abline(0,1)
abline(lsfit(fitted4,meanCD4after2))

plot(fitted5,totCD4gain, ylim= c(-5,30), xlim =c(-5,30))
#abline(0,1)
abline(lsfit(fitted5,totCD4gain))

plot(fitted6,totCD4gain, ylim= c(-5,30), xlim =c(-5,30))
#abline(0,1)
abline(lsfit(fitted6,totCD4gain))

######################################################

rm(model1) 
rm(model2) 
rm(model3) 
rm(model4) 
rm(model5) 
rm(model6) 
rm(hvalues) 
rm(residuals) 
rm(rstandard) 
rm(rstudent)
rm(cooksd)
rm(dstandard)
rm(dfbeta)

write.csv(Pretoria, file = "Pretoria after R.csv")

#####################################################

###inverse proportional weighting (ipw) when you want to use GEE with a time-dependent covariate

library(nlme)
options(show.signif.stars=F)

Pretoria_fail<-read.csv(file.choose())  ###pretoria_long_fromStata.csv
Pretoria_fail

attach(Pretoria_fail)
library(epicalc)
codebook(Pretoria_fail)

#####################All the following unnecessary

sex.f=as.factor(sex)
Pretoria_fail=data.frame(Pretoria_fail,sex.f)

agecat.f=as.factor(agecat)
Pretoria_fail=data.frame(Pretoria_fail,agecat.f)

WHO_stage.f=as.factor(WHO_stage)
Pretoria_fail=data.frame(Pretoria_fail,WHO_stage.f)

dist_clinic_cat.f=as.factor(dist_clinic_cat)
Pretoria_fail=data.frame(Pretoria_fail,dist_clinic_cat.f)

employed.f=as.factor(employed)
Pretoria_fail=data.frame(Pretoria_fail,employed.f)

change.f=as.factor(change)
Pretoria_fail=data.frame(Pretoria_fail,change.f)

interrupted.f=as.factor(interrupted)
Pretoria_fail=data.frame(Pretoria_fail,interrupted.f)

stopped.f=as.factor(stopped)
Pretoria_fail=data.frame(Pretoria_fail,stopped.f)

side_effects.f=as.factor(side_effects)
Pretoria_fail=data.frame(Pretoria_fail,side_effects.f)

defaulter.f=as.factor(defaulter)
Pretoria_fail=data.frame(Pretoria_fail,defaulter.f)

Nonadherer.f=as.factor(Nonadherer)
Pretoria_fail=data.frame(Pretoria_fail,Nonadherer.f)

Transferred.f=as.factor(Transferred)
Pretoria_fail=data.frame(Pretoria_fail,Transferred.f)

Untraceable.f=as.factor(Untraceable)
Pretoria_fail=data.frame(Pretoria_fail,Untraceable.f)

BL_CD4_strata.f=as.factor(BL_CD4_strata)
Pretoria_fail=data.frame(Pretoria_fail,BL_CD4_strata.f)

#################


Pretoria_fail$Nonadherer[Pretoria_fail$Nonadherer=="?"]=NA
Pretoria_fail$Nonadherer[Pretoria_fail$Nonadherer=="Y"]="y"
Pretoria_fail$Untraceable[Pretoria_fail$Untraceable==""]=NA

###################

##reshape
##names(wide_panels_martin)<-c("PT_NUMBER","CD4m.0","week.2","week.3","week.4","week.5","week.6","week.7","week.8")
names(wide_panels_martin)

## varying = c(names(wide_panels_martin)[2:10],names(wide_panels_martin)[25:30])

####Pretoria_LONG<-reshape(wide_panels_martin,idvar=c("PT_NUMBER"), varying=c("CD4m.0","CD4m.6","CD4m.12","CD4m.18","CD4m.24","CD4m.30","CD4m.36","CD4m.42", "CD4m.48", "CD4m.54", "CD4m.60", logVLm.0, logVLm.6,	logVLm.12,	logVLm.18	logVLm.24	logVLm.30	logVLm.36	logVLm.42	logVLm.48	logVLm.54	logVLm.60
"VLm.30"          "VLm.36"          "VLm.42"          "VLm.48"          "VLm.54"         
[25] "VLm.60"          "VL400m.0"        "VL400m.6"        "VL400m.12"       "VL400m.18"       "VL400m.24"      
[31] "VL400m.30"       "VL400m.36"       "VL400m.42"       "VL400m.48"       "VL400m.54"       "VL400m.60"      
[37] "VL400ANY"        "consec400m.0"    "consec400m.6"    "consec400m.12"   "consec400m.18"   "consec400m.24"  
[43] "consec400m.30"   "consec400m.36"   "consec400m.42"   "consec400m.48"   "consec400m.54"   "consec400m.60"  
[49] "consec400ANY"    "consec1000m.0"   "consec1000m.6"   "consec1000m.12"  "consec1000m.18"  "consec1000m.24" 
[55] "consec1000m.30"  "consec1000m.36"  "consec1000m.42"  "consec1000m.48"  "consec1000m.54"  "consec1000m.60" 
[61] "consec1000ANY"   "consec5000m.0"   "consec5000m.6"   "consec5000m.12"  "consec5000m.18"  "consec5000m.24" 
[67] "consec5000m.30"  "consec5000m.36"  "consec5000m.42"  "consec5000m.48"  "consec5000m.54"  "consec5000m.60" 
[73] "consec5000ANY"   "CD4_FAIL_lt500"  "afailm.0"        "afailm.6"        "afailm.12"       "afailm.18"      
[79] "afailm.24"       "afailm.30"       "afailm.36"       "afailm.42"       "afailm.48"       "afailm.54"      
[85] "afailm.60"       "aFAILany"        "bfailm.0"        "bfailm.6"        "bfailm.12"       "bfailm.18"      
[91] "bfailm.24"       "bfailm.30"       "bfailm.36"       "bfailm.42"       "bfailm.48"       "bfailm.54"      
[97] "bfailm.60"       "bFAILany"        "cfailm.0"        "cfailm.6"        "cfailm.12"       "cfailm.18"      
[103] "cfailm.24"       "cfailm.30"       "cfailm.36"       "cfailm.42"       "cfailm.48"       "cfailm.54"      
[109] "cfailm.60"       "cFAILany"        "abcfailm.0"      "abcfailm.6"      "abcfailm.12"     "abcfailm.18"    
[115] "abcfailm.24"     "abcfailm.30"     "abcfailm.36"     "abcfailm.42"     "abcfailm.48"     "abcfailm.54"    
[121] "abcfailm.60"),direction="long")

###########################################################  
library(geepack)
gee1<-geeglm(VL400m~sqrtCD4m, id=PT_NUMBER, family=binomial(link=logit),data=pretoria_long, corstr="ar1", wave=month)
###does not handle missing data


###controlS=glmmPQLControl(maxIter=1200,pnlsMaxIter=1200,msMaxIter=1200,niterEM=1200,pnlsTol=1e-6,##? bose = TRUE)
###glmPQL does not run stepwise, no AIC

library(nlme)
options(show.signif.stars=F)

names(Pretoria_fail)
mod1<- glmmPQL(VL400m~sqrtCD4m+month+BL_CD4_strata2+age_cont+BMI+as.factor(WHO_stage)+employed+change+interrupted+stopped+defaulter+side_effects+Nonadherer+fail_supp_6+Transferred+Untraceable+dist_clinic_cat2+tb+sex,random=~1|PT_NUMBER,family=binomial(link=logit),data=Pretoria_fail)
#remove distance to clinic
mod2<- glmmPQL(VL400m~sqrtCD4m+month+BL_CD4_strata2+age_cont+BMI+as.factor(WHO_stage)+employed+change+interrupted+stopped+defaulter+side_effects+Nonadherer+fail_supp_6+Transferred+Untraceable+tb+sex,random=~1|PT_NUMBER,family=binomial(link=logit),data=Pretoria_fail)
#remove untraceable
mod3<- glmmPQL(VL400m~sqrtCD4m+month+BL_CD4_strata2+age_cont+BMI+as.factor(WHO_stage)+employed+change+interrupted+stopped+defaulter+side_effects+Nonadherer+fail_supp_6+Transferred+as.factor(tb)+sex,random=~1|PT_NUMBER,family=binomial(link=logit),data=Pretoria_fail)
#remove transferred
mod4<- glmmPQL(VL400m~sqrtCD4m+month+BL_CD4_strata2+age_cont+BMI+as.factor(WHO_stage)+employed+change+interrupted+stopped+defaulter+side_effects+Nonadherer+fail_supp_6+as.factor(tb)+sex,random=~1|PT_NUMBER,family=binomial(link=logit),data=Pretoria_fail)
#etc etc
summary(mod4)
  
###Removal of 'stopped' caused the system to crash. Remove 'employed' next
###need glmmPQL


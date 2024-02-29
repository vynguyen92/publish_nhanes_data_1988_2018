library('rms')
library('ggplot2')
#devtools::install_github('chjackson/msm')
library('msm')
library("survival")
##input dataset
analysis_mort <- read.csv("Analysis_mort.csv",header=T)
#keep variables
myvars <- c("SEQN"
            , "RIAGENDR"
            , "RIDAGEYR"
            , "RIDRETH1"
            , "WTMEC4YR"
            , "SDMVPSU"
            , "SDMVSTRA"
            , "LBXTC"
            , "PERyear_EXM"
            , "mort_all"
            , "SDDSRVYR")
analysis_mort <-analysis_mort[myvars]
##categorical variable as factor
analysis_mort$RIAGENDR <- as.factor(analysis_mort$RIAGENDR)
analysis_mort$RIDRETH1 <- as.factor(analysis_mort$RIDRETH1)

######Cox regression without weight########
fit <- cph(Surv(PERyear_EXM,mort_all)~rcs(LBXTC,c(141,200,275))+RIDAGEYR+RIAGENDR+RIDRETH1,data=analysis_mort)
anova(fit) 
#set environment for further estimation.
dd<- datadist(analysis_mort)
options(datadist = 'dd')
dd$limits$LBXTC[2]<-200
fit1 <-update(fit)
HR<- Predict(fit1,LBXTC,fun=exp,ref.zero = TRUE)
p2<-ggplot(HR)
p2


##########Cox regreesion with weight#######
library('survey')
library('rms')
library('ggplot2')
library("survey")
##construct spline
analysis_mort$dose1 <- analysis_mort$LBXTC
analysis_mort$dose2 <-(pmax(analysis_mort$LBXTC-141,0)^3-
                       pmax(analysis_mort$LBXTC-200,0)^3*(275-141)/(275-200)+
                       pmax(analysis_mort$LBXTC-275,0)^3*(200-141)/(275-200))/(275-141)^2   

NHANES_survey <- svydesign(data=analysis_mort, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC4YR, nest=TRUE)
#svy.fit <- svycoxph(Surv(PERyear_EXM,mort_all)~rcs(LBXTC,c(141,200,275))+RIDAGEYR+RIAGENDR+RIDRETH1,design=NHANES_survey,data=analysis_mort)
svy.fit <- svycoxph(Surv(PERyear_EXM,mort_all)~dose1+dose2+RIDAGEYR+RIAGENDR+RIDRETH1,design=NHANES_survey,data=analysis_mort)
summary(svy.fit)
####obtain the hr estimate#########
analysis_mort$lnhr <- -0.005321161*(analysis_mort$dose1-200) + 0.006857583*(analysis_mort$dose2-11.44)
analysis_mort$hr <- exp(analysis_mort$lnhr)
#####draw spline plot#########
ggplot()+theme_classic()+geom_line(data = analysis_mort,
                                   aes(LBXTC,hr),linetype='solid',size=1,alpha=1)+scale_x_continuous(limits=c(100,400),breaks=seq(100,400,100))+
  scale_y_continuous(limits = c(0,3.0),breaks = seq(0,3,0.5))+
  geom_hline(yintercept = 1,linetype=2,size=0.25)

#delta method to estimate SE, and Z distribution to CIs.
##reference
###https://stats.oarc.ucla.edu/r/faq/how-can-i-estimate-the-standard-error-of-transformed-regression-parameters-in-r-using-the-delta-method/

install.packages("packagename")
library(packagename)
update.packages()
library(msm)

analysis_mort$se <- NA
s2 <- vcov(svy.fit)[1:2,1:2]
for (i in 1:8261){
b1 <- analysis_mort$dose1[i]
b2 <- analysis_mort$dose2[i]
analysis_mort$se[i] <- deltamethod( ~ x1*(b1-200) + x2*(b2-11.44), c(-0.005321161, 0.006857583), s2)
}
analysis_mort$lci <- exp(analysis_mort$lnhr-1.96*analysis_mort$se)
analysis_mort$hci <- exp(analysis_mort$lnhr+1.96*analysis_mort$se)


###draw weighted RCS######
ggplot()+geom_line(data = analysis_mort,
                                   aes(LBXTC,hr),linetype='solid',size=1,alpha=1)+
  geom_ribbon(data=analysis_mort,aes(LBXTC,ymin=lci,ymax=hci),alpha=0.2,fill='red')+
  scale_x_continuous(limits=c(100,400),breaks=seq(100,400,100))+
  scale_y_continuous(limits = c(0,3.5),breaks = seq(0,3,0.5))+
  geom_hline(yintercept = 1,linetype=2,size=0.25)


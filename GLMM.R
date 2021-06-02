rm(list=ls()) 
#-----------------------------------------------------------------------------------------------------------------------
# LOAD DATA 
setwd("/Users/xxx")
xdata=read.csv(file="/Users/leo/Dropbox/LEO (1)/C.txt", header=T,sep="\t", comment.char="", as.is=T, quote="")

#-----------------------------------------------------------------------------------------------------------------------

str(xdata)
head(xdata)

# COLUMN CHECK

# COLUMN CHECK
table(xdata$ID, useNA = "ifany")
table(xdata$type, useNA = "ifany")
table(xdata$AA, useNA = "ifany")
hist(xdata$Growth) # normal distributed?

hist(log(xdata$Growth))

# Log Transform the Data
xdata$Growth=log(xdata$Growth)

#hist(sqrt(xdata$Growth))

hist(xdata$AA)

hist(xdata$Growth)

# evtl transform response
#xdata$Growth=sqrt(xdata$Growth)


#----------------------------------------------------------------------------------------------------------------------
# Re-Level factors
xdata$type=as.factor(xdata$type)
xdata$type=relevel(xdata$type, "0")

# z-Transformation
xdata$z.AA=scale(xdata$AA)

#----------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# MODEL PREP
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("lme4")
install.packages("Matrix")
library(Matrix)
library(lme4)
contrl=lmerControl(optimizer="bobyqa")
contrl$optCtrl=list(maxfun=100000)
contrl$calc.derivs=F


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
source("/Users/leo/Dropbox/LEO (1)/RImages and Skripts/Diagnostic_fcns.r")

data.prep=fe.re.tab(fe.model="Growth ~ type * z.AA"
                    , re=c("ID")
                    , data=xdata
                    , treat.covs.as.factors=F
)
# look at the result of function
data.prep$summary
data.prep$pot.terms.with.corr
cdata=data.prep$data


# write dummy into xdata
xdata$type.1 = cdata$type.1



R.log.full=lmer(Growth ~ type * z.AA
                + (1+type.1+z.AA||ID)
                , data=xdata, control=contrl, REML=F)

summary(R.log.full)

#Linear mixed model fit by maximum likelihood  ['lmerMod']
#Formula: Growth ~ type * z.AA + ((1 | ID) + (0 + type.1 | ID) + (0 + z.AA |      ID))
#Data: xdata
#Control: contrl
#
#AIC      BIC   logLik deviance df.resid 
#220.2    244.8   -102.1    204.2      152 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.1310 -0.6729  0.0432  0.6598  3.6987 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#ID       (Intercept) 0.02072  0.1439  
#ID.1     type.1      0.04283  0.2070  
#ID.2     z.AA        0.00000  0.0000  
#Residual             0.18568  0.4309  
#Number of obs: 160, groups:  ID, 20
#
#Fixed effects:
#  Estimate Std. Error t value
#(Intercept) -3.28315    0.06628 -49.536
#type1        0.56124    0.11432   4.909
#z.AA        -0.49496    0.06649  -7.445
#type1:z.AA  -0.14267    0.11468  -1.244
#
#Correlation of Fixed Effects:
#  (Intr) type1  z.AA  
#type1      -0.580              
#z.AA        0.000  0.000       
#type1:z.AA  0.000  0.000 -0.580




#---------------------------------------
# DIAGNOSTIC
source("/Users/leo/Dropbox/LEO (1)/RImages and Skripts/Diagnostic_fcns.r")
diagnostics.plot(R.log.full)
# save plot
dev.copy2pdf(file = "/Users/leo/Dropbox/LEO (1)/diagnosticsC.pdf")

AIC(R.log.full)+2*sum(xdata$Growth) 
# -829.822
ranef.diagn.plot(R.log.full)
dev.copy2pdf(file = "/Users/leo/Dropbox/LEO (1)/diagnosticsC_2.pdf")

#---------------------------------------
# Full model vs Null Model

# null model

R.log.null=lmer(Growth ~ #type * z.AA
                + (1+type.1+z.AA||ID)
                , data=xdata, control=contrl, REML=F)

(LRT.FN=anova(R.log.null,R.log.full, test="Chisq"))


#Data: xdata
#Models:
#  R.log.null: Growth ~ +((1 | ID) + (0 + type.1 | ID) + (0 + z.AA | ID))
#R.log.full: Growth ~ type * z.AA + ((1 | ID) + (0 + type.1 | ID) + (0 + z.AA | 
#                                                                      R.log.full:     ID))
#npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)    
#R.log.null    5 251.03 266.41 -120.52   241.03                        
#R.log.full    8 220.22 244.83 -102.11   204.22 36.81  3  5.048e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# CHECK INTERACTION 
R.log.red=lmer(Growth ~ type + z.AA
               + (1+type.1+z.AA||ID)
               , data=xdata, control=contrl, REML=F)

(LRT.IA=anova(R.log.full,R.log.red, test="Chisq"))

#Data: xdata
#Models:
#  R.log.red: Growth ~ type + z.AA + ((1 | ID) + (0 + type.1 | ID) + (0 + z.AA | 
#                                                                       R.log.red:     ID))
#R.log.full: Growth ~ type * z.AA + ((1 | ID) + (0 + type.1 | ID) + (0 + z.AA | 
#                                                                      R.log.full:     ID))
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#R.log.red     7 219.71 241.24 -102.86   205.71                     
#R.log.full    8 220.22 244.83 -102.11   204.22 1.4844  1     0.2231


(LRT.IA=anova(R.log.null,R.log.red, test="Chisq")) # for when interaction is not significant, B

#Data: xdata
#Models:
#  R.log.null: Growth ~ +((1 | ID) + (0 + type.1 | ID) + (0 + z.AA | ID))
#R.log.red: Growth ~ type + z.AA + ((1 | ID) + (0 + type.1 | ID) + (0 + z.AA | 
#                                                                     R.log.red:     ID))
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
#R.log.null    5 251.03 266.41 -120.52   241.03                         
#R.log.red     7 219.71 241.24 -102.86   205.71 35.325  2  2.134e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



diagnostics.plot(R.log.red)
(R.log.red.drop1=drop1(R.log.red, test="Chisq"))

#Single term deletions
#
#Model:
#  Growth ~ type + z.AA + ((1 | ID) + (0 + type.1 | ID) + (0 + z.AA | 
#                                                            ID))
#npar    AIC    LRT   Pr(Chi)    
#<none>      219.71                     
#type      1 231.11 13.400 0.0002517 ***
#  z.AA      1 246.12 28.405  9.84e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Testing model stability

install.packages("parallel")
library(parallel)
source("/Users/leo/Dropbox/LEO (1)/RImages and Skripts/Glmm_stability.r")
R.log.full.mstab=glmm.model.stab(model.res=R.log.full, contr=contrl, para=T)
R.log.full.mstab$summary


source("/Users/leo/Dropbox/LEO (1)/RImages and Skripts/Boot_glmm.r")
R.log.full.CI=boot.glmm.pred(model.res=R.log.full, excl.warnings=F, nboots=1000, para=F, use=c("species","category"))
round(R.log.full.CI$ci.estimates, 3)

#              orig  X2.5. X97.5.
#(Intercept) -3.283 -3.417 -3.147
#type1        0.561  0.344  0.775
#z.AA        -0.495 -0.619 -0.364
#type1:z.AA  -0.143 -0.368  0.083



# EFFECT SIZES
install.packages("piecewiseSEM", repos = "http://cran.us.r-project.org")
library(piecewiseSEM)
(R.log.full.ES =rsquared(R.log.full))
source(("/Users/leo/Dropbox/LEO (1)/RImages and Skripts/func.r"))
(R.log.full.ESrog =ES(res=R.log.full))

#$ESmarg
#[1] 0.9901265
#
#$EScond
#[1] 0.1246727


# VIF 
install.packages("carData", repos = "http://cran.us.r-project.org")
library(car)
(R.log.full.vif=vif(R.log.full))
# 
#



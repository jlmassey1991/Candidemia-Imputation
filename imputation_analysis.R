
install.packages("mice")
install.packages("Rcpp")
install.packages("writexl")
install.packages("miceafter")
install.packages("devtools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("haven")
install.packages("readr")

require("miceafter")


library(writexl)
library(mice)
library(Rcpp)
library(dplyr)
library(lubridate) #dates
library(haven) #read in sas
library(readxl)
library(miceafter)
library(readr)

sessionInfo()

#setwd("~/EIS/Candidemia/Candidemia mortality MDB/Data/")

#loading environment
load('candidemiaimpute_AF.RData')
load('candidemiaimpute.RData')
load('candidemiaimpute_all.RData')


mortality <- read_sas('mortality_trim.sas7bdat')



#exploring data
#table(mortality$ethnicity,mortality$mortality30d, useNA = 'always')
#table(mortality$initialaf)
#https://rpubs.com/mhanauer/mice

#head(mortality) #looks at top rows
#class(mortality$age_years)#look at the class (numeric vs character) at the age column
#str(mortality) #proc contents equivalent
#summary(mortality)#check frequencies or summary statistics for each variable

#lapply(mortality, class) #listing all classes of variables


#classifying all numeric as factors so dummy variables will be factors
mortality[sapply(mortality, is.numeric)] <- lapply(mortality[sapply(mortality, is.numeric)],as.factor) 
names <- c('age_years' ,'hosp_lengthprior') 
mortality[,names] <- lapply(mortality[,names] , as.numeric) #changing actual numbers back to numeric

#changing all characters to factors
mortality[sapply(mortality, is.character)] <- lapply(mortality[sapply(mortality, is.character)],as.factor) 
#names2 <- c() 
#mortality[,names2] <- lapply(mortality[,names2] , as.character) 

#to check levels of all of the variables in your dataset
summary(mortality)

#defining reference values
mortality$racecat <- relevel(mortality$racecat, ref='1')
mortality$ethnicity <- relevel(mortality$ethnicity, ref='2')
mortality$cvcremovecat <- relevel(mortality$cvcremovecat, ref='1')
#mortality$daystotreatmentcat <- relevel(mortality$daystotreatmentcat, ref='1') 
mortality$initialaf <- relevel(mortality$initialaf, ref='Echinocandins') 
#daystotreatmentcat- 0- tx before DISC, 1- 0-2 days after DISC, 2- 3-5d after DISC, 3- 6+ days after DISC, 4- no tx
#mortality <- subset(mortality, select = -c(daystotreatmentcat, othsites___csf, othsites___pleuralfl))
str(mortality$mortality30d)
mortality$mortality30d <- relevel (mortality$mortality30d, ref="0")


# complete case model
mortality_comp <- mortality[complete.cases(mortality), ]#deleting cases with missing
summary(mortality_comp)
completecase_iaf <- glm(mortality30d ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc +
                          species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                          hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                          underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                          underlycond___malig + underlycond___neuro + underlycond___kidn + 
                          othsites___peritonealfl  + addlorgs + sysantibact + 
                          cvcremovecat + hosp_lengthprior + tpn + initialaf + neutropenia + socialhist___idu +preadmloccat, family="binomial", 
                        data = mortality_comp)
completecase_iaf2 <- step(completecase_iaf, direction="backward")
completecasemodel_AF<- summary(completecase_iaf2)
completecasemodel_AF
completecase_iaf_OR <- exp(coefficients(completecase_iaf2))
completecase_iaf_OR
completecase_iaf_CI <- exp(confint(completecase_iaf2))
completecase_iaf_CI



#looking at patterns of missing
md.pattern(mortality)

install.packages("VIM")
library(VIM)
aggr_plot <- aggr(mortality, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE)
#marginplot(mortality[c(1,2)])
#marginplot(mortality[c(3,4)])
#marginplot(mortality[c(5,6)])

?function()
  #dplyr::mutate
  
  #first run of mice
  
  #Code to manipulate variables??
  #init = mice(mortality, maxit=0) 
  #meth = init$method
  #predM = init$predictorMatrix
  #predM(c('stateid','patientid', 'txhospid'))=0 variables to skip as a predictor, but will be imputed
  #meth(C('stateid','patientid', 'txhospid'))=0 #variables to skip imputing, but will be used as a predictor
  
  imput_AF<- mice(data = mortality,
                  seed = 12345, #allows it to have the same imputation every time
                  m = 10, #make 5 datasets "So if calculation is not prohibitive, we may set m to the average percentage of missing data. The substantive conclusions are unlikely to change as a result of raising beyond m=5."
                  defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                  maxit = 5 #number of iterations
  )

imput_AF

warnings()

#if you have questions, can type "?mice" and that'll give you a list of variable options
?mice


save(imput_AF, file="impute_AF_3-26-24.Rdata") #saving imputed dataset
#imput40AF <- imput_AF_final
load(file = "impute_AF_3-26-24.Rdata") #loading imputed data


imput_AF$loggedEvents
table(mortality$initialaf, mortality$res_azole, mortality$res_echino)



#checking plots
#graphs should be intertwining and converging instead of going everywhere
plot(imput_AF )

#some recommend to set datasets to the percentage missing- overall missing on dataset is 2.7%


#Complete model with all variables?
fit <- with(imput_AF, glm(mortality30d ~ age_years  + sex + racecat + ethnicity + state + year + species_albc + species_glabc #+
                          # species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                          # hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                          #  underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                          # underlycond___malig + underlycond___neuro + underlycond___kidn + 
                          # othsites___peritonealfl  + addlorgs + sysantibact +
                          # cvcremovecat + hosp_lengthprior + tpn + daystotreatmentcat + neutropenia + socialhist___idu
                          # + othsites___pleuralfl + othsites___csf + initialaf
                          , 
                          family = "binomial"))
summary(pool(fit))

#Computation- Stepwise selection
scope <- list(upper = ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc +
                species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                underlycond___malig + underlycond___neuro + underlycond___kidn + 
                othsites___peritonealfl  + addlorgs + sysantibact + 
                cvcremovecat + hosp_lengthprior + tpn + initialaf + neutropenia + socialhist___idu + preadmloccat,
              lower = ~1)
expr <- expression(f1 <- glm(mortality30d ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc +
                               species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                               hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                               underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                               underlycond___malig + underlycond___neuro + underlycond___kidn + 
                               othsites___peritonealfl  + addlorgs + sysantibact + 
                               cvcremovecat + hosp_lengthprior + tpn + initialaf + neutropenia + socialhist___idu + preadmloccat, family="binomial"),
                   f2 <- step(f1, direction="backward"))
fit2 <- with(imput_AF, expr)
?mice
?step

fit2[[1]]

formulas <- lapply(fit2$analyses, formula) 
terms <- lapply(formulas, terms)
votes <- unlist(lapply(terms, labels))
table(votes)


#looking at first imputed dataset
imputeddataset <- complete(imput40AF)
table(imputeddataset$InitialAntifungal, imputeddataset$res_azole, imputeddataset$res_echino)
xyplot(imput40AF,mortality30d ~ addlorgs + age_years + cvcremovecat + hosp_lengthprior +
         hosp90dbeforecx + icubeforecx+ initialaf +  othsites___peritonealfl + 
         socialhist___idu + species_paral + surg___abdom + surg___nonabdom + 
         underlycond___liver + underlycond___malig + underlycond___metabol + year,pch=18,cex=1)


#do significance test to decide whether to keep a specific variable
#othsites___peritonealfl, underlycond___cardio_new and year
#othsites___peritonealfl p=0.1265
#underlycond___cardio_new p=0.2159
#year p=0.1224
#remove all

fit.without <- with(imput_AF, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + hosp_lengthprior +
                                    icubeforecx+ initialaf + neutropenia + preadmloccat + 
                                    socialhist___idu + species_parac + surg___abdom + surg___nonabdom +
                                    underlycond___liver + underlycond___lung + underlycond___malig + underlycond___metabol
                                  , family = "binomial"))
fit.with <- with(imput_AF, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + hosp_lengthprior +
                                 icubeforecx+ initialaf + neutropenia + preadmloccat + 
                                 socialhist___idu + species_parac + surg___abdom + surg___nonabdom +
                                 underlycond___liver + underlycond___lung + underlycond___malig + underlycond___metabol
                               # + hosp90dbeforecx + racecat + res_echino + sysantibact + tpn
                               , family = "binomial"))
D1(fit.with, fit.without)
warning()

+ hosp90dbeforecx + racecat + res_echino + sysantibact + tpn

#testing all variables with <5 votes;
#hosp90dbeforecx- 0.1985
#racecat- 0.2017
#res_echino-0.2839
#sysantibact- 0.1932
#tpn- 0.2370

#all wald test p values are >0.05 so does not impact predictability, will exclude from final model;



# FINAL MODEL, excluding res_echino, underlycond___cardio_new, underlycond___kidn because it was in less than half the MI models and failed Wald test
fit <- with(imput_AF, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + hosp_lengthprior +
                            icubeforecx+ initialaf + neutropenia + preadmloccat + 
                            socialhist___idu + species_parac + surg___abdom + surg___nonabdom +
                            underlycond___liver + underlycond___lung + underlycond___malig + underlycond___metabol
                          ,family = "binomial"(link="logit")))

summary(pool(fit))
summary(pool(fit),conf.int=TRUE,conf.level=0.95)

#creates an Excel file to make copy/pasting easier into table
finalmodel_imput_AF <- summary(pool(fit),conf.int=TRUE,conf.level=0.95)
options(scipen=999)
finalmodel_imput_AF
write_xlsx(finalmodel_imput_AF,"~/EIS/Candidemia/Candidemia mortality MDB/Data/finalmodel_AF.xlsx")

#saving the environment
save.image(file='candidemiaimpute_all.RData')
dir()

round(summary(pool(fit), conf.int = TRUE, exponentiate = TRUE), 3)




#citing R
citation()
version()
R.Version()
citation("mice")









































##############################################
mortality2 <- read_sas('mortality_trim.sas7bdat')

#classifying all numeric as factors so dummy variables will be factors
mortality2[sapply(mortality2, is.numeric)] <- lapply(mortality2[sapply(mortality2, is.numeric)],as.factor) 
names <- c('age_years' ,'hosp_lengthprior', 'year') 
mortality2[,names] <- lapply(mortality2[,names] , as.numeric) #changing actual numbers back to numeric

#changing all characters to factors
mortality2[sapply(mortality2, is.character)] <- lapply(mortality2[sapply(mortality2, is.character)],as.factor) 
#names2 <- c() 
#mortality[,names2] <- lapply(mortality[,names2] , as.character) 


#defining reference values
mortality2$racecat <- relevel(mortality2$racecat, ref='1')
mortality2$ethnicity <- relevel(mortality2$ethnicity, ref='2')
mortality2$cvcremovecat <- relevel(mortality2$cvcremovecat, ref='1')
mortality2$daystotreatmentcat <- relevel(mortality2$daystotreatmentcat, ref='1') 
#daystotreatmentcat- 0- no treatment, 1- tx before DISC, 2- 0-2 days after DISC, 3- 3-5d after DISC, 4- 6+ days after DISC
mortality2 <- subset(mortality2, select = -c(initialaf, othsites___csf, othsites___pleuralfl))
str(mortality$mortality30d)
mortality2$mortality30d <- relevel (mortality2$mortality30d, ref="0")

# complete case model with days to treatment
mortality_comp2 <- mortality2[complete.cases(mortality2), ]#deleting cases with missing
f1_cc2 <- glm(mortality30d ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc +
                species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                underlycond___malig + underlycond___neuro + underlycond___kidn + 
                othsites___peritonealfl  + addlorgs + sysantibact + 
                cvcremovecat + hosp_lengthprior + tpn + daystotreatmentcat + neutropenia + socialhist___idu +preadmloccat
              , family="binomial", 
              data = mortality_comp2)
f2_cc2 <- step(f1_cc2, direction="backward")
completecasemodel_DTT<- summary(f2_cc2)
completecasemodel_DTT
f2_cc2OR <- exp(coefficients(f2_cc2))
f2_cc2OR
f2_cc2CI <- exp(confint(f2_cc2))
f2_cc2CI 

summary(mortality2)

#imputation
imput40_DTT <- mice(data = mortality2,
                    seed = 12345, #allows it to have the same imputation every time
                    m = 10, #make 5 datasets
                    defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                    maxit = 5 #number of iterations, default is 5, depends on missingness)
)
imput40_DTT$loggedEvents #40 iterations, 10 datasets
warnings()

summary(imput40_DTT)

plot(imput40_DTT) 

save(imput40_DTT,file="impute40_DTT.Rdata")

#complete model with all vars
fit_DTT <- with(imput10, glm(mortality30d ~ age_years + sex + racecat + ethnicity + state + year + species_albl + species_glabl +
                               species_paral + species_tropl + species_dubl + species_otherl + species_multil + res_echino + res_azole +
                               hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                               underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                               underlycond___malig + underlycond___neuro + underlycond___kidn + 
                               othsites___peritonealfl + addlorgs + sysantibact +
                               cvcremovecat + hosp_lengthprior + tpn + daystotreatmentcat + neutropenia + socialhist___idu + preadmloccat, 
                             family = "binomial"))
summary(pool(fit_DTT))


#Computation
scope_DTT <- list(upper = ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc+
                    species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                    hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                    underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                    underlycond___malig + underlycond___neuro + underlycond___kidn + 
                    othsites___peritonealfl + addlorgs + sysantibact +
                    cvcremovecat + hosp_lengthprior + tpn + daystotreatmentcat + neutropenia + socialhist___idu + preadmloccat,
                  lower = ~1)
expr_DTT <- expression(f1 <- glm(mortality30d ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc+
                                   species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                                   hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                                   underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                                   underlycond___malig + underlycond___neuro + underlycond___kidn + 
                                   othsites___peritonealfl + addlorgs + sysantibact +
                                   cvcremovecat + hosp_lengthprior + tpn + daystotreatmentcat + neutropenia + socialhist___idu + preadmloccat
                                 , family="binomial"),
                       f2 <- step(f1, direction="backward"))
fit_DTT <- with(imput40_DTT, expr_DTT)

summary(imput40_DTT)

formulas_DTT <- lapply(fit_DTT$analyses, formula)
terms_DTT <- lapply(formulas_DTT, terms)
votes_DTT <- unlist(lapply(terms_DTT, labels))
table(votes_DTT)


#looking at first imputed dataset
imputeddataset <- complete(imput40_DTT)

#do significance test to decide whether to keep a specific variable
#tested variables that showed up in less than half of the models

#hosp90dbeforecx p=.240
#racecat p=.459
#res_echino p=.426

#using the WALD test to determine whether it should be in the final model

fit.without <- with(imput40_DTT, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + daystotreatmentcat + hosp_lengthprior +
                                       hosp90dbeforecx + icubeforecx+ neutropenia +  preadmloccat + 
                                       socialhist___idu + species_parac + surg___abdom + surg___nonabdom + 
                                       underlycond___liver + underlycond___malig + underlycond___metabol
                                     , family = "binomial"))
fit.with <- with(imput40_DTT, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + daystotreatmentcat + hosp_lengthprior +
                                    hosp90dbeforecx + icubeforecx+ neutropenia +  preadmloccat + 
                                    socialhist___idu + species_parac + surg___abdom + surg___nonabdom + 
                                    underlycond___liver + underlycond___malig + underlycond___metabol
                                  +underlycond___lung             
                                  , family = "binomial"))
D1(fit.with, fit.without)
warning()


#testing all variables with <5 votes;
#res_echino- .46796
#sysantibact- .2413
#underlycond___lung- .2100


#all wald test p values are >0.05 so does not impact predictability, will exclude from final model;



# FINAL MODEL, excluding hosp90dbeforecx, racecat, res_echino because it was in less than half the MI models AND did not significantly impact predictive power
fit_DTT <- with(imput40_DTT, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + daystotreatmentcat + hosp_lengthprior +
                                   hosp90dbeforecx + icubeforecx+ neutropenia +  preadmloccat + 
                                   socialhist___idu + species_parac + surg___abdom + surg___nonabdom + 
                                   underlycond___liver + underlycond___malig + underlycond___metabol
                                 ,family = "binomial"))
summary(pool(fit_DTT))
finalmodel_DTT <- summary(pool(fit_DTT),conf.int=TRUE,conf.level=0.95)
write_xlsx(finalmodel_DTT,"~/EIS/Candidemia/Candidemia mortality MDB/Data/finalmodel_DTT.xlsx")

#saving the environment
save.image(file='candidemiaimpute.RData')
dir()










####################################

# Models for combining AF and DTT - JM 

####################################

# var from AF  - initialaf   (should this be turned to a binary yes/no?)
# var from DTT - daystotreatmentcat

# Plan - just add variable "initialaf" to the DTT chunk of code 

# Why didn't imputation work with both AF and DTT in model?

# Just run complete case with both vars and keep the imputation portions separate 



##############################################

### JM: changed to mortality3 ###

mortality3 <- read_sas('mortality_trim.sas7bdat')


###Combining Days to Treatment 3 and 4 to just 3 and classified as:
### "Did Not Receive Treatment Within 6 Days" 
mortality3$daystotreatmentcat <- ifelse(mortality3$daystotreatmentcat == 4, 3,
                                        mortality3$daystotreatmentcat) 

recode(mortality3$daystotreatmentcat, 0, 1, 2, 3)

table(mortality3$daystotreatmentcat)



#classifying all numeric as factors so dummy variables will be factors
mortality3[sapply(mortality3, is.numeric)] <- lapply(mortality3[sapply(mortality3, is.numeric)],as.factor) 
names <- c('age_years' ,'hosp_lengthprior', 'year') 
mortality3[,names] <- lapply(mortality3[,names] , as.numeric) #changing actual numbers back to numeric

#changing all characters to factors
mortality3[sapply(mortality3, is.character)] <- lapply(mortality3[sapply(mortality3, is.character)],as.factor) 
#names2 <- c() 
#mortality[,names2] <- lapply(mortality[,names2] , as.character) 


#defining reference values
### JM: Added "initialaf" variable to reference values ###

mortality3$racecat <- relevel(mortality3$racecat, ref='1')
mortality3$ethnicity <- relevel(mortality3$ethnicity, ref='2')
mortality3$cvcremovecat <- relevel(mortality3$cvcremovecat, ref='1')
mortality3$initialaf <- relevel(mortality3$initialaf, ref='Echinocandins') 
mortality3$daystotreatmentcat <- relevel(mortality3$daystotreatmentcat, ref='1') 
#daystotreatmentcat- 0- no treatment, 1- tx before DISC, 2- 0-2 days after DISC, 3- 3-5d after DISC, 4- 6+ days after DISC
str(mortality3$mortality30d)
mortality3$mortality30d <- relevel (mortality3$mortality30d, ref="0")

# complete case model with days to treatment

### JM: Added "initialaf" to model ###
mortality_comp2 <- mortality3[complete.cases(mortality3), ]#deleting cases with missing
f1_cc2 <- glm(mortality30d ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc +
                species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                underlycond___malig + underlycond___neuro + underlycond___kidn + 
                othsites___peritonealfl  + addlorgs + sysantibact + 
                cvcremovecat + hosp_lengthprior + tpn + initialaf + daystotreatmentcat + neutropenia + socialhist___idu +preadmloccat
              , family="binomial", 
              data = mortality_comp2)
f2_cc2 <- step(f1_cc2, direction="backward")
completecasemodel_DTT<- summary(f2_cc2)
completecasemodel_DTT
f2_cc2OR <- exp(coefficients(f2_cc2))
f2_cc2OR
f2_cc2CI <- exp(confint(f2_cc2))
f2_cc2CI 

f2_pred <-cbind(f2_cc2OR, f2_cc2CI)

summary(mortality3)

table(mortality3$initialaf, mortality3$daystotreatmentcat)
table(mortality3$mortality30d, mortality3$daystotreatmentcat)

################

#imputation

################

# imputation from mortality3, 5 datasets
imp <- mice(mortality3, m = 5, print=F)

# prediction matrix
imp$predictorMatrix


imput40_DTT <- mice(data = mortality3,
                    seed = 12345, #allows it to have the same imputation every time
                    m = 10, #make 10 datasets
                    defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                    maxit = 40 #number of iterations, default is 5, depends on missingness)
)

#Try imputation without initialaf and DTT
#Try with logred first then maybe


imput40_DTT$loggedEvents #40 iterations, 10 datasets
warnings()

summary(imput40_DTT)

plot(imput40_DTT) 

save(imput40_DTT,file="impute40_DTT.Rdata")

#complete model with all vars
### JM: Added "initialaf" variable to complete model ###

fit_DTT <- with(imput40_DTT, glm(mortality30d ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc +
                                   species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                                   hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                                   underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                                   underlycond___malig + underlycond___neuro + underlycond___kidn + 
                                   othsites___peritonealfl + addlorgs + sysantibact +
                                   cvcremovecat + hosp_lengthprior + tpn + initialaf + daystotreatmentcat + neutropenia + socialhist___idu + preadmloccat, 
                                 family = "binomial"))
summary(pool(fit_DTT))


#Computation
### JM: Added "initialaf" variable to computation models ###

scope_DTT <- list(upper = ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc+
                    species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                    hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                    underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                    underlycond___malig + underlycond___neuro + underlycond___kidn + 
                    othsites___peritonealfl + addlorgs + sysantibact +
                    cvcremovecat + hosp_lengthprior + tpn + initialaf + daystotreatmentcat + neutropenia + socialhist___idu + preadmloccat,
                  lower = ~1)
expr_DTT <- expression(f1 <- glm(mortality30d ~ age_years + sex + racecat + ethnicity + state + year + species_albc + species_glabc+
                                   species_parac + species_tropc + species_dubc + species_otherc + species_multi + res_echino + res_azole +
                                   hosp90dbeforecx + surg___abdom + surg___nonabdom + icubeforecx + underlycond___lung + underlycond___metabol +
                                   underlycond___cardio_new + underlycond___immuno + underlycond___gastro + underlycond___liver +
                                   underlycond___malig + underlycond___neuro + underlycond___kidn + 
                                   othsites___peritonealfl + addlorgs + sysantibact +
                                   cvcremovecat + hosp_lengthprior + tpn + initialaf + daystotreatmentcat + neutropenia + socialhist___idu + preadmloccat
                                 , family="binomial"),
                       f2 <- step(f1, direction="backward"))

fit_DTT <- with(imput40_DTT, expr_DTT)

summary(imput40_DTT)

formulas_DTT <- lapply(fit_DTT$analyses, formula)
terms_DTT <- lapply(formulas_DTT, terms)
votes_DTT <- unlist(lapply(terms_DTT, labels))
table(votes_DTT)





#looking at first imputed dataset
imputeddataset <- complete(imput40_DTT)

#do significance test to decide whether to keep a specific variable
#tested variables that showed up in less than half of the models

#hosp90dbeforecx p=.240
#racecat p=.459
#res_echino p=.426

#using the WALD test to determine whether it should be in the final model

fit.without <- with(imput40_DTT, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + daystotreatmentcat + hosp_lengthprior +
                                       hosp90dbeforecx + icubeforecx+ neutropenia +  preadmloccat + 
                                       socialhist___idu + species_parac + surg___abdom + surg___nonabdom + 
                                       underlycond___liver + underlycond___malig + underlycond___metabol
                                     , family = "binomial"))
fit.with <- with(imput40_DTT, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + daystotreatmentcat + hosp_lengthprior +
                                    hosp90dbeforecx + icubeforecx+ neutropenia +  preadmloccat + 
                                    socialhist___idu + species_parac + surg___abdom + surg___nonabdom + 
                                    underlycond___liver + underlycond___malig + underlycond___metabol
                                  +underlycond___lung             
                                  , family = "binomial"))
D1(fit.with, fit.without)
warning()


#testing all variables with <5 votes;
#res_echino- .46796
#sysantibact- .2413
#underlycond___lung- .2100


#all wald test p values are >0.05 so does not impact predictability, will exclude from final model;



# FINAL MODEL, excluding hosp90dbeforecx, racecat, res_echino because it was in less than half the MI models AND did not significantly impact predictive power
fit_DTT <- with(imput40_DTT, glm(mortality30d ~ addlorgs + age_years + cvcremovecat + daystotreatmentcat + hosp_lengthprior +
                                   hosp90dbeforecx + icubeforecx+ neutropenia +  preadmloccat + 
                                   socialhist___idu + species_parac + surg___abdom + surg___nonabdom + 
                                   underlycond___liver + underlycond___malig + underlycond___metabol
                                 ,family = "binomial"))
summary(pool(fit_DTT))
finalmodel_DTT <- summary(pool(fit_DTT),conf.int=TRUE,conf.level=0.95)
write_xlsx(finalmodel_DTT,"~/EIS/Candidemia/Candidemia mortality MDB/Data/finalmodel_DTT.xlsx")

#saving the environment
save.image(file='candidemiaimpute.RData')
dir()




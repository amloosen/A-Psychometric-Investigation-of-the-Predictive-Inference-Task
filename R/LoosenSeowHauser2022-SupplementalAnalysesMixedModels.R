#######################################################
##Loosen, Seow & Hauser (submitted) -   Supplemental ##
##Supplemental Mixed Models                          ##
#######################################################

## CLEAR ALL
rm(list=ls())

##########
## SET DIRECTORY
setwd("~/Documents/GitHub/Probabilistic-Inference-Task-Analysis/R")

##########
## LOADING LIBRARIES
library(reshape2) 
library(data.table)
library(psych)
library(lme4)
options(scipen=999) 
options(max.print=25000) 

#######################
##Loading libraries####
#######################
#questionnaire data
quest_pt1 = data.frame(read.csv("Table_questPIT_pt1.csv",header = T))
quest_t2 = data.frame(read.csv("Table_questPIT_alltp.csv",header = T))
#game data
Game_pt1= data.frame(read.csv("Table_gamePIT_pt1.csv",header = T))#dataset t1 quest and predictive inference task
Game_t2= data.frame(read.csv("Table_gamePIT_t2.csv",header = T))#dataset t1 and t2 combined, quest and predictive inference task

################
##Functions ####
################
IntCons_r <- function(even_values,odd_values,complete_dataset){
  r_tmp <- cor(even_values,odd_values,method = "pearson")#correlate
  r_final <- 2*r_tmp/(1+r_tmp)#correct
  if(missing(complete_dataset)) {
    return(r_final)
  } else {
    r_ci<-r.con(r_final,n=dim(complete_dataset)[1],p=.95,twotailed=TRUE)#confidence interval
    r_complete <- data.frame(r_final,r_ci)
    return (r_complete)
  }
}

##########################
##Scaling questionnaires## 
##########################
quest_pt1$age.sc = scale(quest_pt1$age)
quest_pt1$total_iq.sc = scale(quest_pt1$total_iq)
quest_pt1$gender.sc = scale(quest_pt1$gender)
#t2
quest_t2$age.sc = scale(quest_t2$age)
quest_t2$total_iq.sc = scale(quest_t2$total_iq)
quest_t2$gender.sc = scale(quest_t2$gender)
#scaling the questionnaire scores
quest_t2$total_depr.sc = scale(quest_t2$total_depr) #depression
quest_t2$total_anx.sc = scale(quest_t2$total_anx) #anxiety
quest_t2$total_oc.sc = scale(quest_t2$total_oc) #obsessive compulsive
quest_t2$OCIR_nonCovid.sc = scale(quest_t2$OCIR_nonCovid) #obsessive compulsive without covid-relevant items

#########################################
##Prepare task data for the regressions## 
#########################################
#Remove the first trial of every participant because action updates conceptually only start at the second trial
finalGameData_pt1<- Game_pt1[Game_pt1$nTrial!=1, ]
#t2
finalGameData_t2<- Game_t2[Game_t2$nTrial!=1, ]
#z-score variables within participants and turn id into factor variable
finalGameData_pt1$conf.sc <-ave(finalGameData_pt1$conf, finalGameData_pt1$id, FUN=scale)
finalGameData_pt1$behav_dSD.sc <-ave(finalGameData_pt1$behav_dSD, finalGameData_pt1$id, FUN=scale)#PE but shortest distance, linear
finalGameData_pt1$id <- factor(finalGameData_pt1$id)
#t2
finalGameData_t2$conf.sc <-ave(finalGameData_t2$conf, finalGameData_t2$id, FUN=scale)
finalGameData_t2$behav_dSD.sc <-ave(finalGameData_t2$behav_dSD, finalGameData_t2$id, FUN=scale)
finalGameData_t2$id <- factor(finalGameData_t2$id)

########################################
##Merge datasets and create new ones####
########################################
finalGameQuestData_pt1 = merge(quest_pt1,finalGameData_pt1,by="id")
finalGameQuestData_pt1 <- finalGameQuestData_pt1[with(finalGameQuestData_pt1, order(id, nTrial)), ] #ensure order by trial number and id
#t2
finalGameQuestData_t2 = merge(quest_t2,finalGameData_t2,by="id")
finalGameQuestData_t2 <- finalGameQuestData_t2[with(finalGameQuestData_t2, order(id, nTrial)), ]

#Regression analyses with psychiatric variables and demographics####
actConfAnx<-lmer(actionUpdateSD ~ conf.sc *(total_anx.sc) + age.sc + total_iq.sc + gender + (1 + conf.sc |id), data=finalGameQuestData_t2)#not significant
actConfDepr<-lmer(actionUpdateSD ~ conf.sc *( total_depr.sc) + age.sc + total_iq.sc + gender + (1 + conf.sc |id),control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2)#not significant
actConfOCIR<-lmer(actionUpdateSD ~ conf.sc *(total_oc.sc)+ age.sc + total_iq.sc + gender + (1 + conf.sc |id), control= lmerControl(optimizer="bobyqa"), data=finalGameQuestData_t2)#not significant
actConfOCIRnonCovid<-lmer(actionUpdateSD ~ conf.sc *(OCIR_nonCovid.sc)+ age.sc + total_iq.sc + gender + (1 + conf.sc |id),control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2)#not significant

#Confidence: Regression analyses with psychiatric variables and demographics####
confAnx<-lmer(conf.sc ~ total_anx.sc + age.sc + total_iq.sc + gender + (1 + total_anx.sc |id), data=finalGameQuestData_t2)#not sign
confDepr<-lmer(conf.sc ~ total_depr.sc + age.sc + total_iq.sc + gender + (1 + total_depr.sc |id),  data=finalGameQuestData_t2)# not sign
confOCIR<-lmer(conf.sc ~ total_oc.sc+ age.sc + total_iq.sc + gender + (1 + total_oc.sc |id),  data=finalGameQuestData_t2)#not sign 
confOCIRnonCov<-lmer(conf.sc ~ OCIR_nonCovid.sc+ age.sc + total_iq.sc + gender + (1 + OCIR_nonCovid.sc |id),data=finalGameQuestData_t2)#non sign

#Action: Regression analyses with psychiatric variables and demographics####
actionUpdateAnx<-lmer(actionUpdateSD ~ total_anx.sc + age.sc + total_iq.sc + gender + (1 + total_anx.sc |id),  data=finalGameQuestData_t2)
actionUpdateDepr<-lmer(actionUpdateSD ~ total_depr.sc + age.sc + total_iq.sc + gender + (1 + total_depr.sc |id),  data=finalGameQuestData_t2)
actionUpdateOCIR<-lmer(actionUpdateSD ~ total_oc.sc + age.sc + total_iq.sc + gender + (1 + total_oc.sc |id),control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2)
actionUpdateOCIRnonCov<-lmer(actionUpdateSD ~ OCIR_nonCovid.sc + age.sc + total_iq.sc + gender + (1 + OCIR_nonCovid.sc |id),  data=finalGameQuestData_t2)

#Link to the Alternative Bayesian Learner####
##Excluding criteria (trial-wise)#### 
#1.Exclude trials where estimated learning rate is higher than 99% of all learning rates; 
#2.Exclude trials where PE is zero
#Note the odd and even dataset will be re-computed after this adaptation to ensure the shifting of variables is not introducing errors
finalGameQuestData_pt1Temp = finalGameQuestData_pt1[finalGameQuestData_pt1$behav_lrSD<=quantile(finalGameQuestData_pt1$behav_lrSD, 0.99) ,]
finalGameQuestData_pt1Mod = finalGameQuestData_pt1Temp[finalGameQuestData_pt1Temp$behav_d!=0,]#use the signed PE as the unsigned one has been changed to avoid infinite LRs
finalGameQuestData_pt1Mod <- finalGameQuestData_pt1Mod[with(finalGameQuestData_pt1Mod, order(id, nTrial)), ] 
#t2
finalGameQuestData_t2Temp = finalGameQuestData_t2[finalGameQuestData_t2$behav_lrSD<= quantile(finalGameQuestData_t2$behav_lrSD, 0.99) ,]
finalGameQuestData_t2Mod = finalGameQuestData_t2Temp[finalGameQuestData_t2Temp$behav_d!=0,]
finalGameQuestData_t2Mod <- finalGameQuestData_t2Mod [with(finalGameQuestData_t2Mod, order(id, nTrial)), ] 

##Preparing normative factor regressors#### 
finalGameQuestData_pt1Mod$X1 <- abs(finalGameQuestData_pt1Mod$bayes_dSD)#absolute model PE instead of behavioural PE
finalGameQuestData_pt1Mod$X2 <- finalGameQuestData_pt1Mod$bayes_CPPSD#CPP - computed in a linear manner and based on the bayesian instead of the behavioural PE
finalGameQuestData_pt1Mod$X3 <- finalGameQuestData_pt1Mod$bayes_RUSD
finalGameQuestData_pt1Mod$X4 <- finalGameQuestData_pt1Mod$hitMiss#caught the particle (hit) or not (miss)
#t2
finalGameQuestData_t2Mod$X1 <- abs(finalGameQuestData_t2Mod$bayes_dSD) 
finalGameQuestData_t2Mod$X2 <- finalGameQuestData_t2Mod$bayes_CPPSD
finalGameQuestData_t2Mod$X3 <- (1-finalGameQuestData_t2Mod$bayes_MCSD)
finalGameQuestData_t2Mod$X4 <- finalGameQuestData_t2Mod$hitMiss 

###Preparing IV of the alternative action-update model (normative regressors)####
finalGameQuestData_pt1Mod$X1X2<-finalGameQuestData_pt1Mod$X1*finalGameQuestData_pt1Mod$X2 #model PE*CPP
finalGameQuestData_pt1Mod$X1X2X3<-finalGameQuestData_pt1Mod$X1*finalGameQuestData_pt1Mod$X3*(1-finalGameQuestData_pt1Mod$X2)#model PE*RU
finalGameQuestData_pt1Mod$X1X4<-finalGameQuestData_pt1Mod$X1*finalGameQuestData_pt1Mod$X4#model PE*hitMiss
#t2
finalGameQuestData_t2Mod$X1X2<-finalGameQuestData_t2Mod$X1*finalGameQuestData_t2Mod$X2 
finalGameQuestData_t2Mod$X1X2X3<-finalGameQuestData_t2Mod$X1*finalGameQuestData_t2Mod$X3*(1-finalGameQuestData_t2Mod$X2)
finalGameQuestData_t2Mod$X1X4<-finalGameQuestData_t2Mod$X1*finalGameQuestData_t2Mod$X4

####shift normative predictors for the alternative confidence regression models####
#PE
finalGameQuestData_pt1Mod$shiftX1 <- c(0, finalGameQuestData_pt1Mod$X1[-nrow(finalGameQuestData_pt1Mod)]) #The first line adds a string of lagged (+1) observations #basically lagging it by one trial because its not an "update"
finalGameQuestData_pt1Mod$shiftX1[which(!duplicated(finalGameQuestData_pt1Mod$id))] <- 0 #second string corrects the first entry of each group, as the lagged observation is from previous group.
#t2
finalGameQuestData_t2Mod$shiftX1 <- c(0, finalGameQuestData_t2Mod$X1[-nrow(finalGameQuestData_t2Mod)]) 
finalGameQuestData_t2Mod$shiftX1[which(!duplicated(finalGameQuestData_t2Mod$id))] <- 0 
#CPP
finalGameQuestData_pt1Mod$shiftX2 <- c(0, finalGameQuestData_pt1Mod$X2[-nrow(finalGameQuestData_pt1Mod)]) #The first line adds a string of lagged (+1) observations #basically lagging it by one trial
finalGameQuestData_pt1Mod$shiftX2[which(!duplicated(finalGameQuestData_pt1Mod$id))] <- 0 #second string corrects the first entry of each group, as the lagged observation is from previous group.
#t2
finalGameQuestData_t2Mod$shiftX2 <- c(0, finalGameQuestData_t2Mod$X2[-nrow(finalGameQuestData_t2Mod)]) 
finalGameQuestData_t2Mod$shiftX2[which(!duplicated(finalGameQuestData_t2Mod$id))] <- 0 
#alterenative RU/ X3
finalGameQuestData_pt1Mod$shiftX2X3 <- (1-finalGameQuestData_pt1Mod$shiftX2)*finalGameQuestData_pt1Mod$X3
#t2
finalGameQuestData_t2Mod$shiftX2X3 <- (1-finalGameQuestData_t2Mod$shiftX2)*finalGameQuestData_t2Mod$X3
#hitMiss
finalGameQuestData_pt1Mod$shiftX4 <- c(0, finalGameQuestData_pt1Mod$X4[-nrow(finalGameQuestData_pt1Mod)]) #The first line adds a string of lagged (+1) observations #basically lagging it by one trial
finalGameQuestData_pt1Mod$shiftX4[which(!duplicated(finalGameQuestData_pt1Mod$id))] <- 0 #second string corrects the first entry of each group, as the lagged observation is from previous group.
#t2
finalGameQuestData_t2Mod$shiftX4 <- c(0, finalGameQuestData_t2Mod$X4[-nrow(finalGameQuestData_t2Mod)]) 
finalGameQuestData_t2Mod$shiftX4[which(!duplicated(finalGameQuestData_t2Mod$id))] <- 0 

#Note: we have to create new odd and even datasets after the shifting of the variables
odd_indexes1 <- which(finalGameQuestData_pt1Mod$nTrial %% 2 == 1)            
even_indexes1 <- which(finalGameQuestData_pt1Mod$nTrial %% 2 == 0)           

finalGameQuest_pt1_oddMod = finalGameQuestData_pt1Mod[odd_indexes1,]
finalGameQuest_pt1_evenMod = finalGameQuestData_pt1Mod[even_indexes1,]

odd_indexes2 <- which(finalGameQuestData_t2Mod$nTrial %% 2 == 1)            
even_indexes2 <- which(finalGameQuestData_t2Mod$nTrial %% 2 == 0)          
finalGameQuest_t2_oddMod = finalGameQuestData_t2Mod[odd_indexes2,]
finalGameQuest_t2_evenMod = finalGameQuestData_t2Mod[even_indexes2,]

##scale####
finalGameQuestData_pt1Mod$X1.sc<-ave(finalGameQuestData_pt1Mod$X1,finalGameQuestData_pt1Mod$id, FUN=scale)
finalGameQuestData_pt1Mod$X1X2.sc<-ave(finalGameQuestData_pt1Mod$X1X2,finalGameQuestData_pt1Mod$id, FUN=scale)
finalGameQuestData_pt1Mod$X1X2X3.sc<-ave(finalGameQuestData_pt1Mod$X1X2X3, finalGameQuestData_pt1Mod$id, FUN=scale)
finalGameQuestData_pt1Mod$X1X4.sc<-ave(finalGameQuestData_pt1Mod$X1X4,finalGameQuestData_pt1Mod$id, FUN=scale)
#odd vs. even CPs
finalGameQuest_pt1_oddMod$X1.sc<-ave(finalGameQuest_pt1_oddMod$X1, finalGameQuest_pt1_oddMod$id, FUN=scale)
finalGameQuest_pt1_oddMod$X1X2.sc<-ave(finalGameQuest_pt1_oddMod$X1X2, finalGameQuest_pt1_oddMod$id, FUN=scale)
finalGameQuest_pt1_oddMod$X1X2X3.sc<-ave(finalGameQuest_pt1_oddMod$X1X2X3, finalGameQuest_pt1_oddMod$id, FUN=scale)
finalGameQuest_pt1_oddMod$X1X4.sc<-ave(finalGameQuest_pt1_oddMod$X1X4, finalGameQuest_pt1_oddMod$id, FUN=scale)
finalGameQuest_pt1_evenMod$X1.sc<-ave(finalGameQuest_pt1_evenMod$X1, finalGameQuest_pt1_evenMod$id, FUN=scale)
finalGameQuest_pt1_evenMod$X1X2.sc<-ave(finalGameQuest_pt1_evenMod$X1X2, finalGameQuest_pt1_evenMod$id, FUN=scale)
finalGameQuest_pt1_evenMod$X1X2X3.sc<-ave(finalGameQuest_pt1_evenMod$X1X2X3, finalGameQuest_pt1_evenMod$id, FUN=scale)
finalGameQuest_pt1_evenMod$X1X4.sc<-ave(finalGameQuest_pt1_evenMod$X1X4, finalGameQuest_pt1_evenMod$id, FUN=scale)
#t2
finalGameQuestData_t2Mod$X1.sc<-ave(finalGameQuestData_t2Mod$X1,finalGameQuestData_t2Mod$id, FUN=scale)
finalGameQuestData_t2Mod$X1X2.sc<-ave(finalGameQuestData_t2Mod$X1X2,finalGameQuestData_t2Mod$id, FUN=scale)
finalGameQuestData_t2Mod$X1X2X3.sc<-ave(finalGameQuestData_t2Mod$X1X2X3,finalGameQuestData_t2Mod$id, FUN=scale)
finalGameQuestData_t2Mod$X1X4.sc<-ave(finalGameQuestData_t2Mod$X1X4,finalGameQuestData_t2Mod$id, FUN=scale)
#odd vs. even CPs
finalGameQuest_t2_oddMod$X1.sc<-ave(finalGameQuest_t2_oddMod$X1,finalGameQuest_t2_oddMod$id, FUN=scale)
finalGameQuest_t2_oddMod$X1X2.sc<-ave(finalGameQuest_t2_oddMod$X1X2,finalGameQuest_t2_oddMod$id, FUN=scale)
finalGameQuest_t2_oddMod$X1X2X3.sc<-ave(finalGameQuest_t2_oddMod$X1X2X3,finalGameQuest_t2_oddMod$id, FUN=scale)
finalGameQuest_t2_oddMod$X1X4.sc<-ave(finalGameQuest_t2_oddMod$X1X4,finalGameQuest_t2_oddMod$id, FUN=scale)
finalGameQuest_t2_evenMod$X1.sc<-ave(finalGameQuest_t2_evenMod$X1,finalGameQuest_t2_evenMod$id, FUN=scale)
finalGameQuest_t2_evenMod$X1X2.sc<-ave(finalGameQuest_t2_evenMod$X1X2,finalGameQuest_t2_evenMod$id, FUN=scale)
finalGameQuest_t2_evenMod$X1X2X3.sc<-ave(finalGameQuest_t2_evenMod$X1X2X3,finalGameQuest_t2_evenMod$id, FUN=scale)
finalGameQuest_t2_evenMod$X1X4.sc<-ave(finalGameQuest_t2_evenMod$X1X4,finalGameQuest_t2_evenMod$id, FUN=scale)

# Scaling the regressors
finalGameQuestData_pt1Mod$shiftX1.sc <- ave(finalGameQuestData_pt1Mod$shiftX1, finalGameQuestData_pt1Mod$id, FUN=scale)  
finalGameQuestData_pt1Mod$shiftX2.sc <- ave(finalGameQuestData_pt1Mod$shiftX2, finalGameQuestData_pt1Mod$id, FUN=scale)  
finalGameQuestData_pt1Mod$shiftX2X3.sc <- ave(finalGameQuestData_pt1Mod$shiftX2X3, finalGameQuestData_pt1Mod$id, FUN=scale)  
finalGameQuestData_pt1Mod$shiftX4.sc <- ave(finalGameQuestData_pt1Mod$shiftX4, finalGameQuestData_pt1Mod$id, FUN=scale) 
##odd vs. even CPs
finalGameQuest_pt1_oddMod$shiftX1.sc <- ave(finalGameQuest_pt1_oddMod$shiftX1, finalGameQuest_pt1_oddMod$id, FUN=scale)  
finalGameQuest_pt1_oddMod$shiftX2.sc <- ave(finalGameQuest_pt1_oddMod$shiftX2, finalGameQuest_pt1_oddMod$id, FUN=scale)  
finalGameQuest_pt1_oddMod$shiftX2X3.sc <- ave(finalGameQuest_pt1_oddMod$shiftX2X3, finalGameQuest_pt1_oddMod$id, FUN=scale)  
finalGameQuest_pt1_oddMod$shiftX4.sc <- ave(finalGameQuest_pt1_oddMod$shiftX4, finalGameQuest_pt1_oddMod$id, FUN=scale) 
finalGameQuest_pt1_evenMod$shiftX1.sc <- ave(finalGameQuest_pt1_evenMod$shiftX1, finalGameQuest_pt1_evenMod$id, FUN=scale)  
finalGameQuest_pt1_evenMod$shiftX2.sc <- ave(finalGameQuest_pt1_evenMod$shiftX2, finalGameQuest_pt1_evenMod$id, FUN=scale)  
finalGameQuest_pt1_evenMod$shiftX2X3.sc <- ave(finalGameQuest_pt1_evenMod$shiftX2X3, finalGameQuest_pt1_evenMod$id, FUN=scale)  
finalGameQuest_pt1_evenMod$shiftX4.sc <- ave(finalGameQuest_pt1_evenMod$shiftX4, finalGameQuest_pt1_evenMod$id, FUN=scale) 
#t2
finalGameQuestData_t2Mod$shiftX1.sc <- ave(finalGameQuestData_t2Mod$shiftX1, finalGameQuestData_t2Mod$id, FUN=scale)  
finalGameQuestData_t2Mod$shiftX2.sc <- ave(finalGameQuestData_t2Mod$shiftX2, finalGameQuestData_t2Mod$id, FUN=scale)  
finalGameQuestData_t2Mod$shiftX2X3.sc <- ave(finalGameQuestData_t2Mod$shiftX2X3, finalGameQuestData_t2Mod$id, FUN=scale)  
finalGameQuestData_t2Mod$shiftX4.sc <- ave(finalGameQuestData_t2Mod$shiftX4, finalGameQuestData_t2Mod$id, FUN=scale) 
##odd vs. even CPs
finalGameQuest_t2_oddMod$shiftX1.sc <- ave(finalGameQuest_t2_oddMod$shiftX1, finalGameQuest_t2_oddMod$id, FUN=scale)  
finalGameQuest_t2_oddMod$shiftX2.sc <- ave(finalGameQuest_t2_oddMod$shiftX2, finalGameQuest_t2_oddMod$id, FUN=scale)  
finalGameQuest_t2_oddMod$shiftX2X3.sc <- ave(finalGameQuest_t2_oddMod$shiftX2X3, finalGameQuest_t2_oddMod$id, FUN=scale)  
finalGameQuest_t2_oddMod$shiftX4.sc <- ave(finalGameQuest_t2_oddMod$shiftX4, finalGameQuest_t2_oddMod$id, FUN=scale) 
finalGameQuest_t2_evenMod$shiftX1.sc <- ave(finalGameQuest_t2_evenMod$shiftX1, finalGameQuest_t2_evenMod$id, FUN=scale)  
finalGameQuest_t2_evenMod$shiftX2.sc <- ave(finalGameQuest_t2_evenMod$shiftX2, finalGameQuest_t2_evenMod$id, FUN=scale)  
finalGameQuest_t2_evenMod$shiftX2X3.sc <- ave(finalGameQuest_t2_evenMod$shiftX2X3, finalGameQuest_t2_evenMod$id, FUN=scale)  
finalGameQuest_t2_evenMod$shiftX4.sc <- ave(finalGameQuest_t2_evenMod$shiftX4, finalGameQuest_t2_evenMod$id, FUN=scale) 

##Preparing DV of the alternative action-update model####
#action update= how much did the participant move the bucket, as told by their learning rate * abs (PE) (behavioural variables)
finalGameQuestData_pt1Mod$LrY.sc <- ave(finalGameQuestData_pt1Mod$actionUpdateSDLR, finalGameQuestData_pt1Mod$id, FUN=scale)#using the action update from t to t+1
#odd vs. even CPs
finalGameQuest_pt1_oddMod$LrY.sc <- ave(finalGameQuest_pt1_oddMod$actionUpdateSDLR,finalGameQuest_pt1_oddMod$id, FUN=scale)
finalGameQuest_pt1_evenMod$LrY.sc <- ave(finalGameQuest_pt1_evenMod$actionUpdateSDLR,finalGameQuest_pt1_evenMod$id, FUN=scale)
#t2
finalGameQuestData_t2Mod$LrY.sc <- ave(finalGameQuestData_t2Mod$actionUpdateSDLR, finalGameQuestData_t2Mod$id, FUN=scale)
#odd vs. even CPs
finalGameQuest_t2_oddMod$LrY.sc <- ave(finalGameQuest_t2_oddMod$actionUpdateSDLR,finalGameQuest_t2_oddMod$id, FUN=scale)
finalGameQuest_t2_evenMod$LrY.sc <- ave(finalGameQuest_t2_evenMod$actionUpdateSDLR,finalGameQuest_t2_evenMod$id, FUN=scale)

##Implement Mixed Models
#Confidence: Regression analyses with demographics and normative regressors####
lmConf1 <-lmer(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc + age.sc+gender+total_iq.sc +(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_pt1Mod)

lmConf1CPP <-lmer(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc + age.sc+gender+total_iq.sc +(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_pt1Mod)
lmConf1PE <-lmer(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc + age.sc+gender+total_iq.sc +(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_pt1Mod)

#odd vs. even CPs
lmConf1odd <-lmer(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_oddMod)
lmConf1even <-lmer(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_evenMod)

lmConf1oddPE <-lmer(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_oddMod)
lmConf1evenPE <-lmer(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_evenMod)

lmConf1oddCPP <-lmer(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_oddMod)
lmConf1evenCPP <-lmer(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_evenMod)

#t2
lmConf2 <-lmer(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

lmConf2CPP <-lmer(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConf2PE <-lmer(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

#odd vs. even CPs
lmConf2odd <-lmer(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_oddMod)
lmConf2even <-lmer(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_evenMod)

lmConf2oddPE <-lmer(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_oddMod)
lmConf2evenPE <-lmer(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_evenMod)#don't need to remove corr 

lmConf2oddCPP <-lmer(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_oddMod)
lmConf2evenCPP <-lmer(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_evenMod)

#Confidence: Regression analyses with demographics, psychiatric variables and normative regressors####
lmConfAnxPE<-lmer(conf.sc ~ (shiftX1.sc+shiftX2X3.sc+shiftX4.sc)*(total_anx.sc)+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfDeprPE<-lmer(conf.sc ~ (shiftX1.sc+shiftX2X3.sc+shiftX4.sc)*(total_depr.sc)+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfOCIRPE<-lmer(conf.sc ~ (shiftX1.sc+shiftX2X3.sc+shiftX4.sc)*(total_oc.sc)+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfOCIRnonCovidPE<-lmer(conf.sc ~ (shiftX1.sc+shiftX2X3.sc+shiftX4.sc)*(OCIR_nonCovid.sc)+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

lmConfAnxCPP<-lmer(conf.sc ~ (shiftX2.sc+shiftX2X3.sc+shiftX4.sc)*(total_anx.sc)+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfDeprCPP<-lmer(conf.sc ~ (shiftX2.sc+shiftX2X3.sc+shiftX4.sc)*(total_depr.sc)+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfOCIRCPP<-lmer(conf.sc ~ (shiftX2.sc+shiftX2X3.sc+shiftX4.sc)*(total_oc.sc)+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfOCIRnonCovidCPP<-lmer(conf.sc ~ (shiftX2.sc+shiftX2X3.sc+shiftX4.sc)*(OCIR_nonCovid.sc)+age.sc +gender+total_iq.sc+(1+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

lmConfAnx<-lmer(conf.sc ~ (shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc)*(total_anx.sc)+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc||id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfDepr<-lmer(conf.sc ~ (shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc)*(total_depr.sc)+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc||id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfOCIR<-lmer(conf.sc ~ (shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc)*(total_oc.sc)+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc||id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmConfOCIRnonCovid<-lmer(conf.sc ~ (shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc)*(OCIR_nonCovid.sc)+age.sc +gender+total_iq.sc+(1+shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc||id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

#Action: Regression analyses with demographics and normative regressors####
lmLrY1 <-lmer(LrY.sc ~ X1.sc+X1X2.sc+ X1X2X3.sc + X1X4.sc +age.sc +gender+total_iq.sc+(1+X1.sc+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_pt1Mod)

lmLrY1CPP <-lmer(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc +age.sc +gender+total_iq.sc+(1+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_pt1Mod)
#odd vs. even CPs
lmLrY1odd <-lmer(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc +age.sc +gender+total_iq.sc+(1+X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_oddMod)
lmLrY1even <-lmer(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc +age.sc +gender+total_iq.sc+(1+X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_evenMod)

lmLrY1evenCPP <-lmer(LrY.sc ~ X1.sc+X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1.sc+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_evenMod)

lmLrY1oddCPP <-lmer(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc +age.sc +gender+total_iq.sc+(1+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_oddMod)
lmLrY1evenCPP <-lmer(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_evenMod)
#t2
lmLrY2 <-lmer(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1.sc+X1X2.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

lmLrY2CPP <-lmer(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
#odd vs. even CPs
lmLrY2oddCPP <-lmer(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_oddMod)
lmLrY2evenCPP <-lmer(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_evenMod)

lmLrY2odd <-lmer(LrY.sc ~  X1.sc+X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+ X1.sc+X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_oddMod)
lmLrY2even <-lmer(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_evenMod)

lmLrY1PE <-lmer(LrY.sc ~ X1.sc+  X1X2X3.sc + X1X4.sc +age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_pt1Mod)
#odd vs. even CPs
lmLrY1oddPE <-lmer(LrY.sc ~ X1.sc+  X1X2X3.sc + X1X4.sc +age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_oddMod)
lmLrY1evenPE <-lmer(LrY.sc ~ X1.sc+  X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_pt1_evenMod)
#t2
lmLrY2PE <-lmer(LrY.sc ~ X1.sc+  X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
#odd vs. even CPs
lmLrY2oddPE <-lmer(LrY.sc ~ X1.sc+  X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_oddMod)
lmLrY2evenPE <-lmer(LrY.sc ~ X1.sc+  X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuest_t2_evenMod)


#Action: Regression analyses with psychiatric variables and demographics and normative regressors####
lmLrYAnxPE<-lmer(LrY.sc ~ (X1.sc+  X1X2X3.sc + X1X4.sc)*(total_anx.sc)+age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYDeprPE<-lmer(LrY.sc ~ (X1.sc+  X1X2X3.sc + X1X4.sc)*(total_depr.sc)+age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYOCIRPE<-lmer(LrY.sc ~ (X1.sc+  X1X2X3.sc + X1X4.sc)*(total_oc.sc)+age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYOCIRnonCovidPE<-lmer(LrY.sc ~ (X1.sc+  X1X2X3.sc + X1X4.sc)*(OCIR_nonCovid.sc)+age.sc +gender+total_iq.sc+(1+X1.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

lmLrYAnxCPP<-lmer(LrY.sc ~ ( X1X2.sc+ X1X2X3.sc + X1X4.sc)*(total_anx.sc)+age.sc +gender+total_iq.sc+(1+ X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYDeprCPP<-lmer(LrY.sc ~ ( X1X2.sc+ X1X2X3.sc + X1X4.sc)*(total_depr.sc)+age.sc +gender+total_iq.sc+(1+ X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYOCIRCPP<-lmer(LrY.sc ~ ( X1X2.sc+ X1X2X3.sc + X1X4.sc)*(total_oc.sc)+age.sc +gender+total_iq.sc+(1+ X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYOCIRnonCovidCPP<-lmer(LrY.sc ~ ( X1X2.sc+ X1X2X3.sc + X1X4.sc)*(OCIR_nonCovid.sc)+age.sc +gender+total_iq.sc+(1+ X1X2.sc+ X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

lmLrYAnx<-lmer(LrY.sc ~ (X1.sc+ X1X2.sc+  X1X2X3.sc + X1X4.sc)*(total_anx.sc)+age.sc +gender+total_iq.sc+(1+X1.sc+ X1X2.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYDepr<-lmer(LrY.sc ~ (X1.sc+ X1X2.sc+  X1X2X3.sc + X1X4.sc)*(total_depr.sc)+age.sc +gender+total_iq.sc+(1+X1.sc+ X1X2.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYOCIR<-lmer(LrY.sc ~ (X1.sc+ X1X2.sc+  X1X2X3.sc + X1X4.sc)*(total_oc.sc)+age.sc +gender+total_iq.sc+(1+X1.sc+ X1X2.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)
lmLrYOCIRnonCovid<-lmer(LrY.sc ~ (X1.sc+ X1X2.sc+  X1X2X3.sc + X1X4.sc)*(OCIR_nonCovid.sc)+age.sc +gender+total_iq.sc+(1+X1.sc+ X1X2.sc+  X1X2X3.sc + X1X4.sc|id), control= lmerControl(optimizer="bobyqa"),  data=finalGameQuestData_t2Mod)

##########################################################################
##Investigate the psychometric properties of the regression coefficients##
##########################################################################
# For Confidence adjustments regression with normative predictors####
#get the coefficients into one dataset with the corresponding subject id
lmConfCoefsIntCons1_odd<-data.frame(coef(lmConf1odd)$id)[2:5]
lmConfCoefsIntCons1_odd<- setDT(lmConfCoefsIntCons1_odd, keep.rownames = TRUE)[]
setnames(lmConfCoefsIntCons1_odd, old = c('rn'), new = c('id'))
lmConfCoefsIntCons1_even<-data.frame(coef(lmConf1even)$id)[2:5]
lmConfCoefsIntCons1_even<- setDT(lmConfCoefsIntCons1_even, keep.rownames = TRUE)[]
setnames(lmConfCoefsIntCons1_even, old = c('rn'), new = c('id'))
#merge
lmConfModAllCoefs1_oddeven = merge(lmConfCoefsIntCons1_odd,lmConfCoefsIntCons1_even,by=c('id'))
lmConfModAllCoefs1_oddeven <- lmConfModAllCoefs1_oddeven[with(lmConfModAllCoefs1_oddeven, order(id)), ]
#get internal consistency
var_indicesConf <- c("shiftX1.sc","shiftX2.sc","shiftX2X3.sc", "shiftX4.sc") # Modify with your desired indices
r_linConf <- matrix(NA, nrow = length(var_indicesConf), ncol = 2)
ci.linConf <- matrix(NA, nrow = length(var_indicesConf), ncol = 3)

for (i in seq_along(var_indicesConf)) {
  var_index <- var_indicesConf[i]
  xConf <- lmConfModAllCoefs1_oddeven[[paste0(var_index,".x")]]
  yConf <- lmConfModAllCoefs1_oddeven[[paste0(var_index,".y")]]
  intConsConf <- IntCons_r(xConf, yConf,lmConfModAllCoefs1_oddeven)
  r_linConf[i,1] <- intConsConf[1, 1]
  ci.linConf[i,1:2 ] <- intConsConf[, 2]
  ci.linConf[i,3 ] <- var_indicesConf[i]
  r_linConf[i,2] <- var_indicesConf[i]
}

#t2####
#get the coefficients into one dataset with the corresponding subject id
lmConfCoefsIntCons2_odd<-data.frame(coef(lmConf2odd)$id)[2:5]
lmConfCoefsIntCons2_odd<- setDT(lmConfCoefsIntCons2_odd, keep.rownames = TRUE)[]
setnames(lmConfCoefsIntCons2_odd, old = c('rn'), new = c('id'))
lmConfCoefsIntCons2_even<-data.frame(coef(lmConf2even)$id)[2:5]
lmConfCoefsIntCons2_even<- setDT(lmConfCoefsIntCons2_even, keep.rownames = TRUE)[]
setnames(lmConfCoefsIntCons2_even, old = c('rn'), new = c('id'))
#merge
lmConfModAllCoefs2_oddeven = merge(lmConfCoefsIntCons2_odd,lmConfCoefsIntCons2_even,by=c('id'))
lmConfModAllCoefs2_oddeven <- lmConfModAllCoefs2_oddeven[with(lmConfModAllCoefs2_oddeven, order(id)), ]
#get internal consistency
var_indicesConf <- c("shiftX1.sc","shiftX2.sc","shiftX2X3.sc", "shiftX4.sc") # Modify with your desired indices
r_linConf2 <- matrix(NA, nrow = length(var_indicesConf), ncol = 2)
ci.linConf2 <- matrix(NA, nrow = length(var_indicesConf), ncol = 3)

for (i in seq_along(var_indicesConf)) {
  var_index <- var_indicesConf[i]
  xConf2 <- lmConfModAllCoefs2_oddeven[[paste0(var_index,".x")]]
  yConf2 <- lmConfModAllCoefs2_oddeven[[paste0(var_index,".y")]]
  intConsConf2 <- IntCons_r(xConf2, yConf2,lmConfModAllCoefs2_oddeven)
  r_linConf2[i,1] <- intConsConf2[1, 1]
  ci.linConf2[i,1:2 ] <- intConsConf2[, 2]
  ci.linConf2[i,3 ] <- var_indicesConf[i]
  r_linConf2[i,2] <- var_indicesConf[i]
}

#For Action adjustments regression with normative predictors####
#get the coefficients into one dataset with the corresponding subject id
lmLrYCoefsIntCons1_odd<-data.frame(coef(lmLrY1odd)$id)[1:5]
lmLrYCoefsIntCons1_odd<- setDT(lmLrYCoefsIntCons1_odd, keep.rownames = TRUE)[]
setnames(lmLrYCoefsIntCons1_odd, old = c('rn'), new = c('id'))
lmLrYCoefsIntCons1_even<-data.frame(coef(lmLrY1even)$id)[1:5]
lmLrYCoefsIntCons1_even<- setDT(lmLrYCoefsIntCons1_even, keep.rownames = TRUE)[]
setnames(lmLrYCoefsIntCons1_even, old = c('rn'), new = c('id'))
#merge
lmLrYModAllCoefs1_oddeven = merge(lmLrYCoefsIntCons1_odd,lmLrYCoefsIntCons1_even,by=c('id'))
lmLrYModAllCoefs1_oddeven <- lmLrYModAllCoefs1_oddeven[with(lmLrYModAllCoefs1_oddeven, order(id)), ]
#get internal consistency
var_indicesAct <- c("X1.sc","X1X2.sc","X1X2X3.sc", "X1X4.sc") # Modify with your desired indices
r_linAct <- matrix(NA, nrow = length(var_indicesAct), ncol = 2)
ci.linAct <- matrix(NA, nrow = length(var_indicesAct), ncol = 3)

for (i in seq_along(var_indicesAct)) {
  var_index <- var_indicesAct[i]
  xAct <- lmLrYModAllCoefs1_oddeven[[paste0(var_index,".x")]]
  yAct <- lmLrYModAllCoefs1_oddeven[[paste0(var_index,".y")]]
  intConsAct <- IntCons_r(xAct, yAct,lmLrYModAllCoefs1_oddeven)
  r_linAct[i,1] <- intConsAct[1, 1]
  ci.linAct[i,1:2 ] <- intConsAct[, 2]
  ci.linAct[i,3 ] <- var_indicesAct[i]
  r_linAct[i,2] <- var_indicesAct[i]
}

#t2####
#get the coefficients into one dataset with the corresponding subject id
lmLrYCoefsIntCons2_odd<-data.frame(coef(lmLrY2odd)$id)[1:5]
lmLrYCoefsIntCons2_odd<- setDT(lmLrYCoefsIntCons2_odd, keep.rownames = TRUE)[]
setnames(lmLrYCoefsIntCons2_odd, old = c('rn'), new = c('id'))
lmLrYCoefsIntCons2_even<-data.frame(coef(lmLrY2even)$id)[1:5]
lmLrYCoefsIntCons2_even<- setDT(lmLrYCoefsIntCons2_even, keep.rownames = TRUE)[]
setnames(lmLrYCoefsIntCons2_even, old = c('rn'), new = c('id'))
#merge
lmLrYModAllCoefs2_oddeven = merge(lmLrYCoefsIntCons2_odd,lmLrYCoefsIntCons2_even,by=c('id'))
lmLrYModAllCoefs2_oddeven <- lmLrYModAllCoefs2_oddeven[with(lmLrYModAllCoefs2_oddeven, order(id)), ]
#get internal consistency
var_indicesAct <- c("X1.sc","X1X2.sc","X1X2X3.sc", "X1X4.sc") # Modify with your desired indices
r_linAct2 <- matrix(NA, nrow = length(var_indicesAct), ncol = 2)
ci.linAct2 <- matrix(NA, nrow = length(var_indicesAct), ncol = 3)

for (i in seq_along(var_indicesAct)) {
  var_index2 <- var_indicesAct[i]
  xAct2 <- lmLrYModAllCoefs2_oddeven[[paste0(var_index2,".x")]]
  yAct2 <- lmLrYModAllCoefs2_oddeven[[paste0(var_index2,".y")]]
  intConsAct2 <- IntCons_r(xAct2, yAct2,lmLrYModAllCoefs2_oddeven)
  r_linAct2[i,1] <- intConsAct2[1, 1]
  ci.linAct2[i,1:2 ] <- intConsAct2[, 2]
  ci.linAct2[i,3 ] <- var_indicesAct[i]
  r_linAct2[i,2] <- var_indicesAct[i]
}

##Test-retest reliability####
#For Confidence adjustments regression with normative predictors####
#fit the t1 model again only with ids that also completed t2
finalGameQuestData_pt1Mod_tmp <- finalGameQuestData_pt1Mod[finalGameQuestData_pt1Mod$id %in% unique(unlist(check_idConf)),]
lmConf1_tmp <-lmer(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc+age.sc+gender+total_iq.sc+(1+shiftX2.sc+shiftX2.sc+shiftX2X3.sc+shiftX4.sc|id),control= lmerControl(optimizer="bobyqa"), verbose=0, data=finalGameQuestData_pt1Mod_tmp)
#get the coefficients into one dataset with the corresponding subject id
ConfMod1Coefs<-data.frame(coef(lmConf1_tmp)$id)[1:5]
ConfMod1Coefs$id <-finalGameQuestData_pt1Mod_tmp[!duplicated(finalGameQuestData_pt1Mod_tmp$id), ]$id
#same for t2
ConfMod2Coefs<-data.frame(coef(lmConf2)$id)[1:5]
ConfMod2Coefs$id<-finalGameQuestData_t2Mod[!duplicated(finalGameQuestData_t2Mod$id), ]$id
#merge the timepoints into one dataset
ConfModAllCoefs = merge(ConfMod1Coefs,ConfMod2Coefs,by=c('id'))
ConfModAllCoefs <- ConfModAllCoefs[with(ConfModAllCoefs, order(id)), ]
#get test-retest reliability
predConfRelReg <- data.frame(matrix(NA, 3, 4))
var_indicesConf <- c("shiftX1.sc", "shiftX2.sc", "shiftX2X3.sc", "shiftX4.sc")
# Loop over the time points
for (i in seq_along(var_indicesConf)) {
  var_namex <-ConfModAllCoefs[[paste0(var_indicesConf[i],".x")]]
  var_namey <-ConfModAllCoefs[[paste0(var_indicesConf[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predConfRelReg[i, 1] <- icc_tmp$results[2, 2]
  predConfRelReg[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predConfRelReg[i, 4] <-  var_indicesConf[i]
}

#For Action adjustments regression with normative predictors####
lmLrY1_tmp <-lmer(LrY.sc ~ X1.sc+X1X2.sc+ X1X2X3.sc + X1X4.sc+age.sc +gender+total_iq.sc+(1+X1.sc+X1X2.sc+ X1X2X3.sc + X1X4.sc|id),control= lmerControl(optimizer="bobyqa"), verbose=0, data=finalGameQuestData_pt1Mod_tmp)#use the t1 dataset again with participants that did both time points
#get the coefficients into one dataset with the corresponding subject id
predActMod1Coefs<-data.frame(coef(lmLrY1_tmp)$id)[1:5]
predActMod1Coefs$id <-finalGameQuestData_pt1Mod_tmp[!duplicated(finalGameQuestData_pt1Mod_tmp$id), ]$id
#same for t2
predActMod2Coefs<-data.frame(coef(lmLrY2)$id)[1:5]
predActMod2Coefs$id<-finalGameQuestData_t2Mod[!duplicated(finalGameQuestData_t2Mod$id), ]$id
#merge the timepoints into one dataset
predActModAllCoefs = merge(predActMod1Coefs,predActMod2Coefs,by=c('id'))
predActModAllCoefs <- predActModAllCoefs[with(predActModAllCoefs, order(id)), ]
#get test-retest reliability
predActRelReg <- data.frame(matrix(NA, 3, 4))
var_indicesAct <- c("X1.sc", "X1X2.sc", "X1X2X3.sc", "X1X4.sc")
# Loop over the time points
for (i in seq_along(var_indicesAct)) {
  var_namex <-predActModAllCoefs [[paste0(var_indicesAct[i],".x")]]
  var_namey <-predActModAllCoefs [[paste0(var_indicesAct[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predActRelReg[i, 1] <- icc_tmp$results[2, 2]
  predActRelReg[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predActRelReg[i, 4] <-  var_indicesAct[i]
}

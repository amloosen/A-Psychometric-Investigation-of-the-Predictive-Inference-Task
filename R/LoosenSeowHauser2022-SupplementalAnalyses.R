################################################################################################################
##Loosen, Seow & Hauser (submitted) -   Supplemental Information I                                            ##
##Consistency within change: Evaluating the psychometric properties of a widely-used predictive-inference task##
################################################################################################################

## CLEAR ALL
rm(list=ls())

##########
## SET DIRECTORY
setwd("~/Documents/GitHub/Probabilistic-Inference-Task-Analysis/R")

##########
##LOADING LIBRARIES
# loading tools
library(funchir)#this library enables you to check which libraries are actually used in a script
funchir::stale_package_check('LoosenSeowHauser2022-SupplementalAnalyses.R')# Reliability Analyses Probabilistic inference task
library(dplyr)
library(ggpubr)
library(psych)
library(broom)
library(tidyr)
library(cowplot)
library(data.table)   
options(scipen=999) 
options(max.print=25000)

##################
##Loading data####
##################
#questionnaire data
quest_pt1 = data.frame(read.csv("Table_questPIT_pt1.csv",header = T))
quest_t2 = data.frame(read.csv("Table_questPIT_alltp.csv",header = T))
#game data
Game_pt1= data.frame(read.csv("Table_gamePIT_pt1.csv",header = T))#dataset t1 quest and predictive inference task
Game_t2= data.frame(read.csv("Table_gamePIT_t2.csv",header = T))#dataset t1 and t2 combined, quest and predictive inference task
Game_pt1_relativeCP_tmp= data.frame(read.csv("Table_gamePIT_pt1_relativeCP.csv",header = T))#load trials relative to all change points (CPs)
Game_pt1_relativeCP <- Game_pt1_relativeCP_tmp[with(Game_pt1_relativeCP_tmp,order(id,nTrial_rel)),]#order according to trial number and id
Game_t2_relativeCP_tmp= data.frame(read.csv("Table_gamePIT_t2_relativeCP.csv",header = T))
Game_t2_relativeCP <- Game_t2_relativeCP_tmp[with(Game_t2_relativeCP_tmp,order(id,nTrial_rel)),] 

#merge t1 and t2 data
Game_t2_relativeCP_colnames <- Game_t2_relativeCP
names(Game_t2_relativeCP_colnames) <- paste0(names(Game_t2_relativeCP_colnames), "_t2")#change the col names so we can merge it with t1 data
setnames(Game_t2_relativeCP_colnames, old = c('id_t2','nTrial_rel_t2'), new = c('id','nTrial_rel'))
Game_all_relativeCP = merge(Game_pt1_relativeCP,Game_t2_relativeCP_colnames,by=c('id','nTrial_rel'))
Game_all_relativeCP <- Game_all_relativeCP[with(Game_all_relativeCP, order(id, nTrial_rel)), ]

#create odd and even dataset based on trials - before z-scoring to avoid artificial smoothing 
odd_indexes1 <- which(Game_pt1$nTrial %% 2 == 1)# Create row indicator
even_indexes1 <- which(Game_pt1$nTrial %% 2 == 0)       
Game_pt1_odd = Game_pt1[odd_indexes1,]
Game_pt1_even = Game_pt1[even_indexes1,]

odd_indexes2 <- which(Game_t2$nTrial %% 2 == 1)            
even_indexes2 <- which(Game_t2$nTrial %% 2 == 0)            
Game_t2_odd = Game_t2[odd_indexes2,]
Game_t2_even= Game_t2[even_indexes2,]


##Summarize data across time points#### 
Game_t2_colnames <- Game_t2
names(Game_t2_colnames) <- paste0(names(Game_t2_colnames), "_t2")#change the col names so we can merge it with t1 data
##
dat_analys_t1 <- data.frame(Game_pt1$id, Game_pt1$nTrial,Game_pt1$behav_lrSD,Game_pt1$conf)
colnames(dat_analys_t1) <- c("id","trial", "behav_lrSD","conf")
dat_analys_t2 <- data.frame(Game_t2_colnames$id_t2,Game_t2_colnames$nTrial_t2,Game_t2_colnames$behav_lrSD_t2,Game_t2_colnames$conf_t2)
colnames(dat_analys_t2) <- c("id","trial", "behav_lrSD_t2","conf_t2")
both_ts_analys <- merge(dat_analys_t1,dat_analys_t2, by=c('id','trial'))
both_ts_analys <- data.frame(both_ts_analys)
both_ts_analys <- both_ts_analys[with(both_ts_analys,order(id, trial)), ] #ensure order by id and trial number

both_ts_analys_means <- both_ts_analys %>%#get summary stats
  group_by(id) %>%
  summarise_at(vars(behav_lrSD,behav_lrSD_t2,conf,conf_t2), funs(mean(., na.rm=TRUE)))

################
##Functions ####
################
IntCons_r <- function(even_values,odd_values,complete_dataset){
  r_tmp <- cor(even_values,odd_values,use="complete.obs",method = "pearson")#correlate
  r_final <- 2*r_tmp/(1+r_tmp)#correct
  if(missing(complete_dataset)) {
    return(r_final)
  } else {
    r_ci<-r.con(r_final,n=dim(complete_dataset)[1],p=.95,twotailed=TRUE)#confidence interval
    r_complete <- data.frame(r_final,r_ci)
    return (r_complete)
  }
}

#########################################
##Prepare task data for the regressions## 
#########################################
#Remove the first trial of every participant because action updates conceptually only start at the second trial
finalGameData_pt1<- Game_pt1[Game_pt1$nTrial!=1, ]
#odd vs. even CPs
finalGame_pt1_odd<- Game_pt1_odd[Game_pt1_odd$nTrial!=1, ]
finalGame_pt1_even<- Game_pt1_even
#t2
finalGameData_t2<- Game_t2[Game_t2$nTrial!=1, ]
#odd vs. even CPs
finalGame_t2_odd<- Game_t2_odd[Game_t2_odd$nTrial!=1, ]
finalGame_t2_even<- Game_t2_even
#z-score confidence ratings within participants and turn id into factor variable
finalGameData_pt1$conf.sc <-ave(finalGameData_pt1$conf, finalGameData_pt1$id, FUN=scale)
finalGameData_pt1$behav_dSD.sc <-ave(finalGameData_pt1$behav_dSD, finalGameData_pt1$id, FUN=scale)
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
#odd vs. even CPs
finalGameQuest_pt1_odd = merge(quest_pt1,finalGame_pt1_odd,by="id")
finalGameQuest_pt1_odd <- finalGameQuest_pt1_odd[with(finalGameQuest_pt1_odd, order(id, nTrial)), ] 
finalGameQuest_pt1_even = merge(quest_pt1,finalGame_pt1_even,by="id")
finalGameQuest_pt1_even <- finalGameQuest_pt1_even[with(finalGameQuest_pt1_even, order(id, nTrial)), ] 
#t2
finalGameQuestData_t2 = merge(quest_t2,finalGameData_t2,by="id")
finalGameQuestData_t2 <- finalGameQuestData_t2[with(finalGameQuestData_t2, order(id, nTrial)), ]
#odd vs. even CPs
finalGameQuest_t2_odd = merge(quest_t2,finalGame_t2_odd,by="id")
finalGameQuest_t2_odd <- finalGameQuest_t2_odd[with(finalGameQuest_t2_odd, order(id, nTrial)), ] 
finalGameQuest_t2_even = merge(quest_t2,finalGame_t2_even,by="id")
finalGameQuest_t2_even <- finalGameQuest_t2_even[with(finalGameQuest_t2_even, order(id, nTrial)), ] 

#merge t1 and t2 data####
Game_t2_relativeCP_colnames <- Game_t2_relativeCP
names(Game_t2_relativeCP_colnames) <- paste0(names(Game_t2_relativeCP_colnames), "_t2")#change the col names so we can merge it with t1 data
setnames(Game_t2_relativeCP_colnames, old = c('id_t2','nTrial_rel_t2'), new = c('id','nTrial_rel'))
Game_all_relativeCP = merge(Game_pt1_relativeCP,Game_t2_relativeCP_colnames,by=c('id','nTrial_rel'))
Game_all_relativeCP <- Game_all_relativeCP[with(Game_all_relativeCP, order(id, nTrial_rel)), ]

#get a single entry per id data for each time point (trial == 1)####
Game_t2_singl<- Game_t2[!duplicated(Game_t2$id), ]
Game_pt1_singl <- Game_pt1[!duplicated(Game_pt1$id), ]
Game_all_singl <- merge(Game_pt1_singl,Game_t2_singl,by='id')

#trialwise - create datasets that look at measures relative to CPs####
# Create an empty list to store the subsets
game_pt1_rel <- list()
before <- -4
after <- 4
# Loop through the desired range
for (i in before:after) {
  # Subset the data based on nTrial_rel value
  game_pt1_rel[[paste0("Game_pt1_", ifelse(i >= 0, "aft", "bef"), abs(i))]] <- Game_pt1_relativeCP[Game_pt1_relativeCP$nTrial_rel == i, ]
}

#trialwise t2 (and t1 that completed t2)####
# Loop through the desired range
game_t2_rel <- list()
for (i in before:after) {
  # Subset the data based on nTrial_rel value
  game_t2_rel[[paste0("Game_all_", ifelse(i >= 0, "aft", "bef"), abs(i))]] <- Game_all_relativeCP[Game_all_relativeCP$nTrial_rel == i, ]
}

# Loop through the desired range
game_t2_rel_IntCons <- list()
for (i in before:after) {
  # Subset the data based on nTrial_rel value
  game_t2_rel_IntCons[[paste0("Game_t2_", ifelse(i >= 0, "aft", "bef"), abs(i))]] <- Game_t2_relativeCP[Game_t2_relativeCP$nTrial_rel == i, ]
}
#get a single entry per id data for each time point (trial == 1)####
Game_t2_singl<- Game_t2[!duplicated(Game_t2$id), ]
Game_pt1_singl <- Game_pt1[!duplicated(Game_pt1$id), ]
Game_all_singl <- merge(Game_pt1_singl,Game_t2_singl,by='id')

###################################
#Regression analyses####
DistCPLR1SumStats= finalGameQuestData_pt1%>% group_by(id) %>% do(fitmod = tidy(glm(abs(behav_lr)~distance_toCP, data =.))) %>%  unnest(fitmod)
DistCPLR1SumStats<-reshape2::dcast(DistCPLR1SumStats, id ~ term, value.var = 'estimate')
##odd vs. even CPs (to later check the robustness)
DistCPLR1_oddSumStats = finalGameQuest_pt1_odd%>% group_by(id) %>% do(fitmod = tidy(glm(abs(behav_lr)~distance_toCP, data =.))) %>%  unnest(fitmod)
DistCPLR1_oddSumStats<-reshape2::dcast(DistCPLR1_oddSumStats, id ~ term, value.var = 'estimate')
DistCPLR1_evenSumStats = finalGameQuest_pt1_even%>% group_by(id) %>% do(fitmod = tidy(glm(abs(behav_lr)~distance_toCP, data =.))) %>%  unnest(fitmod)
DistCPLR1_evenSumStats<-reshape2::dcast(DistCPLR1_evenSumStats, id ~ term, value.var = 'estimate')
#t2
DistCPLR2SumStats = finalGameQuestData_t2%>% group_by(id) %>% do(fitmod = tidy(glm(abs(behav_lr)~distance_toCP, data =.))) %>%  unnest(fitmod)
DistCPLR2SumStats<-reshape2::dcast(DistCPLR2SumStats, id ~ term, value.var = 'estimate')
#odd vs even
DistCPLR2_oddSumStats= finalGameQuest_t2_odd%>% group_by(id) %>% do(fitmod = tidy(glm(abs(behav_lr)~distance_toCP, data =.))) %>%  unnest(fitmod)
DistCPLR2_oddSumStats<-reshape2::dcast(DistCPLR2_oddSumStats, id ~ term, value.var = 'estimate')
DistCPLR2_evenSumStats= finalGameQuest_t2_even%>% group_by(id) %>% do(fitmod = tidy(glm(abs(behav_lr)~distance_toCP, data =.))) %>%  unnest(fitmod)
DistCPLR2_evenSumStats<-reshape2::dcast(DistCPLR2_evenSumStats, id ~ term, value.var = 'estimate')

# Link to the Alternative Bayesian Learner####
##Exluding criteria (trial-wise)#### 
#1.Exclude trials where estimated learning rate is higher than 99% of all learning rates; 
#2.Exclude trials where PE is zero
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
finalGameQuestData_pt1Mod$X1X2X3<-finalGameQuestData_pt1Mod$X1*finalGameQuestData_pt1Mod$X3*(1-finalGameQuestData_pt1Mod$X2)#model PE*(1-CPP)*RU
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

# alterenative RU/ X3
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

# Scaling shifted regressors
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

#Confidence Models: 2) Summary statistics approach with linear bayesian factors####
lmConf1CPP_SumStats= finalGameQuestData_pt1Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1CPP_SumStats<-reshape2::dcast(lmConf1CPP_SumStats, id ~ term, value.var = 'estimate')

lmConf1PE_SumStats= finalGameQuestData_pt1Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1PE_SumStats<-reshape2::dcast(lmConf1PE_SumStats, id ~ term, value.var = 'estimate')

#odd vs. even CPs
lmConf1oddPE_SumStats= finalGameQuest_pt1_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1oddPE_SumStats<-reshape2::dcast(lmConf1oddPE_SumStats, id ~ term, value.var = 'estimate')

lmConf1evenPE_SumStats= finalGameQuest_pt1_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1evenPE_SumStats<-reshape2::dcast(lmConf1evenPE_SumStats, id ~ term, value.var = 'estimate')

lmConf1oddCPP_SumStats= finalGameQuest_pt1_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1oddCPP_SumStats<-reshape2::dcast(lmConf1oddCPP_SumStats, id ~ term, value.var = 'estimate')

lmConf1evenCPP_SumStats= finalGameQuest_pt1_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1evenCPP_SumStats<-reshape2::dcast(lmConf1evenCPP_SumStats, id ~ term, value.var = 'estimate')

#t2
lmConf2CPP_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2CPP_SumStats<-reshape2::dcast(lmConf2CPP_SumStats, id ~ term, value.var = 'estimate')

lmConf2PE_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2PE_SumStats<-reshape2::dcast(lmConf2PE_SumStats, id ~ term, value.var = 'estimate')


#odd vs. even CPs
lmConf2oddPE_SumStats= finalGameQuest_t2_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2oddPE_SumStats<-reshape2::dcast(lmConf2oddPE_SumStats, id ~ term, value.var = 'estimate')

lmConf2evenPE_SumStats= finalGameQuest_t2_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2evenPE_SumStats<-reshape2::dcast(lmConf2evenPE_SumStats, id ~ term, value.var = 'estimate')

lmConf2oddCPP_SumStats= finalGameQuest_t2_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2oddCPP_SumStats<-reshape2::dcast(lmConf2oddCPP_SumStats, id ~ term, value.var = 'estimate')

lmConf2evenCPP_SumStats= finalGameQuest_t2_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX2X3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2evenCPP_SumStats<-reshape2::dcast(lmConf2evenCPP_SumStats, id ~ term, value.var = 'estimate')

#Action-Update Models: 3) Summary statistics approach with linear variables####
lmLrY1_SumStats= finalGameQuestData_pt1Mod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~  X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1_SumStats<-reshape2::dcast(lmLrY1_SumStats, id ~ term, value.var = 'estimate')

lmLrY1CPP_SumStats= finalGameQuestData_pt1Mod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1CPP_SumStats<-reshape2::dcast(lmLrY1CPP_SumStats, id ~ term, value.var = 'estimate')

lmLrY1PE_SumStats= finalGameQuestData_pt1Mod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1PE_SumStats<-reshape2::dcast(lmLrY1PE_SumStats, id ~ term, value.var = 'estimate')

#odd vs. even CPs
lmLrY1odd_SumStats= finalGameQuest_pt1_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1odd_SumStats<-reshape2::dcast(lmLrY1odd_SumStats, id ~ term, value.var = 'estimate')

lmLrY1even_SumStats= finalGameQuest_pt1_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1even_SumStats<-reshape2::dcast(lmLrY1even_SumStats, id ~ term, value.var = 'estimate')

lmLrY1oddCPP_SumStats= finalGameQuest_pt1_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1oddCPP_SumStats<-reshape2::dcast(lmLrY1oddCPP_SumStats, id ~ term, value.var = 'estimate')

lmLrY1evenCPP_SumStats= finalGameQuest_pt1_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1evenCPP_SumStats<-reshape2::dcast(lmLrY1evenCPP_SumStats, id ~ term, value.var = 'estimate')

lmLrY1oddPE_SumStats= finalGameQuest_pt1_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1oddPE_SumStats<-reshape2::dcast(lmLrY1oddPE_SumStats, id ~ term, value.var = 'estimate')

lmLrY1evenPE_SumStats= finalGameQuest_pt1_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY1evenPE_SumStats<-reshape2::dcast(lmLrY1evenPE_SumStats, id ~ term, value.var = 'estimate')

#t2
lmLrY2_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2_SumStats<-reshape2::dcast(lmLrY2_SumStats, id ~ term, value.var = 'estimate')

lmLrY2CPP_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2CPP_SumStats<-reshape2::dcast(lmLrY2CPP_SumStats, id ~ term, value.var = 'estimate')

lmLrY2PE_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+  X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2PE_SumStats<-reshape2::dcast(lmLrY2PE_SumStats, id ~ term, value.var = 'estimate')

#odd vs. even CPs
lmLrY2odd_SumStats= finalGameQuest_t2_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2odd_SumStats<-reshape2::dcast(lmLrY2odd_SumStats, id ~ term, value.var = 'estimate')

lmLrY2even_SumStats= finalGameQuest_t2_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2even_SumStats<-reshape2::dcast(lmLrY2even_SumStats, id ~ term, value.var = 'estimate')

lmLrY2oddCPP_SumStats= finalGameQuest_t2_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2oddCPP_SumStats<-reshape2::dcast(lmLrY2oddCPP_SumStats, id ~ term, value.var = 'estimate')

lmLrY2evenCPP_SumStats= finalGameQuest_t2_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1X2.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2evenCPP_SumStats<-reshape2::dcast(lmLrY2evenCPP_SumStats, id ~ term, value.var = 'estimate')

lmLrY2oddPE_SumStats= finalGameQuest_t2_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+ X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2oddPE_SumStats<-reshape2::dcast(lmLrY2oddPE_SumStats, id ~ term, value.var = 'estimate')

lmLrY2evenPE_SumStats= finalGameQuest_t2_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(LrY.sc ~ X1.sc+  X1X2X3.sc + X1X4.sc, data =.))) %>%  unnest(fitmod)
lmLrY2evenPE_SumStats<-reshape2::dcast(lmLrY2evenPE_SumStats, id ~ term, value.var = 'estimate')

##Investigate the psychometric properties of the regression coefficients##
#Internal Consistency####
#1a)LR linear####
# Define the indices for bef and aft
bef_indices <- c("bef4", "bef3", "bef2", "bef1")
aft_indices <- c("aft0", "aft1", "aft2", "aft3", "aft4")

# Initialize empty variables to store the results
rCPpt1.lrSD <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPpt1.lrSD <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)
# Iterate over bef indices
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_pt1_bef <- game_pt1_rel[[paste0("Game_pt1_", bef_index)]]
  
  rCPpt1.lrSD[i] <- IntCons_r(game_pt1_bef$median_behav_lrSD_even,
                            game_pt1_bef$median_behav_lrSD_odd,
                            game_pt1_bef)[1, 1]
  
  ciCPpt1.lrSD[i,] <- IntCons_r(game_pt1_bef$median_behav_lrSD_even,
                              game_pt1_bef$median_behav_lrSD_odd,
                              game_pt1_bef)[, 2]
}
# Iterate over aft indices
for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_pt1_aft <- game_pt1_rel[[paste0("Game_pt1_", aft_index)]]
  
  rCPpt1.lrSD[i+4] <- IntCons_r(game_pt1_aft$median_behav_lrSD_even,
                              game_pt1_aft$median_behav_lrSD_odd,
                              game_pt1_aft)[1, 1]
  
  ciCPpt1.lrSD[i+4,] <- IntCons_r(game_pt1_aft$median_behav_lrSD_even,
                                game_pt1_aft$median_behav_lrSD_odd,
                                game_pt1_aft)[, 2]
}

#t2#
rCPt2.lrSD <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPt2.lrSD <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)

for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_t2_bef <- game_t2_rel_IntCons[[paste0("Game_t2_", bef_index)]]
  
  rCPt2.lrSD[i] <- IntCons_r(game_t2_bef$median_behav_lrSD_even,
                              game_t2_bef$median_behav_lrSD_odd,
                              game_t2_bef)[1, 1]
  
  ciCPt2.lrSD[i,] <- IntCons_r(game_t2_bef$median_behav_lrSD_even,
                                game_t2_bef$median_behav_lrSD_odd,
                                game_t2_bef)[, 2]
}
# Iterate over aft indices
for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_t2_aft <- game_t2_rel_IntCons[[paste0("Game_t2_", aft_index)]]
  
  rCPt2.lrSD[i+4] <- IntCons_r(game_t2_aft$median_behav_lrSD_even,
                                game_t2_aft$median_behav_lrSD_odd,
                                game_t2_aft)[1, 1]
  
  ciCPt2.lrSD[i+4,] <- IntCons_r(game_t2_aft$median_behav_lrSD_even,
                                  game_t2_aft$median_behav_lrSD_odd,
                                  game_t2_aft)[, 2]
}
#1b) LR difference####
rCPpt1.lrSDdiffAft <-IntCons_r(Game_pt1_singl$behav_lrSDDiffevenAft,Game_pt1_singl$behav_lrSDDiffoddAft,Game_pt1_singl)[1,1]
ciCPpt1.lrSDdiffAft<-IntCons_r(Game_pt1_singl$behav_lrSDDiffevenAft,Game_pt1_singl$behav_lrSDDiffoddAft,Game_pt1_singl)[,2]
#t2
rCPpt2.lrdiffAft <-IntCons_r(Game_t2_singl$behav_lrSDDiffevenAft,Game_t2_singl$behav_lrSDDiffoddAft,Game_t2_singl)[1,1]
ciCPpt2.lrdiffAft<-IntCons_r(Game_t2_singl$behav_lrSDDiffevenAft,Game_t2_singl$behav_lrSDDiffoddAft,Game_t2_singl)[,2]
#1c) LR-Distance to CP####
#merge
DistCPLRModAllCoefs1_oddeven = merge(DistCPLR1_oddSumStats,DistCPLR1_evenSumStats,by=c('id'))
DistCPLRModAllCoefs1_oddeven <- DistCPLRModAllCoefs1_oddeven[with(DistCPLRModAllCoefs1_oddeven, order(id)), ]
#get internal consistency
r_DistCPLRoddeven.corrected <- IntCons_r(DistCPLRModAllCoefs1_oddeven$distance_toCP.x,DistCPLRModAllCoefs1_oddeven$distance_toCP.y,DistCPLRModAllCoefs1_oddeven)[1,1]
ci.DistCPLRoddeven <- IntCons_r(DistCPLRModAllCoefs1_oddeven$distance_toCP.x,DistCPLRModAllCoefs1_oddeven$distance_toCP.y,DistCPLRModAllCoefs1_oddeven)[,2]
#t2####
#merge
DistCPLRModAllCoefs2_oddeven = merge(DistCPLR2_oddSumStats,DistCPLR2_evenSumStats,by=c('id'))
DistCPLRModAllCoefs2_oddeven <- DistCPLRModAllCoefs2_oddeven[with(DistCPLRModAllCoefs2_oddeven, order(id)), ]
#get internal consistency
r_DistCPLRoddeven2.corrected <- IntCons_r(DistCPLRModAllCoefs2_oddeven$distance_toCP.x,DistCPLRModAllCoefs2_oddeven$distance_toCP.y,DistCPLRModAllCoefs2_oddeven)[1,1]
ci.DistCPLRoddeven2 <- IntCons_r(DistCPLRModAllCoefs2_oddeven$distance_toCP.x,DistCPLRModAllCoefs2_oddeven$distance_toCP.y,DistCPLRModAllCoefs2_oddeven)[,2]

#1d) corr PE ActUp####
# Initialize empty variables to store the results
rCPpt1.circcor_ActPE <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPpt1.circcor_ActPE <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)
# Iterate over bef indices
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_pt1_bef <- game_pt1_rel[[paste0("Game_pt1_", bef_index)]]
  
  rCPpt1.circcor_ActPE[i] <- IntCons_r(game_pt1_bef$circcor_ActPE_even,
                              game_pt1_bef$circcor_ActPE_odd,
                              game_pt1_bef)[1, 1]
  
  ciCPpt1.circcor_ActPE[i,] <- IntCons_r(game_pt1_bef$circcor_ActPE_even,
                                game_pt1_bef$circcor_ActPE_odd,
                                game_pt1_bef)[, 2]
}
# Iterate over aft indices
for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_pt1_aft <- game_pt1_rel[[paste0("Game_pt1_", aft_index)]]
  
  rCPpt1.circcor_ActPE[i+4] <- IntCons_r(game_pt1_aft$circcor_ActPE_even,
                                game_pt1_aft$circcor_ActPE_odd,
                                game_pt1_aft)[1, 1]
  
  ciCPpt1.circcor_ActPE[i+4,] <- IntCons_r(game_pt1_aft$circcor_ActPE_even,
                                  game_pt1_aft$circcor_ActPE_odd,
                                  game_pt1_aft)[, 2]
}

#1e) corr RU ActUp####
# Initialize empty variables to store the results
rCPpt1.circcor_ActRU <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPpt1.circcor_ActRU <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)
# Iterate over bef indices
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_pt1_bef <- game_pt1_rel[[paste0("Game_pt1_", bef_index)]]
  
  rCPpt1.circcor_ActRU[i] <- IntCons_r(game_pt1_bef$circcor_ActRU_even,
                                       game_pt1_bef$circcor_ActRU_odd,
                                       game_pt1_bef)[1, 1]
  
  ciCPpt1.circcor_ActRU[i,] <- IntCons_r(game_pt1_bef$circcor_ActRU_even,
                                         game_pt1_bef$circcor_ActRU_odd,
                                         game_pt1_bef)[, 2]
}
# Iterate over aft indices
for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_pt1_aft <- game_pt1_rel[[paste0("Game_pt1_", aft_index)]]
  
  rCPpt1.circcor_ActRU[i+4] <- IntCons_r(game_pt1_aft$circcor_ActRU_even,
                                         game_pt1_aft$circcor_ActRU_odd,
                                         game_pt1_aft)[1, 1]
  
  ciCPpt1.circcor_ActRU[i+4,] <- IntCons_r(game_pt1_aft$circcor_ActRU_even,
                                           game_pt1_aft$circcor_ActRU_odd,
                                           game_pt1_aft)[, 2]
}

#t2####
#1d) corr PE ActUp####
# Initialize empty variables to store the results
rCPt2.circcor_ActPE <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPt2.circcor_ActPE <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)
# Iterate over bef indices
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_t2_bef <- game_t2_rel_IntCons[[paste0("Game_t2_", bef_index)]]
  
  rCPt2.circcor_ActPE[i] <- IntCons_r(game_t2_bef$circcor_ActPE_even,
                                       game_t2_bef$circcor_ActPE_odd,
                                       game_t2_bef)[1, 1]
  
  ciCPt2.circcor_ActPE[i,] <- IntCons_r(game_t2_bef$circcor_ActPE_even,
                                         game_t2_bef$circcor_ActPE_odd,
                                         game_t2_bef)[, 2]
}
# Iterate over aft indices
for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_t2_aft <- game_t2_rel_IntCons[[paste0("Game_t2_", aft_index)]]
  
  rCPt2.circcor_ActPE[i+4] <- IntCons_r(game_t2_aft$circcor_ActPE_even,
                                         game_t2_aft$circcor_ActPE_odd,
                                         game_t2_aft)[1, 1]
  
  ciCPt2.circcor_ActPE[i+4,] <- IntCons_r(game_t2_aft$circcor_ActPE_even,
                                           game_t2_aft$circcor_ActPE_odd,
                                           game_t2_aft)[, 2]
}

#1e) corr RU ActUp####
# Initialize empty variables to store the results
rCPt2.circcor_ActRU <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPt2.circcor_ActRU <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)
# Iterate over bef indices
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_t2_bef <- game_t2_rel_IntCons[[paste0("Game_t2_", bef_index)]]
  
  rCPt2.circcor_ActRU[i] <- IntCons_r(game_t2_bef$circcor_ActRU_even,
                                       game_t2_bef$circcor_ActRU_odd,
                                       game_t2_bef)[1, 1]
  
  ciCPt2.circcor_ActRU[i,] <- IntCons_r(game_t2_bef$circcor_ActRU_even,
                                         game_t2_bef$circcor_ActRU_odd,
                                         game_t2_bef)[, 2]
}
# Iterate over aft indices
for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_t2_aft <- game_t2_rel_IntCons[[paste0("Game_t2_", aft_index)]]
  
  rCPt2.circcor_ActRU[i+4] <- IntCons_r(game_t2_aft$circcor_ActRU_even,
                                         game_t2_aft$circcor_ActRU_odd,
                                         game_t2_aft)[1, 1]
  
  ciCPt2.circcor_ActRU[i+4,] <- IntCons_r(game_t2_aft$circcor_ActRU_even,
                                           game_t2_aft$circcor_ActRU_odd,
                                           game_t2_aft)[, 2]
}

#For 2) Action adjustments regression with normative predictors####
linBetasOddEven = merge(lmLrY1odd_SumStats,lmLrY1even_SumStats,by=c('id'))
linBetasOddEven <- linBetasOddEven[with(linBetasOddEven, order(id)), ]
#combined model####
var_indices <- c("X1.sc", "X1X2.sc", "X1X2X3.sc", "X1X4.sc") # Modify with your desired indices
r_linActFull <- matrix(NA, nrow = length(var_indices), ncol = 2)
ci.linActFull <- matrix(NA, nrow = length(var_indices), ncol = 3)

for (i in seq_along(var_indices)) {
  var_index <- var_indices[i]
  xActFull <- linBetasOddEven[[paste0(var_index,".x")]]
  yActFull <- linBetasOddEven[[paste0(var_index,".y")]]
  intCons <- IntCons_r(xActFull, yActFull, linBetasOddEven)
  r_linActFull[i,1] <- intCons[1, 1]
  ci.linActFull[i,1:2 ] <- intCons[, 2]
  ci.linActFull[i,3 ] <- var_indices[i]
  r_linActFull[i,2] <- var_indices[i]
}

#PEmodel####
#merge
linBetasOddEvenPE = merge(lmLrY1oddPE_SumStats,lmLrY1evenPE_SumStats,by=c('id'))
linBetasOddEvenPE <- linBetasOddEvenPE[with(linBetasOddEvenPE, order(id)), ]
#get internal consistency
var_indicesPE <- c("X1.sc","X1X2X3.sc", "X1X4.sc") # Modify with your desired indices
r_linActPE <- matrix(NA, nrow = length(var_indicesPE), ncol = 2)
ci.linActPE <- matrix(NA, nrow = length(var_indicesPE), ncol = 3)

for (i in seq_along(var_indicesPE)) {
  var_index <- var_indicesPE[i]
  xActPE <- linBetasOddEvenPE[[paste0(var_index,".x")]]
  yActPE <- linBetasOddEvenPE[[paste0(var_index,".y")]]
  intConsPE <- IntCons_r(xActPE, yActPE, linBetasOddEvenPE)
  r_linActPE[i,1] <- intConsPE[1, 1]
  ci.linActPE[i,1:2 ] <- intConsPE[, 2]
  ci.linActPE[i,3 ] <- var_indicesPE[i]
  r_linActPE[i,2] <- var_indicesPE[i]
}

#CPPmodel####
#merge
linBetasOddEvenCPP = merge(lmLrY1oddCPP_SumStats,lmLrY1evenCPP_SumStats,by=c('id'))
linBetasOddEvenCPP <- linBetasOddEvenCPP[with(linBetasOddEvenCPP, order(id)), ]
#get internal consistency
var_indicesCPP <- c("X1X2.sc","X1X2X3.sc", "X1X4.sc") # Modify with your desired indices
r_linActCPP <- matrix(NA, nrow = length(var_indicesCPP), ncol = 2)
ci.linActCPP <- matrix(NA, nrow = length(var_indicesCPP), ncol = 3)

for (i in seq_along(var_indicesCPP)) {
  var_index <- var_indicesCPP[i]
  xActCPP <- linBetasOddEvenCPP[[paste0(var_index,".x")]]
  yActCPP <- linBetasOddEvenCPP[[paste0(var_index,".y")]]
  intConsCPP <- IntCons_r(xActCPP, yActCPP, linBetasOddEvenCPP)
  r_linActCPP[i,1] <- intConsCPP[1, 1]
  ci.linActCPP[i,1:2 ] <- intConsCPP[, 2]
  ci.linActCPP[i,3 ] <- var_indicesCPP[i]
  r_linActCPP[i,2] <- var_indicesCPP[i]
}

#t2####
linBetasOddEven2= merge(lmLrY2odd_SumStats,lmLrY2even_SumStats,by=c('id'))
linBetasOddEven2 <- linBetasOddEven2[with(linBetasOddEven2, order(id)), ]
#combined model####
var_indices_t2 <- c("X1.sc", "X1X2.sc", "X1X2X3.sc", "X1X4.sc") # Modify with your desired indices
r_linActFull_t2 <- matrix(NA, nrow = length(var_indices_t2), ncol = 2)
ci.linActFull_t2 <- matrix(NA, nrow = length(var_indices_t2), ncol = 3)

for (i in seq_along(var_indices_t2)) {
  var_index_t2 <- var_indices_t2[i]
  xActFull_t2 <- linBetasOddEven2[[paste0(var_index_t2,".x")]]
  yActFull_t2 <- linBetasOddEven2[[paste0(var_index_t2,".y")]]
  intCons_t2 <- IntCons_r(xActFull_t2, yActFull_t2, linBetasOddEven2)
  r_linActFull_t2[i,1] <- intCons_t2[1, 1]
  ci.linActFull_t2[i,1:2 ] <- intCons_t2[, 2]
  ci.linActFull_t2[i,3 ] <- var_indices_t2[i]
  r_linActFull_t2[i,2] <- var_indices_t2[i]
}

#PEmodel####
#merge
linBetasOddEvenPE2 = merge(lmLrY2oddPE_SumStats,lmLrY2evenPE_SumStats,by=c('id'))
linBetasOddEvenPE2 <- linBetasOddEvenPE2[with(linBetasOddEvenPE2, order(id)), ]
#get internal consistency
var_indicesPE_t2 <- c("X1.sc","X1X2X3.sc", "X1X4.sc") # Modify with your desired indices
r_linActPE_t2 <- matrix(NA, nrow = length(var_indicesPE_t2), ncol = 2)
ci.linActPE_t2 <- matrix(NA, nrow = length(var_indicesPE_t2), ncol = 3)

for (i in seq_along(var_indicesPE_t2)) {
  var_index_t2 <- var_indicesPE_t2[i]
  xActPE_t2 <- linBetasOddEvenPE2[[paste0(var_index_t2,".x")]]
  yActPE_t2 <- linBetasOddEvenPE2[[paste0(var_index_t2,".y")]]
  intConsPE_t2 <- IntCons_r(xActPE_t2, yActPE_t2, linBetasOddEvenPE2)
  r_linActPE_t2[i,1] <- intConsPE_t2[1, 1]
  ci.linActPE_t2[i,1:2 ] <- intConsPE_t2[, 2]
  ci.linActPE_t2[i,3 ] <- var_indicesPE_t2[i]
  r_linActPE_t2[i,2] <- var_indicesPE_t2[i]
}

#CPPmodel####
#merge
linBetasOddEvenCPP2 = merge(lmLrY2oddCPP_SumStats,lmLrY2evenCPP_SumStats,by=c('id'))
linBetasOddEvenCPP2 <- linBetasOddEvenCPP2[with(linBetasOddEvenCPP2, order(id)), ]
#get internal consistency
var_indicesCPP_t2 <- c("X1X2.sc","X1X2X3.sc", "X1X4.sc") # Modify with your desired indices
r_linActCPP_t2 <- matrix(NA, nrow = length(var_indicesCPP_t2), ncol = 2)
ci.linActCPP_t2 <- matrix(NA, nrow = length(var_indicesCPP_t2), ncol = 3)

for (i in seq_along(var_indicesCPP_t2)) {
  var_index_t2 <- var_indicesCPP_t2[i]
  xActCPP_t2 <- linBetasOddEvenCPP2[[paste0(var_index_t2,".x")]]
  yActCPP_t2 <- linBetasOddEvenCPP2[[paste0(var_index_t2,".y")]]
  intConsCPP_t2 <- IntCons_r(xActCPP_t2, yActCPP_t2, linBetasOddEvenCPP2)
  r_linActCPP_t2[i,1] <- intConsCPP_t2[1, 1]
  ci.linActCPP_t2[i,1:2 ] <- intConsCPP_t2[, 2]
  ci.linActCPP_t2[i,3 ] <- var_indicesCPP_t2[i]
  r_linActCPP_t2[i,2] <- var_indicesCPP_t2[i]
}

#For 3) Confidence adjustments regression with normative predictors####
#PEmodel####
#merge
linBetasOddEvenPE_Conf = merge(lmConf1oddPE_SumStats,lmConf1evenPE_SumStats,by=c('id'))
linBetasOddEvenPE_Conf <- llinBetasOddEvenPE_Conf[with(linBetasOddEvenPE_Conf, order(id)), ]
#get internal consistency
var_indicesPE_Conf <- c("shiftX1.sc","shiftX2X3.sc", "shiftX4.sc") # Modify with your desired indices
r_linActPE_Conf <- matrix(NA, nrow = length(var_indicesPE_Conf), ncol = 2)
ci.linActPE_Conf <- matrix(NA, nrow = length(var_indicesPE_Conf), ncol = 3)

for (i in seq_along(var_indicesPE_Conf)) {
  var_indexPE_Conf <- var_indicesPE_Conf[i]
  xActPE_Conf <- linBetasOddEvenPE_Conf[[paste0(var_indexPE_Conf,".x")]]
  yActPE_Conf <- linBetasOddEvenPE_Conf[[paste0(var_indexPE_Conf,".y")]]
  intConsPE_Conf <- IntCons_r(xActPE_Conf, yActPE_Conf, linBetasOddEvenPE_Conf)
  r_linActPE_Conf[i,1] <- intConsPE_Conf[1, 1]
  ci.linActPE_Conf[i,1:2 ] <- intConsPE_Conf[, 2]
  ci.linActPE_Conf[i,3 ] <- var_indicesPE_Conf[i]
  r_linActPE_Conf[i,2] <- var_indicesPE_Conf[i]
}

#CPPmodel####
#merge
linBetasOddEvenCPP_Conf = merge(lmConf1oddCPP_SumStats,lmConf1evenCPP_SumStats,by=c('id'))
linBetasOddEvenCPP_Conf <- llinBetasOddEvenCPP_Conf[with(linBetasOddEvenCPP_Conf, order(id)), ]
#get internal consistency
var_indicesCPP_Conf <- c("shiftX2.sc","shiftX2X3.sc", "shiftX4.sc") # Modify with your desired indices
r_linActCPP_Conf <- matrix(NA, nrow = length(var_indicesCPP_Conf), ncol = 2)
ci.linActCPP_Conf <- matrix(NA, nrow = length(var_indicesCPP_Conf), ncol = 3)

for (i in seq_along(var_indicesCPP_Conf)) {
  var_indexCPP_Conf <- var_indicesCPP_Conf[i]
  xActCPP_Conf <- linBetasOddEvenCPP_Conf[[paste0(var_indexCPP_Conf,".x")]]
  yActCPP_Conf <- linBetasOddEvenCPP_Conf[[paste0(var_indexCPP_Conf,".y")]]
  intConsCPP_Conf <- IntCons_r(xActCPP_Conf, yActCPP_Conf, linBetasOddEvenCPP_Conf)
  r_linActCPP_Conf[i,1] <- intConsCPP_Conf[1, 1]
  ci.linActCPP_Conf[i,1:2 ] <- intConsCPP_Conf[, 2]
  ci.linActCPP_Conf[i,3 ] <- var_indicesCPP_Conf[i]
  r_linActCPP_Conf[i,2] <- var_indicesCPP_Conf[i]
}

#t2####
#PEmodel####
linBetasOddEvenPE_Conf2 = merge(lmConf2oddPE_SumStats,lmConf2evenPE_SumStats,by=c('id'))
linBetasOddEvenPE_Conf2 <- linBetasOddEvenPE_Conf2[with(linBetasOddEvenPE_Conf2, order(id)), ]
#get internal consistency
var_indicesPE_Conf2 <- c("shiftX1.sc","shiftX2X3.sc", "shiftX4.sc") # Modify with your desired indices
r_linActPE_Conf2 <- matrix(NA, nrow = length(var_indicesPE_Conf2), ncol = 2)
ci.linActPE_Conf2 <- matrix(NA, nrow = length(var_indicesPE_Conf2), ncol = 3)

for (i in seq_along(var_indicesPE_Conf2)) {
  var_indexPE_Conf2 <- var_indicesPE_Conf2[i]
  xActPE_Conf2 <- linBetasOddEvenPE_Conf2[[paste0(var_indexPE_Conf2,".x")]]
  yActPE_Conf2 <- linBetasOddEvenPE_Conf2[[paste0(var_indexPE_Conf2,".y")]]
  intConsPE_Conf2 <- IntCons_r(xActPE_Conf2, yActPE_Conf2, linBetasOddEvenPE_Conf2)
  r_linActPE_Conf2[i,1] <- intConsPE_Conf2[1, 1]
  ci.linActPE_Conf2[i,1:2 ] <- intConsPE_Conf2[, 2]
  ci.linActPE_Conf2[i,3 ] <- var_indicesPE_Conf2[i]
  r_linActPE_Conf2[i,2] <- var_indicesPE_Conf2[i]
}

#CPPmodel####
linBetasOddEvenCPP_Conf2 = merge(lmConf2oddCPP_SumStats,lmConf2evenCPP_SumStats,by=c('id'))
linBetasOddEvenCPP_Conf2 <- linBetasOddEvenCPP_Conf2[with(linBetasOddEvenCPP_Conf2, order(id)), ]
#get internal consistency
var_indicesCPP_Conf2 <- c("shiftX2.sc","shiftX2X3.sc", "shiftX4.sc") # Modify with your desired indices
r_linActCPP_Conf2 <- matrix(NA, nrow = length(var_indicesCPP_Conf2), ncol = 2)
ci.linActCPP_Conf2 <- matrix(NA, nrow = length(var_indicesCPP_Conf2), ncol = 3)

for (i in seq_along(var_indicesCPP_Conf2)) {
  var_indexCPP_Conf2 <- var_indicesCPP_Conf2[i]
  xActCPP_Conf2 <- linBetasOddEvenCPP_Conf2[[paste0(var_indexCPP_Conf2,".x")]]
  yActCPP_Conf2 <- linBetasOddEvenCPP_Conf2[[paste0(var_indexCPP_Conf2,".y")]]
  intConsCPP_Conf2 <- IntCons_r(xActCPP_Conf2, yActCPP_Conf2, linBetasOddEvenCPP_Conf2)
  r_linActCPP_Conf2[i,1] <- intConsCPP_Conf2[1, 1]
  ci.linActCPP_Conf2[i,1:2 ] <- intConsCPP_Conf2[, 2]
  ci.linActCPP_Conf2[i,3 ] <- var_indicesCPP_Conf2[i]
  r_linActCPP_Conf2[i,2] <- var_indicesCPP_Conf2[i]
}

##Test-retest reliability####
#1a)LR linear####
icc_behav_lrSD <- data.frame(matrix(NA, 9, 3))

# Loop over the time points
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", bef_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$median_behav_lrSD,
              var_name$median_behav_lrSD_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_behav_lrSD[i, 1] <- icc_tmp$results[2, 2]
  icc_behav_lrSD[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", aft_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$median_behav_lrSD,
              var_name$median_behav_lrSD_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_behav_lrSD[i+4, 1] <- icc_tmp$results[2, 2]
  icc_behav_lrSD[i+4, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

#1b)LR difference####
icc_diffbehav_lrSD_Aftmat <- matrix(NA,1,3)
icc_diffbehav_lrSD_Aftmat_tmp <- psych::ICC(x= cbind(Game_all_singl$behav_lrSDDiffevenAft.x,Game_all_singl$behav_lrSDDiffevenAft.y),alpha=.05,lmer=TRUE)
icc_diffbehav_lrSD_Aftmat[1,1] <- icc_diffbehav_lrSD_Aftmat_tmp$results[2,2]
icc_diffbehav_lrSD_Aftmat[1,c(2:3)] <- icc_diffbehav_lrSD_Aftmat_tmp$results[2,c(7,8)]

#1c)LR - Distance to CP####
DistCPLRModAllCoefs = merge(DistCPLR1SumStats,DistCPLR2SumStats,by=c('id'))
DistCPLRModAllCoefs <- DistCPLRModAllCoefs[with(DistCPLRModAllCoefs, order(id)), ]

DistCPLRMod<- matrix(NA,1,3)
DistCPLRMod_tmp<- psych::ICC(x = cbind(DistCPLRModAllCoefs$distance_toCP.x, DistCPLRModAllCoefs$distance_toCP.y),alpha=.05,lmer=TRUE)#no variance between
DistCPLRMod[1,1] <- DistCPLRMod_tmp$results[2,2]
DistCPLRMod[1,c(2:3)] <- DistCPLRMod_tmp$results[2,c(7,8)]

#1d)corr PE ActUp####
icc_circcor_ActPE  <- data.frame(matrix(NA, 9, 3))

# Loop over the time points
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", bef_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$circcor_ActPE,
              var_name$circcor_ActPE_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_circcor_ActPE[i, 1] <- icc_tmp$results[2, 2]
  icc_circcor_ActPE[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", aft_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$circcor_ActPE,
              var_name$circcor_ActPE_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_circcor_ActPE[i+4, 1] <- icc_tmp$results[2, 2]
  icc_circcor_ActPE[i+4, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

#1e)corr ActUP RU####
icc_circcor_ActRU  <- data.frame(matrix(NA, 9, 3))

# Loop over the time points
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", bef_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$circcor_ActRU,
              var_name$circcor_ActRU_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_circcor_ActRU[i, 1] <- icc_tmp$results[2, 2]
  icc_circcor_ActRU[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", aft_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$circcor_ActRU,
              var_name$circcor_ActRU_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_circcor_ActRU[i+4, 1] <- icc_tmp$results[2, 2]
  icc_circcor_ActRU[i+4, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

########
#For 2) Action adjustments regression with normative predictors####
predActModAllCoefs = merge(lmLrY1_SumStats, lmLrY2_SumStats, by = c('id'))
predActModAllCoefs <- predActModAllCoefs[with(predActModAllCoefs, order(id)),]
predActModAllCoefs_PEmodel = merge(lmLrY1PE_SumStats, lmLrY2PE_SumStats, by =
                                     c('id'))
predActModAllCoefs_PEmodel <-
  predActModAllCoefs_PEmodel[with(predActModAllCoefs_PEmodel, order(id)),]
predActModAllCoefs_CPPmodel = merge(lmLrY1CPP_SumStats, lmLrY2CPP_SumStats, by =
                                      c('id'))
predActModAllCoefs_CPPmodel <-
  predActModAllCoefs_CPPmodel[with(predActModAllCoefs_CPPmodel, order(id)),]

#combined model####
predActRelReg_All <- data.frame(matrix(NA, 3, 4))
var_indicesActAll <- c("X1.sc", "X1X2.sc", "X1X2X3.sc", "X1X4.sc")
# Loop over the time points
for (i in seq_along(var_indicesActAll)) {
  var_namex <-predActModAllCoefs[[paste0(var_indicesActAll[i],".x")]]
  var_namey <-predActModAllCoefs[[paste0(var_indicesActAll[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predActRelReg_All[i, 1] <- icc_tmp$results[2, 2]
  predActRelReg_All[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predActRelReg_All[i, 4] <-  var_indicesActAll[i]
}

#PE model####
predActRelReg_pe <- data.frame(matrix(NA, 3, 4))
var_indicesActPE <- c("X1.sc", "X1X2X3.sc", "X1X4.sc")
# Loop over the time points
for (i in seq_along(var_indicesActPE)) {
  var_namex <-predActModAllCoefs[[paste0(var_indicesActPE[i],".x")]]
  var_namey <-predActModAllCoefs[[paste0(var_indicesActPE[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predActRelReg_pe[i, 1] <- icc_tmp$results[2, 2]
  predActRelReg_pe[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predActRelReg_pe[i, 4] <-  var_indicesActPE[i]
}

#CPP model####
predActRelReg_CPP <- data.frame(matrix(NA, 3, 4))
var_indicesActCPP <- c("X1.sc", "X1X2X3.sc", "X1X4.sc")
# Loop over the time points
for (i in seq_along(var_indicesActCPP)) {
  var_namex <-predActModAllCoefs[[paste0(var_indicesActCPP[i],".x")]]
  var_namey <-predActModAllCoefs[[paste0(var_indicesActCPP[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predActRelReg_CPP[i, 1] <- icc_tmp$results[2, 2]
  predActRelReg_CPP[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predActRelReg_CPP[i, 4] <-  var_indicesActCPP[i]
}

#For 3) Confidence adjustments regression with normative predictors####
ConfModAllCoefs_PEmodel = merge(lmConf1PE_SumStats, lmConf2PE_SumStats, by =
                                  c('id'))
ConfModAllCoefs_PEmodel <-
  ConfModAllCoefs_PEmodel[with(ConfModAllCoefs_PEmodel, order(id)),]
ConfModAllCoefs_CPPmodel = merge(lmConf1CPP_SumStats, lmConf2CPP_SumStats, by =
                                   c('id'))
ConfModAllCoefs_CPPmodel <-
  ConfModAllCoefs_CPPmodel[with(ConfModAllCoefs_CPPmodel, order(id)),]

#get test-retest reliability
#PEmodel####
predConfRelReg_pe <- data.frame(matrix(NA, 3, 4))
var_indicesConfPE <- c("shiftX1.sc", "shiftX2X3.sc", "shiftX4.sc")
# Loop over the time points
for (i in seq_along(var_indicesConfPE)) {
  var_namex <- ConfModAllCoefs_PEmodel[[paste0(var_indicesConfPE[i],".x")]]
  var_namey <- ConfModAllCoefs_PEmodel[[paste0(var_indicesConfPE[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predConfRelReg_pe[i, 1] <- icc_tmp$results[2, 2]
  predConfRelReg_pe[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predConfRelReg_pe[i, 4] <-  var_indicesConfPE[i]
}

#CPPmodel####
predConfRelReg_CPP <- data.frame(matrix(NA, 3, 4))
var_indicesConfCPP <- c("shiftX2.sc", "shiftX2X3.sc", "shiftX4.sc")
# Loop over the time points
for (i in seq_along(var_indicesConfCPP)) {
  var_namex <- ConfModAllCoefs_CPPmodel[[paste0(var_indicesConfCPP[i],".x")]]
  var_namey <- ConfModAllCoefs_CPPmodel[[paste0(var_indicesConfCPP[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predConfRelReg_CPP[i, 1] <- icc_tmp$results[2, 2]
  predConfRelReg_CPP[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predConfRelReg_CPP[i, 4] <-  var_indicesConfCPP[i]
}

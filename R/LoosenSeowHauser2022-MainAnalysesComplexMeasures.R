################################################################################################################
##Loosen, Seow & Hauser (submitted) - Main Manuscript                                                         ##
##Consistency within change: Evaluating the psychometric properties of a widely-used predictive-inference task##
##Part 2                                                                                                      ##
################################################################################################################

## CLEAR ALL
rm(list=ls())

#Set directory####
setwd("~/Documents/GitHub/Probabilistic-Inference-Task-Analysis/R")

## Loading Libraries####
library(dplyr)
library(ggpubr)
library(psych)
library(broom)
library(tidyr)
options(scipen=999) #set scientific notation in this session
options(max.print=25000) #set max print for e.gg., displaying data frames 

#######################
##Loading datasets####
#######################
#questionnaire data
quest_pt1 = data.frame(read.csv("Table_questPIT_pt1.csv",header = T))
quest_t2 = data.frame(read.csv("Table_questPIT_alltp.csv",header = T))
#game data
Game_pt1 = data.frame(read.csv("Table_gamePIT_pt1.csv",header = T))#dataset t1 quest and predictive inference task
Game_t2 = data.frame(read.csv("Table_gamePIT_t2.csv",header = T))#dataset t1 and t2 combined, quest and predictive inference task

#create odd and even dataset based on trials - before z-scoring to avoid artificial smoothing 
odd_indexes1 <- which(Game_pt1$nTrial %% 2 == 1)# Create row indicator
even_indexes1 <- which(Game_pt1$nTrial %% 2 == 0)       
Game_pt1_odd = Game_pt1[odd_indexes1,]
Game_pt1_even = Game_pt1[even_indexes1,]

odd_indexes2 <- which(Game_t2$nTrial %% 2 == 1)            
even_indexes2 <- which(Game_t2$nTrial %% 2 == 0)            
Game_t2_odd = Game_t2[odd_indexes2,]
Game_t2_even= Game_t2[even_indexes2,]

################
##Functions ####
################
IntCons_r <- function(even_values,odd_values,complete_dataset){#compute internal consistency
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
#scaling the questionnaire scores
quest_t2$total_depr.sc = scale(quest_t2$total_depr) #depression
quest_t2$total_anx.sc = scale(quest_t2$total_anx) #anxiety
quest_t2$total_oc.sc = scale(quest_t2$total_oc) #obsessive compulsive
quest_t2$OCIR_nonCovid.sc = scale(quest_t2$OCIR_nonCovid) #obsessive compulsive without covid-relevant items

#########################
##Prepare task data######
#########################
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
finalGameData_pt1$id <- factor(finalGameData_pt1$id)
finalGame_pt1_odd$conf.sc <-ave(finalGame_pt1_odd$conf,finalGame_pt1_odd$id, FUN=scale) 
finalGame_pt1_odd$id <- factor(finalGame_pt1_odd$id)
finalGame_pt1_even$conf.sc <-ave(finalGame_pt1_even$conf,finalGame_pt1_even$id, FUN=scale)
finalGame_pt1_even$id <- factor(finalGame_pt1_even$id)
finalGameData_pt1$actionUpdateSD.sc <-ave(finalGameData_pt1$actionUpdateSD, finalGameData_pt1$id, FUN=scale)
finalGame_pt1_even$actionUpdateSD.sc <-ave(finalGame_pt1_even$actionUpdateSD,finalGame_pt1_even$id, FUN=scale)
finalGame_pt1_odd$actionUpdateSD.sc <-ave(finalGame_pt1_odd$actionUpdateSD,finalGame_pt1_odd$id, FUN=scale) 

#t2
finalGameData_t2$conf.sc <-ave(finalGameData_t2$conf, finalGameData_t2$id, FUN=scale)
finalGameData_t2$id <- factor(finalGameData_t2$id)
finalGame_t2_odd$conf.sc <-ave(finalGame_t2_odd$conf,finalGame_t2_odd$id, FUN=scale) 
finalGame_t2_odd$id <- factor(finalGame_t2_odd$id)
finalGame_t2_even$conf.sc <-ave(finalGame_t2_even$conf,finalGame_t2_even$id, FUN=scale)
finalGame_t2_even$id <- factor(finalGame_t2_even$id)
finalGameData_t2$actionUpdateSD.sc <-ave(finalGameData_t2$actionUpdateSD,finalGameData_t2$id, FUN=scale)
finalGame_t2_odd$actionUpdateSD.sc <-ave(finalGame_t2_odd$actionUpdateSD,finalGame_t2_odd$id, FUN=scale) 
finalGame_t2_even$actionUpdateSD.sc <-ave(finalGame_t2_even$actionUpdateSD,finalGame_t2_even$id, FUN=scale)

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

###################################
# 1) Action-Confidence Coupling####
###################################
#Regression analyses####
actConf1SumStats= finalGameQuestData_pt1%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD~conf.sc, data =.))) %>%  unnest(fitmod)
actConf1SumStats<-reshape2::dcast(actConf1SumStats, id ~ term, value.var = 'estimate')
##odd vs. even CPs (to later check the robustness)
actConf1_oddSumStats = finalGameQuest_pt1_odd%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf1_oddSumStats<-reshape2::dcast(actConf1_oddSumStats, id ~ term, value.var = 'estimate')
actConf1_evenSumStats = finalGameQuest_pt1_even%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf1_evenSumStats<-reshape2::dcast(actConf1_evenSumStats, id ~ term, value.var = 'estimate')

#t2
actConf2SumStats = finalGameQuestData_t2%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf2SumStats<-reshape2::dcast(actConf2SumStats, id ~ term, value.var = 'estimate')
# odd vs even
actConf2_oddSumStats= finalGameQuest_t2_odd%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf2_oddSumStats<-reshape2::dcast(actConf2_oddSumStats, id ~ term, value.var = 'estimate')
actConf2_evenSumStats= finalGameQuest_t2_even%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf2_evenSumStats<-reshape2::dcast(actConf2_evenSumStats, id ~ term, value.var = 'estimate')

##DV z-scored#
actConf1SumStatsZ= finalGameQuestData_pt1%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD.sc ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf1SumStatsZ<-reshape2::dcast(actConf1SumStatsZ, id ~ term, value.var = 'estimate')
##odd vs. even CPs (to later check the robustness)
actConf1_oddSumStatsZ = finalGameQuest_pt1_odd%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD.sc ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf1_oddSumStatsZ<-reshape2::dcast(actConf1_oddSumStatsZ, id ~ term, value.var = 'estimate')
actConf1_evenSumStatsZ = finalGameQuest_pt1_even%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD.sc ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf1_evenSumStatsZ<-reshape2::dcast(actConf1_evenSumStatsZ, id ~ term, value.var = 'estimate')

#t2
actConf2SumStatsZ = finalGameQuestData_t2%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD.sc ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf2SumStatsZ<-reshape2::dcast(actConf2SumStatsZ, id ~ term, value.var = 'estimate')
# odd vs even
actConf2_oddSumStatsZ= finalGameQuest_t2_odd%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD.sc ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf2_oddSumStatsZ<-reshape2::dcast(actConf2_oddSumStatsZ, id ~ term, value.var = 'estimate')
actConf2_evenSumStatsZ= finalGameQuest_t2_even%>% group_by(id) %>% do(fitmod = tidy(glm(actionUpdateSD.sc ~ conf.sc, data =.))) %>%  unnest(fitmod)
actConf2_evenSumStatsZ<-reshape2::dcast(actConf2_evenSumStatsZ, id ~ term, value.var = 'estimate')

###################################
# 2-3) Link to Bayesian Learner####
###################################
##Exluding criteria (trial-wise)#### 
#1.Exclude trials where estimated learning rate is higher than 99% of all learning rates; 
#2.Exclude trials where the linear/ absolute prediction error is equal to 0.
#Note the odd and even dataset will be re-computed after this adaptation to ensure the shifting of variables is not introducing errors
finalGameQuestData_pt1Temp = finalGameQuestData_pt1[finalGameQuestData_pt1$behav_lrSD<=quantile(finalGameQuestData_pt1$behav_lrSD, 0.99) ,]#note behav_lrSD==abs(behav_lr)
finalGameQuestData_pt1Mod = finalGameQuestData_pt1Temp[finalGameQuestData_pt1Temp$behav_d!=0,]
finalGameQuestData_pt1Mod <- finalGameQuestData_pt1Mod[with(finalGameQuestData_pt1Mod, order(id, nTrial)), ] 
#t2
finalGameQuestData_t2Temp = finalGameQuestData_t2[finalGameQuestData_t2$behav_lrSD<= quantile(finalGameQuestData_t2$behav_lrSD, 0.99) ,]
finalGameQuestData_t2Mod = finalGameQuestData_t2Temp[finalGameQuestData_t2Temp$behav_d!=0,]
finalGameQuestData_t2Mod <- finalGameQuestData_t2Mod [with(finalGameQuestData_t2Mod, order(id, nTrial)), ] 

##Preparing normative factor regressors for the confidence regression#### 
finalGameQuestData_pt1Mod$X1 <- abs(finalGameQuestData_pt1Mod$behav_d)#PE
finalGameQuestData_pt1Mod$X2 <- finalGameQuestData_pt1Mod$bayes_CPP#CPP
finalGameQuestData_pt1Mod$X3 <- finalGameQuestData_pt1Mod$bayes_RU#RU
finalGameQuestData_pt1Mod$X4 <- finalGameQuestData_pt1Mod$hitMiss#caught the particle (hit) or not (miss)
#t2
finalGameQuestData_t2Mod$X1 <- abs(finalGameQuestData_t2Mod$behav_d)
finalGameQuestData_t2Mod$X2 <- finalGameQuestData_t2Mod$bayes_CPP
finalGameQuestData_t2Mod$X3 <- finalGameQuestData_t2Mod$bayes_RU
finalGameQuestData_t2Mod$X4 <- finalGameQuestData_t2Mod$hitMiss 

####shift normative predictors for the confidence regression models####
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

#RU
finalGameQuestData_pt1Mod$shiftX3 <- c(0, finalGameQuestData_pt1Mod$X3[-nrow(finalGameQuestData_pt1Mod)]) #The first line adds a string of lagged (+1) observations #basically lagging it by one trial because its not an "update"
finalGameQuestData_pt1Mod$shiftX3[which(!duplicated(finalGameQuestData_pt1Mod$id))] <- 0 #second string corrects the first entry of each group, as the lagged observation is from previous group.
#t2
finalGameQuestData_t2Mod$shiftX3 <- c(0, finalGameQuestData_t2Mod$X3[-nrow(finalGameQuestData_t2Mod)]) 
finalGameQuestData_t2Mod$shiftX3[which(!duplicated(finalGameQuestData_t2Mod$id))] <- 0 

#hitMiss
finalGameQuestData_pt1Mod$shiftX4 <- c(0, finalGameQuestData_pt1Mod$X4[-nrow(finalGameQuestData_pt1Mod)]) #The first line adds a string of lagged (+1) observations #basically lagging it by one trial
finalGameQuestData_pt1Mod$shiftX4[which(!duplicated(finalGameQuestData_pt1Mod$id))] <- 0 #second string corrects the first entry of each group, as the lagged observation is from previous group.
#t2
finalGameQuestData_t2Mod$shiftX4 <- c(0, finalGameQuestData_t2Mod$X4[-nrow(finalGameQuestData_t2Mod)]) 
finalGameQuestData_t2Mod$shiftX4[which(!duplicated(finalGameQuestData_t2Mod$id))] <- 0 

#Note: we have to create new odd and even datasets after the shifting of the variables
odd_indexesMod1 <- which(finalGameQuestData_pt1Mod$nTrial %% 2 == 1)            
even_indexesMod1 <- which(finalGameQuestData_pt1Mod$nTrial %% 2 == 0)           

finalGameQuest_pt1_oddMod = finalGameQuestData_pt1Mod[odd_indexesMod1,]
finalGameQuest_pt1_evenMod = finalGameQuestData_pt1Mod[even_indexesMod1,]

odd_indexesMod2 <- which(finalGameQuestData_t2Mod$nTrial %% 2 == 1)            
even_indexesMod2 <- which(finalGameQuestData_t2Mod$nTrial %% 2 == 0)          
finalGameQuest_t2_oddMod = finalGameQuestData_t2Mod[odd_indexesMod2,]
finalGameQuest_t2_evenMod = finalGameQuestData_t2Mod[even_indexesMod2,]

#Scaling the regressors within participants####
columns_to_scale <- c("shiftX1", "shiftX2", "shiftX2X3", "shiftX3", "shiftX4")

finalGameQuestData_pt1Mod <- finalGameQuestData_pt1Mod %>%
  group_by(id) %>%
  mutate(across(all_of(columns_to_scale), scale, .names = "{.col}.sc"))


finalGameQuestData_pt1Mod$shiftX1.sc <- ave(finalGameQuestData_pt1Mod$shiftX1, finalGameQuestData_pt1Mod$id, FUN=scale)  
finalGameQuestData_pt1Mod$shiftX2.sc <- ave(finalGameQuestData_pt1Mod$shiftX2, finalGameQuestData_pt1Mod$id, FUN=scale)  
finalGameQuestData_pt1Mod$shiftX2X3.sc <- ave(finalGameQuestData_pt1Mod$shiftX2X3, finalGameQuestData_pt1Mod$id, FUN=scale)  
finalGameQuestData_pt1Mod$shiftX3.sc <- ave(finalGameQuestData_pt1Mod$shiftX3, finalGameQuestData_pt1Mod$id, FUN=scale)  
finalGameQuestData_pt1Mod$shiftX4.sc <- ave(finalGameQuestData_pt1Mod$shiftX4, finalGameQuestData_pt1Mod$id, FUN=scale) 
##odd vs. even CPs
finalGameQuest_pt1_oddMod$shiftX1.sc <- ave(finalGameQuest_pt1_oddMod$shiftX1, finalGameQuest_pt1_oddMod$id, FUN=scale)  
finalGameQuest_pt1_oddMod$shiftX2.sc <- ave(finalGameQuest_pt1_oddMod$shiftX2, finalGameQuest_pt1_oddMod$id, FUN=scale)  
finalGameQuest_pt1_oddMod$shiftX3.sc <- ave(finalGameQuest_pt1_oddMod$shiftX3, finalGameQuest_pt1_oddMod$id, FUN=scale)  
finalGameQuest_pt1_oddMod$shiftX4.sc <- ave(finalGameQuest_pt1_oddMod$shiftX4, finalGameQuest_pt1_oddMod$id, FUN=scale) 
finalGameQuest_pt1_evenMod$shiftX1.sc <- ave(finalGameQuest_pt1_evenMod$shiftX1, finalGameQuest_pt1_evenMod$id, FUN=scale)  
finalGameQuest_pt1_evenMod$shiftX2.sc <- ave(finalGameQuest_pt1_evenMod$shiftX2, finalGameQuest_pt1_evenMod$id, FUN=scale)  
finalGameQuest_pt1_evenMod$shiftX3.sc <- ave(finalGameQuest_pt1_evenMod$shiftX3, finalGameQuest_pt1_evenMod$id, FUN=scale)  
finalGameQuest_pt1_evenMod$shiftX4.sc <- ave(finalGameQuest_pt1_evenMod$shiftX4, finalGameQuest_pt1_evenMod$id, FUN=scale) 
#t2
finalGameQuestData_t2Mod$shiftX1.sc <- ave(finalGameQuestData_t2Mod$shiftX1, finalGameQuestData_t2Mod$id, FUN=scale)
finalGameQuestData_t2Mod$shiftX2.sc <- ave(finalGameQuestData_t2Mod$shiftX2, finalGameQuestData_t2Mod$id, FUN=scale)
finalGameQuestData_t2Mod$shiftX3.sc <- ave(finalGameQuestData_t2Mod$shiftX3, finalGameQuestData_t2Mod$id, FUN=scale)
finalGameQuestData_t2Mod$shiftX4.sc <- ave(finalGameQuestData_t2Mod$shiftX4, finalGameQuestData_t2Mod$id, FUN=scale)
##odd vs. even CPs
finalGameQuest_t2_oddMod$shiftX1.sc <- ave(finalGameQuest_t2_oddMod$shiftX1, finalGameQuest_t2_oddMod$id, FUN=scale)
finalGameQuest_t2_oddMod$shiftX2.sc <- ave(finalGameQuest_t2_oddMod$shiftX2, finalGameQuest_t2_oddMod$id, FUN=scale)
finalGameQuest_t2_oddMod$shiftX3.sc <- ave(finalGameQuest_t2_oddMod$shiftX3, finalGameQuest_t2_evenMod$id, FUN=scale)
finalGameQuest_t2_oddMod$shiftX4.sc <- ave(finalGameQuest_t2_oddMod$shiftX4, finalGameQuest_t2_oddMod$id, FUN=scale)
finalGameQuest_t2_evenMod$shiftX1.sc <- ave(finalGameQuest_t2_evenMod$shiftX1, finalGameQuest_t2_evenMod$id, FUN=scale)
finalGameQuest_t2_evenMod$shiftX2.sc <- ave(finalGameQuest_t2_evenMod$shiftX2, finalGameQuest_t2_evenMod$id, FUN=scale)
finalGameQuest_t2_evenMod$shiftX3.sc <- ave(finalGameQuest_t2_evenMod$shiftX3, finalGameQuest_t2_evenMod$id, FUN=scale)
finalGameQuest_t2_evenMod$shiftX4.sc <- ave(finalGameQuest_t2_evenMod$shiftX4, finalGameQuest_t2_evenMod$id, FUN=scale)

#save datasets for the circular regression in Matlab (i.e.,"SignedPECircularRegressions")- run it now!
write.csv(finalGameQuestData_pt1Mod,"finalGameQuestData_pt1Mod.csv")
write.csv(finalGameQuest_pt1_oddMod,"finalGameQuest_pt1_oddMod.csv")
write.csv(finalGameQuest_pt1_evenMod,"finalGameQuest_pt1_evenMod.csv")

write.csv(finalGameQuestData_t2Mod,"finalGameQuestData_t2Mod.csv")
write.csv(finalGameQuest_t2_oddMod,"finalGameQuest_t2_oddMod.csv")
write.csv(finalGameQuest_t2_evenMod,"finalGameQuest_t2_evenMod.csv")

#Confidence: 2) Regression analyses with demographics and normative regressors####
lmConf1All_SumStats= finalGameQuestData_pt1Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX3.sc+shiftX4.sc,data =.))) %>%  unnest(fitmod)
lmConf1All_SumStats<-reshape2::dcast(lmConf1All_SumStats, id ~ term, value.var = 'estimate')

lmConf2All_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX3.sc+shiftX4.sc,data =.))) %>%  unnest(fitmod)
lmConf2All_SumStats<-reshape2::dcast(lmConf2All_SumStats, id ~ term, value.var = 'estimate')

lmConf1CPP_SumStats= finalGameQuestData_pt1Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX3.sc+shiftX4.sc,data =.))) %>%  unnest(fitmod)
lmConf1CPP_SumStats<-reshape2::dcast(lmConf1CPP_SumStats, id ~ term, value.var = 'estimate')

lmConf1PE_SumStats= finalGameQuestData_pt1Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX3.sc+shiftX4.sc,data =.))) %>%  unnest(fitmod)
lmConf1PE_SumStats<-reshape2::dcast(lmConf1PE_SumStats, id ~ term, value.var = 'estimate')

#odd vs. even CPs
lmConf1odd_SumStats= finalGameQuest_pt1_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+ shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1odd_SumStats<-reshape2::dcast(lmConf1odd_SumStats, id ~ term, value.var = 'estimate')

lmConf1even_SumStats= finalGameQuest_pt1_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+ shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1even_SumStats<-reshape2::dcast(lmConf1even_SumStats, id ~ term, value.var = 'estimate')

lmConf1oddPE_SumStats= finalGameQuest_pt1_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1oddPE_SumStats<-reshape2::dcast(lmConf1oddPE_SumStats, id ~ term, value.var = 'estimate')

lmConf1evenPE_SumStats= finalGameQuest_pt1_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1evenPE_SumStats<-reshape2::dcast(lmConf1evenPE_SumStats, id ~ term, value.var = 'estimate')

lmConf1oddCPP_SumStats= finalGameQuest_pt1_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1oddCPP_SumStats<-reshape2::dcast(lmConf1oddCPP_SumStats, id ~ term, value.var = 'estimate')

lmConf1evenCPP_SumStats= finalGameQuest_pt1_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf1evenCPP_SumStats<-reshape2::dcast(lmConf1evenCPP_SumStats, id ~ term, value.var = 'estimate')

#t2
lmConf2_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2_SumStats<-reshape2::dcast(lmConf2_SumStats, id ~ term, value.var = 'estimate')

lmConf2CPP_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2CPP_SumStats<-reshape2::dcast(lmConf2CPP_SumStats, id ~ term, value.var = 'estimate')

lmConf2PE_SumStats= finalGameQuestData_t2Mod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2PE_SumStats<-reshape2::dcast(lmConf2PE_SumStats, id ~ term, value.var = 'estimate')

#odd vs. even CPs
lmConf2odd_SumStats= finalGameQuest_t2_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2odd_SumStats<-reshape2::dcast(lmConf2odd_SumStats, id ~ term, value.var = 'estimate')

lmConf2even_SumStats= finalGameQuest_t2_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2even_SumStats<-reshape2::dcast(lmConf2even_SumStats, id ~ term, value.var = 'estimate')

lmConf2oddPE_SumStats= finalGameQuest_t2_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2oddPE_SumStats<-reshape2::dcast(lmConf2oddPE_SumStats, id ~ term, value.var = 'estimate')

lmConf2evenPE_SumStats= finalGameQuest_t2_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX1.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2evenPE_SumStats<-reshape2::dcast(lmConf2evenPE_SumStats, id ~ term, value.var = 'estimate')

lmConf2oddCPP_SumStats= finalGameQuest_t2_oddMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2oddCPP_SumStats<-reshape2::dcast(lmConf2oddCPP_SumStats, id ~ term, value.var = 'estimate')

lmConf2evenCPP_SumStats= finalGameQuest_t2_evenMod%>% group_by(id) %>% do(fitmod = tidy(glm(conf.sc ~ shiftX2.sc+shiftX3.sc+shiftX4.sc, data =.))) %>%  unnest(fitmod)
lmConf2evenCPP_SumStats<-reshape2::dcast(lmConf2evenCPP_SumStats, id ~ term, value.var = 'estimate')

#Action: 3) Regression analyses with demographics and normative regressors####
##Loading data gained from circular model implemented in Matlab (cf. above and README file)####
circBetas_pt1 = data.frame(read.csv("CircularBetas_t1.csv",header = T))
circBetas_t2 = data.frame(read.csv("CircularBetas_t2.csv",header = T))
#oddeven
circBetasOdd_pt1= data.frame(read.csv('CircularBetasOdd_t1.csv',header = T))
circBetasOdd_t2= data.frame(read.csv('CircularBetasOdd_t2.csv',header = T))
circBetasEven_pt1= data.frame(read.csv('CircularBetasEven_t1.csv',header = T))
circBetasEven_t2= data.frame(read.csv('CircularBetasEven_t2.csv',header = T))

##split models
circBetasPE_pt1 = data.frame(read.csv("CircularBetasPE_t1.csv",header = T))
circBetasPE_t2 = data.frame(read.csv("CircularBetasPE_t2.csv",header = T))
circBetasCPP_pt1 = data.frame(read.csv("CircularBetasCPP_t1.csv",header = T))
circBetasCPP_t2 = data.frame(read.csv("CircularBetasCPP_t2.csv",header = T))

#oddeven
circBetasOddPE_pt1= data.frame(read.csv('CircularBetasOddPE_t1.csv',header = T))
circBetasOddPE_t2= data.frame(read.csv('CircularBetasOddPE_t2.csv',header = T))
circBetasEvenPE_pt1= data.frame(read.csv('CircularBetasEvenPE_t1.csv',header = T))
circBetasEvenPE_t2= data.frame(read.csv('CircularBetasEvenPE_t2.csv',header = T))

circBetasOddCPP_pt1= data.frame(read.csv('CircularBetasOddCPP_t1.csv',header = T))
circBetasOddCPP_t2= data.frame(read.csv('CircularBetasOddCPP_t2.csv',header = T))
circBetasEvenCPP_pt1= data.frame(read.csv('CircularBetasEvenCPP_t1.csv',header = T))
circBetasEvenCPP_t2= data.frame(read.csv('CircularBetasEvenCPP_t2.csv',header = T))

##Investigate the psychometric properties of the regression coefficients######
##Internal-consistency####
#For 1) Action-Confidence Coupling Regression####
#merge
actConfModAllCoefs1_oddeven = merge(actConf1_oddSumStats,actConf1_evenSumStats,by=c('id'))
actConfModAllCoefs1_oddeven <- actConfModAllCoefs1_oddeven[with(actConfModAllCoefs1_oddeven, order(id)), ]
#get internal consistency
r_actoddeven.corrected <- IntCons_r(actConfModAllCoefs1_oddeven$conf.sc.x,actConfModAllCoefs1_oddeven$conf.sc.y,actConfModAllCoefs1_oddeven)[1,1]
ci.actoddeven <- IntCons_r(actConfModAllCoefs1_oddeven$conf.sc.x,actConfModAllCoefs1_oddeven$conf.sc.y,actConfModAllCoefs1_oddeven)[,2]
#t2####
#merge
actConfModAllCoefs2_oddeven = merge(actConf2_oddSumStats,actConf2_evenSumStats,by=c('id'))
actConfModAllCoefs2_oddeven <- actConfModAllCoefs2_oddeven[with(actConfModAllCoefs2_oddeven, order(id)), ]
#get internal consistency
r_actoddeven2.corrected <- IntCons_r(actConfModAllCoefs2_oddeven$conf.sc.x,actConfModAllCoefs2_oddeven$conf.sc.y,actConfModAllCoefs2_oddeven)[1,1]
ci.actoddeven2 <- IntCons_r(actConfModAllCoefs2_oddeven$conf.sc.x,actConfModAllCoefs2_oddeven$conf.sc.y,actConfModAllCoefs2_oddeven)[,2]

##DV z-scored #
#merge
actConfModAllCoefs1_oddevenZ = merge(actConf1_oddSumStatsZ,actConf1_evenSumStatsZ,by=c('id'))
actConfModAllCoefs1_oddevenZ <- actConfModAllCoefs1_oddevenZ[with(actConfModAllCoefs1_oddevenZ, order(id)), ]
#get internal consistency
r_actoddeven.correctedZ <- IntCons_r(actConfModAllCoefs1_oddevenZ$conf.sc.x,actConfModAllCoefs1_oddevenZ$conf.sc.y,actConfModAllCoefs1_oddevenZ)[1,1]
ci.actoddevenZ <- IntCons_r(actConfModAllCoefs1_oddevenZ$conf.sc.x,actConfModAllCoefs1_oddevenZ$conf.sc.y,actConfModAllCoefs1_oddevenZ)[,2]
#t2####
#merge
actConfModAllCoefs2_oddevenZ = merge(actConf2_oddSumStatsZ,actConf2_evenSumStatsZ,by=c('id'))
actConfModAllCoefs2_oddevenZ <- actConfModAllCoefs2_oddevenZ[with(actConfModAllCoefs2_oddevenZ, order(id)), ]
#get internal consistency
r_actoddeven2.correctedZ <- IntCons_r(actConfModAllCoefs2_oddevenZ$conf.sc.x,actConfModAllCoefs2_oddevenZ$conf.sc.y,actConfModAllCoefs2_oddevenZ)[1,1]
ci.actoddeven2Z <- IntCons_r(actConfModAllCoefs2_oddevenZ$conf.sc.x,actConfModAllCoefs2_oddevenZ$conf.sc.y,actConfModAllCoefs2_oddevenZ)[,2]

# For 2) Action adjustments regression with normative predictors####
#full model####
circBetasOddEven = merge(circBetasOdd_pt1,circBetasEven_pt1,by=c('id'))
circBetasOddEven <- circBetasOddEven[with(circBetasOddEven, order(id)), ]

var_indices <- c("PE", "CPP", "RU", "Hit") # Modify with your desired indices
r_circActFull <- matrix(NA, nrow = length(var_indices), ncol = 2)
ci.circActFull <- matrix(NA, nrow = length(var_indices), ncol = 3)

for (i in seq_along(var_indices)) {
  var_index <- var_indices[i]
  xActFull <- circBetasOddEven[[paste0(var_index,".x")]]
  yActFull <- circBetasOddEven[[paste0(var_index,".y")]]
  intCons <- IntCons_r(xActFull, yActFull, circBetasOddEven)
  r_circActFull[i,1] <- intCons[1, 1]
  ci.circActFull[i,1:2 ] <- intCons[, 2]
  ci.circActFull[i,3 ] <- var_indices[i]
  r_circActFull[i,2] <- var_indices[i]
}

#t2#
circBetasOddEven_t2 = merge(circBetasOdd_t2,circBetasEven_t2,by=c('id'))
circBetasOddEven_t2 <- circBetasOddEven_t2[with(circBetasOddEven_t2, order(id)), ]

r_circActFull_t2 <- matrix(NA, nrow = length(var_indices), ncol = 2)
ci.circActFull_t2 <- matrix(NA, nrow = length(var_indices), ncol = 3)

for (i in seq_along(var_indices)) {
  var_index2 <- var_indices[i]
  xActFull2 <- circBetasOddEven_t2[[paste0(var_index2,".x")]]
  yActFull2 <- circBetasOddEven_t2[[paste0(var_index2,".y")]]
  intCons2 <- IntCons_r(xActFull2, yActFull2, circBetasOddEven_t2)
  r_circActFull_t2[i,1] <- intCons2[1, 1]
  ci.circActFull_t2[i,1:2 ] <- intCons2[, 2]
  r_circActFull_t2[i,2] <- var_indices[i]
  ci.circActFull_t2[i,3 ] <- var_indices[i]
  
}

#PE model####
circBetasOddEven_PE = merge(circBetasOddPE_pt1,circBetasEvenPE_pt1,by=c('id'))
circBetasOddEven_PE <- circBetasOddEven_PE[with(circBetasOddEven_PE, order(id)), ]

var_indicesPE <- c("PE", "RU", "Hit") # Modify with your desired indices
r_circAct_PEmodel <- matrix(NA, nrow = length(var_indicesPE), ncol = 1)
ci.circAct_PEmodel <- matrix(NA, nrow = length(var_indicesPE), ncol = 2)

for (i in seq_along(var_indicesPE)) {
  var_indexPE <- var_indicesPE[i]
  xActFullPE <- circBetasOddEven_PE[[paste0(var_indexPE,".x")]]
  yActFullPE <- circBetasOddEven_PE[[paste0(var_indexPE,".y")]]
  intConsPE <- IntCons_r(xActFullPE, yActFullPE, circBetasOddEven_PE)
  r_circAct_PEmodel[i] <- intConsPE[1, 1]
  ci.circAct_PEmodel[i, ] <- intConsPE[, 2]
}

#t2
circBetasOddEven_PE2 = merge(circBetasOddPE_t2,circBetasEvenPE_t2,by=c('id'))
circBetasOddEven_PE2 <- circBetasOddEven_PE2[with(circBetasOddEven_PE2, order(id)), ]

var_indicesPE2 <- c("PE", "RU", "Hit") # Modify with your desired indices
r_circAct_PEmodel2 <- matrix(NA, nrow = length(var_indicesPE2), ncol = 1)
ci.circAct_PEmodel2 <- matrix(NA, nrow = length(var_indicesPE2), ncol = 2)

for (i in seq_along(var_indicesPE2)) {
  var_indexPE2 <- var_indicesPE2[i]
  xActFullPE2 <- circBetasOddEven_PE2[[paste0(var_indexPE2,".x")]]
  yActFullPE2 <- circBetasOddEven_PE2[[paste0(var_indexPE2,".y")]]
  intConsPE2 <- IntCons_r(xActFullPE2, yActFullPE2, circBetasOddEven_PE2)
  r_circAct_PEmodel2[i] <- intConsPE2[1, 1]
  ci.circAct_PEmodel2[i, ] <- intConsPE2[, 2]
}

#CPP model####
#merge
circBetasOddEven_CPP = merge(circBetasOddCPP_pt1,circBetasEvenCPP_pt1,by=c('id'))
circBetasOddEven_CPP <- circBetasOddEven_CPP[with(circBetasOddEven_CPP, order(id)), ]

var_indicesCPP <- c("CPP", "RU", "Hit") # Modify with your desired indices
r_circAct_CPPmodel <- matrix(NA, nrow = length(var_indicesCPP), ncol = 1)
ci.circAct_CPPmodel <- matrix(NA, nrow = length(var_indicesCPP), ncol = 2)

for (i in seq_along(var_indicesCPP)) {
  var_indexCPP <- var_indicesCPP[i]
  xActFullCPP <- circBetasOddEven_CPP[[paste0(var_indexCPP,".x")]]
  yActFullCPP <- circBetasOddEven_CPP[[paste0(var_indexCPP,".y")]]
  intConsCPP <- IntCons_r(xActFullCPP, yActFullCPP, circBetasOddEven_CPP)
  r_circAct_CPPmodel[i] <- intConsCPP[1, 1]
  ci.circAct_CPPmodel[i, ] <- intConsCPP[, 2]
}

#t2
circBetasOddEven_CPP2 = merge(circBetasOddCPP_t2,circBetasEvenCPP_t2,by=c('id'))
circBetasOddEven_CPP2 <- circBetasOddEven_CPP2[with(circBetasOddEven_CPP2, order(id)), ]

var_indicesCPP2 <- c("CPP", "RU", "Hit") # Modify with your desired indices
r_circAct_CPPmodel2 <- matrix(NA, nrow = length(var_indicesCPP2), ncol = 1)
ci.circAct_CPPmodel2 <- matrix(NA, nrow = length(var_indicesCPP2), ncol = 2)

for (i in seq_along(var_indicesCPP2)) {
  var_indexCPP2 <- var_indicesCPP2[i]
  xActFullCPP2 <- circBetasOddEven_CPP2[[paste0(var_indexCPP2,".x")]]
  yActFullCPP2 <- circBetasOddEven_CPP2[[paste0(var_indexCPP2,".y")]]
  intConsCPP2 <- IntCons_r(xActFullCPP2, yActFullCPP2, circBetasOddEven_CPP2)
  r_circAct_CPPmodel2[i] <- intConsCPP2[1, 1]
  ci.circAct_CPPmodel2[i, ] <- intConsCPP2[, 2]
}

# For 3) Confidence adjustments regression with normative predictors####
#Fullmodel####
#merge
lmConfModAllCoefs1_oddeven = merge(lmConf1odd_SumStats,lmConf1even_SumStats,by=c('id'))
lmConfModAllCoefs1_oddeven <- lmConfModAllCoefs1_oddeven[with(lmConfModAllCoefs1_oddeven, order(id)), ]

var_indicesAll <- c("shiftX1.sc", "shiftX2.sc", "shiftX3.sc","shiftX4.sc") # Modify with your desired indices
r_ConfModAllmodel <- matrix(NA, nrow = length(var_indicesAll), ncol = 2)
ci.ConfModAllmodel <- matrix(NA, nrow = length(var_indicesAll), ncol = 3)

for (i in seq_along(var_indicesAll)) {
  var_indexCPP <- var_indicesAll[i]
  xConfModAll <- lmConfModAllCoefs1_oddeven[[paste0(var_indexCPP,".x")]]
  yConfModAll <- lmConfModAllCoefs1_oddeven[[paste0(var_indexCPP,".y")]]
  intConsAll <- IntCons_r(xConfModAll, yConfModAll, lmConfModAllCoefs1_oddeven)
  r_ConfModAllmodel[i,1] <- intConsAll[1, 1]
  ci.ConfModAllmodel[i,1:2] <- intConsAll[, 2]
  r_ConfModAllmodel[i,2] <- var_indicesAll[i]
  ci.ConfModAllmodel[i,3 ] <- var_indicesAll[i]
}

#t2
lmConfModAllCoefs2_oddeven = merge(lmConf2odd_SumStats,lmConf2even_SumStats,by=c('id'))
lmConfModAllCoefs2_oddeven <- lmConfModAllCoefs2_oddeven[with(lmConfModAllCoefs2_oddeven, order(id)), ]
#get internal consistency
var_indicesAll2 <- c("shiftX1.sc", "shiftX2.sc", "shiftX3.sc","shiftX4.sc") # Modify with your desired indices
r_ConfModAllmodel2 <- matrix(NA, nrow = length(var_indicesAll2), ncol = 2)
ci.ConfModAllmodel2<- matrix(NA, nrow = length(var_indicesAll2), ncol = 3)

for (i in seq_along(var_indicesAll2)) {
  var_indexAll2 <- var_indicesAll2[i]
  xConfModAll2 <- lmConfModAllCoefs2_oddeven[[paste0(var_indexAll2,".x")]]
  yConfModAll2 <- lmConfModAllCoefs2_oddeven[[paste0(var_indexAll2,".y")]]
  intConsAll2 <- IntCons_r(xConfModAll2, yConfModAll2, lmConfModAllCoefs2_oddeven)
  r_ConfModAllmodel2[i,1] <- intConsAll2[1, 1]
  ci.ConfModAllmodel2[i,1:2 ] <- intConsAll2[, 2]
  r_ConfModAllmodel2[i,2] <- var_indicesAll2[i]
  ci.ConfModAllmodel2[i,3] <- var_indicesAll2[i]
}

#PEmodel####
lmConfModAllCoefs1_oddevenPE = merge(lmConf1oddPE_SumStats,lmConf1evenPE_SumStats,by=c('id'))
lmConfModAllCoefs1_oddevenPE <- lmConfModAllCoefs1_oddevenPE[with(lmConfModAllCoefs1_oddevenPE, order(id)), ]
#get internal consistency
var_indicesPE <- c("shiftX1.sc", "shiftX3.sc","shiftX4.sc") # Modify with your desired indices
r_ConfModPEmodel <- matrix(NA, nrow = length(var_indicesPE), ncol = 2)
ci.ConfModPEmodel <- matrix(NA, nrow = length(var_indicesPE), ncol = 3)

for (i in seq_along(var_indicesPE)) {
  var_indexPE <- var_indicesPE[i]
  xConfModPE <- lmConfModAllCoefs1_oddevenPE[[paste0(var_indexPE,".x")]]
  yConfModPE <- lmConfModAllCoefs1_oddevenPE[[paste0(var_indexPE,".y")]]
  intConsCPP <- IntCons_r(xConfModPE, yConfModPE, lmConfModAllCoefs1_oddevenPE )
  r_ConfModPEmodel[i,1] <- intConsCPP[1, 1]
  ci.ConfModPEmodel[i,1:2 ] <- intConsCPP[, 2]
  r_ConfModPEmodel[i,2]<- var_indicesPE[i]
  ci.ConfModPEmodel[i,3 ]<- var_indicesPE[i]
}

#t2
lmConfModAllCoefs2_oddevenPE = merge(lmConf2oddPE_SumStats,lmConf2evenPE_SumStats,by=c('id'))
lmConfModAllCoefs2_oddevenPE <- lmConfModAllCoefs2_oddevenPE[with(lmConfModAllCoefs2_oddevenPE, order(id)), ]
#get internal consistency
var_indicesPE2 <- c("shiftX1.sc", "shiftX3.sc","shiftX4.sc") # Modify with your desired indices
r_ConfModPEmodel2 <- matrix(NA, nrow = length(var_indicesPE2), ncol = 2)
ci.ConfModPEmodel2 <- matrix(NA, nrow = length(var_indicesPE2), ncol = 3)

for (i in seq_along(var_indicesPE2)) {
  var_indexPE2 <- var_indicesPE2[i]
  xConfModPE2 <- lmConfModAllCoefs2_oddevenPE[[paste0(var_indexPE2,".x")]]
  yConfModPE2 <- lmConfModAllCoefs2_oddevenPE[[paste0(var_indexPE2,".y")]]
  intConsPE2 <- IntCons_r(xConfModPE2, yConfModPE2, lmConfModAllCoefs2_oddevenPE)
  r_ConfModPEmodel2[i,1] <- intConsPE2[1, 1]
  ci.ConfModPEmodel2[i,1:2] <- intConsPE2[, 2]
  r_ConfModPEmodel2[i,2] <- var_indicesPE2[i]
  ci.ConfModPEmodel2[i,3] <- var_indicesPE2[i]
}

#CPPmodel####
#merge
lmConfModAllCoefs1_oddevenCPP = merge(lmConf1oddCPP_SumStats,lmConf1evenCPP_SumStats,by=c('id'))
lmConfModAllCoefs1_oddevenCPP <- lmConfModAllCoefs1_oddevenCPP[with(lmConfModAllCoefs1_oddevenCPP, order(id)), ]
#get internal consistency
var_indicesCPP <- c("shiftX2.sc", "shiftX3.sc","shiftX4.sc") # Modify with your desired indices
r_ConfModCPPmodel <- matrix(NA, nrow = length(var_indicesCPP), ncol = 2)
ci.ConfModCPPmodel <- matrix(NA, nrow = length(var_indicesCPP), ncol = 3)

for (i in seq_along(var_indicesCPP)) {
  var_indexCPP <- var_indicesCPP[i]
  xConfModCPP <- lmConfModAllCoefs1_oddevenCPP [[paste0(var_indexCPP,".x")]]
  yConfModCPP <- lmConfModAllCoefs1_oddevenCPP [[paste0(var_indexCPP,".y")]]
  intConsCPP <- IntCons_r(xConfModCPP, yConfModCPP, lmConfModAllCoefs1_oddevenCPP )
  r_ConfModCPPmodel[i,1] <- intConsCPP[1, 1]
  ci.ConfModCPPmodel[i,1:2] <- intConsCPP[, 2]
  r_ConfModCPPmodel[i,2] <-  var_indicesCPP[i]
  ci.ConfModCPPmodel[i,3] <-  var_indicesCPP[i]
}

#t2
lmConfModAllCoefs2_oddevenCPP = merge(lmConf2oddCPP_SumStats,lmConf2evenCPP_SumStats,by=c('id'))
lmConfModAllCoefs2_oddevenCPP <- lmConfModAllCoefs2_oddevenCPP[with(lmConfModAllCoefs2_oddevenCPP, order(id)), ]
#get internal consistency
var_indicesCPP2 <- c("shiftX2.sc", "shiftX3.sc","shiftX4.sc") # Modify with your desired indices
r_ConfModCPPmodel2 <- matrix(NA, nrow = length(var_indicesCPP2), ncol = 2)
ci.ConfModCPPmodel2 <- matrix(NA, nrow = length(var_indicesCPP2), ncol = 3)

for (i in seq_along(var_indicesCPP2)) {
  var_indexCPP2 <- var_indicesCPP2[i]
  xConfModCPP2 <- lmConfModAllCoefs2_oddevenCPP[[paste0(var_indexCPP2,".x")]]
  yConfModCPP2 <- lmConfModAllCoefs2_oddevenCPP[[paste0(var_indexCPP2,".y")]]
  intConsCPP2 <- IntCons_r(xConfModCPP2, yConfModCPP2, lmConfModAllCoefs2_oddevenCPP)
  r_ConfModCPPmodel2[i,1] <- intConsCPP2[1, 1]
  ci.ConfModCPPmodel2[i,1:2] <- intConsCPP2[, 2]
  r_ConfModCPPmodel2[i,2] <- var_indicesCPP2[i]
  ci.ConfModCPPmodel2[i,3] <- var_indicesCPP2[i]
}

##Test-retest reliability####
# For 1) Action-Confidence Coupling Regression ####
actConfModAllCoefs = merge(actConf1SumStats,actConf2SumStats,by=c('id'))
actConfModAllCoefs <- actConfModAllCoefs[with(actConfModAllCoefs, order(id)), ]

actConfModRelReg_conf <- matrix(NA,1,3)
actConfModRelReg_conf_tmp<- psych::ICC(x = cbind(actConfModAllCoefs$conf.sc.x, actConfModAllCoefs$conf.sc.y),alpha=.05,lmer=TRUE)#no variance between
actConfModRelReg_conf[1,1] <- actConfModRelReg_conf_tmp$results[2,2]
actConfModRelReg_conf[1,c(2:3)] <- actConfModRelReg_conf_tmp$results[2,c(7,8)]

##DV z-scored#
actConfModAllCoefsZ = merge(actConf1SumStatsZ,actConf2SumStatsZ,by=c('id'))
actConfModAllCoefsZ <- actConfModAllCoefsZ[with(actConfModAllCoefsZ, order(id)), ]

actConfModRelReg_confZ <- matrix(NA,1,3)
actConfModRelReg_confZ_tmp<- psych::ICC(x = cbind(actConfModAllCoefsZ$conf.sc.x, actConfModAllCoefsZ$conf.sc.y),alpha=.05,lmer=TRUE)#no variance between
actConfModRelReg_confZ[1,1] <- actConfModRelReg_confZ_tmp$results[2,2]
actConfModRelReg_confZ[1,c(2:3)] <- actConfModRelReg_confZ_tmp$results[2,c(7,8)]

# For 2) Confidence adjustments regression with normative predictors####
#merge the timepoints into one dataset
ConfModAllCoefs_PEmodel = merge(lmConf1PE_SumStats,lmConf2PE_SumStats,by=c('id'))
ConfModAllCoefs_PEmodel <- ConfModAllCoefs_PEmodel[with(ConfModAllCoefs_PEmodel, order(id)), ]

ConfModAllCoefs_CPPmodel = merge(lmConf1CPP_SumStats,lmConf2CPP_SumStats,by=c('id'))
ConfModAllCoefs_CPPmodel <- ConfModAllCoefs_CPPmodel[with(ConfModAllCoefs_CPPmodel, order(id)), ]

ConfModAllCoefs_Allmodel = merge(lmConf1All_SumStats,lmConf2All_SumStats,by=c('id'))
ConfModAllCoefs_Allmodel <- ConfModAllCoefs_Allmodel[with(ConfModAllCoefs_Allmodel, order(id)), ]

#combined model####
confICCReg_Allmodel_All <- data.frame(matrix(NA, 4, 4))
# Loop over the time points
for (i in seq_along(var_indicesAll)) {
  var_namex <-ConfModAllCoefs_Allmodel[[paste0(var_indicesAll[i],".x")]]
  var_namey <-ConfModAllCoefs_Allmodel[[paste0(var_indicesAll[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  confICCReg_Allmodel_All[i, 1] <- icc_tmp$results[2, 2]
  confICCReg_Allmodel_All[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  confICCReg_Allmodel_All[i, 4] <-  var_indicesAll[i]
}

#PEmodel####
confICCReg_model_PE <- data.frame(matrix(NA, 3, 4))
# Loop over the time points
for (i in seq_along(var_indicesPE)) {
  var_namexPE <-ConfModAllCoefs_PEmodel[[paste0(var_indicesPE[i],".x")]]
  var_nameyPE <-ConfModAllCoefs_PEmodel[[paste0(var_indicesPE[i],".y")]]
  # Compute the ICC
  icc_tmpPE <- psych::ICC(
    x = cbind(var_namexPE,
              var_nameyPE),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  confICCReg_model_PE[i, 1] <- icc_tmpPE$results[2, 2]
  confICCReg_model_PE[i, c(2:3)] <- icc_tmpPE$results[2,c(7,8)]
  confICCReg_model_PE[i, 4] <-  var_indicesPE[i]
}

#CPPmodel####
confICCReg_model_CPP <- data.frame(matrix(NA, 3, 4))
# Loop over the time points
for (i in seq_along(var_indicesCPP)) {
  var_namexCPP <-ConfModAllCoefs_CPPmodel[[paste0(var_indicesCPP[i],".x")]]
  var_nameyCPP <-ConfModAllCoefs_CPPmodel[[paste0(var_indicesCPP[i],".y")]]
  # Compute the ICC
  icc_tmpCPP <- psych::ICC(
    x = cbind(var_namexCPP,
              var_nameyCPP),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  confICCReg_model_CPP[i, 1] <- icc_tmpCPP$results[2, 2]
  confICCReg_model_CPP[i, c(2:3)] <- icc_tmpCPP$results[2,c(7,8)]
  confICCReg_model_CPP[i, 4] <-  var_indicesCPP[i]
}

########
#For 3) Action adjustments regression with normative predictors####
#merge
circBetasAllCoefs = merge(circBetas_pt1,circBetas_t2,by=c('id'))
circBetasAllCoefs <- circBetasAllCoefs[with(circBetasAllCoefs, order(id)), ]

#get test-retest reliability
predActICCReg_Allmodel_All <- data.frame(matrix(NA, 4, 4))
var_indicesActAll <- c("PE","CPP","RU","Hit")
# Loop over the time points
for (i in seq_along(var_indicesActAll)) {
  var_namex <-circBetasAllCoefs[[paste0(var_indicesActAll[i],".x")]]
  var_namey <-circBetasAllCoefs[[paste0(var_indicesActAll[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predActICCReg_Allmodel_All[i, 1] <- icc_tmp$results[2, 2]
  predActICCReg_Allmodel_All[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predActICCReg_Allmodel_All[i, 4] <-  var_indicesActAll[i]
}

#PE model####
#merge
circBetasAllCoefs_PEmodel = merge(circBetasPE_pt1,circBetasPE_t2,by=c('id'))
circBetasAllCoefs_PEmodel <- circBetasAllCoefs_PEmodel[with(circBetasAllCoefs_PEmodel, order(id)), ]

#get test-retest reliability
predActICCReg_model_PE <- data.frame(matrix(NA, 3, 4))
var_indicesActPE <- c("PE","RU","Hit")
# Loop over the time points
for (i in seq_along(var_indicesActPE)) {
  var_namex <-circBetasAllCoefs_PEmodel[[paste0(var_indicesActPE[i],".x")]]
  var_namey <-circBetasAllCoefs_PEmodel[[paste0(var_indicesActPE[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predActICCReg_model_PE[i, 1] <- icc_tmp$results[2, 2]
  predActICCReg_model_PE[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predActICCReg_model_PE[i, 4] <-  var_indicesActPE[i]
}

#CPP model####
#merge
circBetasAllCoefs_CPPmodel = merge(circBetasCPP_pt1,circBetasCPP_t2,by=c('id'))
circBetasAllCoefs_CPPmodel <- circBetasAllCoefs_CPPmodel[with(circBetasAllCoefs_CPPmodel, order(id)), ]
#get test-retest reliability
predActICCReg_model_CPP <- data.frame(matrix(NA, 3, 4))
var_indicesActCPP <- c("CPP","RU","Hit")
# Loop over the time points
for (i in seq_along(var_indicesActCPP)) {
  var_namex <-circBetasAllCoefs_CPPmodel[[paste0(var_indicesActCPP[i],".x")]]
  var_namey <-circBetasAllCoefs_CPPmodel[[paste0(var_indicesActCPP[i],".y")]]
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_namex,
              var_namey),
    alpha = 0.05,
    lmer = TRUE
  )
  # Store the results in the matrix
  predActICCReg_model_CPP[i, 1] <- icc_tmp$results[2, 2]
  predActICCReg_model_CPP[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
  predActICCReg_model_CPP[i, 4] <-  var_indicesActCPP[i]
}

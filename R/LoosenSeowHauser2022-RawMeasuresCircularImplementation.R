################################################################################################################
##Loosen, Seow & Hauser (submitted) - Main Manuscript                                                         ##
##Consistency within change: Evaluating the psychometric properties of a widely-used predictive-inference task##
##Part 1                                                                                                      ##
################################################################################################################

rm(list=ls())

#Set directory####
setwd("~/Documents/GitHub/Probabilistic-Inference-Task-Analysis/R")

#######################
##Loading libraries####
#######################
library(funchir)#this library enables you to check which libraries are actually used in a script
funchir::stale_package_check('LoosenSeowHauser2022-RawMeasuresCircularImplementation.R')# Reliability Analyses Probabilistic inference task
library(data.table)
library(psych)
library(psych)
options(scipen=999) #set scientific notation in this session
options(max.print=25000) #set max print for e.g., displaying data frames 

##################
##Loading data####
##################
Game_pt1= data.frame(read.csv("Table_gamePIT_pt1.csv",header = T))#dataset t1 predictive inference task
Game_t2= data.frame(read.csv("Table_gamePIT_t2.csv",header = T))#dataset t1 and t2 combined predictive inference task
Game_pt1_relativeCP_tmp= data.frame(read.csv("Table_gamePIT_pt1_relativeCP.csv",header = T))#load trials relative to all change points (CPs)
Game_pt1_relativeCP <- Game_pt1_relativeCP_tmp[with(Game_pt1_relativeCP_tmp,order(id,nTrial_rel)),]#order according to trial number and id
Game_t2_relativeCP_tmp= data.frame(read.csv("Table_gamePIT_t2_relativeCP.csv",header = T))
Game_t2_relativeCP <- Game_t2_relativeCP_tmp[with(Game_t2_relativeCP_tmp,order(id,nTrial_rel)),] 

################
##Functions ####
################
##Spearman Brown corrected Pearson correlation for internal consistency
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

#######################
##Prepare task data####
#######################
#Remove the first trial of every participant because action updates and learning conceptually only start at the second trial
Game_pt1<- Game_pt1[Game_pt1$nTrial!=1, ]
Game_t2<- Game_t2[Game_t2$nTrial!=1, ]

#z-score confidence
Game_pt1$conf.sc <-ave(Game_pt1$conf, Game_pt1$id, FUN=scale) 
Game_t2$conf.sc <-ave(Game_t2$conf, Game_t2$id, FUN=scale)

########################################
##Merge datasets and create new ones####
########################################
#merge t1 and t2 data####
Game_t2_relativeCP_colnames <- Game_t2_relativeCP
names(Game_t2_relativeCP_colnames) <- paste0(names(Game_t2_relativeCP_colnames), "_t2")#change the col names so we can merge it with t1 data
setnames(Game_t2_relativeCP_colnames, old = c('id_t2','nTrial_rel_t2'), new = c('id','nTrial_rel'))
Game_all_relativeCP = merge(Game_pt1_relativeCP,Game_t2_relativeCP_colnames,by=c('id','nTrial_rel'))
Game_all_relativeCP <- Game_all_relativeCP[with(Game_all_relativeCP, order(id, nTrial_rel)), ]

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

#####################################################
##Internal consistency for trials relative to CPs####
#####################################################
#t1####
#confidence####
# Define the indices for bef and aft
bef_indices <- c("bef4", "bef3", "bef2", "bef1")
aft_indices <- c("aft0", "aft1", "aft2", "aft3", "aft4")

# Initialize empty variables to store the results
rCPpt1.conf <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPpt1.conf <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)

# Iterate over bef indices
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_pt1_bef <- game_pt1_rel[[paste0("Game_pt1_", bef_index)]]
  
  rCPpt1.conf[i] <- IntCons_r(game_pt1_bef$median_conf_even,
                              game_pt1_bef$median_conf_odd,
                              game_pt1_bef)[1, 1]
  
  ciCPpt1.conf[i,] <- IntCons_r(game_pt1_bef$median_conf_even,
                               game_pt1_bef$median_conf_odd,
                               game_pt1_bef)[, 2]
}

for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_pt1_aft <- game_pt1_rel[[paste0("Game_pt1_", aft_index)]]
  
  rCPpt1.conf[i+4] <- IntCons_r(game_pt1_aft$median_conf_even,
                              game_pt1_aft$median_conf_odd,
                              game_pt1_aft)[1, 1]
  
  ciCPpt1.conf[i+4,] <- IntCons_r(game_pt1_aft$median_conf_even,
                                game_pt1_aft$median_conf_odd,
                                game_pt1_aft)[, 2]
}

#stable versus unstable task phases
conf_internCons_UnStabl <- matrix(NA, nrow = 2, ncol = 4)
conf_internCons_UnStabl[1,1] <- "Stable"
conf_internCons_UnStabl[2,1] <- "Unstable"
conf_internCons_UnStabl[1,2] <- IntCons_r(Game_pt1_relativeCP$median_conf_even_stabl,Game_pt1_relativeCP$median_conf_odd_stabl,Game_pt1_relativeCP)[1,1]
conf_internCons_UnStabl[1,3:4] <- IntCons_r(Game_pt1_relativeCP$median_conf_even_stabl,Game_pt1_relativeCP$median_conf_odd_stabl,Game_pt1_relativeCP)[,2]
conf_internCons_UnStabl[2,2] <- IntCons_r(Game_pt1_relativeCP$median_conf_even_unstabl,Game_pt1_relativeCP$median_conf_odd_unstabl,Game_pt1_relativeCP)[1,1]
conf_internCons_UnStabl[2,3:4] <- IntCons_r(Game_pt1_relativeCP$median_conf_even_unstabl,Game_pt1_relativeCP$median_conf_odd_unstabl,Game_pt1_relativeCP)[,2]

#learning rate####
# Initialize empty variables to store the results
rCPpt1.lr <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPpt1.lr <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)

# Iterate over bef indices
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_pt1_bef <- game_pt1_rel[[paste0("Game_pt1_", bef_index)]]
  
  rCPpt1.lr[i] <- IntCons_r(game_pt1_bef$median_behav_lr_even,
                              game_pt1_bef$median_behav_lr_odd,
                              game_pt1_bef)[1, 1]
  
  ciCPpt1.lr[i,] <- IntCons_r(game_pt1_bef$median_behav_lr_even,
                                game_pt1_bef$median_behav_lr_odd,
                                game_pt1_bef)[, 2]
}

for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_pt1_aft <- game_pt1_rel[[paste0("Game_pt1_", aft_index)]]
  
  rCPpt1.lr[i+4] <- IntCons_r(game_pt1_aft$median_behav_lr_even,
                                game_pt1_aft$median_behav_lr_odd,
                                game_pt1_aft)[1, 1]
  
  ciCPpt1.lr[i+4,] <- IntCons_r(game_pt1_aft$median_behav_lr_even,
                                  game_pt1_aft$median_behav_lr_odd,
                                  game_pt1_aft)[, 2]
}

#stable versus unstable task phases
lr_internCons_UnStabl <- matrix(NA, nrow = 2, ncol = 4)
lr_internCons_UnStabl[1,1] <- "Stable"
lr_internCons_UnStabl[2,1] <- "Unstable"
lr_internCons_UnStabl[1,2] <- IntCons_r(Game_pt1_relativeCP$median_behav_lr_even_stabl,Game_pt1_relativeCP$median_behav_lr_odd_stabl,Game_pt1_relativeCP)[1,1]
lr_internCons_UnStabl[1,3:4] <- IntCons_r(Game_pt1_relativeCP$median_behav_lr_even_stabl,Game_pt1_relativeCP$median_behav_lr_odd_stabl,Game_pt1_relativeCP)[,2]
lr_internCons_UnStabl[2,2] <- IntCons_r(Game_pt1_relativeCP$median_behav_lr_even_unstabl,Game_pt1_relativeCP$median_behav_lr_odd_unstabl,Game_pt1_relativeCP)[1,1]
lr_internCons_UnStabl[2,3:4] <- IntCons_r(Game_pt1_relativeCP$median_behav_lr_even_unstabl,Game_pt1_relativeCP$median_behav_lr_odd_unstabl,Game_pt1_relativeCP)[,2]

#learning rate difference (before versus after CP)####
rCPpt1.lrdiffAft <-IntCons_r(Game_pt1_singl$behav_lrDiffevenAft,Game_pt1_singl$behav_lrDiffoddAft,Game_pt1_singl)[1,1]
ciCPpt1.lrdiffAft<-IntCons_r(Game_pt1_singl$behav_lrDiffevenAft,Game_pt1_singl$behav_lrDiffoddAft,Game_pt1_singl)[,2]

#t2####
#confidence####
# Initialize empty variables to store the results
rCPt2.conf <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)
ciCPt2.conf <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 2)

# Iterate over bef indices
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  game_t2_bef <- game_t2_rel_IntCons[[paste0("Game_all_", bef_index)]]
  
  rCPt2.conf[i] <- IntCons_r(game_t2_bef$median_conf_even_t2,
                                 game_t2_bef$median_conf_odd_t2,
                              game_t2_bef)[1, 1]
  
  ciCPt2.conf[i,] <- IntCons_r(game_t2_bef$median_conf_even_t2,
                                   game_t2_bef$median_conf_odd_t2,
                                game_t2_bef)[, 2]
}

for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  game_t2_aft <- game_t2_rel_IntCons[[paste0("Game_all_", aft_index)]]
  
  rCPt2.conf[i+4] <- IntCons_r(game_t2_aft$median_conf_even_t2,
                                game_t2_aft$median_conf_odd_t2,
                                game_t2_aft)[1, 1]
  
  ciCPt2.conf[i+4,] <- IntCons_r(game_t2_aft$median_conf_even_t2,
                                  game_t2_aft$median_conf_odd_t2,
                                  game_t2_aft)[, 2]
}
 
#stable versus unstable task phases
conf_internCons_UnStabl_t2 <- matrix(NA, nrow = 2, ncol = 4)
conf_internCons_UnStabl_t2[1,1] <- "Stable"
conf_internCons_UnStabl_t2[2,1] <- "Unstable"
conf_internCons_UnStabl_t2[1,2] <- IntCons_r(Game_t2_relativeCP$median_conf_even_stabl,Game_t2_relativeCP$median_conf_odd_stabl,Game_t2_relativeCP)[1,1]
conf_internCons_UnStabl_t2[1,3:4] <- IntCons_r(Game_t2_relativeCP$median_conf_even_stabl,Game_t2_relativeCP$median_conf_odd_stabl,Game_t2_relativeCP)[,2]
conf_internCons_UnStabl_t2[2,2] <- IntCons_r(Game_t2_relativeCP$median_conf_even_unstabl,Game_t2_relativeCP$median_conf_odd_unstabl,Game_t2_relativeCP)[1,1]
conf_internCons_UnStabl_t2[2,3:4] <- IntCons_r(Game_t2_relativeCP$median_conf_even_unstabl,Game_t2_relativeCP$median_conf_odd_unstabl,Game_t2_relativeCP)[,2]

#learning rate####
# Initialize empty variables to store the results
rCPt2.lr <- matrix(NA, nrow = length(bef_indices) + length(aft_indices), ncol = 1)

#stable versus unstable task phases
lr_internCons_UnStabl_t2 <- matrix(NA, nrow = 2, ncol = 4)
lr_internCons_UnStabl_t2[1,1] <- "Stable"
lr_internCons_UnStabl_t2[2,1] <- "Unstable"
lr_internCons_UnStabl_t2[1,2] <- IntCons_r(Game_t2_relativeCP$median_behav_lr_even_stabl,Game_t2_relativeCP$median_behav_lr_odd_stabl,Game_t2_relativeCP)[1,1]
lr_internCons_UnStabl_t2[1,3:4] <- IntCons_r(Game_t2_relativeCP$median_behav_lr_even_stabl,Game_t2_relativeCP$median_behav_lr_odd_stabl,Game_t2_relativeCP)[,2]
lr_internCons_UnStabl_t2[2,2] <- IntCons_r(Game_t2_relativeCP$median_behav_lr_even_unstabl,Game_t2_relativeCP$median_behav_lr_odd_unstabl,Game_t2_relativeCP)[1,1]
lr_internCons_UnStabl_t2[2,3:4] <- IntCons_r(Game_t2_relativeCP$median_behav_lr_even_unstabl,Game_t2_relativeCP$median_behav_lr_odd_unstabl,Game_t2_relativeCP)[,2]

#learning rate difference (before versus after CP)####
rCPt2.lrdiffAft <-IntCons_r(Game_t2_singl$behav_lrDiffevenAft,Game_t2_singl$behav_lrDiffoddAft,Game_t2_singl)[1,1]
ciCPt2.lrdiffAft<-IntCons_r(Game_t2_singl$behav_lrDiffevenAft,Game_t2_singl$behav_lrDiffoddAft,Game_t2_singl)[,2]

############################
#Test-retest reliability####
############################
#learning rate difference####
icc_diffbehav_lr_Befmat <- matrix(NA,1,3)
icc_diffbehav_lr_Befmat_tmp <- psych::ICC(x= cbind(Game_all_singl$behav_lrDiffBef.x,Game_all_singl$behav_lrDiffBef.y),alpha=.05,lmer=TRUE)
icc_diffbehav_lr_Befmat[1,1] <- icc_diffbehav_lr_Befmat_tmp$results[2,2]
icc_diffbehav_lr_Befmat[1,c(2:3)] <- icc_diffbehav_lr_Befmat_tmp$results[2,c(7,8)]

icc_diffbehav_lr_Aftmat <- matrix(NA,1,3)
icc_diffbehav_lr_Aftmat_tmp <- psych::ICC(x= cbind(Game_all_singl$behav_lrDiffAft.x,Game_all_singl$behav_lrDiffAft.y),alpha=.05,lmer=TRUE)
icc_diffbehav_lr_Aftmat[1,1] <- icc_diffbehav_lr_Aftmat_tmp$results[2,2]
icc_diffbehav_lr_Aftmat[1,c(2:3)] <- icc_diffbehav_lr_Aftmat_tmp$results[2,c(7,8)]

#learning rate####
# Define the time points and create empty matrices
icc_behav_lr <- data.frame(matrix(NA, 9, 3))

# Loop over the time points
for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", bef_index)]]
    
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$median_behav_lr,
              var_name$median_behav_lr_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_behav_lr[i, 1] <- icc_tmp$results[2, 2]
  icc_behav_lr[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", aft_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$median_behav_lr,
              var_name$median_behav_lr_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_behav_lr[i+4, 1] <- icc_tmp$results[2, 2]
  icc_behav_lr[i+4, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

#stable versus unstable task phases
icc_behav_lr_stabl <- psych::ICC(x= cbind(Game_all_relativeCP$median_behav_lr_stabl,Game_all_relativeCP$median_behav_lr_stabl_t2),alpha=.05,lmer=TRUE)
icc_behav_lr_stabl$results[2,2]
icc_behav_lr_stabl$results[2,c(7,8)]
icc_behav_lr_unstabl <- psych::ICC(x= cbind(Game_all_relativeCP$median_behav_lr_unstabl,Game_all_relativeCP$median_behav_lr_unstabl_t2),alpha=.05,lmer=TRUE)
icc_behav_lr_unstabl$results[2,2]
icc_behav_lr_unstabl$results[2,c(7,8)]

#confidence####
icc_conf <- data.frame(matrix(NA, 9, 3))

for (i in seq_along(bef_indices)) {
  bef_index <- bef_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", bef_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$median_conf,
              var_name$median_conf_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_conf[i, 1] <- icc_tmp$results[2, 2]
  icc_conf[i, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

for (i in seq_along(aft_indices)) {
  aft_index <- aft_indices[i]
  var_name <-  game_t2_rel[[paste0("Game_all_", aft_index)]]
  
  # Compute the ICC
  icc_tmp <- psych::ICC(
    x = cbind(var_name$median_conf,
              var_name$median_conf_t2),
    alpha = 0.05,
    lmer = TRUE
  )
  
  # Store the results in the matrix
  icc_conf[i+4, 1] <- icc_tmp$results[2, 2]
  icc_conf[i+4, c(2:3)] <- icc_tmp$results[2,c(7,8)]
}

#stable versus unstable task phases
icc_conf_stabl_mat_tmp <- psych::ICC(x= cbind(Game_all_relativeCP$median_conf_stabl,Game_all_relativeCP$median_conf_stabl_t2),alpha=.05,lmer=TRUE)
icc_conf_stabl_mat_tmp$results[2,2]
icc_conf_stabl_mat_tmp$results[2,c(7,8)]
icc_conf_unstabl_mat_tmp <- psych::ICC(x= cbind(Game_all_relativeCP$median_conf_unstabl,Game_all_relativeCP$median_conf_unstabl_t2),alpha=.05,lmer=TRUE)
icc_conf_unstabl_mat_tmp$results[2,2]
icc_conf_unstabl_mat_tmp$results[2,c(7,8)]


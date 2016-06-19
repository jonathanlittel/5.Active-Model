# Load file and set options
#####
  options(scipen=99, digits=3)
  library(dplyr)
  library(tidyr)
  library(testthat)

# Load data 
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)

# Load underwriting Model pds  
  filename <-  "pds_05.28.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")
  
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/5.Active Model"
  setwd(wd)

# Load info for loans in current receiving a payment > $1k, or for hitting the watch list
  filename <- 'Payments in current_watch list_12-31-15.csv' 
  pmt_data <- read.csv(filename)

  # check that all loans in df.rap are in pmt_data
  id_pmt <- pmt_data$LoanID[!duplicated(pmt_data$LoanID)]
  expect_that(sum((df.rap$LoanID %in% id_pmt)), equals(sum(!duplicated(df.rap$LoanID)))) # should be all TRUE
  df.rap <- merge(df.rap, pmt_data)

# Create four dummies for risk categories, which assumes anything in a higher category also hit a lower category
  df.rap$Max_Risk_Category2 <- -1
    df.rap <- df.rap %>% 
      mutate(Max_Risk_Category2 = replace(Max_Risk_Category2, Max_Risk_Category=='Current', 0)) %>%
      mutate(Max_Risk_Category2 = replace(Max_Risk_Category2, Max_Risk_Category=='Special Mention', 6)) %>%
      mutate(Max_Risk_Category2 = replace(Max_Risk_Category2, Max_Risk_Category=='Substandard', 12)) %>%
      mutate(Max_Risk_Category2 = replace(Max_Risk_Category2, Max_Risk_Category=='Doubtful', 18))

  df.rap$Max_Risk_Category <- factor(df.rap$Max_Risk_Category2, 
    levels = c(0, 6, 12, 18),
    labels = c("Current", "Special Mention", "Substandard", "Doubtful")
    )   

  df.rap$Max_Risk_Category <- as.numeric(df.rap$Max_Risk_Category2)

# Create dummies for max risk category
  df.rap$Current <- 1
  df.rap$Special_Mention <- 0
  df.rap$Substandard <- 0
  df.rap$Doubtful <- 0

  df.rap$Special_Mention <- ifelse(df.rap$Max_Risk_Category>0,1,0)
  df.rap$Substandard <- ifelse(df.rap$Max_Risk_Category>6,1,0)
  df.rap$Doubtful <- ifelse(df.rap$Max_Risk_Category>12,1,0)

  df.rap$Special_Mention <- ifelse(df.rap$WO==1,1,df.rap$Special_Mention)
  df.rap$Substandard <- ifelse(df.rap$WO==1,1,df.rap$Substandard )
  df.rap$Doubtful <- ifelse(df.rap$WO==1,1,df.rap$Doubtful )

  df.rap$Max_Risk_Category <- factor(df.rap$Max_Risk_Category, 
    levels = c(0, 6, 12, 18),
    labels = c("Current", "Special Mention", "Substandard", "Doubtful")
    )

  df.rap <- df.rap %>%
            mutate(Special_Mention = replace(Special_Mention, WO=="Writeoff", 1)) %>%
            mutate(Substandard = replace(Substandard, WO=="Writeoff", 1)) %>%
            mutate(Doubtful = replace(Doubtful, WO=="Writeoff", 1)) 

################################################
#                                      # 
#               Models               #   
################################################

# Subset to active / inactive
  df.rap.inactive <- filter(df.rap, active==0)

# create table for recording means of writeoffs of different cuts
    mean_pred <- data.frame(temp=mean(as.numeric(df.rap.inactive$WO)-1))

# test on pd to pd 
    modelColsSM <- c("WO",
                    "pd")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSM]
  glmCurrent <-glm(WO ~ pd + log(pd), data=df.model, family='binomial', na.action=na.exclude)
  pd_current_test <- predict(glmCurrent, df.rap.inactive, type='response')
  plot(pd_current_test, df.rap.inactive$pd)
  abline(a=0, b=1)

# # Prob of migration from Current given pmt and watch list status to WO #
# ###############
#   # df.rap.inactive <- filter(df.rap.inactive ) #
#   # modelColsC <- c("WO",
#   #                   "pd", 'watch_list', 'pmt_1k')
#   df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsC]
#   glmCurPmtWL <-glm(WO ~ pd + log(pd) + watch_list + pmt_1k, data=df.model, family='binomial', na.action=na.exclude)
#   # summary(glmSMWO)
#   # df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
#   df.rap.inactive$pdCurPmtWL <- predict(glmCurPmtWL, df.rap.inactive, family='binomial', type='response')
#   # mean_pred$CurPmtWL <- mean(df.rap.inactive$pdCurPmtWL)
#   ############


# Watch list to WO
###############
  # df.rap.inactive <- filter(df.rap.inactive , watch_list==1) #
  # modelColsC <- c("WO",
  #                   "pd", 'watch_list', 'pmt_1k')
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsC]
  glmWLWO <-glm(WO ~ pd + log(pd) + watch_list , data=df.model, family='binomial', na.action=na.exclude)
  # summary(glmSMWO)
  # df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
  df.rap.inactive$pdWLWO <- predict(glmWLWO, df.rap.inactive, family='binomial', type='response')
  mean_pred$pdWLWO <- mean(df.rap.inactive$pdWLWO)
  ############

# With payment to WO
###############
  # df.rap.inactive <- filter(df.rap.inactive , pmt_1k==1) #
  # modelColsC <- c("WO",
  #                   "pd", 'watch_list', 'pmt_1k')
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsC]
  glmPMTWO <-glm(WO ~ pd + log(pd) + pmt_1k, data=df.model, family='binomial', na.action=na.exclude)
  # summary(glmSMWO)
  # df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
  df.rap.inactive$pdPMT <- predict(glmPMTWO, df.rap.inactive, family='binomial', type='response')
  mean_pred$pdPMT <- mean(df.rap.inactive$pdPMT)
  ############


# # Prob of migration from Current given pmt and watch list status to WO #
# ###############
#   df.rap.inactive <- filter(df.rap.inactive , pmt_1k==1, watch_list==1) #
#   # modelColsC <- c("WO",
#   #                   "pd", 'watch_list', 'pmt_1k')
#   df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsC]
#   glmCurPmtWL <-glm(WO ~ pd + log(pd), data=df.model, family='binomial', na.action=na.exclude)
#   # summary(glmSMWO)
#   # df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
#   df.rap.inactive$pdCurPmtWL <- predict(glmCurPmtWL, df.rap.inactive, family='binomial', type='response')
#   # mean_pred$CurPmtWL <- mean(df.rap.inactive$pdCurPmtWL)
#   ############


# Prob of migration from SM to WO #
###############
  # first need to filter for loans that made it to SM (and thus have a chance of migrating)
  df.rap.inactive <- filter(df.rap, active==0)
  df.rap.inactive <- filter(df.rap.inactive, Special_Mention==1)
  modelColsSM <- c("WO",
                    "pd")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSM]
  glmSMWO <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  # summary(glmSMWO)
  df.rap.inactive$pdSMWO <- predict(glmSMWO, df.rap.inactive, family='binomial', type='response')
  mean_pred$SM <- mean(df.rap.inactive$pdSMWO)
  ############

# Prob of migration from Substandard to WO 
###############
  # first need to filter for loans that made it to SM (and thus have a chance of migrating)
  df.rap.inactive <- filter(df.rap.inactive, Substandard==1)
  modelColsSM <- c("WO",
                    "pd")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSM]
  glmSubWO <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  # summary(glmSubWO)
  df.rap.inactive$pdSubWO <- predict(glmSubWO, df.rap.inactive, family='binomial', type='response')
  mean_pred$Sub <- mean(df.rap.inactive$pdSubWO)
  ############


# Prob of migration from Doubtful to WO 
###############
  # first need to filter for loans that made it to SM (and thus have a chance of migrating)
  df.rap.inactive <- filter(df.rap.inactive, Doubtful==1)
  modelColsSM <- c("WO",
                    "pd")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSM]
  glmDWO <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  # summary(glmDWO)
  df.rap.inactive$pdDWO <- predict(glmDWO, df.rap.inactive, family='binomial', type='response')
  mean_pred$D <- mean(df.rap.inactive$pdDWO)
  ############

# bring back inactive df (had been filtered for risk categories)
  df.rap.inactive <- filter(df.rap, active==0)

# Check that mean predicted is close to average
  df.rap.inactive <- filter(df.rap, active==0)
  df.rap.inactive$WO_num <- as.numeric(df.rap.inactive$WO) - 1
  mean_pred[2,1] <- sum(df.rap.inactive$WO_num) / nrow(df.rap.inactive) 
  mean_pred[2,4] <- sum(df.rap.inactive$WO_num) / sum(df.rap.inactive$Special_Mention)
  mean_pred[2,5] <- sum(df.rap.inactive$WO_num) / sum(df.rap.inactive$Substandard)
  mean_pred[2,6] <- sum(df.rap.inactive$WO_num) / sum(df.rap.inactive$Doubtful) 


  df.rap$WO_num <- as.numeric(df.rap$WO) - 1
  mean_pred[3,1] <- sum(df.rap$WO_num) / nrow(df.rap)   
  mean_pred[3,4] <- sum(df.rap$WO_num) / sum(df.rap$Special_Mention)
  mean_pred[3,5] <- sum(df.rap$WO_num) / sum(df.rap$Substandard)
  mean_pred[3,6] <- sum(df.rap$WO_num) / sum(df.rap$Doubtful) 

  sum(df.rap.inactive$WO_num) / sum(df.rap.inactive$pmt_1k)
  sum(df.rap.inactive$WO_num) / sum(df.rap.inactive$watch_list)

  colnames(mean_pred)[1] <- c('Current')
  row.names(mean_pred) <- c('predicted on subset', 'actual on subset', 'actual all loans')
  mean_pred$temp <- NULL
  mean_pred

##########################
#  predict pds           # 
########################## 
# Predict probs of all pds for all loans
  df.predict <- df.rap

  # for combos of watch list and pmt_1k
  # df.predict$pmt_1k <- 1
  df.predict$watch_list <- 1
  df.predict$pdWL <- predict(glmWLWO, df.predict, family='binomial', type='response')

  df.predict$pmt_1k <- 1
  # df.predict$watch_list <- 0
  df.predict$pdpmt <-   predict(glmPMTWO, df.predict, family='binomial', type='response')

  # df.predict$pmt_1k <- 1
  # df.predict$watch_list <- 1
  # df.predict$pdCurPmtWL <-   predict(glmCurPmtWL, df.predict, family='binomial', type='response')

  # for risk categories
  df.predict$Special_Mention <- 1
  df.predict$Substandard <- 1
  df.predict$Doubtful <- 1

  df.predict$pdSMWO <- predict(glmSMWO, df.predict, family='binomial', type='response')
  df.predict$pdSubWO <- predict(glmSubWO, df.predict, family='binomial', type='response')
  df.predict$pdDWO <- predict(glmDWO, df.predict, family='binomial', type='response')


####
  #   check   #
# Check the means
  apply(filter(pds_to_WO, active==0), 2, mean)
  mean_pred

# Return loanIDs where the pd is lower at any step in migration
  wrong_way <- c()
  wrong_way <- c(wrong_way, df.predict$LoanID[!df.predict$pdWL<df.predict$pdSMWO])  
  wrong_way <- c(wrong_way, df.predict$LoanID[!df.predict$pd<df.predict$pdSMWO])
  wrong_way <- c(wrong_way, df.predict$LoanID[!df.predict$pdSMWO<df.predict$pdSubWO])
  wrong_way <- c(wrong_way, df.predict$LoanID[!df.predict$pdSubWO<df.predict$pdDWO])
  # wrong_way <- dplyr::select(filter(df.predict, LoanID==wrong_way), LoanID, pd, pdSMWO:pdDWO)

################
#   graph      #

pds_to_WO <- dplyr::select(df.predict, LoanID, pd, pdSMWO, pdSubWO, pdDWO, active)

pdsWO <- pds_to_WO %>%
      gather(migration, pds, pd:pdDWO)
pdsWO$migration <- factor(pdsWO$migration,
  levels= c('pd', 'pdSMWO', 'pdSubWO', 'pdDWO'),
  labels= c('Current', 'Special Mention', 'Substandard', 'Doubtful'))

t <- ggplot(pdsWO, aes(x=migration, y=pds))
    t + geom_jitter(aes(colour=migration), size=1, alpha=0.5)

t <- ggplot(pdsWO, aes(x=migration, y=pds))
u <- t + geom_line(aes(group = LoanID), alpha = 0.1) + geom_point(size=0.25, alpha=0.1)
u

##################
#   write output #

# Write the checks for pds which migrate in the wrong direction, for 
  library(xlsx)
  filename <- 'profile model report.xlsx'
  write.xlsx(wrong_way, file=filename, sheetName='wrong direction')
  write.xlsx(mean_pred, file=filename, sheetName='mean pds check', append=TRUE)


# write coefficients
  coefs_out <- t(data.frame(
    'watch list' = coef(glmWLWO),
    'payment received' = coef(glmPMTWO),
    'special mention' = coef(glmSMWO),
    'Substandard' = coef(glmSubWO),
    'Doubtful' = coef(glmDWO)
    ))

  write.xlsx(coefs_out, file=filename, sheetName='coefficients', append=TRUE)

file.show(filename)

# Save glms
  saveRDS(list(glmWLWO, glmPMTWO, glmSMWO, glmSubWO, glmDWO), 'active_glms.rds')

df.predict <- select(df.predict, -(Current:Doubtful))
# df.predict <- optionally could combine the dummies for risk categories
out <- dplyr::select(df.predict, LoanID:Max_Risk_Category, pdSMWO:pdDWO, RC.Opp.Number)
write.csv(out, 'risk_profile_pds.csv')

########################
# additional plot with watch list and payment categories


  pds_to_WO_all <- dplyr::select(df.predict, LoanID, pd, pdWL, pdpmt, pdSMWO, pdSubWO, pdDWO, active)

pdsWO_all <- pds_to_WO_all %>%
      gather(migration, pds, pd:pdDWO)

pdsWO_all$migration <- factor(pdsWO_all$migration,
  levels= c('pd', 'pdWL', 'pdpmt','pdSMWO', 'pdSubWO', 'pdDWO'),
  labels= c('Current', 'Watch List', 'Received Payment',
    'Special Mention', 'Substandard', 'Doubtful'))

t <- ggplot(pdsWO_all, aes(x=migration, y=pds))
    t + geom_jitter(aes(colour=migration), size=1, alpha=0.5)

t <- ggplot(pdsWO_all, aes(x=migration, y=pds))
u <- t + geom_line(aes(group = LoanID), alpha = 0.1) + geom_point(size=0.25, alpha=0.1)
u + theme(axis.text.x=element_text(angle=60,vjust=0.75)) #,hjust=1
 

# filtering for just lower pds
  pdsWO_all <- pds_to_WO_all %>%
      filter(pd<0.25) %>%
      gather(migration, pds, pd:pdDWO)
# pdsWO_all$migration <- factor(pdsWO_all$migration,
#   levels= c('pd', 'pdWL_noPmt', 'pdpmt', 'pdWL_pmt','pdSMWO', 'pdSubWO', 'pdDWO'),
#   labels= c('Current', 'Watch List', 'Received Payment', 'WL and Received Payment',
#     'Special Mention', 'Substandard', 'Doubtful'))

t <- ggplot(pdsWO_all, aes(x=migration, y=pds))
u <- t + geom_line(aes(group = LoanID), alpha = 0.1) + geom_point(size=0.25, alpha=0.1)
u + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

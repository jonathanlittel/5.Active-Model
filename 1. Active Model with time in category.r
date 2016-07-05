# Subset to active / inactive
  df.rap.inactive <- filter(df.rap, active==0)

################################################
#                                      # 
#               Models               #   
################################################

#####
            ###
#############################################################################
# Replace the pd with the cumulative prob of the 1 year pd given the tenor
            df.rap$pd_orig <- df.rap$pd
            df.rap$pd <- df.rap$pd_multiple_years
            df.rap.inactive$pd_orig <- df.rap.inactive$pd
            df.rap.inactive$pd <- df.rap.inactive$pd_multiple_years
#############################################################################


# create table for recording means of writeoffs of different cuts
    mean_pred <- data.frame(temp=mean(as.numeric(df.rap.inactive$WO)-1))

# test on pd to pd 
    modelColsC <- c("WO",
                    "pd")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsC]
  glmCurrent <-glm(WO ~ pd , data=df.model, family='binomial', na.action=na.exclude) # 
  pd_current_test <- predict(glmCurrent, df.rap.inactive, type='response')
  plot( df.rap.inactive$pd, pd_current_test)
  abline(a=0, b=1)
  abline(v=0.2, h=0.2)

# # Prob of migration from Current given pmt and watch list status to WO #
# ###############
#   # df.rap.inactive <- filter(df.rap.inactive ) #
#   # modelColsC <- c("WO",
#   #                   "pd", 'watch_list', 'pmt_1k')
#   df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsC]
#   glmCurPmtWL <-glm(WO ~ . + watch_list + pmt_1k, data=df.model, family='binomial', na.action=na.exclude)
#   # summary(glmSMWO)
#   # df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
#   df.rap.inactive$pdCurPmtWL <- predict(glmCurPmtWL, df.rap.inactive, family='binomial', type='response')
#   # mean_pred$CurPmtWL <- mean(df.rap.inactive$pdCurPmtWL)
#   ############


# Watch list to WO
###############
  # df.rap.inactive <- filter(df.rap.inactive , watch_list==1) #
  modelColsWL <- c("WO",
                    "pd")
  df.model <- filter(df.rap.inactive, watch_list==1)
  df.model <- df.model[,names(df.model) %in% modelColsWL]
  glmWLWO <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  # summary(glmSMWO)
  # df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
  df.model$pdWLWO <- predict(glmWLWO, df.model, family='binomial', type='response')
  mean_pred$pdWLWO <- mean(df.model$pdWLWO)
  ############

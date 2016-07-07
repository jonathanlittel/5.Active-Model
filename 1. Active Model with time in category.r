# # Subset to active / inactive
#   df.rap.inactive <- filter(df.rap, active==0)

# ################################################
# # WO probability models given risk category    #   
# ################################################


# create table for recording means of writeoffs of different cuts
    mean_pred <- data.frame(wl=rep(NA,2))
    rownames(mean_pred) <- c('mean_pd', 'mean_actual')

# test on pd to pd 
  df.current <- filter(df, active==0)
    modelColsC <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.current[,names(df.current) %in% modelColsC]
  glmCurrent <-glm(WO ~ pd , data=df.model, family='binomial', na.action=na.exclude) # 
  pd_current_test <- predict(glmCurrent, df.current, type='response')
  mean_pred$current <- mean(pd_current_test)
  mean_pred$current[2] <- sum(df.current$balance[df.current$WO=='Writeoff']) / sum(as.numeric(df.current$balance))
  plot( df.current$pd, pd_current_test)
  abline(a=0, b=1)
  abline(v=0.2, h=0.2)

###############
# Watch list to WO
###############
  df.wl <- filter(df, active==0, watch_list==1)
  modelColsWL <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.wl[,names(df.wl) %in% modelColsWL]
  glmWLWO <- glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  pd_wl <- predict(glmWLWO, df.wl, family='binomial', type='response')
  mean_pred$wl <- mean(pd_wl)
  mean_pred$wl[2] <-sum(df.wl$balance[df.wl$WO=='Writeoff']) / sum(df.wl$balance)
  summary(df.model$WO)
  plot(df.wl$pd, pd_wl)
  abline(a=0, b=1)
###############
# SM to WO    #
###############
  # first need to filter for loans that made it to SM (and thus have a chance of migrating)
  df.sm <- filter(df, active==0, Special_Mention==1)
  modelColsSM <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.sm[,names(df.sm) %in% modelColsSM]
  glmSMWO <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  pdSMWO <- predict(glmSMWO, df.sm, family='binomial', type='response')
  mean_pred$SM <- mean(pdSMWO)
  mean_pred$SM[2] <- sum(df.sm$balance[df.sm$WO=='Writeoff']) / sum(df.sm$balance)
  plot(df.sm$pd, pdSMWO)


#######################
# Substandard to WO   #
#######################
  df.sub <- filter(df, active==0, Substandard==1)
  modelColsSM <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.sub[,names(df.sub) %in% modelColsSM]
  glmSub <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  pd_sub <- predict(glmSub, df.sub, family='binomial', type='response')
  mean_pred$substandard <- mean(pd_sub)
  mean_pred$substandard[2] <-sum(df.sub$balance[df.sub$WO=='Writeoff']) / sum(df.sub$balance)


#######################
# Doubtful to WO      #
#######################
  df.doubt <- filter(df, active==0, Doubtful==1)
  modelColsSM <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.doubt[,names(df.doubt) %in% modelColsSM]
  glmDWO <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  # summary(glmDWO)
  pd_doubt<- predict(glmDWO, df.doubt, family='binomial', type='response')
  mean_pred$doubtful <- mean(pd_doubt)
  mean_pred$doubtful[2] <-sum(df.doubt$balance[df.doubt$WO=='Writeoff']) / sum(df.doubt$balance)




change <- ggplot(data.frame(df.sm, pdSMWO), 
  aes(x=months_in_risk_cat, y=pdSMWO, group=LoanID))
change + geom_line(aes(color=LoanID))

sum(df.sm$balance * pdSMWO) / sum(df.sm$balance)
sum(df.sm$balance[df.sm$WO=='Writeoff']) / sum(df.sm$balance)

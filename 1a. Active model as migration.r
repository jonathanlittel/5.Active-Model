# #----------------------#----------------------##
# # WO probability models given risk category    #   
# #----------------------#----------------------##

# add active on 4/30/16
  active <- data.frame(
    LoanID = r$LoanID[r$balance>0 & r$date=='2016-04-30'],
    active = 1
    )

  df$active <- NULL

  df <- merge(df, active, by='LoanID', all.x=TRUE)

  df$active <- nazero(df$active)


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
  plot(df.current$pd, pd_current_test)
  abline(a=0, b=1)
  abline(v=0.2, h=0.2)

#----------------------
# Watch list to WO
#----------------------
  df.wl <- filter(df, active==0, watch_list==1, risk_category==1)
  modelColsWL <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.wl[,names(df.wl) %in% modelColsWL]
  glm.wl <- glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  pd_wl <- predict(glm.wl, df.wl, family='binomial', type='response')
  mean_pred$wl <- mean(pd_wl)
  mean_pred$wl[2] <-sum(df.wl$balance[df.wl$WO=='Writeoff']) / sum(df.wl$balance)
  summary(df.model$WO)
  plot(df.wl$pd, pd_wl)
  abline(a=0, b=1)
#----------------------
# Received payment to WO
#----------------------
  df.pmt1k <- filter(df, active==0, pmt_1k==1, risk_category==1, watch_list==0)
  modelColsWL <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.pmt1k[,names(df.pmt1k) %in% modelColsWL]
  glm.pmt1k <- glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  pd_pmt1k <- predict(glm.pmt1k, df.pmt1k, family='binomial', type='response')
  mean_pred$pmt_1k <- mean(pd_pmt1k)
  mean_pred$pmt_1k[2] <-sum(df.pmt1k$balance[df.pmt1k$WO=='Writeoff']) / sum(df.pmt1k$balance)
  summary(df.model$WO)
  plot(df.pmt1k$pd, pd_pmt1k)
  abline(a=0, b=1)

#----------------------
# SM to WO            #
#----------------------
  # first need to filter for loans that made it to SM (and thus have a chance of migrating)
  df.sm <- filter(df, active==0, Special_Mention==1, months_in_risk_cat<12)
  modelColsSM <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.sm[,names(df.sm) %in% modelColsSM]
  glm.sm <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  pdSMWO <- predict(glm.sm, df.sm, family='binomial', type='response')
  mean_pred$SM <- mean(pdSMWO)
  mean_pred$SM[2] <- sum(df.sm$balance[df.sm$WO=='Writeoff']) / sum(df.sm$balance)
  plot(df.sm$pd, pdSMWO)


#----------------------
# Substandard to WO   #
#----------------------
  df.sub <- filter(df, active==0, Substandard==1)
  modelColsSM <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.sub[,names(df.sub) %in% modelColsSM]
  glm.sub <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  pd_sub <- predict(glm.sub, df.sub, family='binomial', type='response')
  mean_pred$substandard <- mean(pd_sub)
  mean_pred$substandard[2] <-sum(df.sub$balance[df.sub$WO=='Writeoff']) / sum(df.sub$balance)



#----------------------
# Doubtful to WO      #
#----------------------
  df.doubt <- filter(df, active==0, Doubtful==1)
  modelColsSM <- c("WO",
                    "pd",
                    'months_in_risk_cat',
                    'months_in_risk_cat_sq')
  df.model <- df.doubt[,names(df.doubt) %in% modelColsSM]
  glm.doubt <-glm(WO ~ ., data=df.model, family='binomial', na.action=na.exclude)
  # summary(glm.doubt)
  pd_doubt <- predict(glm.doubt, df.doubt, family='binomial', type='response')
  mean_pred$doubtful <- mean(pd_doubt)
  mean_pred$doubtful[2] <-sum(df.doubt$balance[df.doubt$WO=='Writeoff']) / sum(df.doubt$balance)
  plot(df.doubt$pd, pd_doubt)



change <- ggplot(filter(data.frame(df.sm, pdSMWO), months_in_risk_cat<20),
  aes(x=months_in_risk_cat, y=pdSMWO, group=LoanID))
change + geom_line(aes(color=LoanID))

sum(df.sm$balance * pdSMWO) / sum(df.sm$balance)
sum(df.sm$balance[df.sm$WO=='Writeoff']) / sum(df.sm$balance)

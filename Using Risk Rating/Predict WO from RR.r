# Predict writeoffs given risk rating

options(scipen=99, digits=3)

wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
            "Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation",
            sep="")
setwd(wd)
#filename <-  "https://rootcapital.box.com/shared/static/d7q5d7pfnvzao08x4ev7af4rj7bm7hve.csv"
filename <-  "predicted_default.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
            "Working Folders for RAP Modules/Risk Profile/Loss Model/2.Model Selection",
            sep="")
setwd(wd)


df.rap$RiskRating <- as.factor(df.rap$Risk.Rating.Category)
df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy)

df.rap.active <- df.rap[which(df.rap$active==1 & df.rap$balance_0915>0 & df.rap$last_year==1),]
df.rap.inactive <- df.rap[which(df.rap$active==0 & df.rap$last_year==1),]

table(df.rap$RiskRating)

# Model #
##################
  modelColsSM <- c("WriteoffsDummy",
                   "RiskRating")
  
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSM]
  glmRR <- glm(WriteoffsDummy ~ RiskRating, data=df.model, family='binomial',na.action=na.exclude)
  summary(glmRR)
  df.rap.inactive$pd_RR <- predict(glmRR, df.rap.inactive, family='binomial', type='response')
  df.rap.active$pd_RR <- predict(glmRR, df.rap.active, family='binomial', type='response')
  df.rap$pd_RR <- predict(glmRR, df.rap, family='binomial', type='response')
  #####

# Write table of RR pds
outputTable <- table(df.rap$pd_RR)
write.csv(outputTable, 'risk_rating_category_to_writeoffs.csv')

# Write csv of all RR pds, balance, and sector for MC loss sim
  wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
            "Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation",
            sep="")
  setwd(wd)  
  lossOutCols <- c('pd_RR', 'balance_0915', 'Sector.and.Perishability', 'LoanID', 'last_year',
                   'Loan.Type', 'amount', 'September_Risk_Category',
                   'Loan.Type')
  out <- df.rap.active[,names(df.rap.active) %in% lossOutCols]
  write.csv(out, 'rr_predicted_default.csv')
  write.csv(df.rap.active,'rr_predicted_default_all.csv') # This is for the underwriting to active migration

  
# Linear Model #
##################

df.rap$RiskRating <- as.numeric(df.rap$Risk.Rating.Category)
df.rap$WriteoffsDummy <- as.numeric(df.rap$WriteoffsDummy) - 1

df.rap.active <- df.rap[which(df.rap$active==1 & df.rap$balance_0915>0 & df.rap$last_year==1),]
df.rap.inactive <- df.rap[which(df.rap$active==0 & df.rap$last_year==1),]

table(df.rap$RiskRating)

  modelColsSM <- c("WriteoffsDummy",
                   "RiskRating")
  
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSM]
  lmRR <- lm(WriteoffsDummy ~ RiskRating, data=df.model, na.action=na.exclude)
  summary(lmRR)
  df.rap$pd_RR_lm <- predict(lmRR, df.rap,  type='response')
  #####

outputTable <- table(df.rap$pd_RR_lm)
write.csv(outputTable, 'risk_rating_category_to_writeoffs_LM.csv')


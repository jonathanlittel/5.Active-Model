# First need to run # Migration to PD to risk category monitoring file, to get the glms for each migration
# Those are 


# Read rap dataset and subset to one row per loan
  wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
            "Working Folders for RAP Modules/Risk Profile/PD Model/",
            "3.Outputs",
            sep="")
  setwd(wd)
  filename <-  "rap_data_02.26.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")
  rap <- subset(df.rap, last_year==1)
  wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
              "Working Folders for RAP Modules/Risk Profile/PD Model/",
              "/2.Model Selection",
              sep="")
  setwd(wd)

# Load risk categories for past years
  filename <- 'September Risk Ratings from Adaptive.csv'
  past_risk_cat_data <- read.csv(filename, header=TRUE, sep=",")

# Load balances for past years (to deterimine if loan was active / current)
  filename <- 'September_Balances_by_loan_id.csv'
  past_loan_bal_data <- read.csv(filename, header=TRUE, sep=",")
  
# Merge data with rap dataset
  rap <- merge(rap,past_risk_cat_data, by=c("LoanID"),all.x=TRUE)
  rap <- merge(rap,past_loan_bal_data, by=c("LoanID"),all.x=TRUE)
  
# Create dummy for past risk cat by year
  rap$active_0911 <- ifelse(rap$balance_0911>0,1,0)
  rap$active_0912 <- ifelse(rap$balance_0912>0,1,0)
  rap$active_0913 <- ifelse(rap$balance_0913>0,1,0)
  rap$active_0914 <- ifelse(rap$balance_0914>0,1,0)
  rap$active_0915 <- ifelse(rap$balance_0915>0,1,0)
  

  rap$Current_0911 <- ifelse(rap$risk_cat_9_2011==0,
                             ifelse(rap$active_0911,1,0),
                                    0)
  rap$Special_Mention_0911 <- ifelse(rap$risk_cat_9_2011==6,1,0)
  rap$Substandard_0911 <- ifelse(rap$risk_cat_9_2011==12,1,0)
  rap$Doubtful_0911 <- ifelse(rap$risk_cat_9_2011==18,1,0)

  rap$Current_0912 <- ifelse(rap$risk_cat_9_2012==0,
                             ifelse(rap$active_0912,1,0),
                             0)
  rap$Special_Mention_0912 <- ifelse(rap$risk_cat_9_2012==6,1,0)
  rap$Substandard_0912 <- ifelse(rap$risk_cat_9_2012==12,1,0)
  rap$Doubtful_0912 <- ifelse(rap$risk_cat_9_2012==18,1,0)

  rap$Current_0913 <- ifelse(rap$risk_cat_9_2013==0,
                             ifelse(rap$active_0913,1,0),
                             0)
  rap$Special_Mention_0913 <- ifelse(rap$risk_cat_9_2013==6,1,0)
  rap$Substandard_0913 <- ifelse(rap$risk_cat_9_2013==12,1,0)
  rap$Doubtful_0913 <- ifelse(rap$risk_cat_9_2013==18,1,0)
  
  rap$Current_0914 <- ifelse(rap$risk_cat_9_2014==0,
                             ifelse(rap$active_0914,1,0),
                             0)
  rap$Special_Mention_0914 <- ifelse(rap$risk_cat_9_2014==6,1,0)
  rap$Substandard_0914 <- ifelse(rap$risk_cat_9_2014==12,1,0)
  rap$Doubtful_0914 <- ifelse(rap$risk_cat_9_2014==18,1,0)
 
  rap$Current_0915 <- ifelse(rap$risk_cat_9_2015==0,
                             ifelse(rap$active_0915,1,0),
                             0)
  rap$Special_Mention_0915 <- ifelse(rap$risk_cat_9_2015==6,1,0)
  rap$Substandard_0915 <- ifelse(rap$risk_cat_9_2015==12,1,0)
  rap$Doubtful_0915 <- ifelse(rap$risk_cat_9_2015==18,1,0)
 

# Order the factor, for plotting later
	df.rap$September_Risk_Category <- 
        	  ordered(df.rap$September_Risk_Category, 
          	  levels= c('Current', 'Special Mention', 'Substandard', 'Doubtful'))

# Start of Migration PD file, underwriting to active
# Subset to active / inactive

##############################
# IMPORTANT
# This section is manually looped by changing the variable names ending in eg _0911
# in order to get the active pd for each time
##############################
  df.rap.active <- rap[which(rap$active_0914==1),]   						# LOOP
  # df.rap.active <- subset(df.rap, active_0911==1) # same thing

############################################################
# Run prediction of above model on September Risk Category # 
############################################################

# WARNING #
# The dummies for Max_Risk_Category get altered here, to dummies for the current risk category
# This is for convenience, it'd be tricky to change the parameters of the model,
# you'd have to manually do the predict function rather than letting R do it.
  
  df.rap.active$Current <- df.rap.active$Current_0914                                                                                #LOOP
  df.rap.active$Special_Mention <- df.rap.active$Special_Mention_0914  				#LOOP
  df.rap.active$Substandard <- df.rap.active$Substandard_0914 				#LOOP
  df.rap.active$Doubtful <- df.rap.active$Doubtful_0914  						#LOOP
  
  # Special Mention #
  ##################
#   modelColsSM <- c("Special_Mention", 
#                    "pd")
#   
#   df.model <- df.rap.active[,names(df.rap.active) %in% modelColsSM]
#   glmSM <- glm(Special_Mention ~ ., data=df.model, family='binomial', na.action=na.exclude)
#   summary(glmSM)
#   df.rap.inactive$pdSM <- predict(glmSM, df.rap.inactive, family='binomial', type='response')
#   df.rap.active$pdSM <- predict(glmSM, df.rap.active, family='binomial', type='response')
  df.rap.active$pdSM <- predict(glmSM, df.rap.active, family='binomial', type='response')
  df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
  df.rap.active$pdD <- predict(glmD, df.rap.active, family='binomial', type='response')
  df.rap.active$pdWO <- predict(glmW, df.rap.active, family='binomial', type='response')
  ############
  
  
# These are the means of the migration rates for loans in a given risk category historically
# Note that it is on the full dataset, before switching the risk categories to current status
	  # mean_SMtoSub <- mean(na.omit(df.rap$pdSub[df.rap$September_Risk_Category=='Special Mention']))
	  # mean_SubtoDoubtful <- mean(na.omit(df.rap$pdD[df.rap$September_Risk_Category=='Substandard']))
	  # mean_DoubtfultoWO <- mean(na.omit(df.rap$pdWO[df.rap$September_Risk_Category=='Doubtful']))
	  
	  # mean_CurrentToWriteoff <- mean_SMtoSub * mean_SubtoDoubtful * mean_DoubtfultoWO
	  # mean_SMToWriteoff <-  mean_SubtoDoubtful * mean_DoubtfultoWO

# Run pd through each equation
	# df.rap.active$pd_active_avg_downgrade <- 
	#   ifelse(df.rap.active$September_Risk_Category=='Current',
	#        df.rap.active$pdSM * mean_CurrentToWriteoff,
	#          ifelse(df.rap.active$September_Risk_Category=='Special Mention',
	#               df.rap.active$pdSub * mean_SMToWriteoff,
	#                    ifelse(df.rap.active$September_Risk_Category=='Substandard',
	#                           df.rap.active$pdD * mean_DoubtfultoWO,
	#                           ifelse(df.rap.active$September_Risk_Category=='Doubtful',
	#                                  df.rap.active$pdWO,
	#                                  NA))))



# Recode all risk cat dummies to 1, to get migration probs for active loans for 
# categories other than the one that they are currently in.

df.rap.active$Special_Mention <- 1
df.rap.active$Substandard <- 1
df.rap.active$Doubtful <- 1

df.rap.active$pdSM_migr <- predict(glmSM, df.rap.active, family='binomial', type='response')                       
df.rap.active$pdSub_migr <- predict(glmSub, df.rap.active, family='binomial', type='response')
df.rap.active$pdD_migr <- predict(glmD, df.rap.active, family='binomial', type='response')
df.rap.active$pdWO_migr <- predict(glmW, df.rap.active, family='binomial', type='response')

# Create active pd, with all loans migrating through to WO at their predicted migration probs
# Note that not modeling upgrade probability
# The migration prob for current risk cat should be the same, eg if in SM, pdSM == pdSM_migr
df.rap.active$pd_active_0914 <-   ifelse(df.rap.active$Current_0914==1,						# LOOP
         df.rap.active$pdSM_migr * df.rap.active$pdSub_migr * df.rap.active$pdD_migr * df.rap.active$pdWO_migr,		
         ifelse(df.rap.active$Special_Mention_0914 ==1,								# LOOP
                df.rap.active$pdSub_migr * df.rap.active$pdD_migr * df.rap.active$pdWO_migr,				
                ifelse(df.rap.active$Substandard_0914 ==1,									# LOOP
                       df.rap.active$pdD_migr * df.rap.active$pdWO_migr,
                       ifelse(df.rap.active$Doubtful_0914 ==1,									# LOOP
                              df.rap.active$pdWO_migr,
                              NA))))

# dollar weighted mean pds
sum(na.omit(df.rap.active$pd * df.rap.active$balance_0914))
sum(na.omit(df.rap.active$pd * df.rap.active$balance_0914)) / sum(na.omit(df.rap.active$balance_0914))
sum(na.omit(df.rap.active$pd_active_0915 * df.rap.active$balance_0915)) / sum(na.omit(df.rap.active$balance_0914))

# mean pds
mean(na.omit(df.rap.active$pd))
mean(na.omit(df.rap.active$pd_active_0914))

write.csv(df.rap.active, 'Sept_2014_active_pds.csv')

# Select the migration prob for the current risk category
# df.rap.active$migration_to_next_cat_prob <- 
#   ifelse(df.rap.active$September_Risk_Category=='Current',df.rap.active$pdSM,
#        ifelse(df.rap.active$September_Risk_Category=='Special Mention', df.rap.active$pdSM,
#               ifelse(df.rap.active$September_Risk_Category=='Substandard', df.rap.active$pdSub,
#                      ifelse(df.rap.active$September_Risk_Category=='Doubtful', df.rap.active$pdD,
#                             NA))))

# Check some things out
# df.rap.active$EL <- df.rap.active$pd_active * df.rap.active$balance_0915
# sum(na.omit(df.rap.active$EL))
# sum(na.omit(df.rap.active$EL)) / sum(na.omit(df.rap.active$balance_0915))
# mean(na.omit(df.rap.active$pd))
# mean(na.omit(df.rap.active$pd_active))

# sum(na.omit(df.rap.active$pd_active * df.rap.active$balance_0915)) 
# sum(na.omit(df.rap.active$pd_active[df.rap.active$September_Risk_Category=='Current'] * 
#               df.rap.active$balance_0915[df.rap.active$September_Risk_Category=='Current']))

# sum(na.omit(df.rap.active$pd_active[!df.rap.active$September_Risk_Category=='Current'] * 
#               df.rap.active$balance_0915[!df.rap.active$September_Risk_Category=='Current']))

# sum(df.rap.active$balance_0915[df.rap.active$September_Risk_Category=='Current'])


# sum(na.omit(df.rap.active$pd_active*df.rap.active$balance_0915)) / 
#   sum(df.rap.active[!is.na(df.rap.active$pd_active),"balance_0915"])

# mean(na.omit(df.rap.active$pd_active[df.rap.active$active==1]))


# outputCols <- c('LoanID', 'pd', 'pd_active', 'Amount', 'Sector.and.Perishability',
#                 'last_year', 'September_Risk_Category', 'balance_0915',
#                 'Loan.Type')
# df.output <- df.rap.active[,names(df.rap.active) %in% outputCols]



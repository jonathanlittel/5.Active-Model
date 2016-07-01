# Load file and set options
#####
  options(scipen=99, digits=3)
  library(dplyr)
  
# Load data 
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)

# Load underwriting Model pds  
  filename <-  "pds_05.28.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")
  
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/5.Active Model"
  setwd(wd)

# Create four dummies for risk categories, which assumes anything in a higher category also hit a lower category
  # df.rap$Max_Risk_Category <- factor(df.rap$Max_Risk_Category, 
  #   levels = c("Current", "Special Mention", "Substandard", "Doubtful"),
  #   labels = c("Current", "Special Mention", "Substandard", "Doubtful")
  #   )

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


# Recode Substandard and Doubtful dummies (originally calculated from Max_Risk_Category) to add the imputed WOs
    df.rap$Special_Mention <- ifelse(df.rap$WO == 1, 1, df.rap$Special_Mention)          
    df.rap$Substandard <- ifelse(df.rap$WO == 1, 1, df.rap$Substandard)
    df.rap$Doubtful <- ifelse(df.rap$WO == 1, 1, df.rap$Doubtful)


  df.rap <- df.rap %>%
            mutate(Special_Mention = replace(Special_Mention, WO==1, 1)) %>%
            mutate(Substandard = replace(Substandard, WO==1, 1)) %>%
            mutate(Doubtful = replace(Doubtful, WO==1, 1)) 

# # Order the factor, for plotting later
# df.rap$Risk_Category <- 
#           ordered(df.rap$Risk_Category, 
#             levels= c('Current', 'Special Mention', 'Substandard', 'Doubtful'))

############
# Subset to active / inactive
  # df.rap.active <- df.rap[which(df.rap$active==1 & df.rap$last_year==1),]
  df.rap.inactive <- filter(df.rap, active==0)

# Prob of migration to #
# Special Mention #
##################
  modelColsSM <- c("Special_Mention", 
                   "pd")
  
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSM]
  glmSM <- glm(Special_Mention ~ ., data=df.model, family='binomial', na.action=na.exclude)
  summary(glmSM)
  df.rap.inactive$pmSM <- predict(glmSM, df.rap.inactive, family='binomial', type='response')
  # df.rap.active$pdSM <- predict(glmSM, df.rap.active, family='binomial', type='response')
  # df.rap$pmSM <- predict(glmSM, df.rap, family='binomial', type='response')
  ############

# Prob of migration to #
# Substandard #
###############
  # first need to filter for loans that made it to SM (and thus have a chance of migrating)
  df.rap.inactive <- filter(df.rap.inactive, Special_Mention==1)
  modelColsSub <- c("Substandard",
                    "pmSM")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSub]
  glmSub <-glm(Substandard ~ ., data=df.model, family='binomial', na.action=na.exclude)
  summary(glmSub)
  # df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
  df.rap.inactive$pmSubstandard <- predict(glmSub, df.rap.inactive, family='binomial', type='response')
  # df.rap$pmSubstandard <- predict(glmSub, df.rap, family='binomial', type='response')
  ############

# Prob of migration to #
# Doubtful #
############
  df.rap.inactive <- filter(df.rap.inactive, Substandard==1)
  modelColsD <- c("Doubtful",
                  "pmSubstandard")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsD]
  glmD <-glm(Doubtful ~ ., data=df.model, family='binomial',na.action=na.exclude)
  summary(glmD)
  # df.rap.active$pdD <- predict(glmD, df.rap.active, family='binomial', type='response')
  df.rap.inactive$pmDoubtful <- predict(glmD, df.rap.inactive, family='binomial', type='response')
  # df.rap$pmDoubtful <- predict(glmD, df.rap, family='binomial', type='response')
  ############

# Prob of migration to #
# Writeoff #
############
  df.rap.inactive <- filter(df.rap.inactive, Doubtful==1)  
  modelColsW <- c("WO",
                  "pmDoubtful")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsW]
  glmW <-glm(WO ~ ., data=df.model, family='binomial',na.action=na.exclude)
  summary(glmW)
  # df.rap.active$pdWO <- predict(glmW, df.rap.active, family='binomial', type='response')
  df.rap.inactive$pmWO <- predict(glmD, df.rap.inactive, family='binomial', type='response')
  # df.rap$pmWO <- predict(glmW, df.rap, family='binomial', type='response')
  ############

  # # Select the pm for the actual risk category
  # df.rap$pm_active <- ifelse(df.rap$September.Risk.Category==0,df.rap$pmSM,
  #                            ifelse(df.rap$September.Risk.Category==6, df.rap$pmSM,
  #                                   ifelse(df.rap$September.Risk.Category==12, df.rap$pmSub,
  #                                          ifelse(df.rap$September.Risk.Category==18, df.rap$pmD,
  #                                                 NA))))
  #   
  
# Predict all migration probs
  df.predict <- df.rap
  df.predict$Special_Mention <- 1
  df.predict$Substandard <- 1
  df.predict$Doubtful <- 1

  df.rap$pmSM <- predict(glmSM, df.rap, family='binomial', type='response')
  df.rap$pmSubstandard <- predict(glmSub, df.rap, family='binomial', type='response')
  df.rap$pmDoubtful <- predict(glmD, df.rap, family='binomial', type='response')
  df.rap$pmWO <- predict(glmW, df.rap, family='binomial', type='response')
  
  df.rap <- df.rap %>%
                mutate(pdSM =  pmSubstandard * pmDoubtful * pmWO) %>%
                mutate(pdSubstandard =  pmDoubtful * pmWO ) %>%
                mutate(pdDoubtful =  pmWO) %>%
                mutate(pdx = pmSM * pmSubstandard * pmDoubtful * pmWO)



head(select(df.rap,  pmSM, pdSM, pmSubstandard, pdSubstandard, pmDoubtful, pd, pdx))

pms <- select(df.rap, pmSM, pmSubstandard, pmDoubtful, pmWO)
pds <- select(df.rap, pd, pdSM, pdSubstandard, pdDoubtful, pdx)
apply(pms, 2, mean)
apply(pds, 2, mean)
plot(pds$pd, pds$pdx)
abline(lsfit(pds$pd, pds$pdx), col='blue')


pds <- select(df.rap, LoanID,  pd, pdSM, pdSubstandard, pdDoubtful, pdx)

library(tidyr)
pds_long <- pds %>%
              gather(migration, pds, pd:pdx)

t <- ggplot(pds_long, aes(x=migration, y=pds))
    t + geom_jitter(aes(colour=LoanID))

t <- ggplot(pds_long, aes(x=migration))
    t + geom_jitter(aes(y=pds, colour=LoanID))


//// halt execution here 
-----1 next part is to run model at various points in time


############################################################
# Run prediction of above model on September Risk Category # 
############################################################

# WARNING #
# The dummies for Max_Risk_Category get altered here, to dummies for the current risk category
# This is for convenience, it'd be tricky to change the parameters of the model,
# you'd have to manually do the predict function rather than letting R do it.
  
  # df.rap.active$Current <- df.rap.active$Current_now
  df.rap$Special_Mention <- 1
  df.rap$Substandard <- 1
  df.rap$Doubtful <- 1
  
  df.rap.active$pmSM <- predict(glmSM, df.rap.active, family='binomial', type='response')
  df.rap.active$pmSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
  df.rap.active$pmD <- predict(glmD, df.rap.active, family='binomial', type='response')
  df.rap.active$pmWO <- predict(glmW, df.rap.active, family='binomial', type='response')
  ############
  
  df.rap$pmSM <- predict(glmSM, df.rap, family='binomial', type='response')
  df.rap$pmSub <- predict(glmSub, df.rap, family='binomial', type='response')
  df.rap$pmD <- predict(glmD, df.rap, family='binomial', type='response')
  df.rap$pmWO <- predict(glmW, df.rap, family='binomial', type='response')
  ############
  
# These are the means of the migration rates for loans in a given risk category historically
# Note that it is on the full dataset, before switching the risk categories to current status
  mean_SMtoSub <- mean(na.omit(df.rap$pmSub[df.rap$Risk_Category=='Special Mention']))
  mean_SubtoDoubtful <- mean(na.omit(df.rap$pmD[df.rap$Risk_Category=='Substandard']))
  mean_DoubtfultoWO <- mean(na.omit(df.rap$pmWO[df.rap$Risk_Category=='Doubtful']))
  
  mean_CurrentToWriteoff <- mean_SMtoSub * mean_SubtoDoubtful * mean_DoubtfultoWO
  mean_SMToWriteoff <-  mean_SubtoDoubtful * mean_DoubtfultoWO

# Run pd through each equation
df.rap.active$pd_active_avg_downgrade <- 
  ifelse(df.rap.active$Risk_Category=='Current',
       df.rap.active$pdSM * mean_CurrentToWriteoff,
         ifelse(df.rap.active$Risk_Category=='Special Mention',
              df.rap.active$pdSub * mean_SMToWriteoff,
                   ifelse(df.rap.active$Risk_Category=='Substandard',
                          df.rap.active$pdD * mean_DoubtfultoWO,
                          ifelse(df.rap.active$Risk_Category=='Doubtful',
                                 df.rap.active$pdWO,
                                 NA))))

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
df.rap.active$pd_active <- 
  ifelse(df.rap.active$Risk_Category=='Current',
         df.rap.active$pdSM_migr * df.rap.active$pdSub_migr * df.rap.active$pdD_migr * df.rap.active$pdWO_migr,
         ifelse(df.rap.active$Risk_Category=='Special Mention',
                df.rap.active$pdSub_migr * df.rap.active$pdD_migr * df.rap.active$pdWO_migr,
                ifelse(df.rap.active$Risk_Category=='Substandard',
                       df.rap.active$pdD_migr * df.rap.active$pdWO_migr,
                       ifelse(df.rap.active$Risk_Category=='Doubtful',
                              df.rap.active$pdWO_migr,
                              NA))))

# remove these dummies that were recoded to 1, to avoid potential confusion
  df.rap.active$Doubtful <- NULL
  df.rap.active$Substandard <- NULL
  df.rap.active$Special_Mention <- NULL


# Select the migration prob for the current risk category
df.rap.active$migration_to_next_cat_prob <- 
  ifelse(df.rap.active$Risk_Category=='Current',df.rap.active$pdSM,
       ifelse(df.rap.active$Risk_Category=='Special Mention', df.rap.active$pdSM,
              ifelse(df.rap.active$Risk_Category=='Substandard', df.rap.active$pdSub,
                     ifelse(df.rap.active$Risk_Category=='Doubtful', df.rap.active$pdD,
                            NA))))

# Check some things out
    df.rap.active$EL <- df.rap.active$pd_active * df.rap.active$balance_1215
    sum(na.omit(df.rap.active$EL))
    sum(na.omit(df.rap.active$EL)) / sum(na.omit(df.rap.active$balance_1215))
    mean(na.omit(df.rap.active$pd))
    mean(na.omit(df.rap.active$pd_active))

  sum(na.omit(df.rap.active$pd_active * df.rap.active$balance_1215)) 
  sum(na.omit(df.rap.active$pd_active[df.rap.active$Risk_Category=='Current'] * 
                df.rap.active$balance_1215[df.rap.active$Risk_Category=='Current']))

  sum(na.omit(df.rap.active$pd_active[!df.rap.active$Risk_Category=='Current'] * 
                df.rap.active$balance_1215[!df.rap.active$Risk_Category=='Current']))

  sum(df.rap.active$balance_1215[df.rap.active$Risk_Category=='Current'])


  sum(na.omit(df.rap.active$pd_active*df.rap.active$balance_1215)) / 
    sum(df.rap.active[!is.na(df.rap.active$pd_active),"balance_1215"])

  mean(na.omit(df.rap.active$pd_active[df.rap.active$active==1]))


# Write output files of PDs, model, and save an rds file
  outputCols <- c('LoanID', 'Sector.and.Perishability', 'Loan.Type',  # Could consider putting the migration probabilities.
                  'Risk_Category', 'balance_1215', 'pd', 'pd_active',
                  'EAD', 'Amount', 'LGD')         
  df.output <- df.rap.active[,names(df.rap.active) %in% outputCols]
  # library(dplyr)
  # df.output<- df.rap.active %>%
  #       select(one_of(outputCols))

  write.csv(df.output, 'risk_profile_pds.csv')
  file.show('risk_profile_pds.csv')
  saveRDS(c('glmD', 'glmD', 'glmSM', 'glmSub', 'glmW') , "active_glms.rds")
    # Write coefficient outputs
  migration.model.coefs <- c(coef(glmSM),coef(glmSub),coef(glmD),coef(glmW))
  migration.models <- cbind(migration.model.coefs)
  write.csv(migration.models,'migration model coefficients.csv') 

  
# To read back in:
# objectname <- readRDS("active_glms.rds")

# stop here"

# Make some plots with the results
# # Plot
# library(ggplot2)
# migrations_plot <- ggplot(data=df.rap.active[order(df.rap.active$pd),], aes(x=seq(1, length(df.rap.active$pd)), y=pd)) +
#   geom_point(aes(x=seq(1, length(df.rap.active$pd)), y=pd), colour=alpha('darkblue', 0.10)) +
#   geom_point(aes(x=seq(1, length(df.rap.active$pd)), y=pdSM), colour=alpha('grey', 0.30)) +
#   geom_point(aes(x=seq(1, length(df.rap.active$pd)), y=pdSub), colour=alpha('chocolate4', 0.50)) +
#   geom_point(aes(x=seq(1, length(df.rap.active$pd)), y=pdD), colour=alpha('forestgreen', 0.70))
# migrations_plot

# library(reshape2)
# dd.sub <- df.rap.active[,c('pd','pdSM','pdSub','pdD','pdWO', 'LoanID', 'Risk_Category')]
# dd <- melt(dd.sub, id=c('LoanID'))
# dd <- dd[order(dd$pd),]

# # Box plots using underwriting PD and active PD
# ggplot(dd.sub) + geom_boxplot(aes(x=Risk_Category, y=pd, colour=factor(Risk_Category)))
#     ggplot(dd.sub) + geom_boxplot(aes(x=Risk_Category, y=pd_active, colour=factor(Risk_Category)))


# ggplot(dd.sub) + geom_point(aes(x=LoanID, y=pd, colour=factor(Risk_Category)))

# # Using Active PD
#     dd.sub <- df.rap.active[,c('pd_active','pdSM','pdSub','pdD','pdWO', 'LoanID', 'Risk_Category')]
#     dd <- melt(dd.sub, id=c('LoanID'))
#     dd <- dd[order(dd$pd_active),]
    


# ggplot(dd.sub) + geom_boxplot(aes(x=seq(1, length(df.rap.active$pd)), y=pd, colour=pd))

# # +facet_wrap(~Risk_Category,nrow=1)

# pd_plot <- ggplot(data=df.rap.active[order(df.rap.active$pd_active),], aes( x = seq(1, length(df.rap.active$pd_active))) +
#                   geom_point(aes(x= seq(1, length(df.rap.active$pd_active)), 
#                              y=pd_active))
                   
#  # colour=factor(df.rap.active$Risk_Category)))
# pd_plot 

# facet
# Show contribution by current risk cat?
# # unordered
# # pd_plot <- ggplot(data=df.rap.active, aes(x=seq(1, length(df.rap.active$pd)), y=pd)) + geom_point()
# # pd_plot

# # Median PDs by september risk category
#   mean(na.omit(df.rap.active$pd[df.rap.active$Risk_Category=='Current']))
#   mean(na.omit(df.rap.active$pd[df.rap.active$Risk_Category=='Special Mention']))
#   mean(na.omit(df.rap.active$pd[df.rap.active$Risk_Category=='Substandard']))
#   mean(na.omit(df.rap.active$pd[df.rap.active$Risk_Category=='Doubtful']))
#   mean(na.omit(df.rap.active$pd[!df.rap.active$Risk_Category=='Current']))



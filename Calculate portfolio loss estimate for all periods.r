library(tidyr)
library(dplyr)
library(lubridate)

  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/5.Active Model"
  setwd(wd)
  	# Using the adaptive import template (deprecated)
	# filename <- 'Import_Template-Adaptive Risk Actuals 06.07.16.csv'
	# rcat <- read.csv(filename)
	# rcat <- rcat[,c(1,3:5)]
	# rcat$RiskDate <- as.Date(as.character(rcat$RiskDate),format='%m/%d/%Y')

	filename <- 'Risk_Actuals.csv'
	rcat <- read.csv(filename, skip=0)
	rcat <- rcat[,c(1,3:5)]
	rcat$RiskDate <- as.Date(as.character(rcat$RiskDate),format='%m/%d/%Y')
	# Change all dates to first day of month
	rcat$RiskDate <- floor_date(rcat$RiskDate, "month")



###  using adaptive report 	- balance only used	
	# 1. Run report Balance and risk category risk profile from lending adaptive
	# 2. format all columns as 'general'. Format first column as 'short date' (eg mm/dd/yyyy)
	# 3. save as csv

	filename <- 'Balance_and_risk_category_risk_profile.csv'
		bal <- read.csv(filename, skip=1)
		names(bal) <- c("time", 'LoanID', 'balance', 'risk_cat')
		bal$risk_cat <- NULL

	# format first column as date
		d <- paste('1-', bal$time, sep="")
		bal$time <- as.Date(as.character(d),format='%d-%b-%Y')
	# fill in missing values with previous value
		# bal$time <- ifelse(bal$time=="1-", NA, bal$time)
		bal <- bal %>%
				fill(time)
	# remove the two padding rows, and last row (which is a subtotal)
		bal <- bal[-c(1:2,nrow(bal)),]

	# remove the rollups
		bal <- filter(bal, LoanID!='Total LoanID (Rollup)')	
		bal <- filter(bal, LoanID!='LoanID (Rollup)')	
		bal <- filter(bal, LoanID!='LoanID (Uncategorized)')

	# change type of any columns needed
		str(bal)
		bal$LoanID <- as.numeric(as.character(bal$LoanID))
###########

# load guarantees and merge with pds
	filename <- 'List of 3rd Party Guarantees_12-30-2015.csv'
	guar <- read.csv(filename, skip=1)
	guar <- guar[,c(1:2,4:5)]
	names(guar) <- c('closeDate', 'LoanID', 'guarantee', 'notes' )
	last_row <- min(match(NA, guar$LoanID)) - 1 # returns row before first NA in LoanID
	tail(guar)
	guar <- guar[1:(last_row),]
	#replace % character with nothing, format to # and divide by 100
	guar$guarantee <- as.numeric(gsub("%","", guar$guarantee)) / 100

# load pds 
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)
  filename <-  "risk_profile_pds_05.28.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")

 # merge pds and guarantees
 	df.rap <- merge(df.rap, guar, by='LoanID', all.x=TRUE)
 	rap <- select(df.rap, LoanID, RCOppNumber=RC.Opp.Number, pd, pdSMWO, pdSubWO, pdDWO, WO, guarantee, notes)

# Merge with balance
 	rap$LoanID <- as.numeric(rap$LoanID)
	pro <- merge(bal, rap, by='LoanID', all=TRUE)
	pro[pro$LoanID==615,]

# Merge pds/guarantes with risk cat
	rcat <- rename(rcat, time=RiskDate)
	rcat$RiskCategory[rcat$RiskCategory=="Substandard "] <- "Substandard"
	pro <- merge(pro, rcat, by=c('RCOppNumber', 'time'), all.x=TRUE)


# clean up NAs, change NA RiskCategory with balance to current,
# and change balances <500 to 0 (including negatives)
	pro <- pro %>%
				mutate(guarantee = replace(guarantee, is.na(guarantee), 0)) %>%
				mutate(RiskCategory = replace(RiskCategory, balance>=0 & is.na(RiskCategory), 'Current')) %>%
				mutate(balance = replace(balance, balance<500, 0))
	
	sum(pro$balance[pro$time=='2015-12-01'], na.rm=TRUE)
	sum(pro$balance[pro$time=='2015-11-01'], na.rm=TRUE)
	sum(pro$balance[pro$time=='2014-01-01'], na.rm=TRUE)	
	missing <- pro$LoanID[is.na(pro$time)]
	missing <- pro$LoanID[is.na(pro$pd)]

# Create pd_profile, which is the pd given the risk category
	pro$pd_profile <- pro$pd
	pro$pd_profile <- ifelse(pro$RiskCategory=='Special Mention', pro$pdSMWO, pro$pd_profile)
	pro$pd_profile <- ifelse(pro$RiskCategory=='Substandard', pro$pdSubWO, pro$pd_profile)	
	pro$pd_profile <- ifelse(pro$RiskCategory=='Doubtful', pro$pdDWO, pro$pd_profile)		

# Create LGD and EL
	lgd <- 0.90
	pro <- pro %>%
			mutate(EL = pd_profile * ( balance * (1 - guarantee) ) * lgd )

# subtotal portfolio by month
	p_total <- pro %>%
			group_by(time) %>%
			summarise(p_balance_all = sum(balance, na.rm=TRUE))

# subtotal portfolio by month for loans with PDs
	p_loss <- pro %>%
			group_by(time) %>%
			filter(!is.na(pd_profile)) %>%
			summarise(EL_portfolio = sum(EL, na.rm=TRUE), port_balance = sum(balance, na.rm=TRUE)) %>%
			mutate(port_loss = EL_portfolio / port_balance)

	p_loss <- merge(p_loss, p_total, by='time')
	p_loss$portfolio_with_data <- p_loss$port_balance / p_loss$p_balance_all

library(ggplot2)
library(ggthemes)
library(scales)
p <- ggplot(p_loss, aes(x=time, y=port_loss))
p + geom_line()

p <- ggplot(p_loss, aes(x=time, y=port_balance))
p + geom_line()

p <- ggplot(p_loss, aes(x=time))
p + geom_line(aes(y=port_loss)) + geom_line(aes(y=p_balance_all), color='green')

p <- ggplot(p_loss, aes(x=time))
p + geom_line(aes(y=port_loss, color=portfolio_with_data)) + scale_y_continuous(labels = scales::percent)

p <- ggplot(filter(p_loss, time>'2011-06-01', time<'2016-01-01'), aes(x=time, color=portfolio_with_data))
p <- p + geom_line(aes(y=port_loss), size=1) + scale_y_continuous(labels = scales::percent,
					 name='Expected Loss of Portfolio',
					 breaks=c(seq(0.04, 0.18, by=0.01))) 
p + guides(colour=guide_legend(title="% of portfolio with data")) + xlab(NULL) #+ theme_bw()

# table of % of portfolio by time by category
	pro2 <- pro %>%
			group_by(time) %>%
			mutate(portfolio_total = sum(balance))

	comp <- pro2 %>%
			group_by(time, RiskCategory) %>%
			summarise(cat_total = sum(balance), balance = mean(portfolio_total)) %>%
			mutate(cat_prop = cat_total / balance)


comp$RiskCategory <- factor(comp$RiskCategory,
	levels=c('Current', 'Special Mention', 'Substandard', 'Doubtful'))

# comp$RiskCategory <- reorder(comp$RiskCategory, comp$cat_prop, mean)

q <- ggplot(filter(comp, RiskCategory!='Current'), aes(x=time, y=cat_prop, fill=RiskCategory, order=as.numeric(RiskCategory)))
q + geom_bar(stat='identity') + scale_y_continuous(labels = scales::percent,
					 name='Portfolio by Risk Cateogry',
					 breaks=c(seq(0.00, 0.30, by=0.05))) 

q <- ggplot(filter(comp, RiskCategory!='Current'), aes(x=time, y=cat_prop, fill=RiskCategory))
q + geom_bar(stat='identity') + scale_y_continuous(labels = scales::percent,
					 name='Portfolio by Risk Cateogry',
					 breaks=c(seq(0.00, 0.30, by=0.05))) 

# q <- ggplot(comp, aes(x=time, y=cat_prop, fill=factor(RiskCategory)))
# q + geom_bar(stat='identity')


# + scale_colour_hc() + scale_colour_solarized("blue")
out <- dplyr::select(pro, LoanID, time, balance, pd:RiskCategory, pd_profile, EL)
write.csv(out, 'expected_loan_loss_by_month.csv')
write.csv(p_loss, 'expected_portfolio_loss_by_month.csv')

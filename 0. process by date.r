# prepare risk profile data
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
# Read data
  wd <- paste('C:/Box Sync/Risk Appetite - Provisioning Project/',
    'Working Folders for RAP Modules/Risk Profile/PD Model/',
    '5.Active Model/data',
    sep = '')
  setwd(wd)

  # payment data
  filename <- 'disb_rpt.csv'
  df.pmt <- read.csv(filename)
  df.pmt <- select(df.pmt, LoanID = Loan.ID, txn_type = Transaction..Record.Type,
  	date = Date, disbursed = Principal.Disbursed, paid = Principal.Paid)
  # monitoring score data
  filename <- 'monitor_cat.csv'
  df.cat <- read.csv(filename)
  df.risk <- select(df.cat, LoanID = Loan.ID, date=Rating.Date, 
  	monitor_score = Final.Risk.Score, risk_category = Final.Risk.Category)

  # maturity date data
  filename <- 'maturity_date.csv'
  df.maturity <- read.csv(filename)
  df.maturity <- select(df.maturity,
  	LoanID = Loan.ID,
  	date = Report.Date,
  	maturity_date = Maturity.Date,
  	outstanding_principal = Outstanding.Principal)

# format date
  df.pmt$date <- as.Date(df.pmt$date, "%m/%d/%Y")
  df.risk$date <- as.Date(df.risk$date, "%m/%d/%Y")
  df.maturity$date <- as.Date(df.maturity$date, "%m/%d/%Y")
  df.maturity$maturity_date <- as.Date(df.maturity$maturity_date, "%m/%d/%Y")
# load balance data
###  using adaptive report 	- balance only used	field
	# 1. Run report Balance and risk category risk profile from lending adaptive
	# 2. format all columns as 'general'. Format first column as 'short date' (eg mm/dd/yyyy)
	# 3. save as csv
	filename <- 'Balance_and_risk_category_risk_profile.csv'
		bal <- read.csv(filename, skip=1)
		names(bal) <- c("date", 'LoanID', 'balance', 'risk_cat')
		bal$risk_cat <- NULL
	# format first column as date
		d <- paste('1-', bal$date, sep="")
		bal$date <- as.Date(as.character(d),format='%d-%b-%Y')
	# fill in missing values with previous value
		# bal$date <- ifelse(bal$date=="1-", NA, bal$date)
		bal <- bal %>%
				fill(date)
	# remove the two padding rows, and last row (which is a subtotal)
		bal <- bal[-c(1:2,nrow(bal)),]
	# remove the rollups
		bal <- filter(bal, LoanID!='Total LoanID (Rollup)')	
		bal <- filter(bal, LoanID!='LoanID (Rollup)')	
		bal <- filter(bal, LoanID!='LoanID (Uncategorized)')
	# change type of any columns needed
		str(bal)
		bal$LoanID <- as.numeric(as.character(bal$LoanID))
		# bal$balance[is.na(bal$LoanID)] # some have no loan id but have 0 bal
		bal <- filter(bal, !is.na(LoanID))
# function to convert to last day of month
	last_day <- function(date) {
	    ceiling_date(date, "month") - days(1)
	}

# change bal$date from 1st day in month to last day
	bal$date <- last_day(bal$date)
# flatten df.risk and df.pmt to one row per month
  df.risk.flat <- df.risk %>%
  			arrange(LoanID, date) %>%
  			mutate(date_last_day = last_day(date)) %>% 
  			group_by(LoanID, date_last_day) %>%
  			filter(row_number()==n())  
  			# filter for single row per year-month (most recent)
  			# removes ~800 rows
  df.pmt$paid[is.na(df.pmt$paid)] <- 0
  df.pmt$disbursed[is.na(df.pmt$disbursed)] <- 0
  # df.pmt[is.na(df.pmt)] <- 0 # could probably just do this to whole df
  df.pmt.flat <- df.pmt %>%
  			arrange(LoanID, date) %>%
  			group_by(LoanID) %>%
  			mutate(paid_cum = cumsum(paid),
  				   disb_cum = cumsum(disbursed),
  				   date_last_day = last_day(date)) %>%
  			group_by(LoanID, date_last_day) %>%
  			filter(row_number()==n())    			
  			# filter for single row per year-month (most recent)
  			# removes ~9000 rows

  df.pmt.flat$date <- df.pmt.flat$date_last_day
  df.risk.flat$date <- df.risk.flat$date_last_day

# merge balance, risk cat, and payment
  df <- merge(bal, df.risk.flat, by=c('LoanID', 'date'), all.x=TRUE)
  df2 <- merge(df, df.pmt.flat, by=c('LoanID', 'date'), all=TRUE)
  df3 <- merge(df2, df.maturity, by=c('LoanID', 'date'), all=TRUE)
  df3 <- select(df3, -txn_type, -date_last_day.y, -date_last_day.x)
  df <- df3
  sum(duplicated(df3[,c('LoanID', 'date')]))
# Fill data for all periods
  df <- df %>%
  		arrange(LoanID, date) %>%
  		group_by(LoanID) %>%
  		fill(monitor_score, risk_category, .direction='down') 

# load and merge pds 
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)
  filename <-  "risk_profile_pds_06.22.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")
  df.rap$Close.Date <- as.Date(df.rap$Close.Date, "%m/%d/%Y")
  df.rap$Maturity.at.Origination <- as.Date(df.rap$Maturity.at.Origination, "%m/%d/%Y")  
  pds <- select(df.rap, LoanID, pd, pd_one_year, Close.Date, Maturity.at.Origination, active)
  df <- merge(df, pds, by=c('LoanID'), all.x=TRUE)

# check for NAs
	map_dbl(df, ~ sum(is.na(.))) 
# fill for missing
  df <- df %>%
  	arrange(LoanID, date) %>%
  	group_by(LoanID) %>%
  	fill( # fill below categories for later dates
  		outstanding_principal, paid_cum, disb_cum,
  		maturity_date, pd_one_year, risk_category,
  		.direction = c('down')
  		) %>%
  	fill( # fill below categories for earlier dates
  		risk_category, monitor_score,
  		.direction = c('up')
  		)

# filter for balance <200 (including negative)
  # df <- filter(df, balance>200)

# clean up df for NAs
	nazero <- function(x) {
		x[is.na(x)] <- 0
		x
	}

# count of months in risk category
  df <- df %>%
  	mutate(counter = 1) %>%
  	arrange(LoanID, date) %>%
    group_by(LoanID, risk_category) %>%
    mutate(months_in_risk_cat = cumsum(counter))

# create tenor at date
  # replace NA maturity date with Maturity.at.Origination
  df$maturity_date <- ifelse(is.na(df$maturity_date),
  	df$Maturity.at.Origination,
  	df$maturity_date)


# create two variables
  df$watch_list <- ifelse(df$risk_category=='Watch List', 1, 0)
  df$pmt_1k <- ifelse(df$paid_cum>=1000, 1, 0)


 

# check for NAs
	map_dbl(df, ~ sum(is.na(.))) 
 	df$pmt_1k <- nazero(df$pmt_1k)
 	df$watch_list <- nazero(df$watch_list)
# remove NA balances and balances < 200 (including negative)
	dim(df)
	df <- filter(df, balance>200)
	dim(df)	


# check balances by date
	plot(
		df %>%
		group_by(date) %>%
		summarise(total_balance = sum(balance, na.rm = TRUE))
		)

# -------------------------------------------------------------------
# Create four dummies for risk categories, which assumes anything in a higher category also hit a lower category
  df$risk_categor_temp <- -1
    df <- df %>% 
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_num=='Current', 0)) %>%
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_num=='Special Mention', 6)) %>%
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_num=='Substandard', 12)) %>%
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_num=='Doubtful', 18))

  df$risk_category_num <- factor(df$risk_categor_temp, 
    levels = c(0, 6, 12, 18),
    labels = c("Current", "Special Mention", "Substandard", "Doubtful")
    )   

  df$risk_category_num <- as.numeric(df$risk_categor_temp)

# Create dummies for max risk category
  df$Current <- 1
  df$Special_Mention <- 0
  df$Substandard <- 0
  df$Doubtful <- 0

  df$Special_Mention <- ifelse(df$risk_category_num>0,1,0)
  df$Substandard <- ifelse(df$risk_category_num>6,1,0)
  df$Doubtful <- ifelse(df$risk_category_num>12,1,0)

  df$Special_Mention <- ifelse(df$WO==1,1,df$Special_Mention)
  df$Substandard <- ifelse(df$WO==1,1,df$Substandard )
  df$Doubtful <- ifelse(df$WO==1,1,df$Doubtful )

  df$risk_category_num <- factor(df$risk_category_num, 
    levels = c(0, 6, 12, 18),
    labels = c("Current", "Special Mention", "Substandard", "Doubtful")
    )

  df <- df %>%
            mutate(Special_Mention = replace(Special_Mention, WO=="Writeoff", 1)) %>%
            mutate(Substandard = replace(Substandard, WO=="Writeoff", 1)) %>%
            mutate(Doubtful = replace(Doubtful, WO=="Writeoff", 1)) 	
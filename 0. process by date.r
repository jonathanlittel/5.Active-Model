# prepare risk profile data
library(dplyr)
library(tidyr)
library(lubridate)

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

# format date
  df.pmt$date <- as.Date(df.pmt$date, "%m/%d/%Y")
  df.risk$date <- as.Date(df.risk$date, "%m/%d/%Y")
# Fill data for all periods

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

# flatten df.risk and df.pmt to one row per month
  df.risk.flat <- df.risk %>%
  			arrange(LoanID, date) %>%

# merge balance, risk cat, and payment
  df <- merge(bal, df.risk, by=c('date', 'LoanID'), all.x=TRUE)
  # df2 <- merge(df, df.pmt, by=c('LoanID', ))

# load pds 
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)
  filename <-  "risk_profile_pds_06.22.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")


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
  	monitor_score = Final.Risk.Score, risk_category_sf = Final.Risk.Category)

  # risk category
  filename <- 'risk_cat.csv'
  df.r_cat <- read.csv(filename)
  names(df.r_cat)[1] <- 'LoanID'

  # maturity date data
  filename <- 'maturity_date.csv'
  df.maturity <- read.csv(filename)
  df.maturity <- select(df.maturity,
  	LoanID = Loan.ID,
  	date = Report.Date,
  	maturity_date = Maturity.Date,
  	outstanding_principal = Outstanding.Principal)
  # collateral data
  filename <- 'collateral.csv'
  collateral <- read.csv(filename)
  collateral <- select(collateral, Loan.ID, Collateral.Type)
  names(collateral) <- c('LoanID', 'collateral_type')

  # guarantee data
  filename <- 'List of 3rd Party Guarantees_12-30-2015.csv'
  guar <- read.csv(filename, skip = 1)
  guar <- select(guar, LoanID = Loan.ID, guarantee = X..Guaranteed)

  # recovery data
  filename <- 'recoveries.csv'
  recov <- read.csv(filename)
  recov <- select(recov, LoanID = as.numeric(Loan.ID), recovery = Internal.Recoveries.Paid)
  recov$LoanID <- as.numeric(recov$LoanID)
  # summarise recovery by LoanID
  recov <- recov %>% group_by(LoanID) %>% summarise(recovery = sum(recovery))
# format date
  df.pmt$date <- as.Date(df.pmt$date, "%m/%d/%Y")
  df.risk$date <- as.Date(df.risk$date, "%m/%d/%Y")
  df.maturity$date <- as.Date(df.maturity$date, "%m/%d/%Y")
  df.maturity$maturity_date <- as.Date(df.maturity$maturity_date, "%m/%d/%Y")
# check for hard collateral  
  collateral$collateral <- grepl('Real Estate', collateral$collateral_type) | grepl('Equipment and Machinery', collateral$collateral_type)
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
		bal$balance_temp <- as.character(bal$balance)
		bal$balance_temp <- gsub(",","", bal$balance_temp)
		bal$balance      <- as.numeric(as.character(bal$balance_temp))
		bal$balance_temp <- NULL
		bal <- filter(bal, !is.na(LoanID))
# function to convert to last day of month
	last_day <- function(date) {
	    ceiling_date(date, "month", change_on_boundary = TRUE) - days(1)
	}

# change bal$date from 1st day in month to last day
	bal$date <- last_day(bal$date)

# change risk cat from wide to long
	df.r_cat <- df.r_cat %>%
		gather(date, risk_category, -LoanID)

# format date in risk category
	df.r_cat$date <- as.Date(df.r_cat$date, 'X%m.%d.%Y')

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
  df  <- merge(bal, df.risk.flat, by=c('LoanID', 'date'), all.x=TRUE)
  df2 <- merge(df, df.pmt.flat, by=c('LoanID', 'date'), all=TRUE)
  df3 <- merge(df2, df.maturity, by=c('LoanID', 'date'), all=TRUE)
  df3 <- select(df3, -txn_type, -date_last_day.y, -date_last_day.x, -disbursed, -paid)
  df4 <- merge(df3, df.r_cat, by=c('LoanID', 'date'), all.x=TRUE)
  df5 <- merge(df4, collateral, by=c('LoanID'), all.x=TRUE)
  df6 <- merge(df5, guar, by='LoanID', all.x=TRUE)
  df7 <- merge(df6, recov, by='LoanID', all.x=TRUE)
  df  <- df7
  sum(duplicated(df3[,c('LoanID', 'date')]))
# Fill data for all periods
  df <- df %>%
  		arrange(LoanID, date) %>%
  		group_by(LoanID) %>%
  		fill(monitor_score, risk_category_sf, .direction='down') 

# load and merge pds 
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)
  filename <-  "risk_profile_pds_06.22.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")
  df.rap$Close.Date <- as.Date(df.rap$Close.Date, "%m/%d/%Y")
  df.rap$Maturity.at.Origination <- as.Date(df.rap$Maturity.at.Origination, "%m/%d/%Y")  
  pds <- select(df.rap, LoanID, pd, pd_one_year, Close.Date, Maturity.at.Origination, active, WO)
  df <- merge(df, pds, by=c('LoanID'), all.x=TRUE)

# check for NAs
	map_dbl(df, ~ sum(is.na(.))) 
# fill for missing
  df <- df %>%
  	arrange(LoanID, date) %>%
  	group_by(LoanID) %>%
  	fill( # fill below categories for later dates
  		outstanding_principal, paid_cum, disb_cum,
  		maturity_date, pd_one_year, risk_category_sf,
  		.direction = c('down')
  		) %>%
  	fill( # fill below categories for earlier dates
  		risk_category_sf, monitor_score,
  		.direction = c('up')
  		) %>%
  	ungroup()

# find 'first date' - presumably 
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
    mutate(months_in_risk_cat_cum = cumsum(counter)) %>%
  	ungroup()

  df$counter <- NULL
  df$months_in_risk_cat_cum_sq <- df$months_in_risk_cat_cum^2

# count of months in risk category restarting on category change
  df <- df %>%
  	arrange(LoanID, date) %>%
    group_by(LoanID) %>%
    mutate(months_in_risk_cat = sequence(rle(as.character(risk_category))[['lengths']])) %>%
  	ungroup()

  df$months_in_risk_cat_sq <- df$months_in_risk_cat^2
  df$months_in_risk_cat_cube <- df$months_in_risk_cat^3
# create tenor at date
  df$remaining_tenor <- ifelse(is.na(df$maturity_date),
  	df$Maturity.at.Origination - df$date,
  	df$maturity_date - df$date)
  # convert to numeric, years, and min of one
  df$remaining_tenor <- as.numeric(df$remaining_tenor) / 365
  df$remaining_tenor[df$remaining_tenor<1] <- 1

# create three variables
  # df$watch_list <- ifelse(df$risk_category_sf=='Watch List', 1, 0)
  df$watch_list <- ifelse(df$monitor_score<70 & df$monitor_score>=60, 1, 0)
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

# calculate cumulative prob of one year pd given remaining tenor
	df$pd <- 1 - ( 1 - df$pd_one_year ) ^ df$remaining_tenor 

# -------------------------------------------------------------------
# Create four dummies for risk categories, which assumes anything in a higher category also hit a lower category
  df$risk_categor_temp <- -1
    df <- df %>% 
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_sf=='Current', 0)) %>%
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_sf=='Watch List', 3)) %>%
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_sf=='Special Mention', 6)) %>%
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_sf=='Substandard', 12)) %>%
      mutate(risk_categor_temp = replace(risk_categor_temp, risk_category_sf=='Doubtful', 18))

  # df$risk_category_sf_num <- factor(df$risk_categor_temp, 
  #   levels = c(0, 3, 6, 12, 18),
  #   labels = c("Current", 'Watch List', "Special Mention", "Substandard", "Doubtful")
  #   )   

  df$risk_category_sf_num <- as.numeric(df$risk_categor_temp)
  df$risk_categor_temp <- NULL
  df$risk_category_sf_num <- factor(df$risk_category_sf_num, 
    levels = c(0, 3, 6, 12, 18),
    labels = c("Current", 'Watch List', "Special Mention", "Substandard", "Doubtful")
    )

# Create dummies for max risk category
  # df$Current <- 1
  df$watch_list <- df$watch_list
  df$Special_Mention <- 0
  df$Substandard <- 0
  df$Doubtful <- 0

  df$Current         <- ifelse(df$risk_category>0,1,0)
  df$Special_Mention <- ifelse(df$risk_category>1,1,0)
  df$Substandard     <- ifelse(df$risk_category>2,1,0)
  df$Doubtful        <- ifelse(df$risk_category>3,1,0)

  df$Special_Mention <- ifelse(df$WO==1,1,df$Special_Mention)
  df$Substandard     <- ifelse(df$WO==1,1,df$Substandard )
  df$Doubtful        <- ifelse(df$WO==1,1,df$Doubtful )

# # Convert to factor
#   df$risk_category <- factor(df$risk_category, 
#     levels = c(1, 2, 3, 4),
#     labels = c("Current", "Special Mention", "Substandard", "Doubtful")
#     )

  df <- df %>%
            mutate(Special_Mention = replace(Special_Mention, WO=="Writeoff", 1)) %>%
            mutate(Substandard = replace(Substandard, WO=="Writeoff", 1)) %>%
            mutate(Doubtful = replace(Doubtful, WO=="Writeoff", 1)) 	

  table(df$risk_category, df$risk_category_sf)
  table(df$risk_category, df$WO)            
  table(df$Special_Mention, df$WO) 

  sum(is.na(df$risk_category[df$balance>0]))
  table(df$risk_category_sf, df$WO)

  df <- select(df, -risk_category_sf_num, -outstanding_principal, -collateral_type, -monitor_score	)
# write data
  wd <- paste('C:/Box Sync/Risk Appetite - Provisioning Project/',
    'Working Folders for RAP Modules/Risk Profile/PD Model/',
    '5.Active Model/data',
    sep = '')
  setwd(wd)
 write.csv(df, 'out.csv')
 file.show('out.csv')
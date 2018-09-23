#NYPD Dataset
library(ggplot2)
library(dplyr)

nypd_df <- read.csv('nypd_complaint.csv')
nypd_all_complete <- nypd_df[complete.cases(nypd_df),]
nypd_all_complete <- nypd_all_complete[which(nypd_all_complete$BORO_NM != ''),]
nypd_all_complete <- within(nypd_all_complete, CMPLNT_FR_MONTH <- as.numeric(substr(CMPLNT_FR_DT,0,2)))
nypd_all_complete <- within(nypd_all_complete, CMPLNT_FR_DAY <- as.numeric(substr(CMPLNT_FR_DT,4,5)))
nypd_all_complete <- within(nypd_all_complete, CMPLNT_FR_YEAR <- as.numeric(substr(CMPLNT_FR_DT,7,10)))
nypd_all_complete <- nypd_all_complete[which(nypd_all_complete$CMPLNT_FR_MONTH != ''),]
nypd_all_complete <- nypd_all_complete[which(nypd_all_complete$CMPLNT_FR_DAY != ''),]
nypd_all_complete <- nypd_all_complete[which(nypd_all_complete$CMPLNT_FR_YEAR != ''),]
nypd_all_complete <- nypd_all_complete[which(as.numeric(nypd_all_complete$CMPLNT_FR_YEAR) >= 2006),]

ggplot(nypd_all_complete, aes(x=factor(BORO_NM))) + geom_bar(stat='count')
ggplot(nypd_all_complete, aes(x=factor(CMPLNT_FR_YEAR))) + geom_bar(stat = 'count')
ggplot(nypd_all_complete, aes(x=factor(CMPLNT_FR_MONTH))) + geom_bar(stat = 'count')
ggplot(nypd_all_complete, aes(x=factor(CMPLNT_FR_YEAR))) + geom_bar(stat = 'count')
num_day_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
crime_per_month_over_day <-c()
for(i in 1:length(num_day_per_month))
{  
  temp_sum <- sum(nypd_all_complete$CMPLNT_FR_MONTH == i)
  temp_sum_over_day <- temp_sum/num_day_per_month[i]
  crime_per_month_over_day <- c(crime_per_month_over_day,temp_sum_over_day)
}

crime_per_month_over_day_df <- data.frame(as.factor(1:12),crime_per_month_over_day)
colnames(crime_per_month_over_day_df) <- c('month', 'crime_per_day')
ggplot(data = crime_per_month_over_day_df, aes(x=month, y= crime_per_day)) + geom_col()

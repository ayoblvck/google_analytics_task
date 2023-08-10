number_of_print <- function(n){
for (i in c(1:n)){
  print ("hello world")
  }
}
#check for column format

hourlyCalories_merged$ActivityHour <- parse_date_time(hourlyCalories_merged$ActivityHour, "%m/%d/%Y %I:%M:%S %p")
dailyCalories_merged$ActivityDay <- as.Date(dailyCalories_merged$ActivityDay, format = "%m/%d/%Y")
dailyIntensities_merged$ActivityDay <- as.Date(dailyIntensities_merged$ActivityDay, format = "%m/%d/%Y")
heartrate_seconds_merged$Time <- parse_date_time(heartrate_seconds_merged$Time, "%m/%d/%Y %I:%M:%S %p")
hourlyCalories_merged$ActivityHour <- parse_date_time(hourlyCalories_merged$ActivityHour, "%d/%m/%Y %I:%M:%S %p")
hourlyIntensities_merged$ActivityHour <- parse_date_time(hourlyIntensities_merged$ActivityHour, "%m/%d/%Y %I:%M:%S %p")
hourlySteps_merged$ActivityHour <- parse_date_time(hourlySteps_merged$ActivityHour, "%m/%d/%Y %I:%M:%S %p")
minuteCaloriesNarrow_merged$ActivityMinute <- parse_date_time(minuteCaloriesNarrow_merged$ActivityMinute, "%m/%d/%Y %I:%M:%S %p")
minuteCaloriesWide_merged$ActivityHour <- parse_date_time(minuteCaloriesWide_merged$ActivityHour,"%m/%d/%Y %I:%M:%S %p" )
minuteIntensitiesNarrow_merged$ActivityMinute <- parse_date_time(minuteIntensitiesNarrow_merged$ActivityMinute,"%m/%d/%Y %I:%M:%S %p" )
minuteIntensitiesWide_merged$ActivityHour <- parse_date_time(minuteIntensitiesWide_merged$ActivityHour, "%m/%d/%Y %I:%M:%S %p")
minuteMETsNarrow_merged$ActivityMinute<- parse_date_time(minuteMETsNarrow_merged$ActivityMinute, "%m/%d/%Y %I:%M:%S %p")
minuteSleep_merged$date<- parse_date_time(minuteSleep_merged$date, "%m/%d/%Y %I:%M:%S %p")
minuteStepsNarrow_merged$ActivityMinute<-parse_date_time(minuteStepsNarrow_merged$ActivityMinute, "%m/%d/%Y %I:%M:%S %p")
minuteStepsWide_merged$ActivityHour<-parse_date_time(minuteStepsWide_merged$ActivityHour, "%m/%d/%Y %I:%M:%S %p")
sleepDay_merged$SleepDay<-parse_date_time(sleepDay_merged$SleepDay, "%m/%d/%Y %I:%M:%S %p")
weightLogInfo_merged$Date<-parse_date_time(weightLogInfo_merged$Date, "%m/%d/%Y %I:%M:%S %p")

dailyCalories_merged<-select(dailyCalories_merged, -...1)

#check for missing values
missingcount <- colSums(is.na(dailyCalories_merged))
missingcount <- colSums(is.na(dailyIntensities_merged))
missingcount <- colSums(is.na(dailySteps_merged))
missingcount <- colSums(is.na(heartrate_seconds_merged))
missingcount <- colSums(is.na(hourlyCalories_merged))
missingcount <- colSums(is.na(hourlyIntensities_merged))
missingcount <- colSums(is.na(minuteCaloriesNarrow_merged))
missingcount <- colSums(is.na(minuteIntensitiesWide_merged))
missingcount <- colSums(is.na(weightLogInfo_merged))
print(missingcount)

#remove missing values if present
hourlyCalories_merged <- hourlyCalories_merged %>% drop_na()
hourlyIntensities_merged <- hourlyIntensities_merged %>% drop_na()
nrow(hourlyIntensities_merged)
which(is.na(heartrate_seconds_merged))
weightLogInfo_merged <- select(weightLogInfo_merged, -Fat)




#save file in new folder
write.csv(dailyCalories_merged, "./clean_dataset/dailyCalories_merged.csv")
write.csv(hourlyCalories_merged, "./clean_dataset/hourlyCalories_merged.csv")
write.csv(hourlyIntensities_merged, "./clean_dataset/hourlyIntensities_merged.csv")
write.csv(dailySteps_merged, "./clean_dataset/dailySteps_merged.csv")
write.csv(heartrate_seconds_merged, "./clean_dataset/heartrate_seconds_merged.csv")
§write.csv(minuteIntensitiesNarrow_merged, "./clean_dataset/minuteIntensitiesNarrow_merged.csv")
write.csv(weightLogInfo_merged, "./clean_dataset/weightLogInfo_merged.csv")
write

#load datasets
dailyActivity_merged <- as.data.frame(read.csv("./dailyActivity_merged.csv"))
dailyintensities_merged <- as.data.frame(read.csv("./dailyintensities_merged.csv"))
dailySteps_merged <- as.data.frame(read.csv("./dailySteps_merged.csv"))
heartrate_seconds_merged <- as.data.frame(read.csv("./heartrate_seconds_merged.csv"))
hourlyCalories_merged <- as.data.frame(read.csv("./hourlyCalories_merged.csv"))
hourlyIntensities_merged <- as.data.frame(read.csv("./hourlyIntensities_merged.csv"))
hourlySteps_merged <- as.data.frame(read.csv("./hourlySteps_merged.csv"))
minuteCaloriesNarrow_merged <- as.data.frame(read.csv("./minuteCaloriesNarrow_merged.csv"))
minuteMETsNarrow_merged <- as.data.frame(read.csv("./minuteMETsNarrow_merged.csv"))
minuteSleep_merged <- as.data.frame(read.csv("./minuteSleep_merged.csv"))
minuteStepsNarrow_merged <- as.data.frame(read.csv("./minuteStepsNarrow_merged.csv"))
sleepDay_merged <- as.data.frame(read.csv("./sleepDay_merged.csv"))
weightLogInfo_merged <- as.data.frame(read.csv("./weightLogInfo_merged.csv"))

#understand and clean the datasets
# 1. dailyActivity_merged.csv This contains the total steps taken 
head(dailyActivity_merged)
str(dailyActivity_merged)
dailyActivity_merged$ActivityDate <- ymd(dailyActivity_merged$ActivityDate)
any(is.na(dailyActivity_merged)) # No
dailyActivity_merged$day <- wday(dailyActivity_merged$ActivityDate)
# sunday - 1 monday-2 Tuesday-3 wednesday-4 Thursday-5 Friday-6 Saturday-7

#1 The day of the week people take the most steps

highest_step <- dailyActivity_merged %>% 
  group_by(day) %>% 
  summarise(Total_Steps = sum(TotalSteps)) %>% 
  select(day, Total_Steps) %>% 
  mutate(Total_Steps = Total_Steps*0.001)

print( highest_step, n = nrow(highest_step))

# Visualise people working using a bar chat
rescale_factor <- 0.001
highest_step$day <- weekdays(as.Date(highest_step$day, origin = "2023-07-01"))
highest_step$day <- factor(highest_step$day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#highest_step$day <- reorder(highest_step$day, highest_step$Total_Steps)

ggplot(highest_step, aes(x = day, y = Total_Steps, fill = day)) +
  geom_bar(stat = "identity") +
  labs(title = "steps taken (in thousands)", x = "Day", y = "Total steps (seconds)") +
  theme_minimal()



# 2. dailyIntensities_merged A
str(dailyintensities_merged)
dailyintensities_merged$ActivityDay<- ymd(dailyintensities_merged$ActivityDay)
any(is.na(dailyintensities_merged))

# 3. dailySteps_merged A
# what days does each person take the most walk
# use a line graph to plot their walk
str(dailySteps_merged)
dailySteps_merged$ActivityDay <- ymd(dailySteps_merged$ActivityDay)
dailySteps_merged
any(is.na(dailySteps_merged))

# 4. heartrate_seconds_merged B
#heart rates of eah person, different set of Ids
str(heartrate_seconds_merged)
view(heartrate_seconds_merged)
heartrate_seconds_merged$Time <- ymd_hms(heartrate_seconds_merged$Time)
any(is.na(heartrate_seconds_merged))



# 5. hourlyCalories_merged A
# calories burnt per day
str(hourlyCalories_merged)
view(hourlyCalories_merged)
hourlyCalories_merged$ActivityHour <- ymd_hms(hourlyCalories_merged$ActivityHour)
any(is.na(hourlyCalories_merged))


hourlyCalories_merged <- hourlyCalories_merged %>% 
  mutate( TimeofDay = case_when(
    hour(ActivityHour) >= 0 & hour(ActivityHour) < 12 ~ 1,
    hour(ActivityHour) >= 12  & hour(ActivityHour) < 18 ~ 2,
    TRUE ~ 3
  ))

head(hourlyCalories_merged)

hourlyCalories_merged <- hourlyCalories_merged %>% 
  group_by(TimeofDay) %>% 
  summarise(total_calories_burnt = sum(Calories), avg_calories = mean(Calories)) %>% 
  select(TimeofDay, total_calories_burnt, avg_calories)

ggplot(hourlyCalories_merged, aes(x ="", y = total_calories_burnt, fill = factor(TimeofDay, 
                                                                                 levels = factor(TimeofDay)))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y")+
  labs(title = "Amount of calories burnt at different times of the day in ", fill = "TimeofDay") +
  scale_fill_manual(values = c("1" = "#007fff", "2" = "#324ab2", "3" = "#008080"), 
                    labels = c( "morning", 
                    "afternoon",
                    "evening")) +
  geom_text(aes(label=total_calories_burnt), position = position_stack(vjust = 0.5)) +
    theme_void()

factor(hourlyCalories_merged$TimeofDay)

##check timeof day people burnt the most calories
hourlyCalories_merged

View(hourlyCalories_merged)

calories_burnt <- hourlyCalories_merged %>% 
  group_by(Id) %>% 
  summarise(sum_calories = sum(Calories)) %>% 
  select(Id, sum_calories)

calories_burnt
calories_burnt = calories_burnt[order(calories_burnt$sum_calories), ]

# 6. hourlyIntensities_merged A
#
str(hourlyIntensities_merged)
hourlyIntensities_merged$ActivityHour <- ymd_hms(hourlyIntensities_merged$ActivityHour)
any(is.na(hourlyIntensities_merged))

# 7. hourlySteps_merged
# steps taken per hour. Figure out what time of the day they walkk most and least
str(hourlySteps_merged)
hourlyIntensities_merged$ActivityHour <- ymd_hms(hourlyIntensities_merged$ActivityHour)
any(is.na(hourlySteps_merged))

hourlySteps_merged$TimeofDay <- Convert_to_TimeofDay(hourlySteps_merged$ActivityHour)

# create graph

ggplot(hourlyCalories_merged, aes(x ="", y = total_calories_burnt, fill = factor(TimeofDay, 
                                                                                 levels = factor(TimeofDay)))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y")+
  labs(title = "Amount of calories burnt at different times of the day in ", fill = "TimeofDay") +
  scale_fill_manual(values = c("1" = "#007fff", "2" = "#324ab2", "3" = "#008080"), 
                    labels = c( "morning", 
                                "afternoon",
                                "evening")) +
  geom_text(aes(label=total_calories_burnt), position = position_stack(vjust = 0.5)) +
  theme_void()

# 8. minuteCaloriesNarrow_merged
# calores burnt per minute
str(minuteCaloriesNarrow_merged)
minuteCaloriesNarrow_merged$ActivityMinute <- ymd_hms(minuteCaloriesNarrow_merged$ActivityMinute)
which(is.na(minuteCaloriesNarrow_merged)) #missing data here

# 9 minuteMETsNarrow_merged
# 
str(minuteMETsNarrow_merged)
minuteMETsNarrow_merged$ActivityMinute <- ymd_hms(minuteMETsNarrow_merged$ActivityMinute)
which(any(is.na(minuteMETsNarrow_merged)))

# 10 minuteSleep_merged
# understand sleep patterns
str(minuteSleep_merged)
minuteSleep_merged$date <- ymd_hms(minuteSleep_merged$date)
any(is.na(minuteSleep_merged))

# 11 minuteStepsNarrow_merged
# steps taken per minute 
# we can merged the dates to chck what happens on those days
str(minuteStepsNarrow_merged)
minuteStepsNarrow_merged$ActivityMinute <- ymd_hms(minuteStepsNarrow_merged$ActivityMinute)
any(is.na(minuteStepsNarrow_merged))

# 12 sleepDay_merged
# sleep records and minutes asleep and in bed
str(sleepDay_merged)
sleepDay_merged$SleepDay <- ymd(sleepDay_merged$SleepDay)
any(is.na(sleepDay_merged))

# weightLogInfo_merged
str(weightLogInfo_merged)
weightLogInfo_merged$Date <- ymd_hms(weightLogInfo_merged$Date)
any(is.na(weightLogInfo_merged))
print(weightLogInfo_merged)
write.csv(weightLogInfo_merged, "./weightLogInfo_merged_cleaned.csv")

# visualize
weight_data <- weightLogInfo_merged %>% 
  group_by(Id) %>% 
  summarise(WeightKg = mean(WeightKg), BMI = mean(BMI)) %>% 
  select(Id, WeightKg, BMI)
weight_step <- dailyActivity_merged %>% 
  group_by(Id) %>% 
  summarise(Total_Steps = sum(TotalSteps), avg_steps = mean(TotalSteps)) %>% 
  select(Id, Total_Steps, avg_steps) %>% 
  mutate(Total_Steps = Total_Steps*0.0001, avg_steps = avg_steps*0.001)


weight_step
merged_weight <- left_join(weight_data, weight_step, by= "Id")
merged_weight$Id <- factor(merged_weight$Id,
                           levels = unique(merged_weight$Id),
                           labels = c("A", "B", "C", "D", "E", "F", "G", "H")
)

# create a grouped bar chart to see the relationship between weigh, BMI and average steps

merged_weight <- gather(merged_weight, key = variable, value = "value", -Id)
merged_weight$Id <- factor(highest_step$day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(merged_weight, aes(x = Id, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "relationship between the weight, BMI and amount of steps of the 6 users")+
  theme_minimal()


# analyse customer behavior H            
  
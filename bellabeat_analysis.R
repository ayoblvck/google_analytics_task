# dailyCalories_merged <- read.csv("./dailyCalories_merged.csv")
# dailyIntensities_merged
# dailySteps_merged
# heartrate_seconds_merged
# hourlyCalories_merged
# hourlyintensities_merged
# hourlySteps_merged
# minuteCaloriesNarrow
# minuteCaloriesWide
# minuteintensi

install.packages("lubridate")
hourlyCalories_merged$ActivityHour

hourlyCalories_merged$ActivityHour <- parse_date_time(hourlyCalories_merged$ActivityHour, "%d/%m/%Y %I:%M:%S %p")
head(dailyActivity_merged$ActivityDate)
as.Date(dailyActivity_merged$ActivityDate, format = "%d/%m/%Y")
class(hourlyCalories_merged$ActivityHour)
library(readr)
dailyActivity_merged <- read_csv("dailyActivity_merged.csv", 
                                 col_types = cols(ActivityDate = col_date(format = "%m/%d/%Y")))
View(dailyActivity_merged)

missing_count <- colSums(is.na(dailyActivity_merged))
print(missing_count)

class(dailyActivity_merged$ActivityDate)
write.csv(dailyActivity_merged, "./clean_dataset/dailyActivity_merged")

# installing packages
install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("arsenal")
install.packages("lubridate")
install.packages("magrittr")

#loading packages
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(ggplot2)
library(arsenal)
library(lubridate)
library(magrittr)

# importing data using read.csv and creating data frames 
daily_activity <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_calories <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
daily_intensities <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")                
daily_steps <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
heartrate_seconds <-read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
minute_METs <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")
daily_sleep <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight_log <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
hourly_calories <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourly_Intensities <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourly_steps <- read.csv("C:/Users/owner/Desktop/Fitbit/Fitbit-analysis/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

# viewing data frames 
head(daily_activity)
colnames(daily_activity)
glimpse(daily_activity)

head(daily_calories)
colnames(daily_calories)
glimpse(daily_calories)

head(daily_intensities)
colnames(daily_intensities)
glimpse(daily_intensities)

head(daily_steps)
colnames(daily_steps)
glimpse(daily_steps)

head(heartrate_seconds)
colnames(heartrate_seconds)
glimpse(heartrate_seconds)

head(minute_METs)
colnames(minute_METs)
glimpse(minute_METs)

head(daily_sleep)
colnames(daily_sleep)
glimpse(daily_sleep)

head(weight_log)
colnames(weight_log)
glimpse(weight_log)


#PROCESS

#daily finding duplicates in the daily_activity dataframe
duplicated(daily_activity)

#count of duplicated data
sum(duplicated(daily_activity))

# to use the daily_activity frame,The daily_activity frame must have the same amount of observations and the same observations for each ID number as the daily_calories, daily_intensities, and daily_steps frames.
# Determinig if the the values in daily_calories, daily_intensities and daily_steps are the same. 
comparedf(daily_activity, daily_calories)
summary(comparedf(daily_activity, daily_calories))
summary(comparedf(daily_activity, daily_intensities)) 
summary(comparedf(daily_activity, daily_steps))
summary(comparedf(daily_activity, minute_METs))

# Creating uniform date formats and storing the time and date in separate columns
# daily_activity table
daily_activity$date<-mdy(daily_activity$ActivityDate)
#adding a new column "day" for finding out the day-wise trends
daily_activity$day<- format(daily_activity$date, "%A")
daily_activity<- subset(daily_activity, select= -c(ActivityDate))
head(daily_activity)

# hourly_intensity table
hourly_Intensities$date<- mdy_hms(hourly_Intensities$ActivityHour)
hourly_Intensities$intensity_hour<-format(hourly_Intensities$date,format= "%H:%M")
hourly_Intensities$intensity_day<-format(hourly_Intensities$date, format= "%A")
hourly_Intensities<- subset(hourly_Intensities, select= -ActivityHour)
head(hourly_Intensities)

#sleep log table
daily_sleep$date<- mdy_hms(daily_sleep$SleepDay)
daily_sleep$day<- format(daily_sleep$date, "%A")

# adding a column to determine the time participants lie awake in bed
new_sleep<-daily_sleep%>%
  mutate(total_time_awake_in_bed= ( TotalTimeInBed- TotalMinutesAsleep) )%>%
  glimpse()
daily_sleep<- subset(daily_sleep, select= -SleepDay)
glimpse(daily_sleep)


#merging daily_activity and new_sleep tables on date, day and IDs.
merged_daily<- merge(daily_activity, daily_sleep, by= c("Id", "date", "day"), all.x = TRUE)
merged_daily<- distinct(merged_daily)
glimpse(merged_daily)

# ANALYZE
# summary statistics of all tables
merged_daily %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, TotalMinutesAsleep, TotalTimeInBed, Calories)%>%
  summary()

#finding the number of days data was collected from each user. 
day<- merged_daily%>%
  group_by(Id)%>%
  summarise(number_of_days_data_collected= n_distinct(date))%>%
  arrange(number_of_days_data_collected)
day


# very active minutes by day 
sum_very_active_mins<-merged_daily%>%
  group_by(day)%>%
  summarise(sum_very_active_mins= sum(VeryActiveMinutes) )
sum_very_active_mins

#Plotting the results down for better understanding, with geom_histogram 
ggplot(merged_daily,mapping=aes(x=day, weight=VeryActiveMinutes, fill=day))+geom_histogram(stat="count")


# very active time of the day
hourly_intensity<- hourly_Intensities%>%
  group_by(intensity_hour)%>%
  summarise(sum_total_intensity= sum(TotalIntensity))
hourly_intensity
# visualizing the results for a better understanding
ggplot(hourly_intensity, aes(intensity_hour, sum_total_intensity, colour=intensity_hour,
                             fill=intensity_hour ))+ geom_col(alpha=0.1)


#CORRELATIONS AND VISULIZATIONS

#Correlation between Very Active Minutes and Total Calories Burned
cor(x=merged_daily$VeryActiveMinutes, y=merged_daily$Calories, method="pearson", "complete.obs")
# plotting for further visualization 
ggplot (data = merged_daily, aes(x=VeryActiveMinutes, y=Calories)) + geom_point(colour= "violet") + stat_smooth(method="loess", colour = "orange") + labs(title="The Correlation between Very Active Minutes and Total Daily Calories Burned")
##geom_smooth() using method ="loess"  using formula 'y ~x’

#Correlation between total minutes in asleep  and Total time in bed
cor(x=merged_daily$TotalMinutesAsleep, y=merged_daily$TotalTimeInBed, method="pearson", use = "complete.obs")
ggplot (data = merged_daily, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point(colour= "violet") + stat_smooth(method="loess", colour = "orange") + labs(title="The Correlation between Total Time In Bed  and Total Minutes Asleep")
##geom_smooth() using method ="loess"  using formula 'y ~x’

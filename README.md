# Fitbit Smart Device Analysis
## Introduction
> Welcome to the Bellabeat data analysis case study! In this case study, you will perform many real-world tasks of a junior data analyst. You will imagine you are working for Bellabeat, a high-tech manufacturer of health-focused products for women, and meet different characters and team members. In order to answer the key business questions, you will follow the steps of the data analysis process: ask, prepare, process, analyze, share, and act.

## Background
> Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that manufactures health-focused smart products. Sršen used her background as an artist to develop beautifully designed technology that informs and inspires women around the world. Collecting data on activity, sleep, stress, and reproductive health has allowed Bellabeat to empower women with knowledge about their own health and habits. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women.

## Scenario
> You are a junior data analyst working on the marketing analyst team at Bellabeat, a high-tech manufacturer of health-focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. You have been asked to focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. The insights you discover will then help guide marketing strategy for the company. You will present your analysis to the Bellabeat executive team along with your high-level recommendations for Bellabeat’s marketing strategy.

## Ask 
### Business Task 
> Sršen asks you to analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smartdevices.She then wants you to select one Bellabeat product to apply these insights to in your presentation. 

>These questions will guide your analysis:
 1. What are some trends in smart device usage?
 2. How could these trends apply to Bellabeat customers?
 3. How could these trends help influence Bellabeat marketing strategy?


## Prepare
>I downloaded and saved the dataset used in this case study.  The analysis's data originates from [Kaggle's FitBit Fitness Tracker Data.](https://www.kaggle.com/datasets/arashnic/fitbit) The dataset was created by participants in a dispersed survey conducted by Amazon Mechanical Turk from December 3, 2016 to December 5, 2016. The submission of personal tracker data, including minute-level output for heart rate, physical activity, and sleep tracking, was approved by thirty Fitbit users who met the eligibility requirements. 

> The sample size and lack of important participant variables, such gender, age, geography, and lifestyle, are limitations for this data. 
>R Studio was used to perform this study because of the wide range of packages and data visualization tools it offers for analyzing data. Of the 18 datasets provided, only 6 were used to complete this business requirement.

``` R
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
```

## Process
>The procedures involved were making sure that the names were consistent in order to guarantee that every table is cleansed to be more exact and meaningful.Look for and eliminate any errors or duplicates.

``` R
#daily finding dupicates in the daily_activity dataframe
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
```
## Analyze
>In order to find significant insights, we delve deeply into the gathered data throughout the project's analysis phase. In order to find patterns, trends, and correlations in the data, this phase is all about crunching statistics, doing statistical tests, and visualizing the data. To get insights, we'll use tables for steps, calories, distance, sleep, and activity. Our goal is to respond to the business problem and make data-driven decisions by means of data analysis.

```R
# summary statistics of all tables
merged_daily %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, TotalMinutesAsleep, TotalTimeInBed, Calories)%>%
  summary()
```

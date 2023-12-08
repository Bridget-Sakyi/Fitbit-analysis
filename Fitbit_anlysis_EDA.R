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

# very active time of the day
hourly_intensity<- hourly_Intensities%>%
  group_by(intensity_hour)%>%
  summarise(sum_total_intensity= sum(TotalIntensity))
hourly_intensity

#Correlation between Very Active Minutes and Total Calories Burned
cor(x=merged_daily$VeryActiveMinutes, y=merged_daily$Calories, method="pearson")

#Correlation between total minutes in asleep  and Total time in bed
cor(x=merged_daily$TotalMinutesAsleep, y=merged_daily$TotalTimeInBed, method="pearson", use = "complete.obs")

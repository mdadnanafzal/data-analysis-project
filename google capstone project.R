#Import libraries
library (tidyverse)
library (dplyr)
library (ggplot2)

activity <- read_csv("BellaBeat fitness/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
calorie <- read_csv("BellaBeat fitness/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
intensity <- read_csv("BellaBeat fitness/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
step <-   read_csv("BellaBeat fitness/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
sleep <- read_csv("BellaBeat fitness/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight_log <-  read_csv("BellaBeat fitness/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

# Check dataset samples
head(activity)
head(calorie)
head(intensity)
head(step)
head(sleep)
head(weight_log)

#Check number of participants
n_distinct(activity$Id)
n_distinct(calorie$Id)
n_distinct(intensity$Id)
n_distinct(step$Id)
n_distinct(sleep$Id)
n_distinct(weight_log$Id)


#calorie
calorie %>%
  select(Calories) %>%
  summary()
#intensity
intensity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  summary()
#step
step %>%
  select(StepTotal) %>%
  summary()
#sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

# Process the data
#Total Steps vs. Calories correlation
cor(activity$TotalSteps, activity$Calories)
#Total Steps vs. Calories visualization
ggplot (data = activity, aes(x = TotalSteps, y = Calories)) + geom_point() + labs (title = 'Total Steps vs. Calories') + geom_smooth(method = 'loess')

#Grouping participant into four categories by Intensity Minutes for better visualization
participant_type <- intensity %>% 
  summarise(summary_participant_type = factor(case_when(
    SedentaryMinutes > mean(SedentaryMinutes) ~ "Passive",
    LightlyActiveMinutes > mean(LightlyActiveMinutes) ~ "Lightly Active",
    FairlyActiveMinutes > mean(FairlyActiveMinutes) ~ "Active",
    VeryActiveMinutes > mean(VeryActiveMinutes) ~ "Very Active",), 
    levels = c("Passive", "Lightly Active", "Active", "Very Active")), .group=Id) %>% 
  drop_na()
#Participant by categories percentage
participant_type %>% 
  group_by(summary_participant_type) %>% 
  summarise (total = n()) %>% 
  mutate (totals = sum(total)) %>% 
  group_by(summary_participant_type) %>% 
  summarise (TotalPercent = total / totals)
#Visualize by categories percentage
participant_type %>% 
  group_by(summary_participant_type) %>% 
  summarise (total = n()) %>% 
  mutate (totals = sum(total)) %>% 
  group_by(summary_participant_type) %>% 
  summarise (TotalPercent = total / totals) %>% 
  ggplot(aes(summary_participant_type, y = TotalPercent, fill=summary_participant_type)) + 
  geom_col() + 
  labs(title = 'Participants Distribution by Active Minutes', x= 'Category') + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = scales::percent)


#Import hourly steps
steps_hourly <- read_csv("BellaBeat fitness/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
head(steps_hourly)
#Separate date and time
steps_hourly$ActivityHour=as.POSIXct(steps_hourly$ActivityHour, format = "%d/%m/%Y %I:%M:%S %p", tz=Sys.timezone())
steps_hourly$time <- format(steps_hourly$ActivityHour, format = "%H:%M:%S")
steps_hourly$date <- format(steps_hourly$ActivityHour, format = "%m/%d/%y")
head(steps_hourly)
#Average steps by hour
steps <- steps_hourly %>% 
  group_by(time) %>% 
  drop_na() %>% 
  summarise(mean_total_steps = mean(StepTotal))
#Visualize
ggplot(data = steps, aes(x=time, y=mean_total_steps)) + 
  geom_histogram(stat = "identity", fill = "blue") + 
  theme (axis.text.x = element_text(angle = 90)) +
  labs (title = "Average Total Steps vs. Time")


#Grouping participant into three categories by sleep time
sleep_type <- sleep %>% 
  summarise(summary_participant_sleep = factor(case_when(
    TotalMinutesAsleep < 420 ~ "Need more sleep",
    TotalMinutesAsleep >= 420 & TotalMinutesAsleep < 540 ~ "Good sleep",
    TotalMinutesAsleep > 540 ~ "Need less sleep",),
    levels = c("Need more sleep", "Good sleep", "Need less sleep")), .group =Id) %>% 
  drop_na()
#Participant by category percentage
sleep_type %>% 
  group_by(summary_participant_sleep) %>% 
  summarise (total = n()) %>% 
  mutate (totals = sum(total)) %>% 
  group_by(summary_participant_sleep) %>% 
  summarise (TotalPercent = total / totals)
#Visualize
sleep_type %>% 
  group_by(summary_participant_sleep) %>% 
  summarise (total = n()) %>% 
  mutate (totals = sum(total)) %>% 
  group_by(summary_participant_sleep) %>% 
  summarise (TotalPercent = total / totals) %>% 
  ggplot(aes(summary_participant_sleep, y= TotalPercent, fill=summary_participant_sleep)) + 
  geom_col() + 
  labs(title='Participants Distribution by Sleep Time', x = 'Category') + 
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::percent)


#Sleep Time vs. Time in Bed correlation
cor(sleep$TotalTimeInBed, sleep$TotalMinutesAsleep)
#Sleep Time vs. Time in Bed visualization
ggplot(data = sleep, aes(x=TotalTimeInBed, y=TotalMinutesAsleep)) + geom_point() + 
  labs(title = 'Total Time in Bed vs. Total Minutes Sleep') + geom_smooth()

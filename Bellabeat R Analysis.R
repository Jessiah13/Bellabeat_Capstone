# Bellabeat wellness Google capstone analysis in R.
# Datasets used in this analysis are dailyActivity_merged.xlsx & SleepDay_merged.xlsx

# Loading the libraries needed for the data analysis

install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# Next I will be importing the 2 data sets to be used in the analysis.

dailyActivity <- read.csv("dailyActivity_merged.csv")
sleep <- read.csv("sleepDay_merged.csv")


# Next I will review the data imported to familiarize myself with their headings and data structure

head(dailyActivity)
colnames(dailyActivity)
str(dailyActivity)
View(dailyActivity)
# Findings: Date column in the dailyActivity df is set to char

head(sleep)
colnames(sleep)
str(sleep)
View(sleep)
# Findings: sleepDay column is also char.


#Checking for duplicates

any(duplicated(dailyActivity))
any(duplicated(sleep))

which(duplicated(sleep))
#Finding: there are duplicated rows in the sleep data set, rows 162, 224, & 381


# Cleaning Dates in both data sets 

dailyActivity$ActivityDate <- as.Date(dailyActivity$ActivityDate, format = "%m/%d/%Y")
sleep$SleepDay <- as.Date(sleep$SleepDay, format = "%m/%d/%Y")

str(dailyActivity)
str(sleep)


# Removing dupilcates from sleep data set

clean_Sleep <- sleep[!duplicated(sleep), ]
str(clean_Sleep)
# 3 rows were removed from data set, observation count went from 413 to 410


# Checking for NA or Null values in the data sets

sum(is.na(dailyActivity))
sum(is.null(dailyActivity))
sum(is.na(clean_Sleep))
sum(is.null(clean_Sleep))
# There are no NA or NUll values in the data sets


# Summary of both data sets

summary(dailyActivity)
summary(clean_Sleep)


# User Activity bar Chart
# calculating the percentage of each activity

Total_Activity_Minutes <- sum(dailyActivity$SedentaryMinutes) + 
  sum(dailyActivity$LightlyActiveMinutes) + 
  sum(dailyActivity$FairlyActiveMinutes) + 
  sum(dailyActivity$VeryActiveMinutes)

Activity_Percent <- data.frame(
  Sedentary = sum(dailyActivity$SedentaryMinutes)/sum (Total_Activity_Minutes) * 100,
  LightlyActive = sum(dailyActivity$LightlyActiveMinutes)/sum (Total_Activity_Minutes) * 100,
  FairlyActive = sum(dailyActivity$FairlyActiveMinutes)/sum (Total_Activity_Minutes) * 100,
  VeryActive = sum(dailyActivity$VeryActiveMinutes)/sum (Total_Activity_Minutes) * 100
)
# Result:  Sedentary LightlyActive FairlyActive VeryActive
# 1  81.32989      15.82049     1.113014   1.736602

Activity_Type <- c("Sedentary", "Lightly_Active", "Fairly_Active", "Very_Active")
Activity_Type_percentage <- c(81.32, 15.82, 1.11, 1.72)

Activity_Type_Data <- data.frame(
  Activity_Type, Activity_Type_percentage
)

Activity_Bar_Chart <- ggplot(
  Activity_Type_Data, aes(x = Activity_Type, y = Activity_Type_percentage, fill = factor(Activity_Type,))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 45)) + labs(
    title = "User Activity Percentage"
  )



# Activity by day of the week (Very Active, Fairly Active & Lightly Active)

Day_Week1 <- dailyActivity
Day_Week1$ActivityDate <- weekdays(Day_Week1$ActivityDate)

Activity_chart <- Day_Week1 %>% 
  group_by(ActivityDate) %>% 
  summarize(lightly_active = sum(LightlyActiveMinutes), fairly_active = sum(FairlyActiveMinutes), very_active = sum(VeryActiveMinutes)) %>% 
  pivot_longer(-ActivityDate, names_to = "Activities") %>% 
  ggplot(aes(ActivityDate, value, fill = Activities)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "User Activity by Day of the Week",
       y = "Minutes",
       x = "Days of the Week")

Activity_chart


# correlation between totla average total steps and the average calories lost.
average_activity <- dailyActivity

average_activity %>% 
  group_by (Id) %>%
  summarise(mean_calories = mean(Calories), mean_total_steps = mean(TotalSteps)) %>% 
  ggplot(average_activity, mapping = (aes(mean_calories, mean_total_steps))) + 
  geom_point(stat = "summary",
             fun = "mean") + geom_smooth() + 
  theme_minimal() + 
  labs(
    title = "Calories Vs Total Steps",
    x = "Average Calories",
    y= "Average Steps")


# Correlation between calories vs active minutes
calories_v_activity <- dailyActivity

  calories_v_activity %>% 
  group_by (Id) %>%
  summarise(mean_calories = mean(Calories), mean_very_active = mean(VeryActiveMinutes)) %>% 
  ggplot(calories_v_activity, mapping = (aes(mean_calories, mean_very_active))) + 
  geom_point(stat = "summary",
             fun = "mean") + geom_smooth(color = "green") + 
  theme_minimal() + 
  labs(
    title = "Calories Vs Very Active Minutes",
    x = "Average Calories",
    y= "Average Active Minutes")
  
  
  # Correlation between calories vs fairly minutes
  
  calories_f_activity <- dailyActivity
  
  calories_f_activity %>% 
    group_by (Id) %>%
    summarise(mean_calories = mean(Calories), mean_fairly_active = mean(FairlyActiveMinutes)) %>% 
    ggplot(calories_f_activity, mapping = (aes(mean_calories, mean_fairly_active))) + 
    geom_point(stat = "summary",
               fun = "mean") + geom_smooth(color = "orange") + 
    theme_minimal() + 
    labs(
      title = "Calories Vs Fairly Active Minutes",
      x = "Average Calories",
      y= "Average Fairly Minutes")
  
  
# Correlation between calories vs fairly minutes
  
  calories_l_activity <- dailyActivity
  
  calories_l_activity %>% 
    group_by (Id) %>%
    summarise(mean_calories = mean(Calories), mean_lightly_active = mean(LightlyActiveMinutes)) %>% 
    ggplot(calories_l_activity, mapping = (aes(mean_calories, mean_lightly_active))) + 
    geom_point(stat = "summary",
               fun = "mean") + geom_smooth(color = "red") + 
    theme_minimal() + 
    labs(
      title = "Calories Vs Lightly Active Minutes",
      x = "Average Calories",
      y= "Average Lightly Minutes")
  
  
# Correlation between calories and sleep
  
  AVG_time_wasted_in_bed <- inner_join(dailyActivity, clean_Sleep, by = "Id")
  
  AVG_time_wasted_in_bed %>% 
    group_by(Id) %>% 
    summarise(mean_calories = mean(Calories), mean_min_asleep = mean(TotalMinutesAsleep)) %>% 
    ggplot(AVG_time_wasted_in_bed, mapping = (aes(mean_calories, mean_min_asleep))) +
    geom_point(
      stat = "summary",
      fun = "mean") + geom_smooth(color = "cyan") +
    labs(
      title = "Calories vs Time Asleep", 
      x = "Average Calories",
      y = "Avearge Minutes Asleep"
    ) + theme_minimal()
  
  # Grouped bar chart showing time spent in bed activities by days of the week.
  
  time_wasted_bar_chart <- as.data.frame(clean_Sleep)
  time_wasted_bar_chart$SleepDay <- weekdays(time_wasted_bar_chart$SleepDay)
  
  dodge_chart <- time_wasted_bar_chart %>% 
    group_by(SleepDay) %>% 
    summarise(total_minutes = sum(TotalTimeInBed), minutes_asleep = sum(TotalMinutesAsleep), wasted_minutes_in_bed = sum(TotalTimeInBed) - sum(TotalMinutesAsleep)) %>% 
    pivot_longer(-SleepDay, names_to = "sleep_activities")
    
    ggplot(dodge_chart, aes(fill = sleep_activities, x = SleepDay, y = value)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(
        title = "Time Spent In Bed",
        x = "Days of the Week", 
        y = "Minutes"
      )
    

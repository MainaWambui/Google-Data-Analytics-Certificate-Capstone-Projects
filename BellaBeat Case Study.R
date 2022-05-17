library(dplyr)
library(tidyr)
library(lubridate)
#######################################Load Data####################################################
#Daily Data
daily_calories<-read.csv("Bellabeat/dailyCalories_merged.csv")
daily_activity<-read.csv("Bellabeat/dailyActivity_merged.csv")
daily_steps<-read.csv("Bellabeat/dailySteps_merged.csv")
daily_intensities<-read.csv("Bellabeat/dailyIntensities_merged.csv")
dailysleep<-read.csv("Bellabeat/sleepDay_merged.csv")
#Hourly data
hourly_calories<-read.csv("Bellabeat/hourlyCalories_merged.csv")
hourly_steps<-read.csv("Bellabeat/hourlySteps_merged.csv")
hourly_intensities<-read.csv("Bellabeat/hourlyIntensities_merged.csv")

#######################################Clean data##########################################
#Daily Activity
#Get the description of the dataframe.
str(daily_activity)
#1)Remove rows with nulls
daily_activity<-na.omit(daily_activity)
print(nrow(daily_activity))
#2)Remove rows with missing values & NA values
daily_activity[!daily_activity$ActivityDate==""]
print(nrow(daily_activity))
#3)Remove duplicates
daily_activity%>%distinct(daily_activity$Id, .keep_all = TRUE)
print(nrow(daily_activity))
#4)Clean date format
daily_activity$ActivityDate=as.POSIXct(daily_activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
#Get a summary of the data
summary(daily_activity)
#Get unique daily intakes for each ID
#daily_activity<-unique(daily_activity[c("Id", "ActivityDate")])
daily_activity %>% distinct(Id, ActivityDate, .keep_all = TRUE)
print(nrow(daily_activity))
#Daily Sleep
#Get the description of the dataframe.
str(dailysleep)
#1)Remove rows with nulls
dailysleep<-na.omit(dailysleep)
#2)Remove rows with missing values & NA values
dailysleep[!dailysleep$SleepDay==""]
#3)Remove duplicates
dailysleep%>%distinct(dailysleep$Id, .keep_all = TRUE)
#4)Clean date format
dailysleep[['SleepDay']] <- as.POSIXct(dailysleep$SleepDay, format="%m/%d/%Y", tz=Sys.timezone())
#dailysleep$SleepDay<-as.Date(dailysleep$SleepDay, format = "%m/%d/%Y")
#format(dailysleep$SleepDay, "%m/%d/%Y")
print(head(dailysleep))
#Get a summary of the data
summary(dailysleep)
#Get unique daily intakes for each ID
#daily_activity<-unique(daily_activity[c("Id", "ActivityDate")])
daily_activity %>% distinct(Id, ActivityDate, .keep_all = TRUE)
print(nrow(daily_activity))
#Rename
names(dailysleep)[names(dailysleep) == 'SleepDay'] <- "ActivityDate"
print(head(dailysleep))
#Merge sleep dataset with the sleep dataset
Daily_data<-inner_join(daily_activity, dailysleep, by=c("Id","ActivityDate"))
Daily_data$month <-months(Daily_data$ActivityDate)
Daily_data$weekday <-weekdays(Daily_data$ActivityDate)
print(head(Daily_data))

#Hourly calories
str(hourly_calories)
hourly_calories$ActivityHour=as.POSIXct(hourly_calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_calories$Time<-format(hourly_calories$ActivityHour, format = "%H:%M:%S")
print(head(hourly_calories))
#Hourly Steps
str(hourly_steps)
hourly_steps$ActivityHour=as.POSIXct(hourly_steps$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_steps$Time<-format(hourly_steps$ActivityHour, format = "%H")
print(head(hourly_steps))

####################################Visualizations#########################################
library(ggplot2)
#Bar chart of calories burnt per hour
library(ggplot2)
library(hrbrthemes)
#Preparation of dataframe
hourly_cal_new <- hourly_calories %>%
  group_by(Time) %>%
  drop_na() %>%
  summarise(total_hourly_calories = sum(Calories))

#BarPlot
ggplot(data=hourly_cal_new, aes(x=Time, y=total_hourly_calories)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Total Calories vs. Time")
#Line plot of total steps per hour
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)

hourly_steps_new <- hourly_steps %>%
  group_by(Time) %>%
  drop_na() %>%
  summarise(total_hourly_steps = mean(StepTotal))
print(hourly_steps_new)

# Plot
hourly_steps_new %>%
  ggplot( aes(x=Time, y=total_hourly_steps)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_ipsum() +
  ggtitle("Total Steps per hour")

hourly_steps_new %>%
  ggplot( aes(x=Time, y=total_hourly_steps)) +
  geom_line() +
  geom_point()

#Lollipop graph - total time in bed vs time asleep
weekday_sleep <- Daily_data %>%
  group_by(weekday) %>%
  drop_na() %>%
  summarise(total_min_asleep = sum(TotalMinutesAsleep),
            total_min_in_bed = sum(TotalTimeInBed))
print(weekday_sleep)

# Plot
ggplot(weekday_sleep) +
  geom_segment( aes(x=weekday, xend=weekday, y=total_min_asleep, yend=total_min_in_bed), color="grey") +
  geom_point( aes(x=weekday, y=total_min_asleep), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=weekday, y=total_min_in_bed), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme(
    legend.position = "left",
  ) +
  xlab("Weekday") +
  ylab("Total Minutes")

#users
daily_average <- Daily_data %>%
  group_by(Id) %>%
  summarise (mean_daily_steps = mean(TotalSteps),
             mean_daily_calories = mean(Calories),
             mean_distance = mean(TotalDistance),
             mean_daily_sleep = mean(TotalMinutesAsleep))

head(daily_average)
user_type <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "Sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "Lightly active", 
    mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "Moderately active", 
    mean_daily_steps >= 10000 ~ "Very active"
  ))

head(user_type)
#pie chart showing the categorisation of users
library(plotly)
user_types<-c("Sedentary users", "Lightly active users", "Moderately active users", "Very active users")
sedentary<-nrow(user_type[user_type$user_type == "Sedentary",])
light<-nrow(user_type[user_type$user_type == "Lightly active",])
moderate<-nrow(user_type[user_type$user_type == "Moderately active",])
active<-nrow(user_type[user_type$user_type == "Very active",])
count_users<-c(sedentary, light, moderate, active)
user_types_df<-data.frame(user_types,count_users)

fig <- plot_ly(user_types_df, labels = ~user_types, values = ~count_users, type = 'pie')
fig <- fig %>% layout(title = 'Classification of Users',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig
#Correlations
#Correlation between calories and steps
ggplot(Daily_data, aes(x=Calories, y=TotalSteps))+
  geom_jitter() +
  geom_smooth(color = "blue") + 
  labs(title = "Daily calories burnt vs Total Steps", x = "Calories burnt", y= "Daily Steps") +
  theme(panel.background = element_blank(),
        plot.title = element_text( size=14))

#correlation between the steps and minutes asleep
ggplot(Daily_data, aes(x=TotalSteps, y=TotalMinutesAsleep))+
  geom_jitter() +
  geom_smooth(color = "blue") + 
  labs(title = "Daily steps vs Minutes asleep", x = "Daily steps", y= "Minutes asleep") +
  theme(panel.background = element_blank(),
        plot.title = element_text( size=14))


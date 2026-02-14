# Loading in the required packages 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)


# Importing the "Daily Activity" dataset
daily_activity <- read.csv("C:/Users/melfl/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/DailyActivityDataset.csv")
head(daily_activity)
glimpse(daily_activity)

# Importing the "Daily Sleep" dataset 
daily_sleep <- read.csv("C:/Users/melfl/Downloads/archive (1)/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/DailySleepDataset.csv")
head(daily_sleep)
glimpse(daily_sleep)

# Importing the "Weight Log" dataset
weight_log <- read.csv("C:/Users/melfl/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
head(weight_log)
glimpse(weight_log)

activity_df = read.csv("C:/Users/melfl/Downloads/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

# Finding the number of participants in each dataset
activity_participants <- n_distinct(activity_df$Id)
sleep_participants <- n_distinct(daily_sleep$Id)
weight_participants <- n_distinct(weight_log$Id)
print(activity_participants)
print(sleep_participants)
print(weight_participants)

datasets_df <- data.frame(
  dataset = c("Activity", "Sleep", "Weight"),
  participants = c(activity_participants, sleep_participants, weight_participants)
  
)

# Bar chart showing the distribution of participants, stratified by dataset
ggplot(data = datasets_df, aes(x=dataset, y=participants, fill=dataset))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("firebrick1", "darkolivegreen1", "dodgerblue1"))+
  labs(x = "Dataset Title", y = "No. of Participants", title="Distribution of Participants by Dataset", fill= "Dataset Title",
       subtitle = "Across the Activity, Sleep, and Weight datasets",
       caption = "Data collected from Fitbit users from April to May 2016. The Weight dataset has 8 participants, far below the recommended minimum of 30, and will be omitted from this analysis.")+
  theme_bw()+
  theme(plot.title = element_text(face ="bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12),
        plot.caption = element_text(face = "italic", size = 8))


# Daily Activity Dataset Processing and Feature Engineering 

# Converting "ActivityDate" to a date-type
glimpse(daily_activity)
daily_activity$ActivityDate_Fixed <- ymd(daily_activity$ActivityDate) 
daily_activity <- select(daily_activity, Id, -ActivityDate, ActivityDate_Fixed, 3:5, -LoggedActivitiesDistance, 7:16) 
glimpse(daily_activity) 
head(daily_activity)


# Converting "SleepDay" to a date-type 
glimpse(daily_sleep)
daily_sleep$SleepDate_Fixed <- ymd(daily_sleep$SleepDay) 
daily_sleep <- select(daily_sleep, Id, -SleepDay, SleepDate_Fixed, 3:6) 
glimpse(daily_sleep) 
head(daily_sleep)

# Finding the number of missing values and duplicates in the " Daily Activity" dataset
sum(is.na(daily_activity))
sum(is.na(daily_sleep))
sum(duplicated(daily_activity))

# Finding the number of and removing the missing values and duplicates in the "Daily Sleep" dataset
sum(duplicated(daily_sleep))
duplicated(daily_sleep)

daily_sleep_clean <- daily_sleep[!duplicated(daily_sleep), ]
sum(duplicated(daily_sleep_clean))

# Combining the "Daily Activity" and "Daily Sleep" datasets with a left join
fitness_data_df <- left_join(daily_activity, daily_sleep_clean, by= c("Id" = "Id", "ActivityDate_Fixed" = "SleepDate_Fixed", "IsWeekend" = "IsWeekend"))
head(fitness_data_df)
glimpse(fitness_data_df)


# Summary statistics for "TotalSteps" (grouped by participant) 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_total_steps = mean(TotalSteps),
    median_total_steps = median(TotalSteps), 
    sd_total_steps = sd(TotalSteps),
    min_total_steps = min(TotalSteps),
    max_total_steps = max(TotalSteps),
    iqr_total_steps = IQR(TotalSteps)
  )


# Summary statistics for "TotalSteps" (grouped by type of day) 
fitness_data_df %>% 
  group_by(IsWeekend) %>% 
  summarize(
    mean_total_steps = mean(TotalSteps),
    median_total_steps = median(TotalSteps), 
    sd_total_steps = sd(TotalSteps),
    min_total_steps = min(TotalSteps),
    max_total_steps = max(TotalSteps),
    iqr_total_steps = IQR(TotalSteps)
  )


# Boxplot showing the step count by participant
fitness_data_factor_df <- fitness_data_df
fitness_data_factor_df$Id <- as.factor(fitness_data_df$Id)
x_axis_labels <- setNames(1:length(levels(fitness_data_factor_df$Id)), levels(fitness_data_factor_df$Id))

ggplot(data=fitness_data_factor_df, mapping=aes(x=Id, y=TotalSteps, fill=Id))+
  geom_boxplot()+
  scale_x_discrete(labels = x_axis_labels)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color= 
                 "black")+
  labs(title="Step Count By Participant",
       subtitle = "Stratified by participant ID. Distribution of total steps, showing the quartiles, outliers, and the mean (denoted by *)",
       caption = "Participant IDs have been mapped to simplified sequential numbers on the horizontal axis for readability.",
       x = "Participant ID",
       y = "Step Count",
       fill = "Participant ID")+
  theme_bw()+
  theme(plot.title = element_text(face ="bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12),
        plot.caption = element_text(face = "italic", size = 8))

# Boxplot showing the overall step count, stratified by weekdays and weekends
ggplot(data=fitness_data_df, mapping=aes(x=IsWeekend, y=TotalSteps, col = IsWeekend))+
         geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color= 
                 "black")+
  stat_summary(fun = "mean", geom = "text", col = "black", vjust=2.5, aes(label = paste("Mean Step Count:", round(..y.., digits=0))))+
  labs(title ="Overall Step Count During Weekdays and Weekends",
       subtitle = "Stratified by day type",
       x = "Is it a Weekend?",
       y = "Step Count",
       color = "Is it a Weekend?")+
  theme_bw()+
  theme(plot.title = element_text(face= "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))


# Summary statistics for "TotalDistance" (grouped by participant) 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_total_distance = mean(TotalDistance),
    median_total_distance = median(TotalDistance), 
    sd_total_distance = sd(TotalDistance),
    min_total_distance = min(TotalDistance),
    max_total_distance = max(TotalDistance),
    iqr_total_distance = IQR(TotalDistance)
  )

# Summary statistics for "TotalDistance" (grouped by type of day) 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_total_distance = mean(TotalDistance),
    median_total_distance = median(TotalDistance), 
    sd_total_distance = sd(TotalDistance),
    min_total_distance = min(TotalDistance),
    max_total_distance = max(TotalDistance),
    iqr_total_distance = IQR(TotalDistance)
  )

# Boxplot showing the overall distance traveled, stratified by weekdays and weekends
ggplot(data=fitness_data_df, mapping=aes(x=IsWeekend, y=TotalDistance, col = IsWeekend))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color= 
                 "black")+
  stat_summary(fun = "mean", geom = "text", col = "black", vjust=3.0, aes(label = paste("Mean Distance:", round(..y.., digits=2), "mi")))+
  labs(title ="Overall Distance Traveled During Weekdays and Weekends",
       subtitle = "Stratified by day type",
       x = "Is it a Weekend?",
       y = "Distance (mi)",
       color = "Is it a Weekend?")+
  theme_bw()+
  theme(plot.title = element_text(face= "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))


# Summary statistics for "VeryActiveDistance" 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_very_active_distance = mean(VeryActiveDistance),
    median_very_active_distance = median(VeryActiveDistance), 
    sd_very_active_distance = sd(VeryActiveDistance),
    min_very_active_distance = min(VeryActiveDistance),
    max_very_active_distance = max(VeryActiveDistance),
    iqr_very_active_distance = IQR(VeryActiveDistance)
  )

# Summary statistics for "ModeratelyActiveDistance" 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_moderately_active_distance = mean(ModeratelyActiveDistance),
    median_moderately_active_distance = median(ModeratelyActiveDistance), 
    sd_moderately_active_distance = sd(ModeratelyActiveDistance),
    min_moderately_active_distance = min(ModeratelyActiveDistance),
    max_moderately_active_distance = max(ModeratelyActiveDistance),
    iqr_moderately_active_distance = IQR(ModeratelyActiveDistance)
  )

# Summary statistics for "LightActiveDistance" 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_light_active_distance = mean(LightActiveDistance),
    median_light_active_distance = median(LightActiveDistance), 
    sd_light_active_distance = sd(LightActiveDistance),
    min_light_active_distance = min(LightActiveDistance),
    max_light_active_distance = max(LightActiveDistance),
    iqr_light_active_distance = IQR(LightActiveDistance)
  )

# Summary statistics for "SedentaryActiveDistance" 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_sedentary_active_distance = mean(SedentaryActiveDistance),
    median_sedentary_active_distance = median(SedentaryActiveDistance), 
    sd_sedentary_active_distance = sd(SedentaryActiveDistance),
    min_sedentary_active_distance = min(SedentaryActiveDistance),
    max_sedentary_active_distance = max(SedentaryActiveDistance),
    iqr_sedentary_active_distance = IQR(SedentaryActiveDistance)
  )


distance_data <- fitness_data_df %>% 
  select("VeryActiveDistance", "ModeratelyActiveDistance", "LightActiveDistance", "SedentaryActiveDistance", "IsWeekend")
head(distance_data)
glimpse(distance_data)

# Converting "Distance Data" to long format to facilitate data visualization 
long_distance <- pivot_longer(distance_data,
                              cols = c("VeryActiveDistance", "ModeratelyActiveDistance", "LightActiveDistance", "SedentaryActiveDistance"),
                              names_to = ("DistanceType"),
                              values_to = ("Distance"))
head(long_distance)
glimpse(long_distance)

# Bar chart showing the average distance traveled, stratified by activity level
ggplot(data = long_distance, mapping = aes(x = DistanceType, y = Distance, fill=DistanceType))+
  geom_bar(position = "dodge", stat = "summary", fun = "mean")+
  stat_summary(fun = "mean", geom = "text", col = "black", vjust=2.0, aes(label = paste("Avg. Distance:", round(..y.., digits=3), "mi")))+
  scale_fill_manual(values = c("lightcoral", "indianred", "mistyrose", "darkred"),
                               labels = c("Light Active", "Moderately Active", "Sedentary Active", "Very Active"))+
  scale_x_discrete(
    labels = c("Light Active", "Moderately Active", "Sedentary Active", "Very Active")
  )+
  scale_color_discrete(labels = c("Light Active", "Moderately Active", "Sedentary Active", "Very Active"))+
  labs(title="Average Distance Traveled By Activity Level",
       subtitle = "Stratified by activity level",
       caption = "Activity levels are defined based on the total distance traveled / steps taken per day: Sedentary Active, Light Active, Moderately Active, and Very Active.",
       x = "Activity Level",
       y = "Distance (mi)",
       fill = "Activity Level")+
  theme_bw()+
  theme(plot.title = element_text(face= "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12),
        plot.caption = element_text(face = "italic", size = 8))


# Summary statistics for "VeryActiveMinutes" 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_very_active_minutes = mean(VeryActiveMinutes),
    median_very_active_minutes= median(VeryActiveMinutes), 
    sd_very_active_minutes = sd(VeryActiveMinutes),
    min_very_active_minutes = min(VeryActiveMinutes),
    max_very_active_minutes = max(VeryActiveMinutes),
    iqr_very_active_minutes = IQR(VeryActiveMinutes)
  )

# Summary statistics for "FairlyActiveMinutes" 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_fairly_active_minutes = mean(FairlyActiveMinutes),
    median_fairly_active_minutes = median(FairlyActiveMinutes), 
    sd_fairly_active_minutes = sd(FairlyActiveMinutes),
    min_fairly_active_minutes = min(FairlyActiveMinutes),
    max_fairly_active_minutes = max(FairlyActiveMinutes),
    iqr_fairly_active_minutes = IQR(FairlyActiveMinutes)
  )

# Summary statistics for "LightlyActiveMinutes" 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_lightly_active_minutes = mean(LightlyActiveMinutes),
    median_lightly_active_minutes = median(LightlyActiveMinutes), 
    sd_lightly_active_minutes = sd(LightlyActiveMinutes),
    min_lightly_active_minutes = min(LightlyActiveMinutes),
    max_lightly_active_minutes = max(LightlyActiveMinutes),
    iqr_lightly_active_minutes = IQR(LightlyActiveMinutes)
  )

# Summary statistics for "SedentaryMinutes" 
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_sedentary_minutes = mean(SedentaryMinutes),
    median_sedentary_minutes = median(SedentaryMinutes), 
    sd_sedentary_minutes = sd(SedentaryMinutes),
    min_sedentary_minutes = min(SedentaryMinutes),
    max_sedentary_minutes = max(SedentaryMinutes),
    iqr_sedentary_minutes = IQR(SedentaryMinutes)
  )


minutes_data <- fitness_data_df %>% 
  select("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes", "IsWeekend")
head(minutes_data)
glimpse(minutes_data)

# Converting "Minutes Data" to long format to facilitate data visualization 
long_minutes <- pivot_longer(minutes_data,
                              cols = c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes"),
                              names_to = ("MinuteType"),
                              values_to = ("Minutes"))


head(long_minutes)
glimpse(long_minutes)

# Bar chart showing the average minutes active, stratified by activity level
ggplot(data = long_minutes, mapping = aes(x = MinuteType, y = Minutes, fill=MinuteType))+
  geom_bar(position = "dodge", stat = "summary", fun = "mean")+
  stat_summary(fun = "mean", geom = "text", col = "black", vjust=2.5, aes(label = paste("Avg. Minutes:", round(..y.., digits=2), "min")))+
  scale_fill_manual(values = c("lightcoral", "indianred", "mistyrose", "darkred"),
                    labels = c("Light Active", "Moderately Active", "Sedentary Active", "Very Active"))+
  scale_x_discrete(
    labels = c("Light Active", "Moderately Active", "Sedentary Active", "Very Active")
  )+
  scale_color_discrete(labels = c("Light Active", "Moderately Active", "Sedentary Active", "Very Active"))+
  labs(title ="Average Minutes Active By Activity Level",
       subtitle = "Stratified by activity level",
       x = "Activity Level",
       y = "Minutes",
       fill = "Activity Level")+
  theme_bw()+
  theme(plot.title = element_text(face= "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))

mean_daily_minutes_df <- fitness_data_df %>% 
  group_by(ActivityDate_Fixed, IsWeekend) %>% 
  summarize(mean_daily_very_active_minutes = mean(VeryActiveMinutes),
            mean_daily_fairly_active_minutes = mean(FairlyActiveMinutes),
            mean_lightly_active_minutes = mean(LightlyActiveMinutes),
            mean_sedentary_minutes = mean(SedentaryMinutes))

head(mean_daily_minutes_df)
glimpse(mean_daily_minutes_df)

long_mean_minutes <- pivot_longer(mean_daily_minutes_df,
                             cols = c("mean_daily_very_active_minutes", "mean_daily_fairly_active_minutes", "mean_lightly_active_minutes", "mean_sedentary_minutes"),
                             names_to = ("MinuteType"),
                             values_to = ("Minutes"))

head(long_mean_minutes)
glimpse(long_mean_minutes)

mean_daily_total_steps_df <- fitness_data_df %>% 
  group_by(ActivityDate_Fixed, IsWeekend) %>% 
  summarize(mean_daily_total_steps = mean(TotalSteps))


head(mean_daily_total_steps_df)
glimpse(mean_daily_total_steps_df)

# Line chart showing the average daily step count, stratified by type of day
ggplot(data = mean_daily_total_steps_df, mapping = aes(x = ActivityDate_Fixed, y = mean_daily_total_steps, color=IsWeekend))+
  geom_line()+
  geom_point()+
  labs(title = "Average Daily Step Count Over One Month",
       subtitle = "Stratified by day type",
       x = "Date",
       y = "Step Count",
       col = "Is it a Weekend?")+
  theme_bw()+
  theme(plot.title = element_text(face= "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))
  


mean_daily_calories_df <- fitness_data_df %>% 
  group_by(ActivityDate_Fixed) %>% 
  summarize(mean_daily_calories = mean(Calories))


head(mean_daily_calories_df)
glimpse(mean_daily_calories_df)

mean_daily_total_steps_calories_df <- merge(mean_daily_total_steps_df, mean_daily_calories_df, by="ActivityDate_Fixed")
head(mean_daily_total_steps_calories_df)
glimpse(mean_daily_total_steps_calories_df)

# Calculating the correlation coefficient between MeanDailyTotalSteps and MeanDailyTotalCalories 
r_value_calories_steps <- cor(mean_daily_total_steps_calories_df$mean_daily_total_steps, mean_daily_total_steps_calories_df$mean_daily_calories, use="complete.obs")

# Scatterplot showing the relationship between average daily calories burned and average daily total steps, stratified by type of day
ggplot(data = mean_daily_total_steps_calories_df, mapping = aes(x = mean_daily_total_steps, y = mean_daily_calories))+
  geom_point(aes(color = IsWeekend))+
  geom_smooth(method = "lm", se = FALSE, color="black")+
  labs(title = "Average Daily Calories Burned by Average Daily Total Steps",
       subtitle = "Stratified by day type",
       caption = paste0("The Pearson correlation coefficient of r = ", round(r_value_calories_steps, 2), " suggests a very strong positive relationship between total calories burned and total steps."),
       x = "Step Count",
       y = "Calories Burned (kcal)",
       color = "Is it a Weekend?")+
  theme_bw()+
  theme(plot.title = element_text(face= "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12),
        plot.caption = element_text(face = "italic", size = 9))

activity_correlation_matrix_df <- fitness_data_df[, c("TotalSteps", "TotalDistance", "VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes", "Calories")]
head(activity_correlation_matrix_df)
glimpse(activity_correlation_matrix_df)

activity_correlation_matrix <- round(cor(activity_correlation_matrix_df), 2)
print(activity_correlation_matrix)

# Heatmap showing the relationship between key variables in the "Daily Activity" dataset
ggcorrplot(activity_correlation_matrix, hc.order = TRUE, type = "upper",
           title = "Correlation Matrix for the Activity Dataset",
           lab = TRUE, 
           lab_col ="black",
           lab_size = 2, ggtheme = theme_grey,
           outline.color = "black",
           colors= c("lightskyblue", "white", "indianred"))+
  theme(plot.title = element_text(face="bold", size = 14))
  
# Daily Sleep Dataset Analysis

# Summary statistics for "TotalMinutesAsleep" (grouped by participant)
fitness_data_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_total_minutes_asleep = mean(TotalMinutesAsleep, na.rm = TRUE),
    median_total_minutes_asleep = median(TotalMinutesAsleep, na.rm = TRUE), 
    sd_total_minutes_asleep = sd(TotalMinutesAsleep, na.rm = TRUE),
    min_total_minutes_asleep = min(TotalMinutesAsleep, na.rm = TRUE),
    max_total_minutes_asleep = max(TotalMinutesAsleep, na.rm = TRUE),
    iqr_total_minutes_asleep = IQR(TotalMinutesAsleep, na.rm = TRUE)
  )

fitness_data_mean_df <- fitness_data_df %>% 
  group_by(ActivityDate_Fixed, IsWeekend) %>% 
  summarize(
    mean_total_minutes_asleep = mean(TotalMinutesAsleep, na.rm = TRUE)
  )


# Summary statistics for "TotalMinutesAsleep" (grouped by type of day)
fitness_data_df %>% 
  group_by(IsWeekend) %>% 
  summarize(
    mean_total_minutes_asleep = mean(TotalMinutesAsleep, na.rm = TRUE),
    median_total_minutes_asleep= median(TotalMinutesAsleep, na.rm = TRUE), 
    sd_total_minutes_asleep = sd(TotalMinutesAsleep, na.rm = TRUE),
    min_total_minutes_asleep = min(TotalMinutesAsleep, na.rm = TRUE),
    max_total_minutes_asleep = max(TotalMinutesAsleep, na.rm = TRUE),
    iqr_total_minutes_asleep = IQR(TotalMinutesAsleep, na.rm = TRUE)
  )


# Boxplot showing the minutes spent asleep, stratified by participant ID
ggplot(data=fitness_data_factor_df, mapping=aes(x=Id, y=TotalMinutesAsleep, fill=Id))+
  geom_boxplot()+
  scale_x_discrete(labels = x_axis_labels)+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color= 
                 "black")+
  labs(title = "Minutes Asleep by Participant",
       subtitle = "Stratified by participant ID",
       caption = "Participant IDs have been mapped to simplified sequential numbers on the horizontal axis for readability.",
       x = "Participant ID",
       y = "Minutes",
       fill = "Participant ID")+
theme_bw()+
  theme(plot.title = element_text(face ="bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))

# Boxplot showing the minutes spent asleep, stratified by type of day 
ggplot(data=fitness_data_df, mapping=aes(x=IsWeekend, y=TotalMinutesAsleep, col = IsWeekend))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color= 
                 "black")+
  stat_summary(fun = "mean", geom = "text", col = "black", vjust=2.5, aes(label = paste("Mean Minutes Asleep:", round(..y.., digits=2))))+
  labs(title= "Minutes Asleep During Weekdays and Weekends",
       subtitle = "Stratified by day type",
       x = "Is it a Weekend?",
       y = "Minutes Asleep",
       col = "Is it a Weekend?")+ 
  theme_bw()+
  theme(plot.title = element_text(face="bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))

fitness_data_sleep_df <- fitness_data_df

# Line chart showing the average daily minutes spent asleep over one month, stratified by type of day
ggplot(data=fitness_data_mean_df, mapping=aes(x=ActivityDate_Fixed, y=mean_total_minutes_asleep, col = IsWeekend))+
  geom_line()+
  geom_point()+
  labs(title = "Average Daily Minutes Asleep Over One Month",
       subtitle = "Stratified by day type",
       x = "Date",
       y = "Step Count",
       col = "Is it a Weekend?")+
  theme_bw()+
  theme(plot.title = element_text(face= "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))



# Adding in a new column, SleepEfficiency, the ratio of total time asleep to total time in bed 
fitness_data_sleep_df$SleepEfficiency <- ((fitness_data_sleep_df$TotalMinutesAsleep / fitness_data_sleep_df$TotalTimeInBed) * 100)
fitness_data_sleep_df
fitness_data_sleep_df <- na.omit(fitness_data_sleep_df)

# Summary statistics for "SleepEfficiency" (grouped by participant)
fitness_data_sleep_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_sleep_efficiency = mean(SleepEfficiency, na.rm = TRUE),
    median_sleep_efficiency = median(SleepEfficiency, na.rm = TRUE), 
    sd_sleep_efficiency = sd(SleepEfficiency, na.rm = TRUE),
    min_sleep_efficiency = min(SleepEfficiency, na.rm = TRUE),
    max_sleep_efficiency = max(SleepEfficiency, na.rm = TRUE),
    iqr_sleep_efficiency = IQR(SleepEfficiency, na.rm = TRUE)
  )

# Summary statistics for "SleepEfficiency" (grouped by type of day)
fitness_data_sleep_df %>% 
  group_by(IsWeekend) %>% 
  summarize(
    mean_sleep_efficiency = mean(SleepEfficiency, na.rm = TRUE),
    median_sleep_efficiency = median(SleepEfficiency, na.rm = TRUE), 
    sd_sleep_efficiency = sd(SleepEfficiency, na.rm = TRUE),
    min_sleep_efficiency = min(SleepEfficiency, na.rm = TRUE),
    max_sleep_efficiency = max(SleepEfficiency, na.rm = TRUE),
    iqr_sleep_efficiency = IQR(SleepEfficiency, na.rm = TRUE)
  )

# Boxplot showing the overall sleep efficiency, stratified by type of day
ggplot(data=fitness_data_sleep_df, mapping=aes(x=IsWeekend, y=SleepEfficiency, col = IsWeekend))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color= 
                 "black")+
  stat_summary(fun = "mean", geom = "text", col = "black", vjust=2.0, aes(label = paste("Mean Sleep Efficiency %:", round(..y.., digits=2))))+
  labs(title ="Overall Sleep Efficiency During Weekdays and Weekends",
       subtitle = "Stratified by day type",
       x = "Is it a Weekend?",
       y = "Sleep Efficiency (%)",
       col = "Is it a Weekend?")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))

mean_daily_sleep_efficiency_df <- fitness_data_sleep_df %>% 
  group_by(ActivityDate_Fixed, IsWeekend) %>% 
  summarize(mean_daily_sleep_efficiency = mean(SleepEfficiency))


head(mean_daily_sleep_efficiency_df)
glimpse(mean_daily_sleep_efficiency_df)

fitness_data_sleep_df_2 <- fitness_data_sleep_df %>% 
  select("TotalSteps", "TotalMinutesAsleep", "SleepEfficiency", "IsWeekend")

head(fitness_data_sleep_df_2)

steps_list<- fitness_data_sleep_df_2$TotalSteps

# Determining the first and third quartiles of TotalSteps to better understand the spread of the data 
q1_steps <- quantile(fitness_data_sleep_df_2$TotalSteps, 0.25)
q3_steps <- quantile(fitness_data_sleep_df_2$TotalSteps, 0.75)


# Adding in a new column, StepBand, which categorizes total steps as Low, Medium, or High relative to the first and third quartiles 
fitness_data_step_band_df <- fitness_data_sleep_df_2 %>% 
  mutate(StepBand = case_when (
    TotalSteps < q1_steps ~ 'Low',
    TotalSteps > q3_steps ~ 'High',
    TRUE ~ 'Medium'
  ))

# Calculating the correlation coefficient between "TotalMinutesAsleep" and "SleepEfficiency" 
r_value_minutes_asleep_sleep_efficiency <- fitness_data_step_band_df %>% 
  summarize(
    cor(fitness_data_step_band_df$TotalMinutesAsleep, fitness_data_step_band_df$SleepEfficiency, use="complete.obs")
  )


# Boxplot showing the overall sleep efficiency, stratified by type of day
ggplot(data=fitness_data_sleep_df, mapping=aes(x=IsWeekend, y=SleepEfficiency, col = IsWeekend))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color= 
                 "black")+
  stat_summary(fun = "mean", geom = "text", col = "black", vjust=2.0, aes(label = paste("Mean Sleep Efficiency %:", round(..y.., digits=2))))+
  labs(title ="Overall Sleep Efficiency During Weekdays and Weekends",
       subtitle = "Stratified by day type",
       x = "Is it a Weekend?",
       y = "Sleep Efficiency (%)",
       col = "Is it a Weekend?")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))

sleep_correlation_matrix_df <- fitness_data_sleep_df[, c("TotalMinutesAsleep", "TotalTimeInBed", "SleepEfficiency")]
head(sleep_correlation_matrix_df)
glimpse(sleep_correlation_matrix_df)

sleep_correlation_matrix <- round(cor(sleep_correlation_matrix_df), 2)
print(sleep_correlation_matrix)

# Heatmap showing the relationship between key variables in the "Daily Sleep" dataset
ggcorrplot(sleep_correlation_matrix, hc.order = TRUE, type = "upper",
           title = "Correlation Matrix for the Sleep Dataset",
           lab = TRUE, 
           lab_col ="black",
           lab_size = 2, ggtheme = theme_grey,
           outline.color = "black",
           colors= c("lightskyblue", "white", "indianred"))+
  theme(plot.title = element_text(face="bold", size = 14))


# Activity-Sleep Analysis

# Scatterplot showing the relationship between total minutes spent asleep and sleep efficiency, stratified by step bands
ggplot(data = fitness_data_step_band_df, mapping = aes(x = TotalMinutesAsleep, y=SleepEfficiency, color = StepBand))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color="black")+
  labs(title = "Minutes Asleep by Sleep Efficiency",
       subtitle = "Stratified by step band",
       caption = paste0("Sleep efficiency is the ratio of total time asleep to total time in bed. The Pearson correlation coefficient of r = ", round(r_value_minutes_asleep_sleep_efficiency, 2), " suggests a weak positive relationship between minutes asleep and sleep efficiency."),
       x = "Minutes Asleep",
       y = "Sleep Efficiency (%)",
       color = "Step Band")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12),
        plot.caption = element_text(face = "italic", size = 8))

fitness_data_sleep_df$Id <- as.factor(fitness_data_sleep_df$Id)

x_axis_labels <- setNames(1:length(levels(fitness_data_sleep_df$Id)), levels(fitness_data_sleep_df$Id))

# Calculating the correlation coefficient between "TotalSteps" and "TotalMinutesAsleep" 
r_value_steps_minutes_asleep <- cor(fitness_data_sleep_df$TotalSteps, fitness_data_sleep_df$TotalMinutesAsleep, use="complete.obs")

# Calculating the correlation coefficient between "TotalSteps" and "SleepEfficiency" 
r_value_steps_sleep_efficiency <- cor(fitness_data_sleep_df$TotalSteps, fitness_data_sleep_df$SleepEfficiency, use="complete.obs")

# Calculating the correlation coefficient between "Calories" and "SleepEfficiency" 
r_value_calories_sleep_efficiency <- cor(fitness_data_sleep_df$Calories, fitness_data_sleep_df$SleepEfficiency, use="complete.obs")

activity_sleep_correlation_matrix_df <- fitness_data_sleep_df [, c("TotalSteps", "TotalDistance", "Calories", "TotalMinutesAsleep", "SleepEfficiency")]
clean_activity_sleep_correlation_matrix_df <- activity_sleep_correlation_matrix_df  %>%
  drop_na()

head(clean_activity_sleep_correlation_matrix_df)
glimpse(clean_activity_sleep_correlation_matrix_df)


clean_activity_sleep_correlation_matrix <- round(cor(clean_activity_sleep_correlation_matrix_df), 2)
print(clean_activity_sleep_correlation_matrix)

# Heatmap showing the relationship between key variables in the "Activity-Sleep" dataset
ggcorrplot(clean_activity_sleep_correlation_matrix, hc.order = TRUE, type = "upper",
           title = "Correlation Matrix for the Activity-Sleep Dataset",
           lab = TRUE, 
           lab_col ="black",
           lab_size = 2, ggtheme = theme_grey,
           outline.color = "black",
           colors= c("lightskyblue", "white", "indianred"))+
  theme(plot.title = element_text(face="bold", size = 14))

# Scatterplot showing the relationship between daily total steps and daily sleep efficiency, stratified by participant 
ggplot(data = fitness_data_sleep_df, mapping = aes(x = TotalSteps, y = SleepEfficiency, col=Id))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color="black")+
  labs(title = "Daily Sleep Efficiency by Daily Total Steps",
       subtitle = "Stratified by participant ID",
       caption = paste0("The Pearson correlation coefficient of r = ", round(r_value_steps_minutes_asleep, 2), " suggests a very weak negative relationship between sleep efficiency and total steps."),
       x = "Step Count",
       y = "Sleep Efficiency (%)",
       color = "Participant ID")+
theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12),
        plot.caption = element_text(face = "italic", size = 8))

# Calculating the change in steps and minutes asleep using the mean steps and minutes asleep on weekdays and weekends, respectively
fitness_data_sleep_mean_steps_sleep <- fitness_data_sleep_df %>% 
  group_by(Id) %>% 
  summarize(
    mean_weekday_steps = mean(TotalSteps[!IsWeekend], na.rm=TRUE),
    mean_weekend_steps = mean(TotalSteps[IsWeekend], na.rm=TRUE), 
    mean_weekday_sleep = mean(TotalMinutesAsleep[!IsWeekend], na.rm=TRUE),
    mean_weekend_sleep = mean(TotalMinutesAsleep[IsWeekend], na.rm=TRUE)
  ) %>% 
mutate(
  delta_steps = mean_weekend_steps - mean_weekday_steps,
  delta_sleep = mean_weekend_sleep - mean_weekday_sleep
)


fitness_data_sleep_mean_steps_sleep <- fitness_data_sleep_mean_steps_sleep %>% 
  filter(!is.na(delta_steps))

# Adding in a new column, BehavioralProfile, which determines participants' behavioral profile--Lazy Sunday, Weekend Warrior, Inactive / Poor Sleep, or Active but Sleep Loss--based on their DeltaSteps and DeltaSleep values
fitness_data_sleep_mean_steps_sleep <- fitness_data_sleep_mean_steps_sleep %>% 
  mutate(BehavioralProfile = case_when (
    delta_steps < 0 & delta_sleep > 0 ~ 'Lazy Sunday',
    delta_steps > 0 & delta_sleep > 0 ~ 'Weekend Warrior',
    delta_steps < 0 & delta_sleep < 0 ~ 'Inactive / Poor Sleep',
    TRUE ~ 'Active but Sleep Loss'
  ))

# Calculating the number of participants in each behavioral profile
count_group_df <- fitness_data_sleep_mean_steps_sleep %>% 
  group_by(BehavioralProfile) %>% 
  summarize(count_group = n()
  )


count_group_percentage_df <- fitness_data_sleep_mean_steps_sleep %>% 
  group_by(BehavioralProfile) %>% 
  summarize(count_group = n(),
            percentage = (count_group / nrow(fitness_data_sleep_mean_steps_sleep)) * 100
)

# Bar chart showing the percentage distribution of behavioral profiles
ggplot(data = count_group_percentage_df, mapping = aes(x = BehavioralProfile, y = percentage, fill=BehavioralProfile))+
  geom_bar(stat="identity")+
  labs(title = "Percentage Distribution of Participants' Activity-Sleep Behavioral Profiles",
       caption = "A participant's behavioral profile is determined using the change in steps and change in minutes asleep.",
       x = "Behavioral Profile", 
       y = "Percentage of Participants (%)",
       fill = "Behavioral Profile")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", size = 14),
        plot.caption = element_text(face = "italic", size = 8))

# Scatterplot showing the change in minutes asleep by the change in total steps per participant, stratified by behavioral profile
ggplot(data = fitness_data_sleep_mean_steps_sleep, mapping = aes(x = delta_steps, y = delta_sleep, color=BehavioralProfile))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  labs(title = "Change in Minutes Asleep by Change in Total Steps",
       subtitle = "Stratified by behavioral profile",
       x = "Δ Total Steps", 
       y = "Δ Minutes ASleep",
       color = "Behavioral Profile")+
  theme_bw()+
  theme(plot.title = element_text(face ="bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12))




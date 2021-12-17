#load libraries and dependencies

library(tidyverse) #helps wrangle data
library(ggplot2) #for visualuization plots
library(dplyr) #for dataframe manipulation
library(tidyr) #to create tidy data
library(lubridate) #helps work on date-time data

setwd("...") #sets working directory (enter your path between the double quotes


#import data as R dataframes

daily_steps <- read.csv("daily_steps.csv")
daily_activity <- read.csv("daily_activity.csv")
daily_calories <- read.csv("daily_calories.csv")
daily_intensities <- read.csv("daily_intensities.csv")
sleep_day <- read.csv("sleep_day.csv")
bmi_df <- read.csv("weightLogInfo_merged.csv")

#view the top 6 observations of each dataframe

head(daily_activity)
head(daily_calories)
head(daily_intensities)
head(daily_steps)
head(bmi_df)
head(sleep_day)

#explore data and and identify what data is required and what isn't

View(daily_activity)        #dont need loggedactivitiesdist, TrackerDistance,VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, SedentaryActiveDistance
View(daily_calories)
View(daily_intensities)
View(daily_steps)
View(bmi_df)                #don't need weight in pounds, fat, ismanualreport, logID
View(sleep_day)


# prepare each dataframe one-by-one

colnames(daily_activity)     #changing column names
daily_activity <- rename(daily_activity,
                         id= Id,
                         calories = Calories,
                         activity_date = ActivityDate,
                         total_steps = TotalSteps,
                         total_dist = TotalDistance,
                         very_active_mins = VeryActiveMinutes,
                         fairly_active_mins = FairlyActiveMinutes,
                         lightly_active_mins = LightlyActiveMinutes,
                         sedentary_mins = SedentaryMinutes)


daily_activity<-daily_activity %>% 
  mutate(active_mins = fairly_active_mins+lightly_active_mins+very_active_mins)       #combine all types of active mins

daily_activity <- daily_activity %>% 
  select(-c(LoggedActivitiesDistance,
            TrackerDistance,VeryActiveDistance,
            ModeratelyActiveDistance,
            LightActiveDistance,
            SedentaryActiveDistance,
            fairly_active_mins,
            lightly_active_mins,
            very_active_mins))                                                         #remove columns that have no use

daily_activity<-daily_activity[,c("id","activity_date","total_steps","total_dist","active_mins","sedentary_mins","calories")]       #rearrange column order


# repeat for all dataframes

colnames(bmi_df)
bmi_df <- rename(bmi_df,
              id=Id,
              activity_date=Date,
              weight=WeightKg,
              bmi=BMI,
              is_healthy=IsHealthy)

bmi_df <- bmi_df %>% 
  select(-c(WeightPounds, Fat, IsManualReport, LogId))

colnames(daily_steps)
daily_steps <- rename(daily_steps,
                      id=Id,
                      total_steps= StepTotal,
                      activity_date=ActivityDay)

colnames(daily_calories)
daily_calories<- rename(daily_calories,
                        id=Id,
                        activity_date=ActivityDay,
                        calories=Calories)

colnames(daily_activity)
daily_intensities<-rename(daily_intensities,
                          id=Id,
                          activity_date=ActivityDay,
                          sedentary_mins=SedentaryMinutes,
                          lightly_active_mins=LightlyActiveMinutes,
                          fairly_active_mins=FairlyActiveMinutes,
                          very_active_mins=VeryActiveMinutes
)
daily_intensities<-daily_intensities %>% 
  mutate(active_mins=fairly_active_mins+lightly_active_mins+very_active_mins) #combine active mins

daily_intensities<-daily_intensities %>% 
  select(-c(SedentaryActiveDistance,
            LightActiveDistance,
            ModeratelyActiveDistance,
            VeryActiveDistance,
            fairly_active_mins,
            lightly_active_mins,
            very_active_mins))                                                #remove columns of no use

daily_intensities<-daily_intensities[,c("id","activity_date","active_mins","sedentary_mins")]

colnames(sleep_day)
sleep_day<-rename(sleep_day,
                  id=Id,
                  activity_date=SleepDay,
                  mins_asleep=TotalMinutesAsleep,
                  mins_in_bed=TotalTimeInBed)
sleep_day<-sleep_day %>% 
  select(-c(TotalSleepRecords))

# CONVERSION OF DATA TYPES

daily_activity <-  mutate(daily_activity, id = as.character(id),
                         activity_date= as_date(activity_date, format=("%m/%d/%Y")),
                         active_mins=as.difftime(active_mins, units = "mins"),
                         sedentary_mins=as.difftime(sedentary_mins, units = "mins"))

daily_calories <-  mutate(daily_calories, id = as.character(id),
                          activity_date= as_date(activity_date, format=("%m/%d/%Y")))

daily_intensities <-  mutate(daily_intensities, id = as.character(id),
                          activity_date= as_date(activity_date, format=("%m/%d/%Y")),
                          active_mins=as.difftime(active_mins, units = "mins"),
                          sedentary_mins=as.difftime(sedentary_mins, units = "mins"))

daily_steps <-  mutate(daily_steps, id = as.character(id),
                          activity_date= as_date(activity_date, format=("%m/%d/%Y")))

sleep_day <-  mutate(sleep_day, id = as.character(id),
                             activity_date= as_date(activity_date, format=("%m/%d/%Y")),
                             mins_asleep=as.difftime(mins_asleep, units = "mins"),
                             mins_in_bed=as.difftime(mins_in_bed, units = "mins"))

bmi_df <-  mutate(bmi_df, id = as.character(id),
                          activity_date= as_date(activity_date, format=("%m/%d/%Y")),
                  bmi=round(bmi,digits = 2))

# Now take a look at all dataframes
View(daily_activity)
View(daily_calories)
View(daily_intensities)
View(daily_steps)
View(bmi_df) 
View(sleep_day)

#combining dataframes 
#executing a full-join
combined_df <- full_join(daily_activity,sleep_day, by=c("id","activity_date"))
combined_df<- full_join(combined_df, bmi_df, by=c("id","activity_date"))

#info of the combined dataframe
View(combined_df)
str(combined_df)
summary(combined_df)

#Let's start the analysis process

#DATAFRAME DESCRIPTION 
  #intensity_vs_calorie = contains aggregated data of all 33 users uniquely, does not contain weight or bmi
  #weight metrics = contains weight and bmi of those only members who fed weight and bmi data
  #bmi_intensity = contains all data for all users
  #complete_weight_data = contains all possible data of only those users who fet weight and bmi data

intensity_vs_calorie<- combined_df %>%                                  #summarizing data for intensity vs calories lost metrics
       group_by(id) %>%
       summarize(mean_sed_mins=round(mean(sedentary_mins)),
                 mean_active_mins=round(mean(active_mins)),
                 mean_sleep_time=round(mean(mins_asleep)),
                 mean_bed_time = round(mean(mins_in_bed)),
                 mean_steps= round(mean(total_steps)),
                 mean_dist = round(mean(total_dist),2),
                 mean_calories=round(mean(calories))) %>% 
       arrange(id)
View(intensity_vs_calorie)
#### HENCE ONLY 3 USERS FED SLEEP DATA

weight_metrics<-bmi_df %>% 
  group_by(id) %>% 
  summarize(mean_weight=mean(weight),mean_bmi= mean(bmi)) %>% 
  arrange(id)

View(weight_metrics)
#### HENCE ONLY 8 of 33 users fed Weight Data 

bmi_intensity<-full_join(intensity_vs_calorie,bmi_df, by="id")

colnames(bmi_intensity)
bmi_intensity<-bmi_intensity[,c("id","activity_date","mean_sed_mins","mean_active_mins","mean_sleep_time","mean_bed_time","mean_steps","mean_dist", "mean_calories","weight", "bmi", "is_healthy")] #rearrange column order

complete_weight_data<- left_join(weight_metrics, intensity_vs_calorie)  #creating the complete_weight_data dataframe using full-join between weight_metrics and intensity_vs_calorie
View(complete_weight_data)


#Visualizing our findings
caption <-"FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)"
  
  
activemins_vs_bmi<-ggplot(complete_weight_data)+
  geom_point(mapping = aes(x=mean_active_mins, y=mean_bmi)) +
  scale_x_continuous(breaks = seq(from = 0, to = 350, by = 25)) +
  scale_y_continuous(breaks = seq(from = 16, to = 50, by = 1))+
  labs(title = "Active Minutes vs. BMI", subtitle = "the scatter-plot shows relation between active minutes and body mass index of users",caption=caption )+
  annotate("rect", xmin=262, xmax=340, ymin=20, ymax=24.75, alpha=0.2,color="dark green", fill="green")+
  annotate("rect", xmin=25, xmax=60, ymin=47, ymax=48, alpha=0.2,color="dark red", fill="red")+
  annotate("rect", xmin=225, xmax=325, ymin=25, ymax=29, alpha=0.2,color="orange", fill="yellow")+
  annotate("text", x=160, y= 22, label="HEALTHY", fontface= "italic", size=4, color="dark green", alpha=0.5)+
  annotate("text", x=120, y= 27, label="SLIGHTLY OVERWEIGHT", fontface= "italic", size=4, color="dark orange", alpha=0.5)+
  annotate("text", x=160, y= 47.5, label="SEVERELY OBESE", fontface= "italic", size=4, color="red", alpha=0.5)+
  annotate("segment", x = 170, xend = 225, y = 27, yend = 27, colour = "dark orange", size=1, alpha=0.5, arrow=arrow())+
  annotate("segment", x = 187, xend = 262, y = 22, yend = 22, colour = "dark green", size=1, alpha=0.5, arrow=arrow())+
  annotate("segment", x = 120, xend = 60, y = 47.5, yend = 47.5, colour = "red", size=1, alpha=0.5, arrow=arrow())+
  annotate("text", x=200, y= 50, label="Only 8 of the 33 users have shared their weight data", size=5, color="blue")+
  annotate("rect", xmin=87, xmax=312, ymin=49, ymax=51, alpha=0.4,color="blue", fill="light blue")+
  theme_classic()

sedmins_vs_bmi<-ggplot(complete_weight_data)+
  geom_point(mapping = aes(x=mean_sed_mins, y=mean_bmi))+
  scale_x_continuous(breaks = seq(from = 500, to =1450, by = 25)) +
  scale_y_continuous(breaks = seq(from = 16, to = 50, by = 1))+
  labs(title = "Sedentary Minutes vs. BMI", subtitle = "the scatter-plot shows relation between sedentary minutes and body mass index of users",caption=caption)+
  annotate("rect", xmin=650, xmax=1112, ymin=21, ymax=24.75, alpha=0.2,color="dark green", fill="green")+
  annotate("rect", xmin=725, xmax=1137, ymin=25, ymax=29, alpha=0.2,color="orange", fill="yellow")+
  annotate("rect", xmin=1300, xmax=1337, ymin=47, ymax=48, alpha=0.2,color="red", fill="red")+
  annotate("text", x=1250, y= 22, label="HEALTHY", fontface= "italic", size=4, color="dark green", alpha=0.5)+
  annotate("text", x=1270, y= 27, label="SLIGHTLY OVERWEIGHT", fontface= "italic", size=4, color="dark orange", alpha=0.5)+
  annotate("text", x=1150, y= 47.5, label="SEVERELY OBESE", fontface= "italic", size=4, color="red", alpha=0.5)+
  annotate("segment", x = 1187, xend = 1137, y = 27, yend = 27, colour = "dark orange", size=1, alpha=0.5, arrow=arrow())+
  annotate("segment", x = 1212, xend = 1112, y = 22, yend = 22, colour = "dark green", size=1, alpha=0.5, arrow=arrow())+
  annotate("segment", x = 1212, xend = 1300, y = 47.5, yend = 47.5, colour = "red", size=1, alpha=0.5, arrow=arrow())+
  annotate("text", x=870, y= 50, label="Only 8 of the 33 users have shared their weight data", size=5, color="blue")+
  annotate("rect", xmin=662, xmax=1075, ymin=49, ymax=51, alpha=0.4,color="blue", fill="light blue")+
  theme_classic()
  

ggsave(filename = "activemins_vs_bmi.png", plot =activemins_vs_bmi )    #saving the visualizations
ggsave(filename = "sedmins_vs_bmi.png", plot =sedmins_vs_bmi )

#summary of sleep and weight data
sleep_summary<- data.frame(
  data_available=c("yes", "no"),
  total_values=c(3, 30)
)


weight_summary<- data.frame(
  data_available=c("yes", "no"),
  total_values=c(8, 25)
)


#lets save all the acquired dataframes for visualizing on Tableau
write.csv(bmi_intensity, file="bmi_intensity.csv")
write.csv(intensity_vs_calorie, file="intensity_vs_calorie.csv")
write.csv(combined_df,file="combined_df.csv")
write.csv(complete_weight_data, file="complete_weight_data.csv")



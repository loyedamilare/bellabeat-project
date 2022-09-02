install.packages("lubridate")
install.packages("skimr")
install.packages("here")
install.packages("janitor")
library(lubridate)
library(skimr)
library(here)
library(janitor)
daily_activity <- read_csv("dailyActivity_merged.csv")
weight_log <- read.csv("weightLogInfo_merged (2).csv")
sleep_day <- read_csv("sleepDay_merged.csv")
calories <- read_csv("dailyIntensities_merged.csv")
intensity <- read_csv("dailyIntensities_merged.csv")

head(daily_activity)
colnames(daily_activity)
str(daily_activity)
head(weight_log)
colnames(weight_log)
str(weight_log)
head(sleep_day)
colnames(sleep_day)
str(sleep_day)
head(calories)
colnames(calories)
str(calories)
head(intensity)
colnames(intensity)
str(intensity)

# process
## converting all the variables to lower case

daily_activity <- rename_with(daily_activity, tolower)
intensity <- rename_with(intensity, tolower)
weight_log <- rename_with(weight_log, tolower)
calories <- rename_with(calories, tolower)
sleep_day <- rename_with(sleep_day, tolower)

## formatting 

### daily_activity

daily_activity$activitydate = as.POSIXct(daily_activity$activitydate, format="%m/%d/%Y", 
                                         tz= Sys.timezone())
daily_activity$date <- format(daily_activity$activitydate, format = "%m/%d/%Y")
str(daily_activity)
daily_activity$date = as.Date(daily_activity$date, format= "%m/%d/%Y")

### intensity
intensity$activityday = as.POSIXct(intensity$activityday, format = "%m/%d/%Y", 
                                   tz= Sys.timezone())
intensity$time <- format(intensity$activityday, format = "%H:%M:%S")
str(intensity)

### sleep 
sleep_day$sleepday = as.POSIXct(sleep_day$sleepday, format= "%m/%d/%Y %T", 
                                tz=Sys.timezone())
sleep_day$date <- format(sleep_day$sleepday, format = "%m/%d/%y")
sleep_day$date=as.Date(sleep_day$date)

#analysis

n_distinct(daily_activity$id)
n_distinct(sleep_day$id)
n_distinct(intensity$id)
n_distinct(weight_log$id)
n_distinct(calories$id)

## summary statistics
daily_activity %>% 
  select(totalsteps, totaldistance, sedentaryminutes, calories) %>% 
  summary()
daily_activity %>% 
  select(veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes) %>% 
  summary()
sleep_day %>% 
  select(totalsleeprecords, totalminutesasleep, totaltimeinbed) %>% 
  summary()

# combining the datasets
merged_data <- merge(daily_activity, sleep_day, by="id", all= TRUE)

n_distinct(merged_data$id)

activity_mins <- c(21.16, 13.56, 192.8, 991.2)
average_intense_mins <- c("very_activemins", "fairly_activemins", "lightly_activemins", "sedentary_mins")
activeness_summary<- data.frame(activity_mins,average_intense_mins)

activeness_summary$average_intense_mins= factor (activeness_summary$average_intense_mins, ordered = TRUE,
                                             levels = c("fairly_activemins","very_activemins","lightly_activemins", "sedentary_mins"))

str(activeness_summary)
head(activeness_summary)

merged_data$activitydays <- format (merged_data$activitydate, format= "%A")



# data visualization

## totalsteps vs calories

ggplot(data= merged_data, aes(x=totalsteps, y= calories)) + geom_point() + geom_smooth() + 
  labs (title = "totalsteps vs calories")

 ggplot(activeness_summary, aes(x="", y= activity_mins, fill=active_range)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start = 0)+
  theme_void() + labs(title= "Average Active Period and Sedentary Period" ) 

ggplot(activeness_summary, aes(x= average_intense_mins, y=activity_mins,)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(title= "Average Active Period and Sedentary Period")
+ annotate ("text",x="fairly_activemins", y=50,
                                     label=13.56, color = 'darkblue')
+ annotate ("text",x="very_activemins", y=70,label=21.16, color = 'darkblue') + 
  annotate("text",x="lightly_activemins", y= 245, label=192.8, color ='darkblue') +
  annotate("text", x="sedentary_mins", y= 1030, label=991.2, color ='darkblue')


new_int <- merged_data %>% 
  group_by(activitydays) %>% 
  summarise(mean_total_sed = mean(sedentaryminutes), avg_very_activemins = mean(veryactiveminutes), mean(fairlyactiveminutes), 
            avg_light_active_mins = mean(lightlyactiveminutes)) %>% 
  arrange(avg_very_activemins)

new_int$activitydays.f = factor(new_int$activitydays, ordered = TRUE, levels = c("Monday","Tuesday","Wednesday",
                                                                                 "Thursday","Friday","Saturday",
                                                                                 "Sunday"))


ggplot(new_int , aes(x=activitydays.f, y= avg_very_activemins)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 0)) 

ggplot(new_int , aes(x=activitydays.f, y= mean_total_sed)) + geom_histogram(stat = "identity", fill='darkblue') + 
  theme(axis.text.x = element_text(angle=0))

ggplot(new_int , aes(x=activitydays.f, y= avg_light_active_mins)) + geom_histogram(stat = "identity", fill='darkblue') + 
  theme(axis.text.x = element_text(angle=0))

ggplot(sleep_day, aes( x= totalminutesasleep , y=totaltimeinbed)) + geom_point(color="darkblue")
     



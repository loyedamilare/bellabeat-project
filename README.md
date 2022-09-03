# bellabeat-project
# bellabeat-project## BELLABEAT CASE STUDY

### Introduction
 This is my Bellabeat Data analysis Case study, In this case study i was given the task to analyse Fitbit Fitness Tracker Data to provide insight and new gowth opportunity for the company.
 
### About the Company
Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that manufactures health-focused smart products.
Sršen used her background as an artist to develop beautifully designed technology that informs and inspires women around the world. Collecting data on activity, sleep, stress, and reproductive health has allowed Bellabeat to empower women with knowledge about their own health and habits. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women.

### ASK: Questions for the analysis

1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?


### Business Task
I am to analyze smart device usage data in order to gain insight into how people are already using their smart devices and identify Trends that can imform bellabeat marketing strategy.


### Preparations: 
* I identified the data source
* Prepared the data by checking for the organization, sorting and filtering the data.

### *Data source:*
Bellabeat encourages me to use public data that explores smart device users’ daily habits from FitBit Fitness Tracker Data. FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius). This Kaggle data set contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. And It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits. So, here is the link to download the dataset:

* FitBit Fitness Tracker Data : https://www.kaggle.com/arashnic/fitbit

### Setting up my environment:

I installed some R packages that will help me in my analysis.

```{r}
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
```

### loading the packages.


```{r}
library(tidyverse)
library(lubridate)
library(skimr)
library(here)
library(janitor)
```
### Importing the dataset: 

 Fitbit fitness tracker data was Imported: I imported *5 datasets* for the analysis.
 
```{r warning=FALSE, include=FALSE}
daily_activity <- read_csv("dailyActivity_merged.csv")
weight_log <- read.csv("weightLogInfo_merged (2).csv")
sleep_day <- read_csv("sleepDay_merged.csv")
calories <- read_csv("dailyIntensities_merged.csv")
intensity <- read_csv("dailyIntensities_merged.csv")

```
 
### Viewing the dataset:

I viewed the dataset to know the structure, number of columns, and formats of the datasets.

```{r}
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
```

## Cleaning the data (Process Phase)

After viewing the files above, I noticed somethings that needs to be corrected by cleaning the data.

* I noticed In-consistent naming of the columns in the datasets
* I noiticed formatting error
* I did not see any duplicated data
* No spelling error
* I noticed the weight data set was too small to be used in my analysis 

### Correcting the Errors
* I converted all variables to lower case using the rename_with() function.
* I corrected the the error in the data types; the formatting error was in the date data. The data typed was converted from character format to date format.
* The weight data set was removed from my analysis after runing the n_distinct function to know the number of distinct rows in the data set.

### Converting the variables to lower case.


```{r message=FALSE, warning=FALSE}
daily_activity <- rename_with(daily_activity, tolower)
intensity <- rename_with(intensity, tolower)
weight_log <- rename_with(weight_log, tolower)
calories <- rename_with(calories, tolower)
sleep_day <- rename_with(sleep_day, tolower)
```

### Correcting the Date time format:


```{r message=FALSE, warning=FALSE}
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
```

### Exploring and Analyzing the data
 * I checked for the total number of participant in each data set using the n_distinct() function
 
 
```{r message=FALSE, warning=FALSE}
n_distinct(daily_activity$id)
n_distinct(sleep_day$id)
n_distinct(intensity$id)
n_distinct(weight_log$id)
n_distinct(calories$id)

```
 
There are 33 participants in the activity, calories and intensities data sets, 24 in the sleep and only 8 in the weight data set. 8 participants is not significant to make any recommendations and conclusions based on this data.

#### some summary statistics of the data:


*Exploring the daily activity data frame*  


```{r}
daily_activity %>% 
  select(totalsteps, totaldistance, sedentaryminutes, calories) %>% 
  summary()
```



*Exploring intensity of activity*


```{r}
daily_activity %>% 
  select(veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes) %>% 
  summary()
```


*Exploring the sleep data*


```{r}
sleep_day %>% 
  select(totalsleeprecords, totalminutesasleep, totaltimeinbed) %>% 
  summary()
```

### Key findings from this Analysis:

* The average total steps in a day was less than the CDC recommended total steps in a day which is 10000 steps. The CDC did a research and found that, compared with taking 4,000 steps per day, a number considered to be low for adults, taking 8,000 steps per day was associated with a 51% lower risk for all-cause mortality (or death from all causes). Taking 12,000 steps per day was associated with a 65% lower risk compared with taking 4,000 steps.

* The average sedentary time is extremely high (16hrs plus a day). It can be reduced with a good and efficient marketing strategy.

* In the intensity of active minutes majority of the participants are lightly active.

* The participants sleep once for an average of approximately 7hrs.

## Visualization

Before visualizing the data I merged the sleep and the daily_activity data sets using outer join

## Merging the data sets

```{r}
merged_data <- merge(daily_activity, sleep_day, by="id", all= TRUE)
n_distinct(merged_data$id)

```


*I created a new variable in the merged data for* **WEEKDAYS**

```{r}
merged_data$activitydays <- format (merged_data$activitydate, format= "%A")
```


### I also created a dataframe for the average intensity and sedentary minutes

 *This was to bring out the mean intensity minutes and the sedentary minutes to make my visualization easier*

```{r}
activity_mins <- c(21.16, 13.56, 192.8, 991.2)
average_intense_mins <- c("very_activemins", "fairly_activemins", "lightly_activemins", "sedentary_mins")
activeness_summary<- data.frame(activity_mins,average_intense_mins)
```

**I converted the *active_range* variable to an *ordered factor* variable**

```{r}
activeness_summary$average_intense_mins= factor (activeness_summary$average_intense_mins, ordered = TRUE,
                                             levels = c("fairly_activemins","very_activemins","lightly_activemins", "sedentary_mins"))

```



### Creating the Visualization

**Relationship between total steps and calories**

```{r}
ggplot(data= merged_data, aes(x=totalsteps, y= calories)) + geom_point() + geom_smooth() + 
  labs (title = "totalsteps vs calories")
```

*There is a positive correlation between Total steps and calories, meaning the more our steps the more calories we burn.*

**Relationship between the different intensity minutes and sedentary minutes**

```{r}
ggplot(activeness_summary, aes(x= average_intense_mins, y=activity_mins,)) + 
  geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(title= "Average Active Period and Sedentary Period") + 
  annotate ("text",x="fairly_activemins", y=50,label=13.56, color = 'darkblue') + 
  annotate ("text",x="very_activemins", y=70,label=21.16, color = 'darkblue') + 
  annotate("text",x="lightly_activemins", y= 245, label=192.8, color ='darkblue') +
  annotate("text", x="sedentary_mins", y= 1030, label=991.2, color ='darkblue')

```

This visualization shows the extremely high sedentary minutes by individuals compared to active minutes

###### Comparing the different intensity of active period and sedentary period for the days of the week


```{r}
new_int <- merged_data %>% 
  group_by(activitydays) %>% 
  summarise(mean_total_sed = mean(sedentaryminutes), avg_very_activemins = mean(veryactiveminutes), mean(fairlyactiveminutes), 
            avg_light_active_mins = mean(lightlyactiveminutes)) %>% 
  arrange(avg_very_activemins)

new_int$activitydays.f = factor(new_int$activitydays, ordered = TRUE, levels = c("Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday", "Sunday"))



ggplot(new_int , aes(x=activitydays.f, y= avg_very_activemins)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 0)) 

```

*From the visualization Sunday had the lowest very active minutes, Monday and Tuesday has the highest very active minutes*


**Comparing the average total sedentary minutes for the different weekdays**

```{r}
ggplot(new_int , aes(x=activitydays.f, y= mean_total_sed)) + geom_histogram(stat = "identity", fill='darkblue') + 
  theme(axis.text.x = element_text(angle=0))

```

This visualization shows that the average sedentary minutes throughout the week days is almost the same. they have only slight variations.

**Comparing light active minutes with weekdays.**

```{r}
ggplot(new_int , aes(x=activitydays.f, y= avg_light_active_mins)) + geom_histogram(stat = "identity", fill='darkblue') + 
  theme(axis.text.x = element_text(angle=0))
```

**Relationship between total minutes asleep and total time in bed.**

```{r}
ggplot(sleep_day, aes( x= totalminutesasleep , y=totaltimeinbed)) + geom_point(color="darkblue") + 
  labs(title="Total minutes asleep vs Total Time in bed")
```

*This visualization shows a positive correlation between time in bed and total time of sleep.

## Conclusions and Recomendations for Bellabeat Company.

 Collecting data on activity, sleep, stress, and reproductive health has allowed Bellabeat to Empower women with knowledge about their own health and habits. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women. 
 
 After the analysis of the Fitbit fitness Tracker Data I gained some insights that can aid or empower the *Marketing strategy of Bellabeat Company.
 
#### Recommendations

* From the daily activity data set the target audience for the bellabeat marketing campaign should be women who work in offices and remotely, and do most of their work on the computer system.

* The sedentary time is too high and needs to be reduced by effective Marketing Strategy, Bellabeat needs to find sectors that have high sedentary time and find a means to do activities like walking.

* The average total steps in a day was less than the CDC recommended total steps in a day which is 10000 steps. The CDC did a research and found that, compared with taking 4,000 steps per day, a number considered to be low for adults, taking 8,000 steps per day was associated with a 51% lower risk for all-cause mortality (or death from all causes). Taking 12,000 steps per day was associated with a 65% lower risk compared with taking 4,000 steps. 
  **Bellabeat can advice women to walk more, at least 8000-10000 steps a day by telling them about     the health benefit.**
* Participants sleep once in every 7 hours and there is a positive correlation between Total time in Bed and Total mins asleep. Bellabeat could consider creating an app notification that can remind people that its time to sleep.
  
  Thank you your Interest in my Bellabeat Case study.

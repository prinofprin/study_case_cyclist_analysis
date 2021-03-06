# Case Study: Use the Bike-Sharing company's historical data to find customer's insight
This is my analysis for Google Data Analytics Professional Certificate Course's case study, 
- Please read the study case question [here](https://github.com/prinofprin/study_case_cyclist_analysis/blob/main/cyclistic_study_case_question.pdf).
- See the R code for this project [here](https://github.com/prinofprin/study_case_cyclist_analysis/blob/main/case_analysis_code.R).
- see in the pdf format [here](https://github.com/prinofprin/study_case_cyclist_analysis/blob/main/Cyclist-study-case.pdf).


## Business Task
Use Cyclistic's historical bike trip data to analyze the different behavior between annual members and causal riders and use the information to recommend the new marketing strategies to convert causal riders to annual members.
 
#### Key Questions
1. How do annual members and casual riders use Cyclist bikes differently?
2. Why would casual riders buy Cyclistic annual membership?
3. How can Cyclisitc use digital media to influence casual riders to become members?

## Data preparation
The public data that is used in the analysis, is provided by Motivate International Inc. [here](https://www.kaggle.com/datasets/arashnic/fitbit)

For this study case, I will use the R language for analysis because the data is exceed Excel row limitation (more than 5 million rows).
First, I load all the data inside a new folder, installed the necessary packages, and Imported all the CSV files for every month in 2021.

```{r install, echo=TRUE, results = 'hide'}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

trip_data_2022_01 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202101-divvy-tripdata.csv")
trip_data_2022_02 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202102-divvy-tripdata.csv")
trip_data_2022_03 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202103-divvy-tripdata.csv")
trip_data_2022_04 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202104-divvy-tripdata.csv")
trip_data_2022_05 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202105-divvy-tripdata.csv")
trip_data_2022_06 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202106-divvy-tripdata.csv")
trip_data_2022_07 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202107-divvy-tripdata.csv")
trip_data_2022_08 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202108-divvy-tripdata.csv")
trip_data_2022_09 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202109-divvy-tripdata.csv")
trip_data_2022_10 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202110-divvy-tripdata.csv")
trip_data_2022_11 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202111-divvy-tripdata.csv")
trip_data_2022_12 <- read.csv("C:/Users/ADMIN/Desktop/Prin/Google Data Analysis/Lesson 8 Capstone Complete a Case Study/Study Case How Does a Bike-Share Navigate Speedy data/202112-divvy-tripdata.csv")
```

Then I combine all of the loaded data into a single data frame.

```{r, echo=TRUE, results = 'hide'}

trip_data_2022 <- rbind(trip_data_2022_01, trip_data_2022_02, trip_data_2022_03, trip_data_2022_04, trip_data_2022_05, trip_data_2022_06, trip_data_2022_07, trip_data_2022_08, trip_data_2022_09, trip_data_2022_10, trip_data_2022_11, trip_data_2022_12)
```


## Clean data
After having a data frame to work with, I clean the data by removed any row that have missing value and rename member_causal column to member_type for clarification.
```{r, echo=TRUE, results = 'hide'}
trip_data_2022_clean <- na.omit(trip_data_2022)
trip_data_2022_clean <- rename(trip_data_2022_clean, member_type = member_casual)
```


## Processing data
After reviewing the cleaned data I notice that there is still lack of the information for behavior analysis, so I create new columns for biking duration, day of the week, and another column for extracted hour from the start_hours column.
```{r, echo=TRUE, results = 'hide'}
trip_data_process <- mutate(trip_data_2022_clean, duration = round((as.numeric(ymd_hms(ended_at) - ymd_hms(started_at)))/60))
trip_data_process <- mutate(trip_data_process, day=weekdays(ymd_hms(started_at)))
trip_data_process <- mutate(trip_data_process, started_hour=format((ymd_hms(started_at)), format = "%H"))
```

## Analyze
I start analyzing the hours first to see the difference between the time causal riders and annual members using the bike.
```{r, echo=TRUE,}
ggplot(data = trip_data_process) + geom_bar(mapping = aes(x = started_hour)) + facet_wrap(~member_type) + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
```
<img src='https://i.postimg.cc/0QVs2xVP/start-hour-graph.png' border='0' alt='start-hour-graph'/></a>

As you see, there is a significant difference in the time both types of users use the bike. Causal riders use the bike lowest at 4 pm and peak at 5 pm, however, annual members have a small peak at 8 am and then gradually increase to the peak usage after work at 5 pm.

I investigate further by create the bar graphs comparing the difference both groups using during the day of work.
```{r, echo=TRUE,}
ggplot(data = trip_data_process) + geom_bar(mapping = aes(x = day)) + facet_wrap(~member_type) + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
```
[![day-graph.png](https://i.postimg.cc/qq5sXhvq/day-graph.png)](https://postimg.cc/8fLfTCWg)

While annual members use bike usage almost the same amount everyday, casual rider's usage is highest on Saturday and Sunday

both of this information suggests that many annual members riders use the bike for commuting to work that why the usage hour peak at 7 am and then higher peak at 5 pm, while causal riders may use bike for the bike free-time activities.


Now I want to find the average duration that the annual members ride the bike to work, so I use the code to group the rider type together and find the average riding duration during 8am and 5pm.
```{r eval=FALSE,}
work_duration_summary <- trip_data_process %>%
  group_by(member_type) %>%
  filter(started_hour == 8 | 17) %>%
  summarise(mean(duration))
```

[![duration-summary.png](https://i.postimg.cc/rpT8gqsN/duration-summary.png)](https://postimg.cc/18Wh3hp8)

The avenge duration that annual members ride to work is 13 min or calculate to roughly 3.5 km. in cycling distance.

Now, let's compare both groups overall riding duration and frequency to see if there is any difference in cycling behavior.
```{r, echo=TRUE,}
work_duration_summary <- trip_data_process %>%
  group_by(member_type) %>%
  summarise(count = n() )

ggplot(data = trip_data_process) + geom_point(mapping = aes(x = member_type, y = duration))

```

[![member-type-duration-graph.png](https://i.postimg.cc/G3ZGrSpT/member-type-duration-graph.png)](https://postimg.cc/RqLNLdKv)

The annual members cycle more but shorter duration, while causal riders cycle less often but usually cycle for longer.

Then I Create a bar graph to see which bike type is use the most for each member type.
```{r, echo=TRUE,}
ggplot(data = trip_data_process) + geom_bar(mapping = aes(x = rideable_type, fill = member_type)) + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
```
[![popularity-graph.png](https://i.postimg.cc/pTwq4fcw/popularity-graph.png)](https://postimg.cc/LYVtZ1pD)

The most popular type of bike for both groups is the classic bike, and the annual members rarely use the docked bike.

finally, I create a bar graph to see which location is the most popular cycling location for the annual members.

```{r, echo=TRUE,}
top_location_summary <- trip_data_process %>%
  group_by(start_station_name) %>%
  filter(start_station_name != "")  %>%
  filter(member_type == "member")  %>%
  summarise(count = n()) %>%
  top_n(n = 10, wt = count) %>%
  arrange(desc(count))
  ggplot(data = top_location_summary) + geom_bar(stat='identity', mapping = aes(x =    start_station_name, y = count))
```
[![popular-station-graph.png](https://i.postimg.cc/NM16z3hc/popular-station-graph.png)](https://postimg.cc/yg83JrYQ)
  
## Shere
#### Key insight to share
- Annual members usually use the bike to commute to work, while causal riders use the bike for much longer activities.

#### additional useful information
1. Both Annual members and causal riders ride the bike most at 5 pm.

2. The duration of most annual members' rides to work is 13 minutes or about 3.5km.

3. Annual members ride more often but shorter duration, causal riders ride less often but for longer.

4. Classical bike is the most popular type of bike for both user types.

5. Clark St & Elm St is the most popular start cycling location for the annual members.

#### See my PowerPoint prsentation [here]("www.github.com/prinofprin/ADMIN/Desktop/Prin/presentation.ppt")

## Act
my recommended strategies to convert causal riders to Annual members are.

1. Promote more people around Clark St & Elm St and in the cities area to cycle to work

2. Decrease renting price on workdays to convert more causal members to cycling during the workday.

3. Promote by targeting office workers who have office syndrome to exercise more frequently.

4. Investigate further for any worries or obstructions that prevent causal riders from cycling to work.

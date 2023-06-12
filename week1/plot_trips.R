########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides (compare a histogram vs. a density plot)

trips %>% filter(tripduration <= 1e4) %>% ggplot(aes(x = tripduration)) + geom_histogram() + scale_x_log10()

# plot the distribution of trip times by rider type indicated using color and fill (compare a histogram vs. a density plot)

trips %>% 
  filter(tripduration <= 1e4) %>% 
  ggplot(aes(x = tripduration, fill = usertype)) + geom_histogram(position = "identity", alpha = 0.5) + scale_x_log10()

# plot the total number of trips on each day in the dataset

ggplot(trips, aes(x = ymd)) + geom_histogram()

trips %>% 
  group_by(ymd) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = ymd, y = count)) + geom_line()

# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)

trips_by_age_and_gender <- trips %>% 
                              select(birth_year, gender) %>% 
                              group_by(birth_year, gender) %>% 
                              summarize(count = n()) %>%
                              drop_na()
                              
trips_by_age_and_gender %>% ggplot(aes(x = birth_year, y = count, color = gender)) + geom_line()

#ggplot(trips, aes(x = birth_year, y = count(starttime), color = gender) + geom_line()

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the pivot_wider() function to reshape things to make it easier to compute this ratio
# (you can skip this and come back to it tomorrow if we haven't covered pivot_wider() yet)

trips_wide <- trips %>% 
              select(birth_year, gender) %>% 
              group_by(birth_year) %>% 
              arrange(birth_year) %>% 
              count(gender, name = "gender_count")

trips_wide <- trips_wide %>% 
              pivot_wider(names_from = gender, values_from = gender_count)

male_trips_wide <- trips_wide %>% 
                  select(birth_year, Male)

female_trips_wide <- trips_wide %>% 
                  select(birth_year, Female)

joined_trips_wide_drop_na <- inner_join(male_trips_wide, female_trips_wide) %>% 
                            drop_na() %>% 
                            mutate(gender_ratio = Male/Female)

ggplot(joined_trips_wide_drop_na, aes(x = birth_year, y=gender_ratio)) + geom_line()

########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)

weather <- read_csv('weather.csv')

weather_min_temp_by_day <- weather %>% select(DATE, TMIN) %>% group_by(DATE) %>% arrange(DATE)

ggplot(weather_min_temp_by_day, aes(x = DATE, y=TMIN)) + geom_line()


# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the pivot_longer() function for this to reshape things before plotting
# (you can skip this and come back to it tomorrow if we haven't covered reshaping data yet)

#before pivot_longer()
weather_min_max_temp_by_day <- weather %>%
                              select(DATE, TMIN, TMAX) %>%
                              group_by(DATE) %>% 
                              arrange(DATE)

#Using pivot_longer()
weather_min_max_temp_by_day <- weather %>% 
                              select(DATE, TMIN, TMAX) %>% 
                              group_by(DATE) %>% 
                              arrange(DATE) %>% 
                              pivot_longer(c(TMIN, TMAX), names_to = "TMP")

ggplot(weather_min_max_temp_by_day, aes(x = DATE, y=value, color=TMP)) + geom_line()

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by=c('ymd'='DATE'))

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this

trips_with_weather_trips_by_day <- trips_with_weather %>%
                                  group_by(ymd) %>% 
                                  arrange(ymd) %>% 
                                  summarize(count = n())

trips_with_weather_min_temp_by_day <- weather %>% select(DATE, TMIN)

joined_trips_with_weather_trips_by_temp <- inner_join(trips_with_weather_trips_by_day, trips_with_weather_min_temp_by_day, by=c('ymd' = 'DATE'))

joined_trips_with_weather_trips_by_temp %>% ggplot(aes(x=TMIN, y=count)) + geom_point() + geom_smooth(method = "lm")



# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

unique(trips_with_weather$PRCP) %>% mean()
#returns 0.686 as mean, so anything above that will be considered "substantial precipitation"

#################################COPY AND PASTE THING HERE !!!!!!


# add a smoothed fit on top of the previous plot, using geom_smooth

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

trips_with_weather_hour <- trips_with_weather %>% 
                              select(ymd, starttime) %>% 
                              mutate(hour = hour(starttime)) %>%
                              group_by(hour) %>%
                              count(ymd, name = "trips") %>%
                              summarize( average_trips = mean(trips), standard_dis = sd(trips))

# plot the above

trips_with_weather_hour %>% 
  pivot_longer(cols = 2:3, names_to = "col1",values_to = "values" ) %>% 
  ggplot(aes(x = hour, y = values, color = col1)) + geom_line()



# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package

trips_with_weather_weekday <- trips_with_weather %>%
                                select(ymd) %>%
                                mutate(day = wday(ymd)) %>%
                                group_by(day) %>%
                                count(ymd, name = "trips") %>%
                                summarize(average_trips = mean(trips), standard_dev = sd(trips))
                                
trips_with_weather_weekday %>% 
    pivot_longer(cols = 2:3, names_to = "col1", values_to = "values" ) %>%
    ggplot(aes(x = day, y = values, color = col1)) + geom_line()


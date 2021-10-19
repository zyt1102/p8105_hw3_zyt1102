# p8105_hw3_zyt1102

---
title: "Homework 3"
author: "zyt1102"
date: "10/19/2021"
output: html_document
---
```{r}
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(viridisLite)
library(dplyr)
knitr::opts_chunk$set(
	echo = TRUE,
	warning = FALSE,
	fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
theme_set(theme_minimal() + theme(legend.position = "bottom"))
```

#Question 1
```{r}
require("devtools")
devtools::install_github("p8105/p8105.datasets")
library(p8105.datasets)
data("instacart") 
summary(instacart)
```
The instacart data set contains `r nrow(instacart)` rows and `r ncol(instacart)` columns. The columns consist of information collected from a grocery store on it's most popular items and what time the groceries were purchased. Notably, some of these variables are :

`order_dow`: which specifies the day of the week the order was placed
`aisle_id`: the unique aisle in which the product was found
`order_hour_of_day`: the time the order was made
`product_name`: item that was purchased
`department`: grocery department where the purchased item was located

Based on the dataset and headings, we can determine that as part of user 112108's purchase, they bought Bulgarian yogurt on the 4th day of the week, 10th hour of the day. It was 9 days since their prior order. Additionally, they had purchased this item before and it was located from the yogurt aisle in the dairy/eggs department.

#identify number of aisles
```{r}
library(dplyr)
n_distinct(pull(instacart, aisle_id))
```
#arrange aisles by number of goods in an aisle
```{r}
instacart %>%
  group_by(aisle) %>%
  summarize(n_obs = n()) %>%
  arrange(desc(n_obs))
```
#plot of aisles with more than 10,000 items
```{r}
library(ggplot2)
instacart %>%
  group_by(aisle) %>%
  summarize(n_obs = n()) %>% 
  filter(n_obs > 10000) %>%
  ggplot(aes(x = aisle, y = n_obs)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
    labs(
    title = "Popularity of aisles with more than 10,000 purchases",
    x = "Aisle",
    y = "Number of purchases",
    caption = "Data from instacart dataset"
  ) + 
    scale_y_continuous(
    breaks = c(10000, 50000, 100000, 150000), 
    labels = c("10000", "50000", "100000", "150000"))
```

#table of three most popular items “baking ingredients”, “dog food care”, and “packaged vegetables fruits”
```{r}
instacart %>%
  filter(aisle == "baking ingredients" | aisle == "dog food care" | aisle == "packaged vegetables fruits") %>%
  group_by(aisle, product_name) %>%
  summarize(n_obs = n()) %>%
  mutate(ranking = rank(-n_obs)) %>%
  filter(ranking <= 3) %>%
  arrange(aisle, ranking) %>%
  knitr::kable()
```

The most popular item in the dog food care aisle is sold 300x less than the most popular item from packaged fruits vegetables. Baby spinach is the most popular item by far of these three aisles. Small dog biscuits is the least popular item from this table. 

#table of the mean hour of the day when Pink Lady apples and Coffee are bought each day of the week
```{r}
library(tidyr)
instacart %>%
  filter(product_name == "Pink Lady Apple" | product_name == "Coffee Ice Cream") %>%
  group_by(product_name, order_dow) %>%
  summarize(mean_hour_of_day = mean(order_hour_of_day)) %>%
  pivot_wider(
    names_from = order_dow,
    values_from = mean_hour_of_day) %>%
  knitr::kable()
```

Pink Lady Apples and Coffee Ice cream are fairly comparable across the week. The largest difference in purchases is on day 3. In this dataset, the day that they are closest in popularity is day 1. 

#Question 2
```{r}
library(p8105.datasets)
data("brfss_smart2010")
#rename dataset and clean names/data
brfss =
  janitor::clean_names(brfss_smart2010)  %>%
  filter(topic == "Overall Health") %>% 
  filter(response == "Excellent" | response == "Very good" | response == "Good" | response == "Fair" | response == "Poor") %>%
  mutate(respone = as.factor(response),
  response = forcats::fct_relevel(response, c("Poor", "Fair", "Good", "Very good", "Excellent")))
```

#States observed at 7 or more locations
```{r}
#states with 7 or more testing locations in 2002
brfss %>% 
  filter(year == "2002") %>%
  group_by(locationabbr) %>%
  distinct(locationdesc) %>%
  summarize(total_testing_location = n()) %>%
  filter(total_testing_location >= 7) %>%
  arrange(total_testing_location) %>%
  knitr::kable(digits = 1)
#states with 7 or more testing locations in 2010
brfss %>% 
  filter(year == "2010") %>%
  group_by(locationabbr) %>%
  distinct(locationdesc) %>%
  summarize(total_testing_location = n()) %>%
  filter(total_testing_location >= 7) %>%
  arrange(total_testing_location) %>%
  knitr::kable(digits = 1)
```

There were more states tested at 7 or more locations in 2010 than were tested in 2002. Not all of the same states were tested at 7 or more locations between the 2 years. For example, CT had 7 testing locations in 2002 but fewer than this in 2010 and therefore did not make the list. 

#Excellent response data from brfss
```{r}
#create dataset for excellent data
excellent_data = 
brfss %>% 
  filter(response == "Excellent") %>%
  group_by(locationabbr, year) %>%
  mutate(mean_data_value = mean(data_value, na.rm = TRUE)) 
#create spaghetti plot of the excellent responses
excellent_data %>%    
ggplot(aes(x = year, y = mean_data_value, group = locationabbr, color = locationabbr)) + 
  geom_line() +
  labs(
    title = "The average data value from 2002-2010 for those with Excellent Overall Health in BRFSS",
    x = "Year",
    y = "Average data value",
    caption = "Data from brfss_smart2010 dataset "
  )

```

#two-panel plot of 2006 and 2010, and mean_data_value vs response
```{r}
brfss %>%
  filter(year == "2006" | year == "2010",
         locationabbr == "NY") %>%
  ggplot(aes(x = response, y = data_value)) +
  geom_col() +
  facet_wrap(~year) + 
    labs(
    title = "The distribution of data value across Overall Health responses in 2006 and 2010 in NY",
    x = "Overall Health Response",
    y = "Data value",
    caption = "Data from brfss_smart2010 dataset "
  )
```

#Question 3
```{r}
library(dplyr)
require(readr)
library(tidyr)
accel = read_csv("/Users/liaojiangrui/Desktop/大地的盐/accel_data.csv")
accel_tidy =  
  janitor::clean_names(accel) %>%
  mutate(weekend = if_else(day == "Saturday" | day == "Sunday", "weekend", "weekday")) %>%
  pivot_longer(
    activity_1:activity_1440, 
    names_to = "activity", 
    values_to = "activity_counts") %>%
  separate(activity, into = c("activity", "minute")) %>%
  select(-activity) %>%
  mutate(
    minute = as.numeric(minute), 
    day = as.factor(day), 
    day = forcats::fct_relevel(day, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

```

#aggregate across a day to create total activity variable
```{r}
accel_tidy %>%
  group_by(day_id) %>%
  summarize(total_activity = sum(activity_counts)) %>%
  knitr::kable()
```


```{r}

```


#create plot to show fluctuation over each day
```{r}
#fluctuation in activity counts over the hours in each day
library(ggplot2)
accel_tidy %>%
  mutate(
    hour = minute %/% 60,
    hour = as.integer(hour),
    unique_day = paste(week, day)) %>%
  group_by(unique_day, hour) %>%
  summarize(mean_activity_counts = mean(activity_counts)) %>%
    ggplot(aes(x = hour, y = mean_activity_counts, color = unique_day)) +
             geom_line() + 
      labs(
    title = "Distribution of Activity Counts Across MHours in a Day for Each Day of the study",
    x = "Hours of the day",
    y = "Activity count",
    caption = "Data from accel dataset "
  )
```
```{r}
#weekend vs weekday comparison of activity levels
accel_tidy %>%
  group_by(weekend) %>%
  ggplot(aes(x = minute, y = activity_counts, color = weekend)) +
   geom_smooth() +
   theme(axis.text.x = element_text(angle = 90))  + 
    labs(
    title = "Distribution of Activity Counts Across Minutes in a Day for Weekend vs Weekday",
    x = "Minute of the day",
    y = "Activity count",
    caption = "Data from accel dataset "
  )
```

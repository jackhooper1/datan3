## Exercises chapter 5

library(nycflights13)
library(tidyverse)

summary(flights)

## 5.2.4
#1
flights %>%
  filter(arr_delay>=120)
#2
flights %>%
  filter(dest=="IAH"|dest=="HOU")

filter(flights, dest %in% c("IAH", "HOU"))

#3
filter(flights, carrier=="AA"|carrier=="DL"|carrier=="UA")

#4
filter(flights, month==7|month==8|month==9)

#5
filter(flights, dep_delay<=0, arr_delay>120)

#6
filter(flights, dep_delay>=60, dep_delay - arr_delay >30)

#7
filter(flights, dep_time<=600 | dep_time==2400)

## 5.3.1

#1
arrange(flights, desc(is.na(dep_time)),dep_time)
#2
arrange(flights, desc(dep_delay))
#3
arrange(flights, desc(distance/air_time))
#4
arrange(flights, distance)
arrange(flights, desc(distance))


##5.4.1

#1
flights[,names(flights) %in% c("dep_time", "dep_delay", "arr_time", "arr_delay")]

select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, 4,6,7,9)

#2 if you select same column multiple times it doesn't matter
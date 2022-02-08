## CH 13 Exercises - Week 4

# Mutating joins - new variables from matching observations
# Filtering joins - filter obs  from one df based on whether they match obs in df2
# Set operations - treat obs as if they were set elements

# Foreign key = same info but in different language kind of! and has to all be there

#13.2.1

library(tidyverse)
library(nycflights13)

#1
# flights -> origin and dest airport
# Airports -> long lat of each airport

flights_latlon <- flights %>%
  inner_join(select(airports, origin = faa, origin_lat = lat, origin_lon = lon),
             by = "origin"
  ) %>%
  inner_join(select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
             by = "dest"
  )

flights_latlon %>%
  slice(1:100) %>%
  ggplot(aes(
    x = origin_lon, xend = dest_lon,
    y = origin_lat, yend = dest_lat
  )) +
  borders("state") +
  geom_segment(arrow = arrow(length = unit(0.1, "cm"))) +
  coord_quickmap() +
  labs(y = "Latitude", x = "Longitude")

#2

# airports and weather could be linked by origin (weather) and faa (airport code) (airports)

#3 

# To match faa and origin variables, all values are required to relate (to be a foreign key)
# Without all the airports, the two could not be joined.
# Now you can match the destination airport with its weather by (hour, month, etc.)

#4 

# special_days <- tribble(
~year, ~month, ~day, ~holiday,
2013, 01, 01, "New Years Day",
2013, 07, 04, "Independence Day",
2013, 11, 29, "Thanksgiving Day",
2013, 12, 25, "Christmas Day"
)

# connects via year, month, day

#13.3.1 

# key = connecting variable
# Primary Key = unique identifier of each obs in its own table
# Foreign Key =  uniiquly identifies obs in another table
# Can be both primary and foreign
# Good idea to use count to see if they are the same!

# Surrogate key 
# give each row a number ----  mutate() and row_number()

# Can make diagrams with package datamodelr or OmniGraffle

#13.4.6



#13.5.1

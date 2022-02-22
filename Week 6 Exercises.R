## Week 6 exercises
library(tidyverse)

#3.3.1
#1 the points are not blue because the argument 'colour' is not an aes
#(aesthetic argument) it should be after the brackets e.g.
ggplot(data=mpg)+
geom_point(mapping=aes(x=displ, y=hwy),colour="blue")

#2
?mpg
summary(mpg) #shows character/integer
# can also print head(mpg)
glimpse(mpg)
# categorical = manafacturer, model, trans, drv, fl, class
# continuous = displ, year, cyl, cty, hwy

#3
?shape
mpg %>%
  ggplot()+
  geom_point(mapping=aes(x=cty, y=hwy, colour=cyl))
mpg %>%
  ggplot()+
  geom_point(mapping=aes(x=cty, y=hwy, size=cyl))
# continuous variable cannot be mapped to shape
# shape, colour and size can be used as a 'third' variable 

#4
mpg %>%
  ggplot()+
  geom_point(mapping=aes(x=cty, y=hwy, colour=cty, shape=trans))
# you can map a variable to multiple aesthetics but it is redundant

#5
#what does the stroke aesthetic do?
mtcars %>%
  ggplot(aes(x=wt, y=mpg))+
  geom_point(stroke=5, shape=22,size=1)
# stroke changes the size of the border for shapes 21-25

#6
mpg %>%
  ggplot()+
  geom_point(mapping=aes(x=cty,y=hwy,colour=displ <5))
# can add logical variable (T or F)

#3.5.1
#facet_wrap and facet_wrap

#1 facet on cont variable
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
mpg %>%
  ggplot()+
  geom_point(mapping=aes(x=displ, y=hwy))+
  facet_wrap(~cty, nrow=3)
# continuous -> categoric - massively long

#2 empty cells in a facet grid are areas that just places with no obs

#3 what does `.` do in facet_grid
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
# full stop before = x-axis
# full stop after = y-axis

#4 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
# facet vs color
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour=class))
# for a small third variable, colour can be good, and all data in one place
# more data, more confusing - so use facet
# facet perhaps more difficult to compare

# 5 - you can change nrow and ncol in facet_wrap not facet_grid

#6 - facet_grid() - put variable with more levels in columns as more horizontal 
# room

#3.6.1

#1 geom:
# line chart = geom_line() , boxplot = geom_boxplot(), hist = geom_histogram()
# area chart = geom_area()

#2 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

#3 The theme option show.legend = FALSE hides the legend box.

#4 se argument geom_smooth() do?
# standard error bands (area) -> t/f

#5 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
# they are the same

#6
mpg %>%
 ggplot(mapping=aes(x=displ, y=hwy))+
  geom_point()+
  geom_smooth(se=F)

mpg %>%
  ggplot(mapping=aes(x=displ, y=hwy, group=drv))+
  geom_point()+
  geom_smooth(se=F)

mpg %>%
  ggplot(mapping=aes(x=displ, y=hwy, colour=drv))+
  geom_point()+
  geom_smooth(se=F)

mpg %>%
  ggplot(mapping=aes(x=displ, y=hwy))+
  geom_point(mapping=aes(colour=drv))+
  geom_smooth(se=F)

mpg %>%
  ggplot(mapping=aes(x=displ, y=hwy))+
  geom_point(mapping=aes(colour=drv))+
  geom_smooth(mapping=aes(linetype=drv),se=F)

mpg %>%
  ggplot(mapping=aes(x=displ, y=hwy))+
  geom_point(size=4, colour="white")+
  geom_point(aes(colour=drv))

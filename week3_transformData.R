# Data Analysis in Social Science 3 (2021)
# AB
# 24 Jan 2021

# Week 3. Data transformation.

library(tidyverse)

# Reading in individual level data from wave 10.

W10 <- read_tsv("UKDA-6614-tab/tab/ukhls_w10/j_indall.tab")
W10

# Pipe
# Pipe (%>%) lets you pass the output from one function to another.

table(1:5)
prop.table(table(1:5))

# or

1:5 %>% table() %>% prop.table()

#############################################################################

# Select variables.

W10 %>%
  select(pidp, j_sex, j_dvage)

Sexage10 <- W10 %>%
  select(pidp, j_sex, j_dvage)

? select

# the alternatives in base R are not as pretty

W10[,names(W10) %in% c("pidp", "j_sex", "j_dvage")]

subset(W10, select = c("pidp", "j_sex", "j_dvage"))

W10 %>%
  subset(select = c("pidp", "j_sex", "j_dvage"))

#############################################################################

# Select observations.

# men only

Sexage10 %>%
  filter(j_sex == 1)

# people aged between 18 and 25

Sexage10 %>%
  filter(j_dvage %in% 18:25)

Sexage10 %>%
  filter(j_dvage >=18 & j_dvage<= 25)

# Base R:
Sexage10[Sexage10$j_dvage %in% 18:25,]

? filter

# Useful functions: & (AND), | (OR), ! (NOT), is.na(), !is.na()


#############################################################################

# Sorting observations.

Sexage10 %>%
  arrange(j_dvage)

Sexage10 %>%
  arrange(desc(j_dvage))

Sexage10 %>%
  arrange(j_sex, desc(j_dvage))

? arrange

#############################################################################

# Renaming variables

Sexage10 %>%
  rename(age10 = j_dvage)

#############################################################################

# Creating a new variable

# with a constant

Sexage10 %>%
  mutate(year = 2020)

# as a function of an existing variable
# (recoding data)

# recode sex to male or female

Sexage10 %>%
  count(j_sex)

# with ifelse

Sexage10 %>%
  mutate(sex2 = ifelse(j_sex == 1, "male",
                       ifelse(j_sex == 2, "female", NA))) %>%
  count(sex2)

# with recode

Sexage10 %>%
  mutate(sex2 = recode(j_sex,
                       '1' = "male",
                       '2' = "female",
                       .default = NA_character_)
         ) %>%
  count(sex2)

# with case_when
                       
Sexage10 %>%
  mutate(sex2 = case_when(
      j_sex == 1 ~ "male",
      j_sex == 2 ~ "female")
  ) %>%
  count(sex2)

# case_when can also handle several variables

# men 18 to 40 only

Sexage10 %>%
  mutate(youngmen = case_when(
    j_sex == 1 & j_dvage %in% 18:40 ~ "young men")
  ) 

# we can also specify the order of recoding

Sexage10 %>%
  mutate(youngmen = case_when(
    j_sex == 1 & j_dvage %in% 18:40 ~ "young men",
    TRUE ~ "other")
  ) 

# Note that this wouldn't work.

Sexage10 %>%
  mutate(youngmen = case_when(
    TRUE ~ "other",
    j_sex == 1 & j_dvage %in% 18:40 ~ "young men")
  ) 

#############################################################################

# Grouped operations

# summarise data by sex

Sexage10 %>%
  mutate(sex2 = case_when(
    j_sex == 1 ~ "male",
    j_sex == 2 ~ "female")
  )  %>%
  group_by(sex2) %>%
  summarise(
    meanAge = mean(j_dvage)
  )

Sexage10 %>% count(j_dvage) %>% print(n = Inf)

# cleaning the age variable

Sexage10 %>%
  mutate(sex2 = case_when(
    j_sex == 1 ~ "male",
    j_sex == 2 ~ "female")
  )  %>%
  mutate(age = ifelse(j_dvage >= 0, j_dvage, NA)) %>%
  group_by(sex2) %>%
  summarise(
    meanAge = mean(age, na.rm = TRUE)
  )

# you can use other functions inside summarise

Sexage10 %>%
  mutate(sex2 = case_when(
    j_sex == 1 ~ "male",
    j_sex == 2 ~ "female")
  )  %>%
  mutate(age = ifelse(j_dvage >= 0, j_dvage, NA)) %>%
  group_by(sex2) %>%
  summarise(
    maxAge = max(age, na.rm = TRUE),
    n = n()
  )

? summarise


# There are many other things dplyr can do (see the user manuals at https://cran.r-project.org/web/packages/dplyr/index.html)
# and there can be more complex cases.









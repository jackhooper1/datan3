# Data Analysis in Social Science 3 (2021)
# AB
# 1 Feb 2021

library(tidyverse)
library(vroom)

# Joining data sets.

df1 <- tibble(name = c("Anna", "Maria", "Alex"), x = 1:3)
df1

df2 <- tibble(name = c("Anna", "Maria", "Alex"), y = 4:6)
df2

# joining two data frames together

df1 %>%
  left_join(df2)

# Read R for Data Science, ch.13 (Relational Data): https://r4ds.had.co.nz/relational-data.html

# With the UndSoc data

# Create a balanced panel of all individuals who took part in the Understanding Society from wave 1 to 4.
# (Balanced means that you only want to include the individuals who took part in ALL 4 waves.)
# Only keep the following variables: person’s unique identifier, sex and age.
# Are there any inconsistencies in the data?

W1 <- read_tsv("UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab")
W1 <- W1 %>%
  select(pidp, a_sex, a_dvage)
W2 <- read_tsv("UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab")
W2 <- W2 %>%
  select(pidp, b_sex, b_dvage)
W3 <- vroom("UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab",
            col_select = c(pidp, c_sex, c_dvage))
W4 <- vroom("UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab",
            col_select = c(pidp, d_sex, d_dvage))

# Joining: balanced panel
Joined <- W1 %>%
  inner_join(W2, by = "pidp") %>%
  inner_join(W3, by = "pidp") %>%
  inner_join(W4, by = "pidp")

# Joining: every individual who ever took part in any of the waves 1 to 4
Joined2 <- W1 %>%
  full_join(W2, by = "pidp") %>%
  full_join(W3, by = "pidp") %>%
  full_join(W4, by = "pidp")

# Checking consistency of coding sex across the waves
Joined %>%
  count(a_sex, b_sex, c_sex, d_sex)

# Pulling out pidp of individuals with inconsistent coding
Joined %>%
  mutate(sumSex = a_sex + b_sex + c_sex + d_sex) %>%
  filter(sumSex != 8 & sumSex != 4) %>%
  pull(pidp)

# Other options:
# left_join()
# right_join()

# Construct a table with the average number of calls per household by region in waves 1 and 8.
# You’ll need to use the following data tables: household data from wave 1 and 8 
# (a_gor_dv and h_gor_dv identify region) and call records from waves 1 and 8 
# (callrec, a_ivtnc and h_ivtnc show the total number of calls).

# Read data for wave 1
# household data
H1 <- vroom("UKDA-6614-tab/tab/ukhls_w1/a_hhresp.tab")
# call record data
CR1 <- vroom("UKDA-6614-tab/tab/ukhls_w1/a_callrec.tab")

# select required variables
H1 <- H1 %>% select(a_hidp, a_gor_dv)
CR1ed <- CR1 %>%
  select(a_hidp, a_ivtnc) %>%
  # this aggregates data at the household level. The original data frame has calls (rather than households) as observations
  group_by(a_hidp) %>%
  summarise(
    ncalls = mean(a_ivtnc)
  )

# Joining the data tables by household identifier. Note the use of left_join to drop households where an interview was not obtained
Joined1 <- H1 %>%
  left_join(CR1ed, by = "a_hidp") %>%
  select(a_hidp, a_gor_dv, ncalls)

# Produce an aggregated table with summary statistics by region
Aggr1 <- Joined1 %>%
  group_by(a_gor_dv) %>%
  summarise(
    meanCall = mean(ncalls, na.rm = TRUE)
  )
Aggr1

# Now performing the same steps for wave 10.

H10 <- vroom("UKDA-6614-tab/tab/ukhls_w10/j_hhresp.tab")
CR10 <- vroom("UKDA-6614-tab/tab/ukhls_w10/j_callrec.tab")

H10 <- H10 %>% select(j_hidp, j_gor_dv)
CR10ed <- CR10 %>%
  select(j_hidp, j_ivtnc) %>%
  group_by(j_hidp) %>%
  summarise(
    ncalls = mean(j_ivtnc)
  )

Joined10 <- H10 %>%
  left_join(CR10ed, by = "j_hidp")

Aggr10<- Joined10 %>%
  group_by(j_gor_dv) %>%
  summarise(
    meanCall = mean(ncalls, na.rm = TRUE)
  ) %>%
  # filter out observations with missing region
  filter(j_gor_dv != -9)
Aggr10


# at this stage you need to check in the codebook that the numerical codes for regions are the same in waves 1 and 8 (they are)

# Joining the aggregated data tables for waves 1 and 10 using region as the key. 

Aggr1 %>%
  full_join(Aggr10, by = c("a_gor_dv" = "j_gor_dv")) %>%
  rename(region = a_gor_dv) %>%
  rename(wave1 = meanCall.x) %>%
  rename(wave10 = meanCall.y) %>%
  # Adding the labels for regions.
  # https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/variable/gor_dv
  mutate(regionLabel = recode(region,
                               '1' = "North East",
                               '2' = "North West",
                              '3' = "Yorkshire and the Humber",
                              '4' = "East Midlands",
                              '5' = "West Midlands",
                              '6' = "East of England",
                              '7' = "London",
                              '8' = "South East",
                              '9' = "South West",
                              '10' = "Wales",
                              '11' = "Scotland",
                              '12' = "Northern Ireland"
  ))










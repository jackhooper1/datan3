
library(tidyverse)
library(vroom)


## Data visualisation
#Communicate statistical information better through graph over numbers.
#Charts need to be uploaded to github seperately

#1. Read the indresp file from Wave 11 and keep the following variables: pidp, derived sex and age , and net personal income (fimnnet_dv). Visualise the distribution of income with a histogram, a density plot and a box plot.


k_indresp <- vroom("UKDA-6614-tab/tab/ukhls/k_indresp.tab",
                   col_select = c(pidp, k_sex_dv, k_age_dv, k_fimnnet_dv))

#always explore variable first

k_indresp %>%
  pull(k_fimnnet_dv) %>% summary()

k_indresp %>%
  ggplot(mapping =aes(x=k_fimnnet_dv))+
  geom_histogram()

# set limits and change bins
k_indresp %>%
  ggplot(mapping =aes(x=k_fimnnet_dv))+
  geom_histogram(bins = 50)+
  xlim(-1000, 10000)+
  xlab("Net personal income (wave 11)")+
  ylab("n")+
  ggtitle("Personal income in wave 11")+
  theme(plot.title = element_text(hjust=0.5))

# take log due to extremes in non-negative cases

#people with 0 income

k_indresp %>%
  filter(k_fimnnet_dv ==0) %>%
  count(k_age_dv)

k_indresp %>%
  filter(k_fimnnet_dv ==0) %>%
  ggplot(aes(x=k_age_dv))+
           geom_histogram()

#density plot
k_indresp %>%
  ggplot(mapping =aes(x=k_fimnnet_dv))+
  geom_density()

k_indresp %>%
  ggplot(mapping =aes(x=k_fimnnet_dv))+
  geom_density()+
  xlim(-1000, 10000)+
  xlab("Net personal income (wave 11)")+
  ylab("n")+
  ggtitle("Personal income in wave 11")+
  theme(plot.title = element_text(hjust=0.5))

#boxplot
# y is more normal than x
k_indresp %>%
  ggplot(mapping =aes(y=k_fimnnet_dv))+
  geom_boxplot()

k_indresp %>%
  filter(k_fimnnet_dv >0 & k_fimnnet_dv <5000) %>%
  ggplot(mapping =aes(y=k_fimnnet_dv))+
  geom_boxplot()+
  ylim(-1000, 10000)+
  ylab("Net personal income (wave 11)")+
  xlab("n")+
  ggtitle("Personal income in wave 11")+
  theme(plot.title = element_text(hjust=0.5))

k_indresp %>%
  mutate(sex = case_when(
    k_sex_dv == 1 ~ "Male",
    k_sex_dv == 2 ~ "Female"
  )) %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x=sex, y=k_fimnnet_dv))+
  geom_boxplot(notch=T)+
  ylim(-1000,10000)

#2. Visualise the distribution of sex with a bar chart. Create a variable for age groups and visualise the distribution by age, both as a continuous and categorical variable.

k_indresp %>% 
  mutate(sex = case_when(
    k_sex_dv == 1 ~ "Male",
    k_sex_dv == 2 ~ "Female")) %>%
  filter(!is.na(sex)) %>%
  count(sex) %>%
  mutate(perc=n/sum(n)*100) %>%
  ggplot(aes(x=sex,y=perc))+
  geom_bar(stat='identity')



#3. Think of the best way to visualise the relationship between sex and income.
# do filtering every time you make graph, not to df


k_indresp %>% 
  mutate(sex = case_when(
    k_sex_dv == 1 ~ "Male",
    k_sex_dv == 2 ~ "Female")) %>%
  filter(!is.na(sex)) %>%
  ggplot(mapping =aes(x=k_fimnnet_dv,colour=sex))+
  geom_histogram(bins = 50)+
  xlim(-1000, 10000)+
  xlab("Net personal income (wave 11)")+
  ylab("n")+
  ggtitle("Personal income in wave 11")+
  theme(plot.title = element_text(hjust=0.5))

k_indresp %>% 
  mutate(sex = case_when(
    k_sex_dv == 1 ~ "Male",
    k_sex_dv == 2 ~ "Female")) %>%
  filter(!is.na(sex)) %>%
  group_by(sex) %>%
  summarise(
    meanInc=mean(k_fimnnet_dv,na.rm=T),
    sdInd =sd(k_fimnnet_dv,na.rm=T),
    n = n(),
    seInc=sdInd/sqrt(n)
  ) %>%
  ggplot(aes(x=sex,y=meanInc))+
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=meanInc -2*seInc, ymax=meanInc +2*seInc), width=.5)

k_indresp %>% 
  mutate(sex = case_when(
    k_sex_dv == 1 ~ "Male",
    k_sex_dv == 2 ~ "Female")) %>%
  filter(!is.na(sex)) %>%
  group_by(sex) %>%
  summarise(
    meanInc=mean(k_fimnnet_dv,na.rm=T),
    sdInd =sd(k_fimnnet_dv,na.rm=T),
    n = n(), # ????
    seInc=sdInd/sqrt(n)
  ) %>%
  ggplot(aes(y = sex, x = meanInc)) + 
  geom_point() + 
  xlim(1000, 3000)



#4. Think of the best way to visualise the relationship between age and income.



#5. Visualise the joint distribution of age, sex and income.

k_indresp %>% 
  mutate(sex = case_when(
    k_sex_dv == 1 ~ "Male",
    k_sex_dv == 2 ~ "Female")) %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x=k_age_dv, y=k_fimnnet_dv, colour=sex))+
  geom_smooth()+
  xlim(0,75)

k_indresp %>% 
  mutate(sex = case_when(
    k_sex_dv == 1 ~ "Male",
    k_sex_dv == 2 ~ "Female")) %>%
  mutate(age =case_when(
      k_age_dv %in% 16:30 ~"16-30",
      k_age_dv %in% 31:45 ~ "31-45",
      k_age_dv %in% 60:150 ~"60+")) %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x=sex, y=k_fimnnet_dv))+
  geom_smooth()+
  xlim(0,75)+
  facet_wrap(~age)


k_indresp %>% 
  mutate(sex = case_when(
    k_sex_dv == 1 ~ "Male",
    k_sex_dv == 2 ~ "Female")) %>%
  mutate(age =case_when(
    k_age_dv %in% 16:30 ~"16-30",
    k_age_dv %in% 31:45 ~ "31-45",
    k_age_dv %in% 60:150 ~"60+"
  )) %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(age)) %>%
  ggplot(mapping =aes(x=sex, y=k_fimnnet_dv))+
  geom_bar()+
  xlim(-1000, 10000)+
  xlab("Net personal income (wave 11)")+
  ylab("n")+
  ggtitle("Personal income in wave 11")+
  theme(plot.title = element_text(hjust=0.5))+
  facet_wrap(~age)

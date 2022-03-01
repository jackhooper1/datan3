## Lecture 7
## Data mining
# 1600 people
# Over phone
# Russian attitude towards ukraine
# .sav = spss files
# combine the two

library(haven)

df1 <- read_sav("wciom2505.sav")
df2 <- read_sav("wciom27.02.sav")

colnames(df1)
colnames(df2)
# Extra variable - vo5

class(str(df2$vo5))
# In your opinion, what is the main goal pursued by Russia when conducting a 
# special military operation in Ukraine?

#"Protect the Russian-speaking population of the DNR and LNR" "Demilitarize 
# Ukraine, secure its borders" "Do not allow NATO military bases to be located 
# on the territory of Ukraine" "Divide Ukraine and establish its influence on 
# parts of Ukraine"

str(df1$SEX)
class(str(df1$SEX))
# 1 male, 2 female

str(df1$vo4)
# 1,2, 99 - support, not support, not answer

# Please tell me whether you support the decision to conduct a special military 
# operation of Russia in Ukraine or not? (vo4)
# Special military operation... bad wording

df1 <- df1 %>%
  mutate(date= "Friday")
df2 <- df2 %>%
  mutate(date= "Sunday")

df <- df1 %>%
  bind_rows(df2)

df2 %>%
  count(EDU)

# Sex
# Age
# 1 - primary or none
#2 - incomplete 2ndary
#3 - complete 2ndary
#4 - secondary/vocational
#5 - incomplete higher or higher
#6 - two or more higher education

## recoding and exploring outcome variable (English)
# visualise, percentages, differences

df %>%
  count(vo4)

df <- df %>%
  mutate(support3values = case_when(
    vo4 == 1 ~ "Support",
    vo4 == 2 ~ "Not Support",
    vo4 == 99 ~ "No answer"
  )) %>%
  mutate(support = ifelse(vo4 ==1,1,0)) %>% #also includes NA as non support
  mutate(notSupport = ifelse(vo4 ==2,1,0)) #also includes NA as support

df %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100)

df %>% 
  group_by(date) %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100)

# support may have increased
# similar so can combine to increase sample size

df %>%
  ggplot(aes(x= support3values)) +
  geom_bar()+
  facet_wrap(~date)

df %>% 
  group_by(date) %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100) %>%
  ggplot(aes(x=support3values,y=perc))+
  geom_bar(stat='identity')+
  facet_wrap(~date)

# make a dodgy bar graph (geom_col, position="dodge") - Date different bars
# next to each other

df %>% 
  group_by(date) %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100) %>%
  ggplot(aes(x=support3values,y=perc, fill=date))+
  geom_col(position= "dodge")


# post stratification weights
# try to  get population closer to population of census
# by weight coefficient
# so each woman counts as 0.7 people
# each man counts as 1.3 people
# as more women than men answer
# corrects for age

# 5-10% response rate to phone interviews

df1 %>%
  pull(weight1) %>%
  summary()

# so we need to take weights into account
# in 90% of the cases, this wont make a difference

## important in understanding society 
## because combination of different samples (British household survey + others)
## for any serious analysis use weights

library("survey")
library("srvyr")

surveyDF <- svydesign(id =~0, weights=~weight1, data=df)

df %>%
  as_survey_design(ids=1, weights=weight1) %>%
  summarise(
    Support = survey_mean(support),
    NonSupport = survey_mean(notSupport))
# makes 0.3% difference

## don't actually need to do this for the ELE test
## ideally do weights in report!!

## 66% support
## 25% not support
## NA

# phrasing of the question
# authoritarian country - do not know who is calling and the consequences
# social (disability/destability) bias

df <- df %>%
  mutate(sex = ifelse(SEX==2,"Female",ifelse(SEX==1,"Male","NA")))
df %>%
  count(sex)

df %>%
  as_survey_design(ids=1, weights=weight1) %>%
  group_by(support3values) %>%
  summarise(
    Total = survey_total(),
    Proportion = survey_mean())


df %>% 
  group_by(sex) %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100)

df %>% 
  group_by(sex) %>%
  summarise(
    meanSupport = mean(support,na.rm=T),
    meanNotSupport = mean(notSupport, na.rm=T))

df %>% 
  group_by(sex) %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100) %>%
  ggplot(aes(x=support3values,y=perc, fill=sex))+
  geom_col(position= "dodge")

# categorical varibles = tables

# quantitative variables = graph
# line chart
# geom_smooth()

## Age

df %>%
  count(AGE) %>%
  print(n=Inf)
 
df %>%
  ggplot(mapping=aes(x=AGE, y=support))+
  geom_smooth()+
  xlim(16,75)

df %>%
  ggplot(mapping=aes(x=AGE, y=notSupport))+
  xlim(16,75)+
  geom_smooth()

## Age is a factor!

# Education
df <- df %>%
  mutate(education = case_when(
    EDU == 1 ~ "Primary or None",
    EDU == 2 ~ "Incomplete Secondary",
    EDU == 3 ~ "Complete Secondary",
    EDU == 4 ~ "Secondary/Vocational",
    EDU == 5 ~ "Incomplete Higher or Higher",
    EDU == 6 ~ "Two or More Higher Educations",
  ))

df %>% 
  group_by(education) %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100)

df %>% 
  group_by(education) %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100) %>%
  ggplot(aes(x=support3values,y=perc, fill=education))+
  geom_col(position= "dodge")

df %>% 
  group_by(education) %>%
  count(support3values) %>%
  mutate(perc=n/sum(n)*100) %>%
  ggplot(aes(x=support3values,y=perc))+
  geom_col(position= "dodge")+
  facet_wrap(~education)


## linear model
# most important is telling a story

lm(support ~ AGE, data=df) %>% summary()
# don't say people become more supportive as get older, say people of different 
# ages show different support

lm(support ~ AGE + sex, data=df) %>% summary()

lm(support ~ AGE + sex + education, data=df) %>% summary()
# take care with reference group

summary(lm(data=df, support ~ AGE + sex + education))
summary(lm(data=df, notSupport ~ AGE + sex + education))
# men 9 percentage points not percent support 
# after controlling for age, difference between men and women more important
# women live longer than men, diff =10yrs in Russia, so on average men are 
# younger

# Support: AGE, Sex, education 
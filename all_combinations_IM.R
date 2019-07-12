# Reproducing mHOMR
# Vincent Major
# July 1, 2019


library(tidyverse)

#### initializing
## initializing realistic ranges for continuous variables and 
## all possible values for categorical variables
ambulances = seq(0, 2)
age = seq(35, 105)
ed = seq(0, 2)
gender = c('female', 'male')
home = c('home', 'rehab', 'home with care', 'nursing', 'chronic')
urgency = c('elective', 'ED', 'ED with ambulance')
readmit = c(0, 1)
directToICU = c(0, 1)

## building up coefficients/weights as small tables relevant to ^these vectors
intercept = -7.063915192

dict.ambulances = tibble(ambulances) %>% 
  mutate(w.ambulances = (1/(ambulances+1))*-1.566596305)

dict.age = tibble(age) %>% 
  mutate(w.age = sqrt(age)*0.871636062)

dict.ed = tibble(ed) %>% 
  mutate(w.ed = (1/sqrt(ed+1))*-0.640014591)

dict.gender = tibble(gender,
                     w.gender = c(0, 0.308462457))

dict.home = tibble(home, 
                   w.home = c(0, -0.006794503, -1.20399116, 0.155148147, 0.378611273) )

dict.urgency = tibble(urgency,
                      w.urgency = c(0, -0.798684042, -0.578781696))

dict.readmit = tibble(readmit,
                      w.readmit = c(0, 0.110983695))

dict.directToICU = tibble(directToICU,
                          w.directToICU = c(0, 0.506184712))

#### interaction terms
## first, living status x admissions by ambulance
## crossing finds all unique combinations of the vector arguments
dict.amb_home = crossing(home = home,
                         ambulances = ambulances) %>% 
  ## each living status has its own weight
  left_join(tibble(home = home, 
                   temp.w = c(0, 0.395684431, 1.751591008, 0.605470559, 0.915996966)), 
            by = "home") %>% 
  ## which gets multiplied by 1/(amb+1) but only when ambulances > 0
  mutate(w.amb_home = temp.w * (1/(ambulances + 1)),
         w.amb_home = if_else(ambulances == 0, 0, w.amb_home)) %>% ## ambulances = 0 --> 0 weight here.
  select(-temp.w) ## remove temporary home weight

## similar for urgent x admissions by ambulance
## but only two non-zero weights so using case_when
dict.urg_amb = crossing(urgency = urgency,
                       ambulances = ambulances) %>% 
  mutate(w.urg_amb = case_when(
    urgency == 'ED' ~ (1/(ambulances+1))*0.503792951,
    urgency == 'ED with ambulance' ~ (1/(ambulances+1))*0.803525355 ,
    urgency == 'elective' ~ 0,
    T ~ NA_real_) ) ## fail-safe


#### computing all combinations
## crossing finds all combinations of data
## then join on all dict. type tables adding columns of weights, e.g. w.age
## then sum all weights and intercept for the logit and compute probability
all <- crossing(ambulances, age, ed, gender, home, urgency, readmit, directToICU) %>% 
  mutate(intercept = intercept) %>% 
  left_join(dict.ambulances, by = 'ambulances') %>% 
  left_join(dict.age, by = 'age') %>% 
  left_join(dict.ed, by = 'ed') %>% 
  left_join(dict.gender, by = 'gender') %>% 
  left_join(dict.home, by = 'home') %>% 
  left_join(dict.urgency, by = 'urgency') %>% 
  left_join(dict.readmit, by = 'readmit') %>% 
  left_join(dict.directToICU, by = 'directToICU') %>% 
  left_join(dict.amb_home, by = c('home', 'ambulances')) %>% 
  left_join(dict.urg_amb, by = c('urgency', 'ambulances')) %>% 
  mutate(logit = intercept + w.ambulances + w.age + w.ed + w.gender + w.home + w.urgency + w.readmit + w.directToICU + w.amb_home + w.urg_amb,
         probability = exp(logit)/(1+exp(logit)) )

#### outlier cohorts

## youngest person that exceeds 0.21 threshold with almost no other data
## least acute data for home and elective
all %>% 
  filter(ambulances == 0, ed == 0, readmit == 0, directToICU == 0, home == 'home', urgency == 'elective', probability >= 0.21 ) %>% 
  arrange(age) %>% slice(1)
## 77 yo M with P = 0.212

## youngest female
all %>% 
  filter(ambulances == 0, ed == 0, gender == 'female', readmit == 0, directToICU == 0, home == 'home', urgency == 'elective', probability >= 0.21 ) %>% 
  arrange(age) %>% slice(1)
## 84 yo F with P = 0.217


## the other extreme is someone with history AND presenting acutely
## what is the minimum age to exceed 0.21?
all %>% 
  filter(ambulances == 2, ed == 2, readmit == 1, directToICU == 1, urgency == 'ED with ambulance', home == 'home with care', probability >= 0.21) %>% 
  arrange(age) %>% slice(1)
## 58 yo M with P = 0.210

## minimum age for female
all %>% 
  filter(ambulances == 2, ed == 2, gender == 'female', readmit == 1, directToICU == 1, urgency == 'ED with ambulance', home == 'home with care', probability >= 0.21) %>% 
  arrange(age) %>% slice(1)
## 64 yo F with P = 0.215

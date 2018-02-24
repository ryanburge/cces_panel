
library(fst)

cces <- read.fst("C://cces_panel.fst")

library(tidyverse)
library(car)
library(janitor)
library(reshape2)
library(ggalluvial)
library(extrafont)

source("D://cces/ggthemes.R")




cces <- cces %>% 
  mutate(prot_10 = religpew_protestant_10 + 100) %>% 
  mutate(bapt_10 = religpew_baptist_10 + 200)

prot <- cces %>% 
  select(caseid, prot_10, religpew_10, bapt_10) %>% 
  filter(religpew_10 ==1) %>% 
  select(-religpew_10) %>% 
  rename(relig10 = prot_10)

prot1 <- prot %>% 
  filter(relig10 ==101) %>% 
  select(-relig10) %>% 
  rename(relig10 = bapt_10) 

prot <- prot %>% 
  select(-bapt_10)

nprot <- cces %>% 
  select(caseid, religpew_10) %>% 
  filter(religpew_10 !=1) %>% 
  rename(relig10 = religpew_10)


r10 <- bind_rows(prot, prot1, nprot) 
r10$relig10[is.na(r10$relig10)] <- 0

r10 <- r10 %>% filter(relig10 != 101)


cces <- cces %>% 
  mutate(prot_12 = religpew_protestant_12 + 100) %>% 
  mutate(bapt_12 = religpew_baptist_12 + 200)

prot <- cces %>% 
  select(caseid, prot_12, religpew_12, bapt_12) %>% 
  filter(religpew_12 ==1) %>% 
  select(-religpew_12) %>% 
  rename(relig12 = prot_12)

prot1 <- prot %>% 
  filter(relig12 ==101) %>% 
  select(-relig12) %>% 
  rename(relig12 = bapt_12) 

prot <- prot %>% 
  select(-bapt_12)

nprot <- cces %>% 
  select(caseid, religpew_12) %>% 
  filter(religpew_12 !=1) %>% 
  rename(relig12 = religpew_12)


r12 <- bind_rows(prot, prot1, nprot) 
r12$relig12[is.na(r12$relig12)] <- 0

r12 <- r12 %>% filter(relig12 != 101)

cces <- cces %>% 
  mutate(prot_14 = religpew_protestant_14 + 100) %>% 
  mutate(bapt_14 = religpew_baptist_14 + 200)

prot <- cces %>% 
  select(caseid, prot_14, religpew_14, bapt_14) %>% 
  filter(religpew_14 ==1) %>% 
  select(-religpew_14) %>% 
  rename(relig14 = prot_14)

prot1 <- prot %>% 
  filter(relig14 ==101) %>% 
  select(-relig14) %>% 
  rename(relig14 = bapt_14) 

prot <- prot %>% 
  select(-bapt_14)

nprot <- cces %>% 
  select(caseid, religpew_14) %>% 
  filter(religpew_14 !=1) %>% 
  rename(relig14 = religpew_14)


r14 <- bind_rows(prot, prot1, nprot) 
r14$relig14[is.na(r14$relig14)] <- 0

r14 <- r14 %>% filter(relig14 != 101)



full <- left_join(r10, r12) %>% left_join(., r14)

cces <- left_join(cces, full, by='caseid')









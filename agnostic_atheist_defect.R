


test <- cces %>% 
  select(caseid, religpew_10, religpew_14)


test <- test %>% filter(religpew_10 ==11)

test <- melt(test, id = c("caseid")) %>% arrange(caseid) 

# gender <- cces %>% select(caseid, gender_10)
# 
# test <- left_join(test, gender)


test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))

test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  filter(all(value != "Hindu")) %>% 
  filter(all(value != "Muslim")) %>% 
  filter(all(value != "Orthodox")) %>% 
  filter(all(value != "Buddhist")) %>% 
  filter(all(value != "Mormon")) %>% 
  filter(all(value != "Jewish")) %>% 
  ungroup(caseid)

test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  ungroup(caseid)

test <- test %>% 
   mutate(value = fct_relevel(value,  "Agnostic", "Atheist", "Nothing", "Catholic", "Protestant", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))


test <- test %>% 
  mutate(variable = recode(variable, "'religpew_10'= '2010'; 'religpew_12'= '2012'; 'religpew_14'= '2014'"))

test %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid)) +
  geom_alluvium(aes(fill = value), width = 1/12) +
  geom_stratum(aes(fill = value)) +
  geom_flow() + bar_rb() +
  geom_label(stat = "stratum", label.strata = TRUE, size = 10) +
  labs(x= "Year", y = "Number of Respondents", title = "How Nothings Change Religious Affiliation", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none") +
  annotate("text", x = 2.25, y = 1100, label = "13.7%", size = 10) +
  annotate("text", x = 2.25, y = 950, label = "7.0%", size = 10) +
  annotate("text", x = 2.25, y = 575, label = "62.2%", size = 10) +
  annotate("text", x = 2.25, y = 170, label = "3.9%", size = 10) +
  annotate("text", x = 2.25, y = 75, label = "13.3%", size = 10) 
  
ggsave(file="sankey_2bar_nothing_annotate.png", type = "cairo-png", width = 18, height = 15)




test <- cces %>% 
  select(caseid, religpew_10, religpew_14)


test <- test %>% filter(religpew_10 ==10)

test <- melt(test, id = c("caseid")) %>% arrange(caseid) 

# gender <- cces %>% select(caseid, gender_10)
# 
# test <- left_join(test, gender)


test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))

test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  filter(all(value != "Hindu")) %>% 
  filter(all(value != "Muslim")) %>% 
  filter(all(value != "Orthodox")) %>% 
  filter(all(value != "Buddhist")) %>% 
  filter(all(value != "Mormon")) %>% 
  filter(all(value != "Jewish")) %>% 
  ungroup(caseid)

test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  ungroup(caseid)

test <- test %>% 
  mutate(value = fct_relevel(value,  "Agnostic", "Atheist", "Nothing", "Catholic", "Protestant", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))


test <- test %>% 
  mutate(variable = recode(variable, "'religpew_10'= '2010'; 'religpew_12'= '2012'; 'religpew_14'= '2014'"))

test %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid)) +
  geom_alluvium(aes(fill = value), width = 1/12) +
  geom_stratum(aes(fill = value)) +
  geom_flow() + bar_rb() +
  geom_label(stat = "stratum", label.strata = TRUE, size = 10) +
  labs(x= "Year", y = "Number of Respondents", title = "How Agnostics Change Religious Affiliation", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none") +
  annotate("text", x = 2.25, y = 375, label = "54.1%", size = 10) +
  annotate("text", x = 2.25, y = 175, label = "22.5%", size = 10) +
  annotate("text", x = 2.25, y = 80, label = "18.9%", size = 10) +
  annotate("text", x = 2.25, y = 20, label = "1.8%", size = 10) +
  annotate("text", x = 2.25, y = 0, label = "2.7%", size = 10) 

ggsave(file="sankey_2bar_agnostic_annotate.png", type = "cairo-png", width = 18, height = 15)




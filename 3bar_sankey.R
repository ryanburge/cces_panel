
# library(haven)
# cces <- read_dta("cces_panel.dta")
# 
# library(fst)
# write.fst(cces, "cces_panel.fst")


cces <- read.fst("C://cces_panel.fst")

library(tidyverse)
library(car)
library(janitor)

test <- cces %>% select(caseid, religpew_10, religpew_12, religpew_14)

test <- melt(test, id = c("caseid")) %>% arrange(caseid) 


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

test <- test %>% 
  mutate(value = fct_relevel(value, "Agnostic", "Atheist", "Nothing", "Catholic", "Protestant"))
  

test <- test %>% 
  mutate(variable = recode(variable, "'religpew_10'= '2010'; 'religpew_12'= '2012'; 'religpew_14'= '2014'"))

test %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid)) +
  geom_stratum(aes(fill = as.factor(value))) +
  geom_flow() + bar_rb() +
  geom_label(stat = "stratum", label.strata = TRUE) +
  labs(x= "Year", y = "Number of Respondents", title = "How Individuals Change Religious Affiliation", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none")


ggsave(file="sankey_3bar.png", type = "cairo-png", width = 18, height = 15)

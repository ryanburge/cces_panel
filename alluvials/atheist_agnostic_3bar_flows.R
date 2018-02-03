

test <- cces %>% 
  filter(religpew_10 == 11) %>% 
  select(caseid, religpew_10, religpew_12, religpew_14)


test <- melt(test, id = c("caseid")) %>% arrange(caseid) 


test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))


# test<- test %>% 
#   group_by(caseid) %>% 
#   filter(all(value != "12")) %>% 
#   filter(all(value != "Muslim")) %>% 
#   filter(all(value != "Hindu")) %>% 
#   filter(all(value != "Orthodox")) %>% 
#   filter(all(value != "Buddhist")) %>% 
#   filter(all(value != "Mormon")) %>% 
#   filter(all(value != "Jewish")) %>% 
#   ungroup(caseid)


test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  ungroup(caseid)

test <- test %>% 
  mutate(value = fct_relevel(value, "Agnostic", "Atheist", "Nothing", "Catholic", "Protestant", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))


test <- test %>% 
  mutate(variable = recode(variable, "'religpew_10'= '2010'; 'religpew_12'= '2012'; 'religpew_14'= '2014'"))

small <- test %>% head(5000)

small<- small %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  ungroup(caseid)

ggplot(test,
       aes(x = variable, stratum = value, alluvium = caseid,
           fill = value, label = value)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") +
  geom_stratum() +
  geom_label(fill = "white", stat = "stratum", size = 10, colour = "black") +
  bar_rb() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(x= "Year", y = "Number of Respondents", title = "How 'Nothing in Particular' Changes Religious Affiliation", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none") +
  annotate("text", x = 3.3, y = 1050, label = "13.7%", size = 10) +
  annotate("text", x = 3.3, y = 935, label = "7.0%", size = 10) +
  annotate("text", x = 3.3, y = 555, label = "62.2%", size = 10) +
  annotate("text", x = 3.3, y = 185, label = "3.9%", size = 10) +
  annotate("text", x = 3.3, y = 90, label = "13.3%", size = 10) 


ggsave(file="3bar_flow_nothings.png", type = "cairo-png", width = 18, height = 21)




test <- cces %>% 
  filter(religpew_10 == 10) %>% 
  select(caseid, religpew_10, religpew_12, religpew_14)


test <- melt(test, id = c("caseid")) %>% arrange(caseid) 


test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))


# test<- test %>% 
#   group_by(caseid) %>% 
#   filter(all(value != "12")) %>% 
#   filter(all(value != "Muslim")) %>% 
#   filter(all(value != "Hindu")) %>% 
#   filter(all(value != "Orthodox")) %>% 
#   filter(all(value != "Buddhist")) %>% 
#   filter(all(value != "Mormon")) %>% 
#   filter(all(value != "Jewish")) %>% 
#   ungroup(caseid)


test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  ungroup(caseid)

test <- test %>% 
  mutate(value = fct_relevel(value, "Agnostic", "Atheist", "Nothing", "Catholic", "Protestant", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))


test <- test %>% 
  mutate(variable = recode(variable, "'religpew_10'= '2010'; 'religpew_12'= '2012'; 'religpew_14'= '2014'"))

small <- test %>% head(5000)

small<- small %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  ungroup(caseid)

ggplot(test,
       aes(x = variable, stratum = value, alluvium = caseid,
           fill = value, label = value)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") +
  geom_stratum() +
  geom_label(fill = "white", stat = "stratum", size = 10, colour = "black") +
  bar_rb() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(x= "Year", y = "Number of Respondents", title = "How Agnostics Change Religious Affiliation", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none") +
  annotate("text", x = 3.3, y = 365, label = "54.1%", size = 10) +
  annotate("text", x = 3.3, y = 185, label = "22.5%", size = 10) +
  annotate("text", x = 3.3, y = 75, label = "18.9%", size = 10) +
  annotate("text", x = 3.3, y = 30, label = "1.8%", size = 10) +
  annotate("text", x = 3.3, y = 15, label = "2.7%", size = 10) 



ggsave(file="3bar_flow_agnostic.png", type = "cairo-png", width = 18, height = 21)


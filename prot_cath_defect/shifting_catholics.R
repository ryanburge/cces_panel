

test <- cces %>% 
  filter(religpew_10 ==2) %>% 
  select(caseid, religpew_10, religpew_12, religpew_14)

test <- melt(test, id = c("caseid")) %>% arrange(caseid) 

test <- test %>% 
  mutate(value = recode(value, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'"))


test<- test %>% 
  group_by(caseid) %>% 
  filter(all(value != "12")) %>% 
  ungroup(caseid)


test <- test %>% 
  mutate(value = fct_relevel(value, "Agnostic", "Atheist", "Nothing",  "Catholic", "Protestant", "Orthodox", "Mormon", "Jewish", "Hindu", "Muslim", "Buddhist"))


test <- test %>% 
  mutate(variable = recode(variable, "'religpew_10'= '2010'; 'religpew_12'= '2012'; 'religpew_14'= '2014'"))

test %>% 
  ggplot(., aes(x = variable, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_stratum() +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") + bar_rb() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = my.cols) +
  geom_label(fill = "white", stat = "stratum", size = 7, colour = "black") +
  labs(x= "Year", y = "Number of Respondents", title = "How Have Catholics Shifted?", caption = "Data: CCES Panel (2010-2014)") + theme(legend.position="none") +
  annotate("text", x = 3.3, y = 2035, label = "6.4%", size = 10) +
  annotate("text", x = 3.3, y = 25, label = "2.7%", size = 10) +
  annotate("text", x = 3.3, y = 1000, label = "90.9%", size = 10) 

ggsave(file="3bar_catholics.png", type = "cairo-png", width = 18, height = 21)

cces %>% 
  filter(religpew_14 ==2) %>% 
  filter(religpew_14 != 12) %>% 
  tabyl(religpew_10)
  




## Create the Defection Rate Bar Chart


test <- cces %>% 
  select(caseid, religpew_protestant_10, religpew_protestant_12, religpew_protestant_14)

count <- test %>% group_by(religpew_protestant_10) %>% 
  count(religpew_protestant_14) %>% mutate(pct =  prop.table(n)) %>% ungroup(religpew_protestant_10)

defect <- count[count$religpew_protestant_10==count$religpew_protestant_14,]

defect <- defect %>% 
  # filter(n > 200)  %>% 
  mutate(rate = 1 - pct) %>% 
  ungroup(religpew_10)


defect <- defect %>% 
  mutate(relig = recode(religpew_protestant_10, "1 = 'Baptist'; 2 = 'Methodist'; 3 = 'Nondenominational'; 4 = 'Lutheran'; 5 = 'Presbyterian'; 6 = 'Muslim'; 7 = 'Episcopalian'")) %>% 
  select(relig, rate)

defect %>% 
  filter(relig > 8) %>% 
  filter(relig != "Muslim") %>% 
  filter(relig != 90) %>% 
  filter(relig != 9) %>% 
  ggplot(., aes(x=reorder(relig, -rate), y = rate)) + geom_col(fill = "seagreen4", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "Denominational Family", y = "Family Defection Rate", title = "The Defection Rate of Each Denominational Family (2010-2014)", subtitle = "Defection rate is the % of Individuals Who Were in a Different Denominational Family in 2014 than they were in 2010", caption = "Data: CCES Panel (2010-2014)")


ggsave(file="defection_rate_protestants.png", type = "cairo-png", width = 18, height = 15)


cces %>% 
  filter(religpew_protestant_10 ==3) %>% 
  filter(religpew_14 !=12) %>% 
  tabyl(religpew_14)

cces %>% 
  filter(religpew_baptist_10 ==1) %>% 
  filter(religpew_protestant_14 !=90) %>% 
  tabyl(religpew_protestant_14)

cces %>% 
  filter(religpew_baptist_10 ==1) %>% 
  # filter(religpew_14 !=12) %>% 
  tabyl(religpew_14)


cces %>% 
  filter(religpew_protestant_10 ==1) %>% 
  filter(religpew_14 !=12) %>% 
  tabyl(religpew_14)


cces %>% 
  filter(religpew_protestant_10 ==5) %>% 
  filter(religpew_14 !=12) %>% count()
  tabyl(religpew_protestant_14)



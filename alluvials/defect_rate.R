
## Create the Defection Rate Bar Chart

test <- cces %>% 
  select(caseid, religpew_10, religpew_12,  religpew_14)

count <- test %>% group_by(religpew_10) %>% 
  count(religpew_14) %>% mutate(pct =  prop.table(n)) %>% ungroup(religpew_10)

defect <- count[count$religpew_10==count$religpew_14,]

defect <- defect %>% 
  filter(n > 200)  %>% 
  mutate(rate = 1 - pct) %>% 
  ungroup(religpew_10)


defect <- defect %>% 
  mutate(relig = recode(religpew_10, "1 = 'Protestant'; 2 = 'Catholic'; 3 = 'Mormon'; 4 = 'Orthodox'; 5 = 'Jewish'; 6 = 'Muslim'; 7 = 'Buddhist'; 8 = 'Hindu'; 9 = 'Atheist'; 10 = 'Agnostic'; 11 = 'Nothing'")) %>% 
  select(relig, rate)

defect %>% 
  ggplot(., aes(x=reorder(relig, -rate), y = rate)) + geom_col(fill = "dodgerblue3", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "Religious Tradition", y = "Defection Rate", title = "The Defection Rate of Each Religious Tradition (2010-2014)", subtitle = "Defection rate is the % of Individuals Who Claimed a Different Tradition in 2014 than they did in 2010", caption = "CCES: 2010-2014")


ggsave(file="defection_rate_religs.png", type = "cairo-png", width = 18, height = 15)

cces <- cces %>% 
  mutate(muchmore = recode(diff, "2:5 =1; else =0")) %>% 
  mutate(muchless = recode(diff, "-5:-2=1; else =0"))


lesspid <- cces %>% 
  filter(muchless ==1) %>% 
  filter(pid7_10 <8) %>% 
  filter(pid7_14 <8) %>% 
  mutate(piddiff = pid7_14 - pid7_10) %>% 
  tabyl(piddiff)


lesspid <- lesspid %>% 
  mutate(piddiff = recode(piddiff, "-6:-1 = 'More Democrat'; 0 = 'Stayed the Same'; 1:5 = 'More Republican' ")) %>% 
  group_by(piddiff) %>% 
  summarise(sum = sum(n)) %>% 
  mutate(pct = sum/409) %>% 
  mutate(label = c("Attending Much Less"))
  

## Negative numbers mean more liberal in 2014 than 2010


morepid <- cces %>% 
  filter(muchmore ==1) %>% 
  filter(pid7_10 <8) %>% 
  filter(pid7_14 <8) %>% 
  mutate(piddiff = pid7_14 - pid7_10) %>% 
  tabyl(piddiff)


morepid <- morepid %>% 
  mutate(piddiff = recode(piddiff, "-6:-1 = 'More Democrat'; 0 = 'Stayed the Same'; 1:5 = 'More Republican' ")) %>% 
  group_by(piddiff) %>% 
  summarise(sum = sum(n)) %>% 
  mutate(pct = sum/349) %>% 
  mutate(label = c("Attending Much More"))


samepid <- cces %>% 
  filter(diff ==0) %>% 
  filter(pid7_10 <8) %>% 
  filter(pid7_14 <8) %>% 
  mutate(piddiff = pid7_14 - pid7_10) %>% 
  tabyl(piddiff)


samepid <- samepid %>% 
  mutate(piddiff = recode(piddiff, "-6:-1 = 'More Democrat'; 0 = 'Stayed the Same'; 1:6 = 'More Republican' ")) %>% 
  group_by(piddiff) %>% 
  summarise(sum = sum(n)) %>% 
  mutate(pct = sum/5756) %>% 
  mutate(label = c("Attending the Same"))


three <- bind_rows(morepid, lesspid, samepid)
three$piddiff <- factor(three$piddiff, levels = c("More Democrat", "Stayed the Same", "More Republican"))
three$label = factor(three$label, levels=c('Attending Much Less','Attending the Same','Attending Much More'))


three %>% 
  ggplot(., aes(x=piddiff, y = pct, fill = piddiff)) + 
  geom_col(color = "black") + 
  facet_grid(.~ label) +
  bar_rb() +
  scale_fill_manual(values=c("dodgerblue3", "darkgrey", "firebrick1")) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none")  +
  labs(x = "Shift in Party ID from 2010-2014", y = "Percent", caption = "Data: CCES Panel (2010-2014)", title = "Shifts in Church Attendance and Party ID", subtitle = "For those who are attending less, they are twice as likely to become more Democrat than more Republican") +
  theme(plot.title = element_text(family = "Product Sans", size = 40, vjust =2, face = "bold"))


ggsave(file="D://cces_panel/attend/pid_facets.png", type = "cairo-png", width = 20, height = 14)




### Checking if this is because of some type of religious conversion. Basically, no. 

switch <- cces %>% 
  filter(muchless ==1) %>% 
  select(caseid, religpew_10, religpew_12, religpew_14)

count <- switch %>% group_by(religpew_10) %>% 
  count(religpew_14) %>% mutate(pct =  prop.table(n)) %>% ungroup(religpew_10) %>% 
  filter(religpew_10 != 12) %>% 
  filter(religpew_14 != 12)


switch <- cces %>% 
  filter(muchmore ==1) %>% 
  select(caseid, religpew_10, religpew_12, religpew_14)

count <- switch %>% group_by(religpew_10) %>% 
  count(religpew_14) %>% mutate(pct =  prop.table(n)) %>% ungroup(religpew_10) %>% 
  filter(religpew_10 != 12) %>% 
  filter(religpew_14 != 12)

test <- cces %>% 
  select(caseid, religpew_10, religpew_12,  religpew_14)

count <- test %>% group_by(religpew_10) %>% 
  count(religpew_14) %>% mutate(pct =  prop.table(n)) %>% ungroup(religpew_10)

defect <- count[count$religpew_10==count$religpew_14,]

test <- cces %>% 
  filter(muchless ==1) %>% 
  select(caseid, religpew_10, religpew_12,  religpew_14)

count <- test %>% group_by(religpew_10) %>% 
  count(religpew_14) %>% mutate(pct =  prop.table(n)) %>% ungroup(religpew_10)

defect <- count[count$religpew_10==count$religpew_14,]

test <- cces %>% 
  filter(muchmore ==1) %>% 
  select(caseid, religpew_10, religpew_12,  religpew_14)

count <- test %>% group_by(religpew_10) %>% 
  count(religpew_14) %>% mutate(pct =  prop.table(n)) %>% ungroup(religpew_10)

defect <- count[count$religpew_10==count$religpew_14,]








